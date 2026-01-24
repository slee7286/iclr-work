library(tidyverse)

setwd("C:/Users/slee7/Documents/ICLR/Ricardo-Backend/Logs")

simulate_sensor <- function(T_true, P_true,
                            dt,
                            b_const,    # length-2: c(b_T, b_P)
                            tau,        # length-2: c(tau_T, tau_P)
                            q,          # length-2: c(q_T, q_P)
                            noise_sd,   # length-2: c(sd_T, sd_P)
                            use_quant = FALSE,
                            quant_step = c(0.01, 0.06))  # [K, Pa] if used
{
  # Ensure numeric vectors
  T_true <- as.numeric(T_true)
  P_true <- as.numeric(P_true)
  n      <- length(T_true)
  stopifnot(length(P_true) == n)
  
  # Preallocate
  T_meas <- numeric(n)
  P_meas <- numeric(n)
  
  # Bias state: [temperature_bias, pressure_bias]
  b_prev <- c(0, 0)
  
  # Precompute phi
  phi <- exp(-dt / tau)   # length-2
  
  for (k in seq_len(n)) {
    # --- Gauss–Markov drift update -------------------------------
    omega <- sqrt(q) * rnorm(2)          # N(0, q)
    b_drift <- phi * b_prev + omega      # elementwise
    b_prev  <- b_drift                   # persist
    
    # Composite bias
    b_comp <- b_const + b_drift          # length-2
    
    # Measurement noise
    noise <- noise_sd * rnorm(2)         # length-2
    
    # Apply to true values (order: [T, P])
    meas <- c(T_true[k], P_true[k]) + b_comp + noise
    
    # Optional quantisation
    if (use_quant) {
      meas <- quant_step * round(meas / quant_step)
    }
    
    T_meas[k] <- meas[1]
    P_meas[k] <- meas[2]
  }
  
  data.frame(T_meas = T_meas, P_meas = P_meas)
}

telem_data1 <- read.csv("fc_telem_24_01_26_11_57_43_821831.csv", stringsAsFactors = F)

telem_data1 <- telem_data1[, c(34:44)]

telem_data1 <- telem_data1[, -c(6:10)]

temp_avg <- mean(telem_data1$baro_temp)

pressure_avg <- mean(telem_data1$baro_press)

true_temp <- 300

true_pressure <- 99200

temp_bias <- temp_avg - true_temp

pressure_bias <- pressure_avg - true_pressure

dt <- 0.0276

# LSE for phi

telem_data1[c("baro_temp")] <- telem_data1[c("baro_temp")] - temp_bias

telem_data1[c("baro_press")] <- telem_data1[c("baro_press")] - pressure_bias

  
telem_data1 <- telem_data1 %>%
  mutate(baro_temp_lead = lead(baro_temp),
         baro_press_lead = lead(baro_press)) %>%
  mutate(phi_temp_num = baro_temp * baro_temp_lead,
         phi_press_num = baro_press * baro_press_lead)

phi_temp <- sum(telem_data1$phi_temp_num, na.rm = T)/sum((telem_data1$baro_temp)^2, na.rm = T)
  
phi_press <- sum(telem_data1$phi_press_num, na.rm = T)/sum((telem_data1$baro_press)^2, na.rm = T)

tau_hat_temp <- -dt / log(phi_temp)

tau_hat_press <- -dt / log(phi_press)

telem_data1 <- telem_data1 %>%
  mutate(eps_temp = baro_temp_lead - (phi_temp *baro_temp),
         eps_press = baro_press_lead - (phi_press *baro_press))

q_temp <- sd(telem_data1$eps_temp, na.rm = T)
q_press <- sd(telem_data1$eps_press, na.rm = T)

# Constants
P_surface <- 99438        # Pa
T_surface <- 288.15        # K
M_0 <- 28.9644             # kg/kmol
R <- 8.31446e3             # N·m/(kmol·K)
g_0 <- 9.80665             # m/s^2
L <- -0.0065               # K/m
h_0 <- 0                   # m


exponent = - (R * L) / (g_0 * M_0)

telem_data1 <- telem_data1 %>%
  mutate(pred_height = (R * baro_temp) / (g_0 * M_0) * log(P_surface / baro_press))

# Simulate adjusted temperature and pressure readings

b_const <- c(-1.9576, -64.3354)
tau <- c(46.6667, 46.6601)
q <- c(0.005501, 0.3838)
noise_sd <- c(0.01, 0.35)
quant_step <- c(0.01, 0.06)

sensor_out <- simulate_sensor(telem_data1$baro_temp, telem_data1$baro_press,
                              dt       = dt,
                              b_const  = b_const,
                              tau      = tau,
                              q        = q,
                              noise_sd = noise_sd,
                              use_quant = TRUE,
                              quant_step = quant_step)

sensor_out <- sensor_out %>%
  mutate(adj_pred_height = (R * T_meas) / (g_0 * M_0) * log(P_surface / P_meas))

