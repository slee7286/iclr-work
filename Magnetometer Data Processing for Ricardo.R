library(tidyverse)

setwd("C:/Users/slee7/Documents/ICLR/Ricardo-Backend/Logs")

telem_data1 <- read.csv("fc_28_02_26_12_01_41_625255.csv", stringsAsFactors = F)

telem_data1 <- telem_data1[, c(36:38)]

telem_data1 <- telem_data1 %>% # Calculates moving average
  mutate(
    mx = stats::filter(mx, rep(1/100,100), sides=1),
    my = stats::filter(my, rep(1/100,100), sides=1),
    mz = stats::filter(mz, rep(1/100,100), sides=1)
  )

telem_data1 <- na.omit(telem_data1)

mx_avg <- mean(telem_data1$mx)
my_avg <- mean(telem_data1$my)
mz_avg <- mean(telem_data1$mz)


true_mx <- mx_avg # Assume no constant bias
true_my <- my_avg # Assume no constant bias
true_mz <- mz_avg # Assume no constant bias

mx_bias <- mx_avg - true_mx
my_bias <- my_avg - true_my
mz_bias <- mz_avg - true_mz

dt <- 0.02 # CAN Bus limit 20 ms

# LSE for phi

telem_data1[c("mx")] <- telem_data1[c("mx")] - mx_avg - mx_bias
telem_data1[c("my")] <- telem_data1[c("my")] - my_avg - my_bias
telem_data1[c("mz")] <- telem_data1[c("mz")] - mz_avg - mz_bias


telem_data1 <- telem_data1 %>%
  mutate(mx_lead = lead(mx),
         my_lead = lead(my),
         mz_lead = lead(mz)) %>%
  mutate(phi_mx_num = mx * mx_lead,
         phi_my_num = my * my_lead,
         phi_mz_num = mz * mz_lead)

phi_mx <- sum(telem_data1$phi_mx_num, na.rm = T)/sum((telem_data1$mx)^2, na.rm = T)
phi_my <- sum(telem_data1$phi_my_num, na.rm = T)/sum((telem_data1$my)^2, na.rm = T)
phi_mz <- sum(telem_data1$phi_mz_num, na.rm = T)/sum((telem_data1$mz)^2, na.rm = T)

tau_hat_mx <- -dt / log(phi_mx)
tau_hat_my <- -dt / log(phi_my)
tau_hat_mz <- -dt / log(phi_mz)

telem_data1 <- telem_data1 %>%
  mutate(eps_mx = mx_lead - (phi_mx * mx),
         eps_my = my_lead - (phi_my * my),
         eps_mz = mz_lead - (phi_mz * mz))

q_mx <- var(telem_data1$eps_mx, na.rm = T)
q_my <- var(telem_data1$eps_my, na.rm = T)
q_mz <- var(telem_data1$eps_mz, na.rm = T)

