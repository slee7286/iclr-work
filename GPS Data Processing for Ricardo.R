library(tidyverse)
library(ggplot2)

setwd("C:/Users/slee7/Documents/ICLR/Ricardo-Backend/Logs")

telem_data1 <- read.csv("fc_01_03_26_16_17_20_424511.csv", stringsAsFactors = F)

telem_data1 <- telem_data1[, c(18:26)]

telem_data1 <- telem_data1 %>%
  filter(fix == 3)

telem_data1 <- telem_data1 %>% # Calculates moving average
  mutate(
    lat = stats::filter(lat, rep(1/100,100), sides=1),
    lng = stats::filter(lng, rep(1/100,100), sides=1),
    alt = stats::filter(alt, rep(1/100,100), sides=1),
    v_n = stats::filter(v_n, rep(1/100,100), sides=1),
    v_e = stats::filter(v_e, rep(1/100,100), sides=1),
    v_d = stats::filter(v_d, rep(1/100,100), sides=1),
  )

telem_data1 <- na.omit(telem_data1)

telem_data1 <- telem_data1 %>%
  group_by(sat) %>%
  mutate(
    lat = lat - mean(lat),
    lng = lng - mean(lng),
    alt = alt - mean(alt),
    v_n = v_n - mean(v_n),
    v_e = v_e - mean(v_e),
    v_d = v_d - mean(v_d)
  ) %>%
  ungroup()

dt <- 0.02 # CAN Bus limit 20 ms

# LSE for phi

telem_data1 <- telem_data1 %>%
  group_by(sat) %>%
  mutate(
    lat_lead = lead(lat),
    lng_lead = lead(lng),
    alt_lead = lead(alt),
    vn_lead = lead(v_n),
    ve_lead = lead(v_e),
    vd_lead = lead(v_d)
    ) %>%
  mutate(
    phi_lat_num = lat * lat_lead,
    phi_lng_num = lng * lng_lead,
    phi_alt_num = alt * alt_lead,
    phi_vn_num = v_n * vn_lead,
    phi_ve_num = v_e * ve_lead,
    phi_vd_num = v_d * vd_lead
    ) %>%
  ungroup()

telem_data1 <- telem_data1 %>%
  group_by(sat) %>%
  mutate(
    phi_lat = sum(phi_lat_num, na.rm = T)/sum((lat)^2, na.rm = T),
    phi_lng = sum(phi_lng_num, na.rm = T)/sum((lng)^2, na.rm = T),
    phi_alt = sum(phi_alt_num, na.rm = T)/sum((alt)^2, na.rm = T),
    phi_vn = sum(phi_vn_num, na.rm = T)/sum((v_n)^2, na.rm = T),
    phi_ve = sum(phi_ve_num, na.rm = T)/sum((v_e)^2, na.rm = T),
    phi_vd = sum(phi_vd_num, na.rm = T)/sum((v_d)^2, na.rm = T)
  ) %>%
  mutate(
    tau_hat_lat = -dt / log(phi_lat),
    tau_hat_lng = -dt / log(phi_lng),
    tau_hat_alt = -dt / log(phi_alt),
    tau_hat_vn = -dt / log(phi_vn),
    tau_hat_ve = -dt / log(phi_ve),
    tau_hat_vd = -dt / log(phi_vd)
  ) %>%
  ungroup()

telem_data1 <- telem_data1 %>%
  group_by(sat) %>%
  mutate(
    eps_lat = lat_lead - (phi_lat * lat),
    eps_lng = lng_lead - (phi_lng * lng),
    eps_alt = alt_lead - (phi_alt * alt),
    eps_vn = vn_lead - (phi_vn * v_n),
    eps_ve = ve_lead - (phi_ve * v_e),
    eps_vd = vd_lead - (phi_vd * v_d)
    ) %>%
  mutate(
    q_lat = var(eps_lat, na.rm = T),
    q_lng = var(eps_lng, na.rm = T),
    q_alt = var(eps_alt, na.rm = T),
    q_vn = var(eps_vn, na.rm = T),
    q_ve = var(eps_ve, na.rm = T),
    q_vd = var(eps_vd, na.rm = T)
  ) %>%
  ungroup()

colnames(telem_data1)
telem_data1[1,27]

# Remove Unnecessary Data
telem_data1 <- telem_data1[,-c(10:27)]

# Time Constant (tau)

# Group for Graphing Time Constant LLA and Velocity
telem_data1_wide <- telem_data1 %>%
  pivot_longer(
    cols = starts_with("tau_hat"),
    names_to = "tau_type",
    values_to = "tau_val"
  ) %>%
  mutate(
    category = if_else(tau_type %in% c("tau_hat_lat","tau_hat_lng","tau_hat_alt"),
                       "LLA", "Velocity")
  )

# Show Relative Number of Data Points
telem_data1_wide <- telem_data1_wide %>%
  group_by(sat) %>%
  mutate(
    rows = sum(sat == sat)
  ) %>%
  ungroup()
  
max_rows <- as.numeric(max(telem_data1_wide$rows))

telem_data1_wide <- telem_data1_wide %>%
  group_by(sat) %>%
  mutate(
    rows = rows / max_rows * 200
  ) %>%
  ungroup()

# Graph Time Constant LLA
telem_data1_wide %>%
  filter(category == "LLA") %>%
  ggplot(aes(x = sat, y = tau_val, color = tau_type, linetype = tau_type)) +
  geom_line(aes(group = tau_type), linewidth = 1.5) +
  geom_point(aes(x = sat, y = rows), color = "red", size = 2) +
  labs(
    x    = "Satellite",
    y    = "Time Constant (s)",
    title = "Time Constant for LLA by Number of Satellites"
  ) +
  theme_bw()

# Graph Time Constant Velocity
telem_data1_wide %>%
  mutate(rows = rows / 200.0) %>%
  filter(category == "Velocity") %>%
  ggplot(aes(x = sat, y = tau_val, color = tau_type, linetype = tau_type)) +
  geom_line(aes(group = tau_type), linewidth = 1.5) +
  geom_point(aes(x = sat, y = rows), color = "red", size = 2) +
  labs(
    x    = "Satellite",
    y    = "Time Constant (s)",
    title = "Time Constant for Velocity by Number of Satellites"
  ) +
  theme_bw()

# Variance (q)

# Group for Graphing Time Constant LLA and Velocity
telem_data1_q <- telem_data1 %>%
  pivot_longer(
    cols = starts_with("q"),
    names_to = "q_type",
    values_to = "q_val"
  ) %>%
  mutate(
    category = case_when(
      q_type %in% c("q_lat","q_lng") ~ "LL",
      q_type == "q_alt" ~ "Alt",
      TRUE ~ "Velocity"
    )
  )

# Show Relative Number of Data Points
telem_data1_q <- telem_data1_q %>%
  group_by(sat) %>%
  mutate(
    rows = sum(sat == sat)
  ) %>%
  ungroup()

max_rows <- as.numeric(max(telem_data1_q$rows))

telem_data1_q <- telem_data1_q %>%
  group_by(sat) %>%
  mutate(
    rows = rows / max_rows
  ) %>%
  ungroup()

# Graph Variance LL
telem_data1_q %>%
  mutate(rows = rows * 1.095071e-10) %>%
  filter(category == "LL") %>%
  ggplot(aes(x = sat, y = q_val, color = q_type, linetype = q_type)) +
  geom_line(aes(group = q_type), linewidth = 1.5) +
  geom_point(aes(x = sat, y = rows), color = "red", size = 2) +
  labs(
    x    = "Satellite",
    y    = "Variance",
    title = "Variance for Latitude and Longitude by Number of Satellites"
  ) +
  theme_bw()

# Graph Variance Altitude
telem_data1_q %>%
  mutate(rows = rows * 1403242) %>%
  filter(category == "Alt") %>%
  ggplot(aes(x = sat, y = q_val, color = q_type, linetype = q_type)) +
  geom_line(aes(group = q_type), linewidth = 1.5) +
  geom_point(aes(x = sat, y = rows), color = "red", size = 2) +
  labs(
    x    = "Satellite",
    y    = "Variance",
    title = "Variance for Altitude by Number of Satellites"
  ) +
  theme_bw()

# Graph Variance Velocity
telem_data1_q %>%
  mutate(rows = rows * 10000) %>%
  filter(category == "Velocity") %>%
  ggplot(aes(x = sat, y = q_val, color = q_type, linetype = q_type)) +
  geom_line(aes(group = q_type), linewidth = 1.5) +
  geom_point(aes(x = sat, y = rows), color = "red", size = 2) +
  labs(
    x    = "Satellite",
    y    = "Variance",
    title = "Variance for Velocity by Number of Satellites"
  ) +
  theme_bw()
