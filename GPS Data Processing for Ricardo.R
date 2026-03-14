library(tidyverse)
library(ggplot2)

setwd("C:/Users/slee7/Documents/ICLR/Ricardo-Backend/Logs")

telem_data1 <- read.csv("fc_01_03_26_16_17_20_424511.csv", stringsAsFactors = F)

telem_data1 <- telem_data1[, c(18:26)]

telem_data1 <- telem_data1 %>%
  filter(fix == 3)

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

# Show Gra
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
  ggplot(aes(x = sat, y = tau_v_val, color = tau_v, linetype = tau_v)) +
  geom_line(aes(group = tau_v), linewidth = 1.5) +
  labs(
    x    = "Satellite",
    y    = "Time Constant (s)",
    title = "Time Constant for Velocity by Number of Satellites"
  ) +
  theme_bw()

# Variance Q

# Group for Graphing Q LLA
telem_data1_wide <- telem_data1 %>%
  group_by(sat) %>%
  pivot_longer(
    cols = c(q_lat, q_lng, q_alt),
    names_to = "q_lla",
    values_to = "q_lla_val"
  ) %>%
  ungroup()

# Group for Graphing Q Velocity
telem_data1_wide <- telem_data1 %>%
  group_by(sat) %>%
  pivot_longer(
    cols = c(q_vn, q_ve, q_vd),
    names_to = "q_v",
    values_to = "q_v_val"
  ) %>%
  ungroup()

# Graph Q LLA
telem_data1_wide %>%
  ggplot(aes(x = sat, y = q_val, color = q_lla, linetype = q_lla)) +
  geom_line(aes(group = q_lla), linewidth = 1.5) +
  labs(
    x    = "Satellite",
    y    = "Variance",
    title = "Variance for LLA by Number of Satellites"
  ) +
  theme_bw()

# Graph Q Velocity
telem_data1_wide %>%
  ggplot(aes(x = sat, y = tau_v_val, color = tau_v, linetype = tau_v)) +
  geom_line(aes(group = tau_v), linewidth = 1.5) +
  labs(
    x    = "Year",
    y    = "Variance",
    title = "Variance for Velocity by Number of Satellites"
  ) +
  theme_bw()