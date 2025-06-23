library(unmarked)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(patchwork)

DetectionSummaryGibbonOnlySuitable <- droplevels(subset(DetectionSummaryGibbon45Days,Habitat.type=='Evergreen'))
# 3 day rolling -----------------------------------------------------------

# Parameters
window_size <- 3
min_date <- as.Date(min(DetectionSummaryGibbonOnlySuitable$Date))
max_date <- as.Date(max(DetectionSummaryGibbonOnlySuitable$Date))
dates <- seq(min_date, max_date - window_size + 1, by = "day")

RollingResults <- data.frame()

for (d in dates) {
  d <- as.Date(d)  # force d to be a Date explicitly
  win_dates <- seq(d, d + window_size - 1, by = "day")
  # Filter data to the 3-day window
  Subset <- DetectionSummaryGibbonOnlySuitable %>%
    filter(Date %in% win_dates, Habitat.type == "Evergreen") %>%
    group_by(Plot, Date) %>%
    summarise(
      Detect = as.integer(any(GibbonPresent == TRUE)),
      Rain = mean(PRECTOTCORR, na.rm = TRUE),
      .groups = "drop"
    )

  # Pivot to detection matrix
  ymat <- Subset %>%
    select(Plot, Date, Detect) %>%
    pivot_wider(names_from = Date, values_from = Detect, values_fill = 0) %>%
    arrange(Plot)

  y <- as.matrix(ymat[,-1])
  rownames(y) <- ymat$Plot

  rainmat <- Subset %>%
    select(Plot, Date, Rain) %>%
    pivot_wider(names_from = Date, values_from = Rain, values_fill = 0) %>%
    arrange(Plot)

  rain <- as.matrix(rainmat[,-1])
  rownames(rain) <- rainmat$Plot
  mean_rain <- mean(rain, na.rm = TRUE)  # NEW: average rainfall across window

  umf <- unmarkedFrameOccu(y = y, obsCovs = list(rain = rain))
  mod <- occu(~ rain ~ 1, data = umf)
  mod_sum <- summary(mod)

  psi <- plogis(mod_sum$state$Estimate[1])
  p <- plogis(mod_sum$det$Estimate[1])
  RollingResults <- rbind(
    RollingResults,
    data.frame(Date = d, psi = psi, p = p, rain = mean_rain)
  )}

# Plot
p.occ.3 <- ggplot(RollingResults, aes(x = Date, y = psi)) +
  geom_line(color = "navy", linewidth = 1) +
  geom_point(color = "navy") +
  geom_smooth(method = "loess", se = TRUE, color = "black", span = 0.3) +
  labs(title = "Rolling 3-Day Occupancy Estimates",
       y = "Occupancy (ψ)", x = "Survey Start Date") +
  theme_minimal()+ylim(0.2,1)


p.det.3 <- ggplot(RollingResults, aes(x = Date,y=p)) +
  geom_line(aes(y = p), color = "darkred", linewidth = 1) +
  geom_point(aes(y = p), color = "darkred") +
  geom_smooth(method = "loess", se = TRUE, color = "black", span = 0.3) +

  labs(title = "Rolling 3-Day Detection Probability",
       y = "Detection Probability (p)", x = "Survey Start Date") +
  theme_minimal()

# Plot rainfall
p.rain.3 <- ggplot(RollingResults, aes(x = Date, y = rain)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "loess", se = TRUE, color = "black", span = 0.3) +
  labs(title = "Rolling 3-Day Mean Rainfall",
       y = "Rainfall (mm)", x = "Survey Start Date") +
  theme_minimal()

# Combine panels
(p.occ.3 / p.det.3 / p.rain.3)


# 7 day rolling -----------------------------------------------------------

# Parameters
window_size <- 7
min_date <- as.Date(min(DetectionSummaryGibbonOnlySuitable$Date))
max_date <- as.Date(max(DetectionSummaryGibbonOnlySuitable$Date))
dates <- seq(min_date, max_date - window_size + 1, by = "day")

RollingResults <- data.frame()

for (d in dates) {
  d <- as.Date(d)  # force d to be a Date explicitly
  win_dates <- seq(d, d + window_size - 1, by = "day")
  # Filter data to the 7-day window
  Subset <- DetectionSummaryGibbonOnlySuitable %>%
    filter(Date %in% win_dates, Habitat.type == "Evergreen") %>%
    group_by(Plot, Date) %>%
    summarise(
      Detect = as.integer(any(GibbonPresent == TRUE)),
      Rain = mean(PRECTOTCORR, na.rm = TRUE),
      .groups = "drop"
    )

  # Pivot to detection matrix
  ymat <- Subset %>%
    select(Plot, Date, Detect) %>%
    pivot_wider(names_from = Date, values_from = Detect, values_fill = 0) %>%
    arrange(Plot)

  y <- as.matrix(ymat[,-1])
  rownames(y) <- ymat$Plot

  rainmat <- Subset %>%
    select(Plot, Date, Rain) %>%
    pivot_wider(names_from = Date, values_from = Rain, values_fill = 0) %>%
    arrange(Plot)

  rain <- as.matrix(rainmat[,-1])
  rownames(rain) <- rainmat$Plot
  mean_rain <- mean(rain, na.rm = TRUE)  # NEW: average rainfall across window

  umf <- unmarkedFrameOccu(y = y, obsCovs = list(rain = rain))
  mod <- occu(~ rain ~ 1, data = umf)
  mod_sum <- summary(mod)

  psi <- plogis(mod_sum$state$Estimate[1])
  p <- plogis(mod_sum$det$Estimate[1])
  RollingResults <- rbind(
    RollingResults,
    data.frame(Date = d, psi = psi, p = p, rain = mean_rain)
  )}

# Plot
p.occ.7 <- ggplot(RollingResults, aes(x = Date, y = psi)) +
  geom_line(color = "navy", linewidth = 1) +
  geom_point(color = "navy") +
  geom_smooth(method = "loess", se = TRUE, color = "black", span = 0.3) +
  labs(title = "Rolling 7-Day Occupancy Estimates",
       y = "Occupancy (ψ)", x = "Survey Start Date") +
  theme_minimal()+ylim(0.2,1)


p.det.7 <- ggplot(RollingResults, aes(x = Date,y=p)) +
  geom_line(aes(y = p), color = "darkred", linewidth = 1) +
  geom_point(aes(y = p), color = "darkred") +
  geom_smooth(method = "loess", se = TRUE, color = "black", span = 0.3) +

  labs(title = "Rolling 7-Day Detection Probability",
       y = "Detection Probability (p)", x = "Survey Start Date") +
  theme_minimal()

# Plot rainfall
p.rain.7 <- ggplot(RollingResults, aes(x = Date, y = rain)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "loess", se = TRUE, color = "black", span = 0.3) +
  labs(title = "Rolling 7-Day Mean Rainfall",
       y = "Rainfall (mm)", x = "Survey Start Date") +
  theme_minimal()

# Combine panels
(p.occ.7 / p.det.7 / p.rain.7)

# 14 day rolling -----------------------------------------------------------

# Parameters
window_size <- 14
min_date <- as.Date(min(DetectionSummaryGibbonOnlySuitable$Date))
max_date <- as.Date(max(DetectionSummaryGibbonOnlySuitable$Date))
dates <- seq(min_date, max_date - window_size + 1, by = "day")

RollingResults <- data.frame()

for (d in dates) {
  d <- as.Date(d)  # force d to be a Date explicitly
  win_dates <- seq(d, d + window_size - 1, by = "day")
  # Filter data to the 7-day window
  Subset <- DetectionSummaryGibbonOnlySuitable %>%
    filter(Date %in% win_dates, Habitat.type == "Evergreen") %>%
    group_by(Plot, Date) %>%
    summarise(
      Detect = as.integer(any(GibbonPresent == TRUE)),
      Rain = mean(PRECTOTCORR, na.rm = TRUE),
      .groups = "drop"
    )

  # Pivot to detection matrix
  ymat <- Subset %>%
    select(Plot, Date, Detect) %>%
    pivot_wider(names_from = Date, values_from = Detect, values_fill = 0) %>%
    arrange(Plot)

  y <- as.matrix(ymat[,-1])
  rownames(y) <- ymat$Plot

  rainmat <- Subset %>%
    select(Plot, Date, Rain) %>%
    pivot_wider(names_from = Date, values_from = Rain, values_fill = 0) %>%
    arrange(Plot)

  rain <- as.matrix(rainmat[,-1])
  rownames(rain) <- rainmat$Plot
  mean_rain <- mean(rain, na.rm = TRUE)  # NEW: average rainfall across window

  umf <- unmarkedFrameOccu(y = y, obsCovs = list(rain = rain))
  mod <- occu(~ rain ~ 1, data = umf)
  mod_sum <- summary(mod)

  psi <- plogis(mod_sum$state$Estimate[1])
  p <- plogis(mod_sum$det$Estimate[1])
  RollingResults <- rbind(
    RollingResults,
    data.frame(Date = d, psi = psi, p = p, rain = mean_rain)
  )}

# Plot
p.occ.14 <- ggplot(RollingResults, aes(x = Date, y = psi)) +
  geom_line(color = "navy", linewidth = 1) +
  geom_point(color = "navy") +
  geom_smooth(method = "loess", se = TRUE, color = "black", span = 0.3) +
  labs(title = "Rolling 14-Day Occupancy Estimates",
       y = "Occupancy (ψ)", x = "Survey Start Date") +
  theme_minimal()+ylim(0.2,1)


p.det.14 <- ggplot(RollingResults, aes(x = Date,y=p)) +
  geom_line(aes(y = p), color = "darkred", linewidth = 1) +
  geom_point(aes(y = p), color = "darkred") +
  geom_smooth(method = "loess", se = TRUE, color = "black", span = 0.3) +

  labs(title = "Rolling 14-Day Detection Probability",
       y = "Detection Probability (p)", x = "Survey Start Date") +
  theme_minimal()

# Plot rainfall
p.rain.14 <- ggplot(RollingResults, aes(x = Date, y = rain)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "loess", se = TRUE, color = "black", span = 0.3) +
  labs(title = "Rolling 14-Day Mean Rainfall",
       y = "Rainfall (mm)", x = "Survey Start Date") +
  theme_minimal()

# Combine panels
(p.occ.14 / p.det.14 / p.rain.14)

# 21 day rolling -----------------------------------------------------------

# Parameters
window_size <- 21
min_date <- as.Date(min(DetectionSummaryGibbonOnlySuitable$Date))
max_date <- as.Date(max(DetectionSummaryGibbonOnlySuitable$Date))
dates <- seq(min_date, max_date - window_size + 1, by = "day")

RollingResults <- data.frame()

for (d in dates) {
  d <- as.Date(d)  # force d to be a Date explicitly
  win_dates <- seq(d, d + window_size - 1, by = "day")
  # Filter data to the 7-day window
  Subset <- DetectionSummaryGibbonOnlySuitable %>%
    filter(Date %in% win_dates, Habitat.type == "Evergreen") %>%
    group_by(Plot, Date) %>%
    summarise(
      Detect = as.integer(any(GibbonPresent == TRUE)),
      Rain = mean(PRECTOTCORR, na.rm = TRUE),
      .groups = "drop"
    )

  # Pivot to detection matrix
  ymat <- Subset %>%
    select(Plot, Date, Detect) %>%
    pivot_wider(names_from = Date, values_from = Detect, values_fill = 0) %>%
    arrange(Plot)

  y <- as.matrix(ymat[,-1])
  rownames(y) <- ymat$Plot

  rainmat <- Subset %>%
    select(Plot, Date, Rain) %>%
    pivot_wider(names_from = Date, values_from = Rain, values_fill = 0) %>%
    arrange(Plot)

  rain <- as.matrix(rainmat[,-1])
  rownames(rain) <- rainmat$Plot
  mean_rain <- mean(rain, na.rm = TRUE)  # NEW: average rainfall across window

  umf <- unmarkedFrameOccu(y = y, obsCovs = list(rain = rain))
  mod <- occu(~ rain ~ 1, data = umf)
  mod_sum <- summary(mod)
  print(mod_sum)
  psi <- plogis(mod_sum$state$Estimate[1])
  p <- plogis(mod_sum$det$Estimate[1])
  RollingResults <- rbind(
    RollingResults,
    data.frame(Date = d, psi = psi, p = p, rain = mean_rain)
  )}

# Plot
p.occ.21 <- ggplot(RollingResults, aes(x = Date, y = psi)) +
  geom_line(color = "navy", linewidth = 1) +
  geom_point(color = "navy") +
  geom_smooth(method = "loess", se = TRUE, color = "black", span = 0.3) +
  labs(title = "Rolling 21-Day Occupancy Estimates",
       y = "Occupancy (ψ)", x = "Survey Start Date") +
  theme_minimal()+ylim(0.2,1)


p.det.21 <- ggplot(RollingResults, aes(x = Date,y=p)) +
  geom_line(aes(y = p), color = "darkred", linewidth = 1) +
  geom_point(aes(y = p), color = "darkred") +
  geom_smooth(method = "loess", se = TRUE, color = "black", span = 0.3) +

  labs(title = "Rolling 21-Day Detection Probability",
       y = "Detection Probability (p)", x = "Survey Start Date") +
  theme_minimal()

# Plot rainfall
p.rain.21 <- ggplot(RollingResults, aes(x = Date, y = rain)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "loess", se = TRUE, color = "black", span = 0.3) +
  labs(title = "Rolling 21-Day Mean Rainfall",
       y = "Rainfall (mm)", x = "Survey Start Date") +
  theme_minimal()

# Combine panels
(p.occ.21 / p.det.21 / p.rain.21)


# Combine all  ------------------------------------------------------------

# Combine each rolling set of panels
combined_3  <- p.occ.3  / p.det.3  / p.rain.3  + plot_annotation(title = "3-Day Rolling Window")
combined_7  <- p.occ.7  / p.det.7  / p.rain.7  + plot_annotation(title = "7-Day Rolling Window")
combined_14 <- p.occ.14 / p.det.14 / p.rain.14 + plot_annotation(title = "14-Day Rolling Window")
combined_21 <- p.occ.21 / p.det.21 / p.rain.21 + plot_annotation(title = "21-Day Rolling Window")

library(patchwork)

# Label each column
combined_3  <- p.occ.3  / (p.det.3 +ylim(0,1))  / p.rain.3  + plot_annotation(title = "3-Day Rolling Window")
combined_7  <- p.occ.7  / (p.det.7 +ylim(0,1))  / p.rain.7  + plot_annotation(title = "7-Day Rolling Window")
combined_14 <- p.occ.14 / (p.det.14 +ylim(0,1)) / p.rain.14 + plot_annotation(title = "14-Day Rolling Window")
combined_21 <- p.occ.21 / (p.det.21 +ylim(0,1)) / p.rain.21 + plot_annotation(title = "21-Day Rolling Window")

# Combine side by side
final_plot <- combined_3 | combined_7 | combined_14 | combined_21

# Display
final_plot

