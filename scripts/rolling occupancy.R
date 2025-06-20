library(unmarked)
library(dplyr)
library(tidyr)


# Roling 3 day ------------------------------------------------------------

DetectionSummaryGibbon$Date <- as.Date(DetectionSummaryGibbon$Date)
unique_dates <- sort(unique(DetectionSummaryGibbon$Date))

OccupancyResults <- data.frame()

# Loop over rolling 3-day windows
for (i in 1:(length(unique_dates) - 2)) {
  window_dates <- unique_dates[i:(i + 2)]

  survey_window <- DetectionSummaryGibbon %>%
    filter(Date %in% window_dates)

  # Summarize per site
  window_summary <- survey_window %>%
    group_by(Plot, Date) %>%
    summarise(
      DetectNum = as.integer(any(GibbonPresent == TRUE)),
      Habitat.type = first(Habitat.type),
      Rain = mean(PRECTOTCORR, na.rm = TRUE),
      .groups = "drop"
    )

  # Detection matrix
  y_wide <- window_summary %>%
    select(Plot, Date, DetectNum) %>%
    pivot_wider(names_from = Date, values_from = DetectNum, values_fill = NA) %>%
    arrange(Plot)

  y <- as.matrix(y_wide[, -1])
  rownames(y) <- y_wide$Plot

  site_covs <- window_summary %>%
    select(Plot, Habitat.type) %>%
    distinct() %>%
    arrange(Plot) %>%
    tibble::column_to_rownames("Plot")

  # Optional: observation covariate (rain)
  rain_cov <- window_summary %>%
    select(Plot, Date, Rain) %>%
    pivot_wider(names_from = Date, values_from = Rain, values_fill = 0) %>%
    arrange(Plot) %>%
    tibble::column_to_rownames("Plot")

  umf <- unmarkedFrameOccu(y = y, siteCovs = site_covs, obsCovs = list(rain = as.matrix(rain_cov)))

  # Fit model (simple: ~1 ~1)
  fm <- tryCatch(occu(~1 ~1, data = umf), error = function(e) NULL)

  if (!is.null(fm)) {
    summ <- summary(fm)
    psi <- plogis(summ$state$Estimate[1])
    det <- plogis(summ$det$Estimate[1])

    OccupancyResults <- rbind(OccupancyResults, data.frame(
      start_date = window_dates[1],
      end_date = window_dates[3],
      psi = psi,
      detect = det
    ))
  }
}

# View results
head(OccupancyResults)

# Make sure dates are Date objects
OccupancyResults$start_date <- as.Date(OccupancyResults$start_date)
OccupancyResults$end_date <- as.Date(OccupancyResults$end_date)

# Plot Occupancy (ψ) over time
p1 <- ggplot(OccupancyResults, aes(x = start_date, y = psi)) +
  geom_line(color = "darkblue") +
  geom_point(color = "darkblue") +
  scale_x_date(date_labels = "%b %d") +
  labs(title = "Rolling 3-Day Occupancy Estimates",
       x = "Survey Start Date", y = expression(Occupancy~(psi))) +
  theme_minimal()

# Plot Detection Probability (p) over time
p2 <- ggplot(OccupancyResults, aes(x = start_date, y = detect)) +
  geom_line(color = "darkred") +
  geom_point(color = "darkred") +
  scale_x_date(date_labels = "%b %d") +
  labs(title = "Rolling 3-Day Detection Probability",
       x = "Survey Start Date", y = "Detection Probability (p)") +
  theme_minimal()

# Display plots
library(patchwork)
p1 / p2



# 7 day rolling -----------------------------------------------------------

library(unmarked)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(zoo)

# Parameters
window_size <- 21
min_date <- as.Date(min(DetectionSummaryGibbon45Days$Date))
max_date <- as.Date(max(DetectionSummaryGibbon45Days$Date))
dates <- seq(min_date, max_date - window_size + 1, by = "day")

RollingResults <- data.frame()

for (d in dates) {
  d <- as.Date(d)  # force d to be a Date explicitly
  win_dates <- seq(d, d + window_size - 1, by = "day")
  # Filter data to the 7-day window
  Subset <- DetectionSummaryGibbon45Days %>%
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
p.occ.21 <- ggplot(RollingResults, aes(x = Date, y = psi)) +
  geom_line(color = "navy", linewidth = 1) +
  geom_point(color = "navy") +
  geom_smooth(method = "loess", se = TRUE, color = "black", span = 0.3) +
  labs(title = "Rolling 21-Day Occupancy Estimates",
       y = "Occupancy (ψ)", x = "Survey Start Date") +
  theme_minimal()


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
