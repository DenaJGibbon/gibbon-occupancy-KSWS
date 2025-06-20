library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Factorial design: vary number of surveys and length of each survey (in consecutive days) ----------------------

DetectionSummaryGibbon <- DetectionSummaryGibbon45Days

survey_lengths <- 1:15        # number of consecutive days per survey
num_surveys_vec <- 3:10        # number of replicate surveys

OccupancyDF <- data.frame()

for (b in 1:10) {  # bootstrap replicate
  for (nsurvey_days in survey_lengths) {
    for (nsurveys in num_surveys_vec) {

      DetectionSummaryGibbon$Date <- as.Date(DetectionSummaryGibbon$Date)

      StartDates <- unique(DetectionSummaryGibbon$Date)
      StartDates <- StartDates[StartDates >= as.Date("2024-04-06") & StartDates <= as.Date("2024-07-23")]
      all_dates <- sort(StartDates)

      if (length(all_dates) < nsurvey_days * nsurveys) next

      random_start <- sample(all_dates[1:(length(all_dates) - nsurvey_days * nsurveys + 1)], 1)
      start_index <- which(all_dates == random_start)

      survey_indices <- lapply(0:(nsurveys - 1), function(i) {
        idx_start <- start_index + i * nsurvey_days
        idx_end <- idx_start + nsurvey_days - 1
        if (idx_end <= length(all_dates)) all_dates[idx_start:idx_end] else NULL
      })

      if (any(sapply(survey_indices, is.null))) next

      CombinedSurvey <- data.frame()
      for (survey_dates in survey_indices) {
        SingleSurvey <- DetectionSummaryGibbon[DetectionSummaryGibbon$Date %in% survey_dates, ]

        SingleSurveyPlotSummary <- SingleSurvey %>%
          group_by(Plot) %>%
          summarise(
            DetectNum = as.integer(any(GibbonPresent == TRUE)),
            TotalRain = sum(PRECTOTCORR, na.rm = TRUE),
            Habitat.type = first(Habitat.type),
            Date = first(Date),
            PRECTOTCORR = mean(PRECTOTCORR),
            .groups = "drop"
          )
        CombinedSurvey <- rbind(CombinedSurvey, SingleSurveyPlotSummary)
      }

      Season <- ifelse(random_start >= rain_start$YYYYMMDD, 'Monsoon', 'Dry')

      CombinedSurvey <- droplevels(CombinedSurvey %>% filter(Habitat.type == "Evergreen"))
      if (nrow(CombinedSurvey) == 0) next

      y_wide <- CombinedSurvey %>%
        select(Plot, Date, DetectNum) %>%
        pivot_wider(names_from = Date, values_from = DetectNum, values_fill = NA) %>%
        arrange(Plot)

      y <- as.matrix(y_wide[, -1])
      rownames(y) <- y_wide$Plot

      site.covs <- CombinedSurvey %>%
        select(Plot, Habitat.type) %>%
        distinct(Plot, .keep_all = TRUE) %>%
        arrange(Plot) %>%
        column_to_rownames("Plot")

      rain.obs <- CombinedSurvey %>%
        arrange(Plot, Date) %>%
        select(Plot, Date, PRECTOTCORR) %>%
        pivot_wider(names_from = Date, values_from = PRECTOTCORR) %>%
        arrange(Plot) %>%
        column_to_rownames("Plot")

      umf <- unmarkedFrameOccu(y = y, siteCovs = site.covs, obsCovs = list(rain = as.matrix(rain.obs)))

      fm <- occu(~ rain ~ 1, umf)
      fmsummary <- summary(fm)

      RainDetect <- fmsummary$det$Estimate[2]
      RainDetectSE <- fmsummary$det$SE[2]
      Occupancy <- plogis(fmsummary$state$Estimate)
      OccupancySE <- fmsummary$state$SE
      BaselineDetect <- plogis(fmsummary$det$Estimate[1])
      BaselineDetectSE <- fmsummary$det$SE[1]

      psi_pred <- cbind.data.frame(
        RainDetect, RainDetectSE, Occupancy, OccupancySE,
        nsurvey_days = nsurvey_days,
        nsurveys = nsurveys,
        random = b,
        Season = Season,
        BaselineDetect = BaselineDetect,
        BaselineDetectSE=BaselineDetectSE
      )

      OccupancyDF <- rbind(OccupancyDF, psi_pred)
    }
  }
}

head(OccupancyDF)


library(dplyr)

summary_stats <- OccupancyDF %>%
  group_by(nsurvey_days, nsurveys) %>%
  summarise(
    mean_psi = median(Occupancy, na.rm = TRUE),
    se_psi = median(OccupancySE, na.rm = TRUE),
    mean_rain_effect = mean(RainDetect, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(summary_stats, aes(x = nsurvey_days, y = nsurveys, fill = se_psi)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "SE(ψ)") +
  labs(
    x = "Length of Each Survey (Consecutive Days)",
    y = "Number of Surveys (Replicates)",
    title = "Precision of Occupancy Estimates (lower SE is better)"
  ) +
  theme_minimal()



ggplot(summary_stats, aes(x = nsurvey_days, y = nsurveys, z = se_psi)) +
  geom_contour_filled() +
  scale_fill_viridis_d(name = "SE(ψ)") +
  labs(
    title = "Occupancy Precision Across Survey Designs",
    x = "Survey Duration (days)",
    y = "Number of Replicates"
  )


ggline(data=OccupancyDF,x='nsurvey_days', y='Occupancy',
            color = 'nsurveys')

ggplot(OccupancyDF, aes(x = nsurvey_days, y = Occupancy)) +
  geom_point(alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3) +
  geom_smooth(method = "loess", se = TRUE, span = 0.5, color = "blue", linetype = "solid") +
  labs(title = "",
       x = "Survey Length (days)", y = "Initial Occupancy (ψ)") +
  facet_wrap(~ nsurveys, scales = "free_y") +
  theme_minimal()

ggplot(OccupancyDF, aes(x = nsurvey_days, y = Occupancy)) +
  geom_point(alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3) +
  geom_smooth(method = "loess", se = TRUE, span = 0.5, color = "blue", linetype = "solid") +
  labs(title = "",
       x = "Survey Length (days)", y = "Initial Occupancy (ψ)") +
  facet_wrap(~ nsurveys, scales = "free_y") +
  theme_minimal()


# Heatmaps ----------------------------------------------------------------


# Step 1: Summarize mean and SE by design
SummaryHeatmap <- OccupancyDF %>%
  group_by(nsurvey_days, nsurveys) %>%
  summarise(
    mean_psi = mean(Occupancy, na.rm = TRUE),
    se_psi = mean(OccupancySE, na.rm = TRUE),
    .groups = 'drop'
  )

# Step 2: Plot mean occupancy heatmap
p1 <- ggplot(SummaryHeatmap, aes(x = nsurvey_days, y = nsurveys, fill = mean_psi)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue", name = "Mean ψ") +
  labs(x = "Survey Length (days)", y = "Number of Surveys", title = "Mean Occupancy (ψ)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

# Step 3: Plot SE heatmap
p2 <- ggplot(SummaryHeatmap, aes(x = nsurvey_days, y = nsurveys, fill = se_psi)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "firebrick", name = "SE(ψ)") +
  labs(x = "Survey Length (days)", y = "Number of Surveys", title = "SE of Occupancy (ψ)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

# Step 4: Combine plots
p1 + p2 + plot_layout(ncol = 2)


# Asymptote detection prob ------------------------------------------------

# Average detection probability by replicate length
avg_p <- OccupancyDF %>%
  group_by(nsurvey_days) %>%
  summarise(mean_p = median(BaselineDetect, na.rm = TRUE))

# Fit asymptotic nonlinear model
fit <- nls(mean_p ~ a * (1 - exp(-b * nsurvey_days)), data = avg_p,
           start = list(a = max(avg_p$mean_p), b = 0.1))

# Extract asymptote and effort to reach 95% of it
a <- coef(fit)["a"]
b <- coef(fit)["b"]
effort_95 <- -log(1 - 0.95) / b

# Plot
ggplot(avg_p, aes(x = nsurvey_days, y = mean_p)) +
  geom_point() +
  stat_function(fun = function(x) a * (1 - exp(-b * x)), color = "blue", size = 1.2) +
  geom_hline(yintercept = a, linetype = "dashed", color = "darkgreen") +
  geom_vline(xintercept = effort_95, linetype = "dotted", color = "red") +
  labs(title = "Asymptotic Fit of Detection Probability",
       x = "Number of Survey Days", y = "Detection Probability") +
  theme_minimal()


