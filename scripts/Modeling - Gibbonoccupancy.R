# Load library
library(dplyr)
library(readr)
library(stringr)
library(readxl)
library(lubridate)
library(unmarked)
library(ggpubr)
library(nasapower)
library(zoo)
library(tidyr)
library(tibble)

# Read in metadata --------------------------------------------------------
Metadata <- read.csv("data/Habitat reclassification_DJCupdated.csv")
Metadata$Plot <- paste0("T", sprintf("%02d", as.integer(Metadata$Plot)))
Metadata <- Metadata[,c("Plot","Habitat.type" )]
head(Metadata)

# Read in data --------------------------------------------------------
DetectionSummaryGibbon <-
  read.csv('data/DetectionSummaryGibbon.csv')

# Subset based on time frame when have > 20 units running
table(DetectionSummaryGibbon$Date)
DetectionSummaryGibbon45Days <- DetectionSummaryGibbon#subset(DetectionSummaryGibbon, Date >= "2024-04-05" & Date <= "2024-08-12")
table(DetectionSummaryGibbon45Days$Date)

# Stop before monsoon
# DetectionSummaryGibbon45Days <- subset(DetectionSummaryGibbon, Date >= "2024-05-01" & Date <= "2024-06-01")
# table(DetectionSummaryGibbon45Days$Date)

# Convert number of detections to binary
DetectionSummaryGibbon45Days$DetectNum <-
  ifelse(DetectionSummaryGibbon45Days$GibbonPresent==TRUE,1,0)

# Merge with metadata for Habitat.type
DetectionSummaryGibbon45Days <- merge(DetectionSummaryGibbon45Days, Metadata, by = "Plot")
DetectionSummaryGibbon45Days$Habitat.type <- as.factor(DetectionSummaryGibbon45Days$Habitat.type)
DetectionSummaryGibbon45Days$Habitat.type <- plyr::revalue(DetectionSummaryGibbon45Days$Habitat.type,
                                                           c('2'= 'Evergreen',
                                                             '3' = 'NotSuitable',
                                                             '5'= 'NotSuitable',
                                                             '6'= 'NotSuitable'))

length(unique(DetectionSummaryGibbon45Days$Date))
#
rowSums( table(DetectionSummaryGibbon45Days$Plot,
               DetectionSummaryGibbon45Days$DetectNum))

#Remove T38 because only have one day
DetectionSummaryGibbon45Days <-
  subset(DetectionSummaryGibbon45Days, Plot!='T38')

rain_data <- read.csv('data/Dep01raindata.csv')

ggbarplot(data=rain_data,
          x='YYYYMMDD',y='PRECTOTCORR')

# Sort by date
rain_df <- rain_data %>% arrange(YYYYMMDD)

# Calculate 7-day rolling sum
rain_df <- rain_df %>%
  mutate(rollsum_7 = zoo::rollapply(PRECTOTCORR, width = 5, FUN = sum, align = "right", fill = NA))

# Find first day where 7-day sum exceeds threshold (e.g., 25 mm)
rain_start <- rain_df %>%
  filter(rollsum_7 > 100) %>%
  slice(1)

print(rain_start$YYYYMMDD)

DetectionSummaryGibbon45Days$Season <- ifelse(DetectionSummaryGibbon45Days$Date>= rain_start$YYYYMMDD,'Monsoon','Dry')

# Occupancy for extended survey - habitat effects -------------------------

# Step 3: Get dimensions
R <- length(unique(DetectionSummaryGibbon45Days$Plot))  # number of sites
J <- length(unique(DetectionSummaryGibbon45Days$Date))  # number of occasions

# Step 4: Create detection matrix y (1 = detection, 0 = no detection)
# Assuming one row per Date-Plot combination and a column `DetectionPresent` exists
# Step 2: Pivot to wide format (rows = Plot, cols = Date)
y_wide <- DetectionSummaryGibbon45Days %>%
  select(Plot, Date, DetectNum) %>%
  pivot_wider(names_from = Date, values_from = DetectNum, values_fill = NA) %>%
  arrange(Plot)

# Step 3: Extract detection matrix
y <- as.matrix(y_wide[, -1])  # remove Plot column
rownames(y) <- y_wide$Plot

site.covs <- DetectionSummaryGibbon45Days %>%
  select(Plot, Habitat.type, Season) %>%
  distinct(Plot, .keep_all = TRUE) %>%
  arrange(Plot) %>%
  tibble::column_to_rownames("Plot")

# Assume 'PRECTOTCORR' is daily rainfall per Plot and Date
# and that 'y' is your detection matrix: rows = plots, columns = days

# Get dates used in y
used_dates <- colnames(y)
used_plots <- rownames(y)

# Subset and reshape rainfall to match
rain.obs <- DetectionSummaryGibbon45Days %>%
  filter(Date %in% used_dates, Plot %in% used_plots) %>%
  arrange(Plot, Date) %>%
  select(Plot, Date, PRECTOTCORR) %>%
  pivot_wider(names_from = Date, values_from = PRECTOTCORR) %>%
  arrange(Plot) %>%
  tibble::column_to_rownames("Plot")

# Ensure column order matches y
rain.obs <- rain.obs[, colnames(y)]

umf <- unmarkedFrameOccu(y = y, siteCovs = site.covs, obsCovs = list(rain = as.matrix(rain.obs)))


# Occupancy ~ Habitat; Detection ~ Rainfall
fm_rain <- occu(~ rain+Habitat.type ~ Habitat.type, umf)
summary(fm_rain)

# Occupancy ~ Habitat; Detection ~ Rainfall
fm_season <- occu(~ Season+Habitat.type ~ Habitat.type, umf)
summary(fm_season)

# Fit null occupancy model (update formula as needed)
fm_null <- occu(~1 ~1, umf)
summary(fm_null)

fm <- occu(~1 ~ Habitat.type , umf)

fm_det_by_habitat <- occu(~ Habitat.type ~ Habitat.type, umf)

# View results
summary(fm_det_by_habitat)

fl <- fitList(
  Null = fm_null,
  OccOnly = fm,
  DetByHabitat = fm_det_by_habitat,
  Rain= fm_rain,
  Season=fm_season
)

ms <- modSel(fl, nullmod="Null")
ms

summary(fm_rain)

# Step 1: Define new data for prediction
new_data <- data.frame(Habitat.type = c("NotSuitable",  "Evergreen"))

# Step 2: Get occupancy predictions from your fitted model
pred <- predict(fm_rain, type = "state", newdata = new_data)
pred$Habitat.type <- new_data$Habitat.type

pred


# Step 3: Plot using ggplot2
ggplot(pred, aes(x = Habitat.type, y = Predicted)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  ylab("Occupancy Probability") +
  xlab("Habitat Type") +
  theme_minimal()

# Example matrix (replace with your real 'y' matrix if already loaded)
# If your matrix is called 'y', and is already in wide format (as shown), use this:
y_long <- y %>%
  as.data.frame() %>%
  rownames_to_column("Plot") %>%
  pivot_longer(-Plot, names_to = "Date", values_to = "Detection")

# Convert date strings to Date format
y_long$Date <- as.Date(y_long$Date)

# Plot heatmap
ggplot(y_long, aes(x = Date, y = Plot, fill = factor(Detection))) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_manual(
    values = c("0" = "gray90", "1" = "steelblue", "NA" = "white"),
    na.value = "white",
    name = "Detection",
    labels = c("No", "Yes")
  ) +
  labs(
    title = "",
    x = "Date",
    y = "Plot"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    legend.position = "right"
  )

# Occupancy vs number of consecutive survey days --------------------------------------
DetectionSummaryGibbon <- DetectionSummaryGibbon45Days

nsurvey.days <- 2:45
nsurveys <- 1

OccupancyDF <- data.frame()

for(b in 1:100){
  for(a in 1:length(nsurvey.days)){
    # Ensure Date is in Date format
    DetectionSummaryGibbon$Date <- as.Date(DetectionSummaryGibbon$Date)

    # Choose when have at least 20 units going
    StartDates <-
      unique(DetectionSummaryGibbon$Date)[unique(DetectionSummaryGibbon$Date) >= "2024-04-06" & unique(DetectionSummaryGibbon$Date) <=  "2024-07-23"  ]

    StartDates <- StartDates
    random_start <- sample(StartDates, 1)

    # Step 1: Get all unique dates
    all_dates <- sort(StartDates)

    # Step 2: Get index of random start in all dates
    start_index <- which(all_dates == random_start)

    # Step 3: try consecutive days
    step_size <- nsurvey.days[a]
    subsample_indices <- start_index:(start_index + step_size-1)

    if(length(na.omit(subsample_indices)) > 0){

      CombinedSurvey <- data.frame()

        SingleSurvey <- DetectionSummaryGibbon[DetectionSummaryGibbon$Date %in% all_dates[subsample_indices],]

        SingleSurveyPlotSummary <- SingleSurvey %>%
          group_by(Plot) %>%
          summarise(
            DetectNum = as.integer(any(GibbonPresent == TRUE)), # or max(DetectNum)
            TotalRain = sum(PRECTOTCORR, na.rm = TRUE),
            Habitat.type = first(Habitat.type),  # assuming same for each plot
            Date = first(Date),  # assuming same for each plot
            PRECTOTCORR = mean(PRECTOTCORR),
            .groups = "drop"
          )
        CombinedSurvey <- rbind.data.frame(CombinedSurvey,SingleSurveyPlotSummary)

      Season <- ifelse(all_dates[start_index] >= rain_start$YYYYMMDD,'Monsoon','Dry')

      # Merge with metadata for Habitat.type
      # Merge with metadata (optional if not filtering by habitat)
      # Optional: filter to Evergreen only if still desired
       CombinedSurvey <- droplevels(CombinedSurvey %>%
       filter(Habitat.type == "Evergreen"))  # or recoded 'Evergreen' if applicable

      # Detection matrix: wide format
      y_wide <- CombinedSurvey %>%
        select(Plot, Date, DetectNum) %>%
        tidyr::pivot_wider(names_from = Date, values_from = DetectNum, values_fill = NA) %>%
        arrange(Plot)

      y <- as.matrix(y_wide[, -1])  # remove Plot column
      rownames(y) <- y_wide$Plot

      site.covs <- CombinedSurvey %>%
        select(Plot, Habitat.type) %>%
        distinct(Plot, .keep_all = TRUE) %>%
        arrange(Plot) %>%
        tibble::column_to_rownames("Plot")

      # Assume 'PRECTOTCORR' is daily rainfall per Plot and Date
      # and that 'y' is your detection matrix: rows = plots, columns = days

      # Reshape rainfall to match y’s dimensions
      rain.obs <- CombinedSurvey %>%
        arrange(Plot, Date) %>%
        select(Plot, Date, PRECTOTCORR) %>%
        pivot_wider(names_from = Date, values_from = PRECTOTCORR) %>%
        arrange(Plot) %>%
        tibble::column_to_rownames("Plot")

      umf <- unmarkedFrameOccu(y = y, siteCovs = site.covs, obsCovs = list(rain = as.matrix(rain.obs)))

      # Occupancy ~ Habitat; Detection ~ Rainfall
      fm <- occu(~ rain ~ 1, umf)
      fmsummary <- (summary(fm))
      RainDetect <- fmsummary$det$Estimate[2]
      RainDetectSE <- fmsummary$det$SE[2]
      BaselineDetect <- plogis(fmsummary$det$Estimate[1])  # intercept only
      Occupancy <- plogis(fmsummary$state$Estimate)
      OccupancySE <- fmsummary$state$SE
      psi_pred <- cbind.data.frame(RainDetect,RainDetectSE,Occupancy,OccupancySE,BaselineDetect)
      # Add metadata for tracking
      psi_pred$nsurveydays <- step_size
      psi_pred$random <- b
      psi_pred$Season <- Season
      OccupancyDF <- rbind.data.frame(OccupancyDF,psi_pred)
    }
  }
}

head(OccupancyDF)

#OccupancyDF <- na.omit(OccupancyDF)

library(dplyr)
library(ggplot2)
library(ggpubr)


# Asymptote for occupancy -------------------------------------------------

# Average occupancy probability by replicate length
avg_psi <- OccupancyDF %>%
  group_by(nsurveydays) %>%
  summarise(mean_psi = median(Occupancy, na.rm = TRUE))

# Fit asymptotic nonlinear model
fit_psi <- nls(mean_psi ~ a * (1 - exp(-b * nsurveydays)), data = avg_psi,
               start = list(a = max(avg_psi$mean_psi), b = 0.1))

# Extract asymptote and effort to reach 95% of it
a_psi <- coef(fit_psi)["a"]
b_psi <- coef(fit_psi)["b"]
effort_95_psi <- -log(1 - 0.99) / b_psi

# Plot with asymptote line and effort threshold
ggplot(avg_psi, aes(x = nsurveydays, y = mean_psi)) +
  geom_point() +
  stat_function(fun = function(x) a_psi * (1 - exp(-b_psi * x)), color = "blue", size = 1.2) +
  geom_hline(yintercept = a_psi, linetype = "dashed", color = "darkgreen") +
  geom_vline(xintercept = effort_95_psi, linetype = "dotted", color = "red") +
  annotate("text", x = effort_95_psi + 1, y = 0.1, label = paste0("99% at ~", round(effort_95_psi, 1), " days"),
           color = "red", hjust = 0) +
  labs(title = "Asymptotic Fit of Occupancy Probability",
       x = "Number of Survey Days",
       y = "Occupancy Probability (Ψ)") +
  theme_minimal()


# Asymptote for detection -------------------------------------------------

library(ggpubr)
library(ggplot2)
library(dplyr)

# Fit asymptotic model per season
fit_asymptote_det <- function(df) {
  nls(
    BaselineDetect ~ a * (1 - exp(-b * nsurveydays)),
    data = df,
    start = list(a = max(df$BaselineDetect, na.rm = TRUE), b = 0.1),
    control = nls.control(maxiter = 100, warnOnly = TRUE)
  )
}

# Aggregate mean detection per day per season
summary_det <- OccupancyDF %>%
  group_by(Season, nsurveydays) %>%
  summarise(BaselineDetect = mean(BaselineDetect, na.rm = TRUE), .groups = "drop")

# Fit models by season
asymptote_det_results <- summary_det %>%
  group_by(Season) %>%
  group_map(~{
    mod <- try(fit_asymptote_det(.x), silent = TRUE)
    if (inherits(mod, "try-error")) return(NULL)
    a <- coef(mod)["a"]
    b <- coef(mod)["b"]
    effort_95 <- -log(1 - 0.99) / b
    tibble(Season = .y$Season, a = a, b = b, effort_95 = effort_95)
  }) %>% bind_rows()

# Add label positions
label_df <- asymptote_det_results %>%
  mutate(label_a = paste0("Asymptote = ", round(a, 2)),
         label_effort = paste0("                   99% at ~", round(effort_95, 0), " days"))

# Final annotated plot
ggerrorplot(data = OccupancyDF,
            x = 'nsurveydays',
            y = 'BaselineDetect',
            desc_stat = "mean_se",
            color = "black",
            ylab = "Detection Probability (p)",
            xlab = "Number of Survey Days",
            title = "",
            add = "mean_se",
            facet.by = 'Season',
            nrow=2) +
  geom_hline(data = asymptote_det_results,
             aes(yintercept = a),
             linetype = "dashed",
             color = "darkgreen") +
  geom_vline(data = asymptote_det_results,
             aes(xintercept = effort_95),
             linetype = "dotted",
             color = "red") +
  geom_text(data = label_df,
            aes(x = 5, y = a + 0.02, label = label_a),
            color = "darkgreen", size = 3, inherit.aes = FALSE) +
  geom_text(data = label_df,
            aes(x = effort_95 + 1, y = min(a) - 0.15, label = label_effort),
            color = "red", size = 3, inherit.aes = FALSE) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 12)
  )

