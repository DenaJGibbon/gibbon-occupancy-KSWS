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

# Read in metadata --------------------------------------------------------
Metadata <- read.csv("data/Habitat reclassification_DJCupdated.csv")
Metadata$Plot <- paste0("T", sprintf("%02d", as.integer(Metadata$Plot)))
Metadata <- Metadata[,c("Plot","Habitat.type" )]
head(Metadata)

# Read in data --------------------------------------------------------
DetectionSummaryGibbon <-
  read.csv('data/DetectionSummaryGibbon.csv')

# Subset based on time frame when have > 20 units running
DetectionSummaryGibbon45Days <- subset(DetectionSummaryGibbon, Date >= "2024-04-06" & Date <= "2024-07-23")
table(DetectionSummaryGibbon45Days$Date)

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
  select(Plot, Habitat.type) %>%
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
  Rain= fm_rain
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

# Get the unique habitat types used in the model
habitats <- unique(site.covs$Habitat.type)

# Create a grid of rain and habitat combinations
new_data <- expand.grid(
  rain = seq(0, 100, by = 1),
  Habitat.type = habitats
)

# Predict
pred <- predict(fm_rain, type = "state", newdata = new_data)
pred$rain <- new_data$rain
pred$Habitat.type <- new_data$Habitat.type

ggline(data=pred,x = "rain", y = "Predicted",
       facet.by = 'Habitat.type')


# Occupancy vs number of consecutive survey days --------------------------------------
DetectionSummaryGibbon <- DetectionSummaryGibbon45Days

nsurvey.days <- 1:20
nsurveys <- 3

OccupancyDF <- data.frame()
for(b in 1:10){
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

    # Step 3: Generate non-consecutive indices (every nth day)
    step_size <- nsurvey.days[a]
    subsample_indices <- seq(start_index, length(all_dates), by = step_size)[1:nsurveys]
    if(length(na.omit(subsample_indices)) >= nsurveys){

      CombinedSurvey <- data.frame()
      for(a in 1: (length(subsample_indices)-1) ){
        SingleSurvey <- DetectionSummaryGibbon[DetectionSummaryGibbon$Date %in% all_dates[subsample_indices[a]: (subsample_indices[a+1]-1)],]

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

      }


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
      Occupancy <- plogis(fmsummary$state$Estimate)
      OccupancySE <- fmsummary$state$SE
      psi_pred <- cbind.data.frame(RainDetect,RainDetectSE,Occupancy,OccupancySE)
      # Add metadata for tracking
      psi_pred$nsurveydays <- step_size
      psi_pred$random <- b
      psi_pred$Season <- Season
      OccupancyDF <- rbind.data.frame(OccupancyDF,psi_pred)
    }
  }
}

head(OccupancyDF)

OccupancyDF <- na.omit(OccupancyDF)

ggerrorplot(data = OccupancyDF,
            x = 'nsurveydays',
            y = 'Occupancy',
            desc_stat = "mean_se",
            color = "black",
            ylab = "Occupancy Probability \n (Ψ)",
            xlab = "Number of Survey Days",
            title = "",
            add = "mean_se",
            facet.by = 'Season') +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 12)
  )

ggerrorplot(data = OccupancyDF,
            x = 'nsurveydays',
            y = 'RainDetect',
            desc_stat = "mean_se",
            color = "black",
            ylab = "Detection probability",
            xlab = "Number of Survey Days",
            title = "",
            add = "mean_se",
            facet.by = 'Season') +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 12)
  )

