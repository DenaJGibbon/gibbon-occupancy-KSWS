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

# Rain data ---------------------------------------------------------------
# rain_data <- get_power(
#   community = "RE",
#   lonlat = c(107.106802, 12.357878),
#   dates = c("2024-02-20", "2024-08-27"),
#   temporal_api = "DAILY",
#   pars = c("PRECTOTCORR", "T2M")
# )

#write.csv(rain_data,'data/Dep01raindata.csv')
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
  filter(rollsum_7 > 25) %>%
  slice(1)

print(rain_start$YYYYMMDD)

hist(rain_df$rollsum_7)
# Read in metadata --------------------------------------------------------
Metadata <- read.csv("data/Habitat reclassification_DJCupdated.csv")
Metadata$Plot <- paste0("T", sprintf("%02d", as.integer(Metadata$Plot)))
head(Metadata)
Metadata <- Metadata[,c("Plot","Habitat.type" )]

# Read in effort file to determine sampling period ------------------------

EffortGibbonscsv <- read.csv('/Volumes/DJC Files/KSWS Gibbon Occupancy/Dep01_GibbonKSWSeffort.csv')

EffortGibbonscsv <-
  as.data.frame(EffortGibbonscsv)

colnames(EffortGibbonscsv) <- 'TempName'

EffortGibbonscsv$Date <-
  str_split_i(EffortGibbonscsv$TempName,'_',10)

EffortGibbonscsv$Date <-
  as.Date(EffortGibbonscsv$Date, format = "%Y%m%d")

EffortGibbonscsv$Time <-
  substr(str_split_i(EffortGibbonscsv$TempName,'_',11),1,2)

EffortGibbonscsv$Plot <-
  str_split_i(EffortGibbonscsv$TempName,'_',8)

EffortGibbonscsv$Plot <- str_remove_all(EffortGibbonscsv$Plot,'WA-')

EffortGibbonscsv <- EffortGibbonscsv[,c("Date",
                          "Time", "Plot")]

table(EffortGibbonscsv$Plot)

# Read in sorted detections -----------------------------------------------

# Sorted detections
Files <-
  list.files('/Volumes/DJC Files/KSWS Gibbon Occupancy/Deop01/Detections/CrestedGibbons/above_0.96/Positive/',)

Plot <- str_split_fixed(Files,
                        pattern = '_',n=8)[,7]

Date <- str_split_fixed(Files,
                        pattern = '_',n=10)[,9]

Date <- as.Date(Date, format = "%Y%m%d")

Time <- substr(str_split_fixed(Files,
                               pattern = '_',n=10)[,10],1,2)

Plot <- str_split_fixed(Plot,
                        pattern = '-',n=2)[,2]

CombinedDF <- cbind.data.frame(Plot,Date,Time)

CombinedDF$Common.Name <-'CrestedGibbons'

CombinedDF$Date <- as.Date(CombinedDF$Date, format = "%Y-%m-%d")
CombinedDF$Date <- as.Date(as.character(CombinedDF$Date), format = "%Y-%m-%d")
#write.csv(CombinedDF,'data/gibbonverifieddetections0.96.csv',row.names =FALSE )

# Step 1: Merge EffortGibbons (complete) with detection data (may be missing)
EffortGibbonsFull <- merge(EffortGibbonscsv, CombinedDF, by = c("Date", "Time", "Plot"), all.x = TRUE)
head(EffortGibbonsFull)
table(EffortGibbonsFull$Common.Name)


# Step 2: Label detections vs. no detections (assumes 'Species' is your detection column)
EffortGibbonsFull$DetectionStatus <- ifelse(is.na(EffortGibbonsFull$Common.Name), "No Detection", "Detection")

# Optional: Quickly check summary
table(EffortGibbonsFull$DetectionStatus)

EffortGibbonsFull <-
  merge(EffortGibbonsFull,rain_data, by.x = 'Date', by.y = 'YYYYMMDD')

DetectionSummaryGibbon <- EffortGibbonsFull %>%
  group_by(Date, Plot,PRECTOTCORR) %>%
  dplyr::summarise(GibbonPresent = any(DetectionStatus == "Detection")) %>%
  ungroup()


DetectionSummaryGibbon45Days <- DetectionSummaryGibbon#subset(DetectionSummaryGibbon, Date >= "2024-05-05" & Date <= "2024-05-10")

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
# DetectionSummaryGibbon45Days <-
#   droplevels(subset(DetectionSummaryGibbon45Days,Habitat.type!='Grassland'))


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

# Reshape rainfall to match y’s dimensions
rain.obs <- DetectionSummaryGibbon45Days %>%
  arrange(Plot, Date) %>%
  select(Plot, Date, PRECTOTCORR) %>%
  pivot_wider(names_from = Date, values_from = PRECTOTCORR) %>%
  arrange(Plot) %>%
  tibble::column_to_rownames("Plot")

umf <- unmarkedFrameOccu(y = y, siteCovs = site.covs, obsCovs = list(rain = as.matrix(rain.obs)))

# Occupancy ~ Habitat; Detection ~ Rainfall
fm <- occu(~ rain ~ Habitat.type, umf)
summary(fm)



umf <- unmarkedFrameOccu(y = y, siteCovs = site.covs)

# Fit null occupancy model (update formula as needed)
fm_null <- occu(~1 ~1, umf)
summary(fm_null)

fm <- occu(~1 ~ Habitat.type + , umf)

fm_det_by_habitat <- occu(~ Habitat.type ~ Habitat.type, umf)

# View results
summary(fm_det_by_habitat)

fl <- fitList(
  Null = fm_null,
  OccOnly = fm,
  DetByHabitat = fm_det_by_habitat
)

ms <- modSel(fl, nullmod="Null")
ms

# Step 1: Define new data for prediction
new_data <- data.frame(Habitat.type = c("NotSuitable",  "Evergreen"))

# Step 2: Get occupancy predictions from your fitted model
pred <- predict(fm, type = "state", newdata = new_data)
pred$Habitat.type <- new_data$Habitat.type

pred

# Step 3: Plot using ggplot2
ggplot(pred, aes(x = Habitat.type, y = Predicted)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  ylab("Occupancy Probability") +
  xlab("Habitat Type") +
  theme_minimal()


# Occupancy vs number of consecutive survey days --------------------------------------
DetectionSummaryGibbon <- DetectionSummaryGibbon45Days

nsurvey.days <- 3:9

OccupancyDF <- data.frame()
for(b in 1:5){
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
  subsample_indices <- seq(start_index, length(all_dates), by = step_size)[1:5]
  if(length(na.omit(subsample_indices)) > 4){

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
  # CombinedSurvey <- CombinedSurvey %>%
  #   filter(Habitat.type == "2")  # or recoded 'Evergreen' if applicable

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
  fm <- occu(~ rain ~ Habitat.type, umf)
  summary(fm)

  # Predict occupancy (just one value)
  psi_pred <- predict(fm, type = "state")
  psi_pred <- psi_pred[1, ]
  # Add metadata for tracking
  psi_pred$nsurveydays <- step_size
  psi_pred$random <- b
  psi_pred$Season <- Season
  # # Estimate detection probability (p) from the fitted model
  # p_pred_det <- predict(fm_null, type = "det")
  #
  # # View result
  #
  # psi_pred$p_pred_det <- p_pred_det[1,1]

OccupancyDF <- rbind.data.frame(OccupancyDF,psi_pred)
}
}
}

head(OccupancyDF)

ggerrorplot(data = OccupancyDF,
            x = 'nsurveydays',
            y = 'Predicted',
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


