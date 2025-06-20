library(unmarked)
library(tidyr)
library(bbmle)
library(stringr)
library(dplyr)
library(ggpubr)
library(lubridate)

Metadata <- read.csv("data/Acoustics wide array data input_KSWS_20240904.csv")
head(Metadata)

KSWSFilesCombinedTest <- read.csv('data/KSWSFilesCombined_gibbons.csv')
nrow(KSWSFilesCombinedTest)
range(KSWSFilesCombinedTest$Confidence)

KSWSFilesCombinedTest <- subset(KSWSFilesCombinedTest, Confidence > 0.9)
write.csv(KSWSFilesCombinedTest,
          'data/KSWSFilesCombined_gibbons_subsetover0.9conf.csv',
          row.names = FALSE)

# Plot UTM coordinates with labels
ggplot(Metadata, aes(x = UTM.E, y = UTM.N, label = Plot, color = Habitat.type)) +
  geom_point( size = 3) +
  geom_text(vjust = -1, color = "black") +
  labs(x = "UTM Easting", y = "UTM Northing", title = "") +
  theme_minimal()


Metadata <- Metadata[,c("Plot", "Unit.name" ,"Habitat.type" )]

# Check plot ID
table(str_split_fixed(KSWSFilesCombinedTest$TempName,pattern = '_',4)[,3])

#Check date
table(str_split_fixed(KSWSFilesCombinedTest$TempName,pattern = '_',7)[,5])

# Subset data to extract non-detections
KSWSFilesCombinedNoDetect <- subset(KSWSFilesCombinedTest, Common.Name == 'nocall' | Confidence < 0.95)
head(KSWSFilesCombinedNoDetect)

# Subset data to extract detections (Common.Name != 'nocall' and Confidence >= 0.95)
KSWSFilesCombinedDetect <- subset(KSWSFilesCombinedTest, Common.Name != 'nocall' & Confidence >=  0.95)
nrow(KSWSFilesCombinedDetect)

KSWSFilesCombinedNoDetect$Detect <- '0'
KSWSFilesCombinedDetect$Detect <- '1'

KSWSFilesCombinedDetect$Begin.Path <- str_replace_all(basename(KSWSFilesCombinedDetect$Begin.Path),
                pattern = "\\\\",
                replacement = "/")

KSWSFilesCombinedDetect$Begin.Path <- str_replace_all(basename(KSWSFilesCombinedDetect$Begin.Path),
                                                      pattern = ".flac",
                                                      replacement = ".wav")

KSWSFilesCombinedDetect$DetectionPath <-paste(KSWSFilesCombinedDetect$Begin.Time..s.,
      KSWSFilesCombinedDetect$End.Time..s.,
      KSWSFilesCombinedDetect$Begin.Path ,sep='_')

ShortDetectPaths <-
  str_split_fixed(Files,'CrestedGibbons_',n=2)[,2]

# Non-matching rows
# Flag matches
KSWSFilesCombinedDetect$DetectMatch <- ifelse(   KSWSFilesCombinedDetect$DetectionPath %in% ShortDetectPaths , 1, 0)

# Split into matched and unmatched
MatchedDetects <- KSWSFilesCombinedDetect %>% filter(DetectMatch == 1)
UnmatchedDetects <- KSWSFilesCombinedDetect %>% filter(DetectMatch == 0)
UnmatchedDetects

# Add a new column 'Detect' with binary values (0 for no detection, 1 for detection)
MatchedDetects$Detect <- '0'
UnmatchedDetects$Detect <- '1'

MatchedDetects_clean <- MatchedDetects %>% select(-DetectMatch,-DetectionPath)

UnmatchedDetects_clean <- UnmatchedDetects %>% select(-DetectMatch,-DetectionPath)

KSWSFilesCombinedNoDetect <- rbind.data.frame(KSWSFilesCombinedNoDetect,
                                              UnmatchedDetects_clean)

KSWSFilesCombinedDetect <- MatchedDetects_clean

# Combine the two subsets into a single data frame for occupancy modeling
GibbonOccupancyModel <- rbind.data.frame(KSWSFilesCombinedNoDetect, KSWSFilesCombinedDetect)

# Split the 'TempName' column using '_' as a delimiter to extract relevant information
TempVals <- str_split_fixed(GibbonOccupancyModel$TempName, pattern = '_', n = 6)

# Extract 'Recorder' (third element) and 'Date' (fifth element) from the split values
Recorder <- TempVals[, 3]
Date <- TempVals[, 5]

# Convert 'Date' to Date format (YYYYMMDD) for further analysis
Date <- as.Date(Date, format = "%Y%m%d")

# Extract 'Hour' (first two digits from the sixth element) and convert to numeric
Hour <- as.numeric(substr(TempVals[, 6], 1, 2))

# Extract 'Month' from the 'Date' for potential seasonal analysis
Month <- as.numeric(format(Date, "%m"))

# Extract 'Plot' information from the 'Recorder' using '-' as a delimiter
Plot <- str_split_fixed(Recorder, pattern = '-', n = 2)[, 2]

# Combine extracted variables into the final GibbonOccupancyModel data frame
GibbonOccupancyModel <- cbind.data.frame(Plot, Date, Hour, GibbonOccupancyModel)

# Assuming 'Detection' column contains 1 for detected, 0 for not detected
GibbonOccurrenceModel <- GibbonOccupancyModel %>%
  mutate(Occurrence = ifelse(Detect == 1, 1, 0))

# Create an unmarkedFrame for occurrence (single visit binary data)
occurrence_history <- GibbonOccurrenceModel$Occurrence
umf_occurrence <- unmarkedFrameOccu(y = occurrence_history,
                                    siteCovs = siteCovs)

# Fit a simple model for occurrence, e.g., using habitat type as a covariate
occurrence_model <- occu(formula = ~1 ~ Habitat.type, data = umf_occurrence)

# Summary of the occurrence model
summary(occurrence_model)

# Subset data to include only records within the date range (May 5 to June 18, 2024)
GibbonOccupancyModel45Days <- subset(GibbonOccupancyModel, Date >= "2024-05-05" & Date <= "2024-06-18")

# Confirm the date range of the final filtered data
range(GibbonOccupancyModel45Days$Date)

# Focus on hours with gibbons
GibbonOccupancyModel45Days$Detect <-
  ifelse(GibbonOccupancyModel45Days$Hour %in% c(5, 6, 7) & GibbonOccupancyModel45Days$Detect == 1, 1, 0)

head(GibbonOccupancyModel45Days)

# Ensure Detect is numeric
GibbonOccupancyModel45Days$Detect <- as.numeric(GibbonOccupancyModel45Days$Detect)

# Sum Detect values by Plot and Date
GibbonOccupancyModelByDay <- GibbonOccupancyModel45Days %>%
  group_by(Plot, Date) %>%
  summarise(TotalDetect = sum(Detect), .groups = 'drop')

# Check resulting structure
head(GibbonOccupancyModelByDay)

# Create BinaryDetect column based on TotalDetect
GibbonOccupancyModelByDay$BinaryDetect <-
  ifelse(GibbonOccupancyModelByDay$TotalDetect > 0, 1, 0)

# Merge with metadata for Habitat.type
GibbonOccupancyModelByDay <- merge(GibbonOccupancyModelByDay, Metadata, by = "Plot")

GibbonOccupancyModelByDay$Habitat.type <- plyr::revalue(GibbonOccupancyModelByDay$Habitat.type,
                                                        c('2'= 'Evergreen',
                                                          '3' = 'DDF',
                                                          '5'= 'Grassland'))

GibbonOccupancyModelByDay <-
  subset(GibbonOccupancyModelByDay,Habitat.type!='Grassland')


GibbonOccupancyModelByDay$Plot_single <- GibbonOccupancyModelByDay$Plot
GibbonOccupancyModelByDay$Plot <- paste(GibbonOccupancyModelByDay$Plot,
                                        GibbonOccupancyModelByDay$Habitat.type, sep='_')

ggerrorplot(data=GibbonOccupancyModelByDay,
            y='BinaryDetect',x='Habitat.type')

# GibbonOccupancyModelByDay <-
#   GibbonOccupancyModelByDay %>%
#   mutate(Habitat.type =
#            if_else(Plot_single %in%
#                      c("T09","T17",
#                        "T25","T26",
#                        "T27","T35"), "Mixed", Habitat.type))


table(GibbonOccupancyModelByDay$Habitat.type)


# 1. Make sure Date is in Date format
GibbonOccupancyModelByDay <- GibbonOccupancyModelByDay %>%
  mutate(Date = as.Date(Date))

# 2. Create a new column for Week number or week start date
GibbonOccupancyModelByDay <- GibbonOccupancyModelByDay %>%
  mutate(Week = floor_date(Date, unit = "week", week_start = 1))  # weeks starting on Monday

# 3. Summarize by Plot and Week (binary detect per week: 1 if detected on any day that week)
WeeklyDetection <- GibbonOccupancyModelByDay %>%
  group_by(Plot, Week, Habitat.type) %>%
  summarize(BinaryDetect = as.integer(any(BinaryDetect == 1)), .groups = 'drop')

# 4. Pivot wider into detection matrix
DetectionMatrixByWeek <- WeeklyDetection %>%
  pivot_wider(names_from = Week, values_from = BinaryDetect,
              values_fill = list(BinaryDetect = 0))

# Prepare site-level covariates (including Habitat.type)
siteCovs <- DetectionMatrixByWeek %>%
  select(Plot, Habitat.type)  # Selecting only the relevant columns


# Detection history (columns that are weeks)
detection_history <- DetectionMatrixByWeek %>%
  select(-Plot, -Habitat.type)

umf <- unmarkedFrameOccu(y = detection_history,
                         siteCovs = siteCovs)

model_without_covs <- occu(~1 ~ 1 , data = umf)
summary(model_without_covs)

model_with_covs <- occu(~1 ~ Habitat.type , data = umf)
summary(model_with_covs)

fl <- fitList(Null=model_without_covs, model_with_covs)
fl

ms <- modSel(fl, nullmod="Null")
ms

# Extract predicted occupancy probabilities from your model
predictions <- predict(model_with_covs, type = "state")  # Occupancy probabilities
predictions <- cbind.data.frame(predictions,siteCovs)

# Plot the occupancy probabilities for each habitat type

ggplot(predictions, aes(x = Habitat.type, y = Predicted )) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Habitat Type", y = "Estimated Occupancy Probability") +
  theme_minimal() +
  ggtitle("Occupancy Probability by Habitat Type")


# Get predicted detection probabilities
detection_predictions <- predict(model_with_covs, type = "det", se = TRUE)
detection_predictions <- cbind.data.frame(detection_predictions,siteCovs)
head(detection_predictions)

# Plot detection probabilities with confidence intervals
ggplot(detection_predictions, aes(x = Habitat.type, y = Predicted)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(
    x = "Habitat Type",
    y = "Predicted Detection Probability",
    title = "Predicted Detection Probability by Habitat Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

