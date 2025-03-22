library(unmarked)
library(tidyr)
library(bbmle)

# Subset data to extract non-detections (Common.Name == 'nocall' or Confidence < 0.95)
KSWSFilesCombinedNoDetect <- subset(KSWSFilesCombined, Common.Name == 'nocall' | Confidence < 0.98)
head(KSWSFilesCombinedNoDetect)

# Subset data to extract detections (Common.Name != 'nocall' and Confidence >= 0.95)
KSWSFilesCombinedDetect <- subset(KSWSFilesCombined, Common.Name != 'nocall' & Confidence >=  0.98)

# Add a new column 'Detect' with binary values (0 for no detection, 1 for detection)
KSWSFilesCombinedNoDetect$Detect <- '0'
KSWSFilesCombinedDetect$Detect <- '1'

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

# Subset data to include only records within the date range (May 5 to June 18, 2024)
GibbonOccupancyModel45Days <- subset(GibbonOccupancyModel, Date >= "2024-05-05" & Date <= "2024-06-18")

# Confirm the date range of the final filtered data
range(GibbonOccupancyModel45Days$Date)

head(GibbonOccupancyModel45Days)

library(dplyr)

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
                                 '3' = 'DDF'))
# Remove duplicates
GibbonOccupancyModelByDay <- 
  GibbonOccupancyModelByDay[-which(duplicated(GibbonOccupancyModelByDay)),]

# Create the Detection Matrix (using values_fill as a list)
DetectionMatrix <- GibbonOccupancyModelByDay %>%
  pivot_wider(names_from = Date, values_from = BinaryDetect, 
              values_fill = list(BinaryDetect = 0))

# Prepare site-level covariates (including Habitat.type)
siteCovs <- DetectionMatrix %>%
  select(Plot, Habitat.type)  # Selecting only the relevant columns

# Prepare the detection history (remove Plot column)
y <- DetectionMatrix %>%
  select(-Plot, -Habitat.type, -TotalDetect, -Unit.name)  # Remove Plot and Habitat.type from y

# Prepare site-level covariates (keep only Plot and Habitat.type)
siteCovs <- DetectionMatrix %>%
  select(Plot, Habitat.type)

# Create the unmarked frame
umf <- unmarkedFrameOccu(y = as.matrix(y), siteCovs = siteCovs)

# Fit the occupancy model
occ_model <- occu(~1 ~ Habitat.type, data = umf)

# View the model output
summary(occ_model)

# Fit the null model (no covariates for both detection and occupancy)
null_model <- occu(~1 ~1, data = umf)

# View the null model output
summary(null_model)

# AIC comparison will provide you with the AIC values for both models
# Lower AIC indicates a better fit
fl <- fitList(Null=null_model, occ_model)
fl

ms <- modSel(fl, nullmod="Null")
ms

