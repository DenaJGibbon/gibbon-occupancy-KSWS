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
  filter(rollsum_7 > 40) %>%
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
  list.files('/Volumes/DJC Files/KSWS Gibbon Occupancy/Deop01/Detections/CrestedGibbons/above_0.95/Positive/',)

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
#write.csv(CombinedDF,'data/gibbonverifieddetections0.95.csv',row.names =FALSE )

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

write.csv(DetectionSummaryGibbon,'data/DetectionSummaryGibbon.csv')

