# Load library
library(dplyr)
library(readr)
library(stringr)
library(readxl)
library(lubridate)
library(unmarked)
library(ggpubr)


# Read in metadata --------------------------------------------------------


Metadata <- read.csv("data/Habitat reclassification_DJCupdated.csv")
Metadata$Plot <- paste0("T", sprintf("%02d", as.integer(Metadata$Plot)))
head(Metadata)
Metadata <- Metadata[,c("Plot","Habitat.type" )]

#KSWSFilesCombinedTest <- read.csv('data/KSWSFilesCombined_gibbons.csv')
nrow(KSWSFilesCombinedTest)
range(KSWSFilesCombinedTest$Confidence)
table(KSWSFilesCombinedTest$Common.Name)



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


DetectionSummaryGibbon <- EffortGibbonsFull %>%
  group_by(Date, Plot) %>%
  dplyr::summarise(GibbonPresent = any(DetectionStatus == "Detection")) %>%
  ungroup()

head(DetectionSummaryGibbon)


DetectionSummaryGibbon45Days <- DetectionSummaryGibbon#subset(DetectionSummaryGibbon, Date >= "2024-05-05" & Date <= "2024-06-05")

DetectionSummaryGibbon45Days$DetectNum <-
  ifelse(DetectionSummaryGibbon45Days$GibbonPresent==TRUE,1,0)

ggpubr::ggbarplot(data=DetectionSummaryGibbon45Days,
                  x='Date',y='DetectNum')

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
  distinct() %>%                           # one row per plot
  arrange(Plot) %>%                        # match order of y_wide
  column_to_rownames("Plot")              # set Plot as rownames

library(unmarked)

umf <- unmarkedFrameOccu(y = y, siteCovs = site.covs)

# Fit null occupancy model (update formula as needed)
fm_null <- occu(~1 ~1, umf)
summary(fm_null)

fm <- occu(~1 ~ Habitat.type, umf)

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

library(unmarked)
library(ggplot2)

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


# Try with 5-day sampling period ------------------------------------------
EffortGibbonsFull$Date <- as.Date(EffortGibbonsFull$Date)

# Step 1: Collapse to 5-day sampling windows
DetectionSummaryGibbon <- EffortGibbonsFull %>%
  group_by(Date, Plot) %>%
  summarise(GibbonPresent = any(DetectionStatus == "Detection"), .groups = "drop") %>%
  arrange(Plot, Date) %>%
  group_by(Plot) %>%
  mutate(Window = ceiling(row_number() / 5))  # 5-day blocks per plot

# Step 2: Summarise detection per 5-day window
DetectionSummaryGibbon5day <- DetectionSummaryGibbon %>%
  group_by(Plot, Window) %>%
  summarise(GibbonPresent = any(GibbonPresent), .groups = "drop") %>%
  mutate(DetectNum = ifelse(GibbonPresent, 1, 0))

DetectionSummaryGibbon <- EffortGibbonsFull %>%
  group_by(Date, Plot) %>%
  dplyr::summarise(GibbonPresent = any(DetectionStatus == "Detection")) %>%
  ungroup()

head(DetectionSummaryGibbon)


DetectionSummaryGibbon45Days <- subset(DetectionSummaryGibbon, Date >= "2024-05-05" & Date <= "2024-06-05")

DetectionSummaryGibbon45Days$DetectNum <-
  ifelse(DetectionSummaryGibbon45Days$GibbonPresent==TRUE,1,0)

ggpubr::ggbarplot(data=DetectionSummaryGibbon45Days,
                  x='Date',y='DetectNum')

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
# table(DetectionSummaryGibbon45Days$Plot,DetectionSummaryGibbon45Days$Habitat.type,
#       DetectionSummaryGibbon45Days$DetectNum)
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
  pivot_wider(names_from = Date, values_from = DetectNum, values_fill = 0) %>%
  arrange(Plot)

# Step 3: Extract detection matrix
y <- as.matrix(y_wide[, -1])  # remove Plot column
rownames(y) <- y_wide$Plot

site.covs <- DetectionSummaryGibbon45Days %>%
  select(Plot, Habitat.type) %>%
  distinct() %>%                           # one row per plot
  arrange(Plot) %>%                        # match order of y_wide
  column_to_rownames("Plot")              # set Plot as rownames

library(unmarked)

umf <- unmarkedFrameOccu(y = y, siteCovs = site.covs)

# Fit null occupancy model (update formula as needed)
fm_null <- occu(~1 ~1, umf)
summary(fm_null)

fm <- occu(~1 ~ Habitat.type, umf)

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

library(unmarked)
library(ggplot2)

# Step 1: Define new data for prediction
new_data <- data.frame(Habitat.type = c("NotSuitable",  "Evergreen"))

# Step 2: Get occupancy predictions from your fitted model
pred <- predict(fm, type = "state", newdata = new_data)
pred$Habitat.type <- new_data$Habitat.type

# Step 3: Plot using ggplot2
ggplot(pred, aes(x = Habitat.type, y = Predicted)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  ylab("Occupancy Probability") +
  xlab("Habitat Type") +
  theme_minimal()


