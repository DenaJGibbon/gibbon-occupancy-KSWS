# Load library
library(dplyr)
library(readr)
library(stringr)
library(readxl)
library(lubridate)
library(unmarked)
library(ggpubr)

Metadata <- read.csv("data/Acoustics wide array data input_KSWS_20240904.csv")
head(Metadata)
Metadata <- Metadata[Metadata$Date.collect != "", ]
Metadata <- Metadata[,c("Plot", "Unit.name" ,"Habitat.type" )]


Effortcsv <- read.csv('/Volumes/DJC Files/KSWS Gibbon Occupancy/Dep01BirdNETDefaultAnalysisEffort.csv')
Effortcsv <- as.data.frame(Effortcsv[2:nrow(Effortcsv),])
colnames(Effortcsv) <- 'AnalysedFiles'

Effortcsv$Date <-
  str_split_i(Effortcsv$AnalysedFiles,'_',10)

Effortcsv$Date <-
  as.Date(Effortcsv$Date, format = "%Y%m%d")

Effortcsv$Time <-
  substr(str_split_i(Effortcsv$AnalysedFiles,'_',11),1,2)

Effortcsv$Plot <-
  str_split_i(Effortcsv$AnalysedFiles,'_',8)


HornbillDetectsKSWS <- read.csv('/Volumes/DJC Files/KSWS Gibbon Occupancy/grehor1/AllKSWSDep01GreatHornbill.csv')
nrow(HornbillDetectsKSWS)
range(HornbillDetectsKSWS$Confidence)

HornbillDetectsKSWS$Date <-
  str_split_i(HornbillDetectsKSWS$TempName,'_',5)

HornbillDetectsKSWS$Date <- as.Date(HornbillDetectsKSWS$Date, format = "%Y%m%d")

HornbillDetectsKSWS$Time <-
 substr(str_split_i(HornbillDetectsKSWS$TempName,'_',6),1,2)

# Step 1: Merge effort (complete) with detection data (may be missing)
EffortFull <- merge(Effortcsv, HornbillDetectsKSWS, by = c("Date", "Time", "Plot"), all.x = TRUE)

# Step 2: Label detections vs. no detections (assumes 'Species' is your detection column)
EffortFull$DetectionStatus <- ifelse(is.na(EffortFull$Common.Name), "No Detection", "Detection")
EffortFull$Plot <- str_remove_all(EffortFull$Plot, "WA-" )
# Optional: Quickly check summary
table(EffortFull$DetectionStatus)

library(dplyr)

DetectionSummary <- EffortFull %>%
  group_by(Date, Plot) %>%
  dplyr::summarise(HornbillPresent = any(DetectionStatus == "Detection")) %>%
  ungroup()

head(DetectionSummary)


DetectionSummary45Days <- subset(DetectionSummary, Date >= "2024-05-05" & Date <= "2024-06-05")

DetectionSummary45Days$DetectNum <-
ifelse(DetectionSummary45Days$HornbillPresent==TRUE,1,0)

ggpubr::ggbarplot(data=DetectionSummary45Days,
                    x='Date',y='DetectNum')

# Merge with metadata for Habitat.type
DetectionSummary45Days <- merge(DetectionSummary45Days, Metadata, by = "Plot")

DetectionSummary45Days$Habitat.type <- plyr::revalue(DetectionSummary45Days$Habitat.type,
                                                        c('2'= 'Evergreen',
                                                          '3' = 'DDF',
                                                          '5'= 'Grassland'))

length(unique(DetectionSummary45Days$Date))
# Fake data
R <- length(unique(DetectionSummary45Days$Plot)) # number of sites
J <- length(unique(DetectionSummary45Days$Date)) # number of visits
y <- matrix(c(
  1,1,0,
  0,0,0,
  1,1,1,
  1,0,1), nrow=R, ncol=J, byrow=TRUE)
y

site.covs <- data.frame(x1=1:4, x2=factor(c('A','B','A','B')))
site.covs

obs.covs <- list(
  x3 = matrix(c(
    -1,0,1,
    -2,0,0,
    -3,1,0,
    0,0,0), nrow=R, ncol=J, byrow=TRUE),
  x4 = matrix(c(
    'a','b','c',
    'd','b','a',
    'a','a','c',
    'a','b','a'), nrow=R, ncol=J, byrow=TRUE))
obs.covs

umf <- unmarkedFrameOccu(y=y, siteCovs=site.covs,
                         obsCovs=obs.covs)   # organize data
umf                     # look at data
summary(umf)            # summarize
fm <- occu(~1 ~1, umf)  # fit a model


library(plyr)
library(unmarked)

DetectionSummary45Days <-
  droplevels(subset(DetectionSummary45Days,Habitat.type!='Grassland'))


# Step 3: Get dimensions
R <- length(unique(DetectionSummary45Days$Plot))  # number of sites
J <- length(unique(DetectionSummary45Days$Date))  # number of occasions

# Step 4: Create detection matrix y (1 = detection, 0 = no detection)
# Assuming one row per Date-Plot combination and a column `DetectionPresent` exists
# Step 2: Pivot to wide format (rows = Plot, cols = Date)
y_wide <- DetectionSummary45Days %>%
  select(Plot, Date, DetectNum) %>%
  pivot_wider(names_from = Date, values_from = DetectNum, values_fill = 0) %>%
  arrange(Plot)

# Step 3: Extract detection matrix
y <- as.matrix(y_wide[, -1])  # remove Plot column
rownames(y) <- y_wide$Plot

site.covs <- DetectionSummary45Days %>%
  select(Plot, Habitat.type) %>%
  distinct() %>%                           # one row per plot
  arrange(Plot) %>%                        # match order of y_wide
  column_to_rownames("Plot")              # set Plot as rownames

library(unmarked)

umf <- unmarkedFrameOccu(y = y, siteCovs = site.covs)

# Fit null occupancy model (update formula as needed)
fm <- occu(~1 ~ Habitat.type, umf)

# View results
summary(fm)

