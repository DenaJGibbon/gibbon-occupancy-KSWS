
# Load library -------------------------------------------------------------------------
#
library(dplyr)
library(readr)
library(stringr)
library(readxl)
library(lubridate)
library(unmarked)
library(ggpubr)
library(plyr)
library(unmarked)


# Read in metadata --------------------------------------------------------

Metadata <- read.csv("data/Acoustics wide array data input_KSWS_20240904.csv")
head(Metadata)
Metadata <- Metadata[Metadata$Date.collect != "", ]
Metadata <- Metadata[,c("Plot", "Unit.name" ,"Habitat.type" )]


# Read in effort files ----------------------------------------------------
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


# Read in verified detections ---------------------------------------------
HornbillDetectsKSWSList <-
  list.files('/Volumes/DJC Files/KSWS_acoustic/hornbill_detections/Positive/')

HornbillDetectsKSWS <- data.frame(TempName = HornbillDetectsKSWSList)

HornbillDetectsKSWS$Date <-
  str_split_i(HornbillDetectsKSWS$TempName,'_',8)

HornbillDetectsKSWS$Date <- as.Date(HornbillDetectsKSWS$Date, format = "%Y%m%d")

HornbillDetectsKSWS$Time <-
 substr(str_split_i(HornbillDetectsKSWS$TempName,'_',9),1,2)

HornbillDetectsKSWS$Plot <-
  str_split_i(HornbillDetectsKSWS$TempName,'_',6)

HornbillDetectsKSWS$Common.Name <- 'Detection'

gghistogram(data=HornbillDetectsKSWS,x='Date')

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


DetectionSummary45Days <-subset(DetectionSummary, Date >= "2024-04-05" & Date <= "2024-06-05")

DetectionSummary45Days$DetectNum <-
ifelse(DetectionSummary45Days$HornbillPresent==TRUE,1,0)

ggpubr::ggbarplot(data=DetectionSummary45Days,
                    x='Date',y='DetectNum')

# Merge with metadata for Habitat.type
DetectionSummary45Days <- merge(DetectionSummary45Days, Metadata, by = "Plot")

DetectionSummary45Days$Habitat.type <- as.factor(DetectionSummary45Days$Habitat.type)

DetectionSummary45Days$Habitat.type <- plyr::revalue(DetectionSummary45Days$Habitat.type,
                                                        c('2'= 'Evergreen',
                                                          '3' = 'DDF',
                                                          '5'= 'Grassland'))

DetectionSummary45Days <-
  droplevels(subset(DetectionSummary45Days,Habitat.type != 'Grassland' & Habitat.type != 'Chamka' ))

# Step 3: Get dimensions
R <- length(unique(DetectionSummary45Days$Plot))  # number of sites
J <- length(unique(DetectionSummary45Days$Date))  # number of occasions

# Step 4: Create detection matrix y (1 = detection, 0 = no detection)
# Assuming one row per Date-Plot combination and a column `DetectionPresent` exists
# Step 2: Pivot to wide format (rows = Plot, cols = Date)
y_wide <- DetectionSummary45Days %>%
  select(Plot, Date, DetectNum) %>%
  pivot_wider(names_from = Date, values_from = DetectNum, values_fill = NA) %>%
  arrange(Plot)

# Step 3: Extract detection matrix
y <- as.matrix(y_wide[, -1])  # remove Plot column
rownames(y) <- y_wide$Plot

site.covs <- DetectionSummary45Days %>%
  select(Plot, Habitat.type) %>%
  distinct(Plot, .keep_all = TRUE) %>%
  arrange(Plot) %>%
  tibble::column_to_rownames("Plot")


umf <- unmarkedFrameOccu(y = y, siteCovs = site.covs)

# Fit null occupancy model (update formula as needed)
fm_null <- occu(~1 ~1, umf)
summary(fm_null)

fm <- occu(~1 ~ Habitat.type , umf)
summary(fm)

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

