library(stringr)
library(dplyr)
Dep01Files <- list.files("S:/projects/2024_WCS_Cambodia_S1139/2024_WCS_Cambodia_S1139/S1139_Dep01_FLAC",
           recursive = TRUE, full.names = FALSE)


Dep01Filesbase <- basename(Dep01Files)

TempVals <-str_split_fixed(Dep01Filesbase,pattern='_',n=6)

Recorder <- TempVals[,3]
Date <- TempVals[,5]
Date <- as.Date(Date, format = "%Y%m%d")
Hour <- substr(TempVals[,6],1,2)

Month <- as.numeric(substr(Date,5,6))
Hour <- as.numeric((Hour))

Plot <- str_split_fixed(Recorder, pattern = '-',n=2)[,2]

CombinedRecordingDF <- cbind.data.frame(Plot,Date,Hour,Dep01Filesbase)
ggpubr::gghistogram(data=CombinedRecordingDF,x='Date', facet.by='Plot', 
                    fill="grey",stat="count")

head(CombinedRecordingDF)

# Count the number of unique plots per date
plot_count_per_date <- CombinedRecordingDF %>%
  group_by(Date) %>%
  summarise(NumPlots = n_distinct(Plot))

# Find the dates with the maximum number of plots
max_plots <- max(plot_count_per_date$NumPlots)
date_range <- plot_count_per_date %>%
  filter(NumPlots == max_plots)

# Have a period of 45 days where 36 units are recording
length(unique((date_range$Date)))
range(date_range$Date)

