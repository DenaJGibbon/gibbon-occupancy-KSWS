library(stringr)
library(ggpubr)

Metadata <- read.csv("data/Acoustics wide array data input_KSWS_20240904.csv")
head(Metadata)

# Plot UTM coordinates with labels
ggplot(Metadata, aes(x = UTM.E, y = UTM.N, label = Plot)) +
  geom_point(color = "blue", size = 3) +
  geom_text(vjust = -1, color = "black") +
  labs(x = "UTM Easting", y = "UTM Northing", title = "UTM Coordinate Plot with Labels") +
  theme_minimal()


Metadata <- Metadata[,c("Plot", "Unit.name" ,"Habitat.type" )]


KSWSFiles <- list.files("C:/Users/djc426/Desktop/KSWS_WA_trial/gibbons", pattern = '.txt', recursive=T, full.names=T)
length(KSWSFiles)

KSWSFilesCombined <- data.frame()
for(a in 3002:length(KSWSFiles) ){
  print(a)
  TempFile <- read.delim(KSWSFiles[a])
  TempName <- basename(KSWSFiles[a])
  TempName <-str_split_fixed(TempName,pattern='.BirdNET',n=2)[,1]
  TempFile$TempName <- TempName
  KSWSFilesCombined <- rbind.data.frame(KSWSFilesCombined,TempFile)
}

write.csv(KSWSFilesCombined,'data/KSWSFilesCombined_gibbons.csv',row.names = F)


KSWSFilesCombinedSubset <- subset(KSWSFilesCombined,Common.Name !='nocall' & Confidence >= 0.95)
write.csv(KSWSFilesCombinedSubset,'data/KSWSFilesCombinedSubset_gibbons.csv',row.names = F)

table(KSWSFilesCombinedSubset$Species.Code)
hist(KSWSFilesCombinedSubset$Confidence)

TempVals <-str_split_fixed(KSWSFilesCombinedSubset$TempName,pattern='_',n=6)

KSWSFilesCombinedSubset$Recorder <- TempVals[,3]
KSWSFilesCombinedSubset$Date <- TempVals[,5]
KSWSFilesCombinedSubset$Hour <- substr(TempVals[,6],1,2)

KSWSFilesCombinedSubset$Month <- as.numeric(substr(KSWSFilesCombinedSubset$Date,5,6))
KSWSFilesCombinedSubset$Hour <- as.numeric((KSWSFilesCombinedSubset$Hour))

KSWSFilesCombinedSubset$Plot <- str_split_fixed(KSWSFilesCombinedSubset$Recorder, pattern = '-',n=2)[,2]

KSWSFilesCombinedSubset <- merge(KSWSFilesCombinedSubset,Metadata,"Plot")

KSWSFilesCombinedSubset$Habitat.type <- plyr::revalue(KSWSFilesCombinedSubset$Habitat.type,
              c('2'= 'Evergreen',
              '3' = 'DDF'))

ggpubr::gghistogram(data=KSWSFilesCombinedSubset,x='Month', facet.by="Common.Name",stat="count")

ggpubr::gghistogram(data=KSWSFilesCombinedSubset,x='Hour',stat="count",facet.by="Common.Name")+xlim(0,24)
ggpubr::gghistogram(data=KSWSFilesCombinedSubset,x='Hour', facet.by='Plot', 
                    fill="Habitat.type",stat="count")+xlim(0,24)


KSWSFilesCombinedSubset$Date <- as.Date(KSWSFilesCombinedSubset$Date, format = "%Y%m%d")
ggpubr::gghistogram(data=KSWSFilesCombinedSubset,x='Date', #facet.by='Plot', 
                    fill="Habitat.type",stat="count")


