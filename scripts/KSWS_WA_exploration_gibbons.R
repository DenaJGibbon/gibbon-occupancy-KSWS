library(stringr)
library(ggpubr)

KSWSFiles <- list.files("C:/Users/djc426/Desktop/KSWS_WA_trial/gibbons", pattern = '.txt', recursive=T, full.names=T)
length(KSWSFiles)

KSWSFilesCombined <- data.frame()
for(a in 1:length(KSWSFiles) ){
  print(a)
  TempFile <- read.delim(KSWSFiles[a])
  TempName <- basename(KSWSFiles[a])
  TempName <-str_split_fixed(TempName,pattern='.BirdNET',n=2)[,1]
  TempFile$TempName <- TempName
  KSWSFilesCombined <- rbind.data.frame(KSWSFilesCombined,TempFile)
}

write.csv(KSWSFilesCombined,'data/KSWSFilesCombined_gibbons.csv',row.names = F)

Metadata <- read.csv("C:/Users/djc426/Desktop/KSWS_WA_trial/Acoustics wide array data input_KSWS_20240904.csv")
head(Metadata)

# Plot UTM coordinates with labels
ggplot(Metadata, aes(x = UTM.E, y = UTM.N, label = Plot)) +
  geom_point(color = "blue", size = 3) +
  geom_text(vjust = -1, color = "black") +
  labs(x = "UTM Easting", y = "UTM Northing", title = "UTM Coordinate Plot with Labels") +
  theme_minimal()

Metadata <- Metadata[,c("Plot", "Unit.name" ,"Habitat.type" )]
KSWSFilesCombined <- subset(KSWSFilesCombined,Common.Name !='nocall' & Confidence >= 0.95)
table(KSWSFilesCombined$Species.Code)
hist(KSWSFilesCombined$Confidence)

TempVals <-str_split_fixed(KSWSFilesCombined$TempName,pattern='_',n=6)

KSWSFilesCombined$Recorder <- TempVals[,3]
KSWSFilesCombined$Date <- TempVals[,5]
KSWSFilesCombined$Hour <- substr(TempVals[,6],1,2)

KSWSFilesCombined$Month <- as.numeric(substr(KSWSFilesCombined$Date,5,6))
KSWSFilesCombined$Hour <- as.numeric((KSWSFilesCombined$Hour))

KSWSFilesCombined$Plot <- str_split_fixed(KSWSFilesCombined$Recorder, pattern = '-',n=2)[,2]

KSWSFilesCombined <- merge(KSWSFilesCombined,Metadata,"Plot")

KSWSFilesCombined$Habitat.type <- plyr::revalue(KSWSFilesCombined$Habitat.type,
              c('2'= 'Evergreen',
              '3' = 'DDF'))

ggpubr::gghistogram(data=KSWSFilesCombined,x='Month', facet.by="Common.Name",stat="count")

ggpubr::gghistogram(data=KSWSFilesCombined,x='Hour',stat="count",facet.by="Common.Name")+xlim(0,24)
ggpubr::gghistogram(data=KSWSFilesCombined,x='Hour', facet.by='Plot', 
                    fill="Habitat.type",stat="count")+xlim(0,24)
