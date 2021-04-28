#This script uses Global Biodiversity Information Facility (GBIF) data to looks at species occurance across community observations, herbaria, and culture collection records. 
#The search was conditioned on looking across only "preserved specimens", "human observations", and "living specimin" for all fungi. 

#Citation: GBIF.org (27 April 2021) GBIF Occurrence Download https://doi.org/10.15468/dl.9733fq
#18,670,313 occurrences included in download before subsetting any information

#setwd
setwd("~")
library("rgbif")
library(data.table)
library(classInt)
library(RColorBrewer)
library(rworldmap)
library(ggplot2)

#see what cols you want to read in
#test<-fread("0262104-200613084148143.csv", sep = "\t", strip.white=TRUE, nrows = 10)

data <- fread("0262303-200613084148143.csv", sep = "\t", strip.white=TRUE, select = c("phylum", "genus", "countryCode", "basisOfRecord"))


#format data
#remove all with blank country codes
nrow(data) #18670289 records
data<- data[data$countryCode !="",]
nrow(data) #18180792 records

#split by type
preserved_specimen<- data[data$basisOfRecord == "PRESERVED_SPECIMEN",]
nrow(preserved_specimen) #6,583,270 records
human_observation<- data[data$basisOfRecord == "HUMAN_OBSERVATION",]
nrow(human_observation) #11,485,089 records
culture_collection<- data[data$basisOfRecord == "LIVING_SPECIMEN",]
nrow(culture_collection) #112,433 records

#get totals
preserved_specimen_table<- data.frame(table(preserved_specimen$countryCode))
human_observation_table<- data.frame(table(human_observation$countryCode))
culture_collection_table<- data.frame(table(culture_collection$countryCode))

#how many countries are represented in each dataset?
length(unique(preserved_specimen_table$Var1))
length(unique(human_observation_table$Var1))
length(unique(culture_collection_table$Var1))

##plot maps 

#create mapdatatype
preserved_specimen_table_mapob <- joinCountryData2Map(preserved_specimen_table
                            ,joinCode = "ISO2"
                            ,nameJoinColumn = "Var1"
                            ,mapResolution = "low")

human_observation_table_mapob <- joinCountryData2Map(human_observation_table
                             ,joinCode = "ISO2"
                             ,nameJoinColumn = "Var1"
                             ,mapResolution = "low")

culture_collection_table_mapob <- joinCountryData2Map(culture_collection_table
                                                     ,joinCode = "ISO2"
                                                     ,nameJoinColumn = "Var1"
                                                     ,mapResolution = "low")

#set class
preserved_specimen_table_mapob_classint <- classIntervals( preserved_specimen_table_mapob[["Freq"]]
                            ,n=6, style = "jenks")
preserved_specimen_table_mapob_classint_catMethod = preserved_specimen_table_mapob_classint[["brks"]]

human_observation_table_mapob_classint <- classIntervals(human_observation_table_mapob[["Freq"]]
                                                           ,n=6, style = "jenks")
human_observation_table_mapob_classint_catMethod = human_observation_table_mapob_classint[["brks"]]

culture_collection_table_mapob_classint <- classIntervals(culture_collection_table_mapob[["Freq"]]
                                                         ,n=6, style = "jenks")
culture_collection_table_mapob_classint_catMethod = culture_collection_table_mapob_classint[["brks"]]



#plot maps - herbaria collections
#set color
blues <- colorRampPalette(c("white", "#124558"))
blues_me<-blues(6)
#set new device 
mapDevice(device = "dev.new", rows = 3, columns = 1) #create world map 
#plot
mapParams_spec <- mapCountryData(preserved_specimen_table_mapob
                            ,nameColumnToPlot="Freq"
                            ,addLegend=FALSE
                            ,catMethod = preserved_specimen_table_mapob_classint_catMethod
                            ,colourPalette=blues_me,
                            mapTitle = "Preserved Specimens")

#add legend
do.call(addMapLegend
        ,c(mapParams_spec
           ,legendLabels="all"
           ,legendWidth=0.5
           ,legendIntervals="data"
           ,legendMar = 2))


#plot maps - observations
#set color
greens <- colorRampPalette(c("white", "#455448"))
greens_me<-greens(6)
#plot
mapParams_human <- mapCountryData(human_observation_table_mapob
                            ,nameColumnToPlot="Freq"
                            ,addLegend=FALSE
                            ,catMethod = human_observation_table_mapob_classint_catMethod
                            ,colourPalette=greens_me,
                            mapTitle = "Observations")
#adding legend
do.call(addMapLegend
        ,c(mapParams_human
           ,legendLabels="all"
           ,legendWidth=0.5
           ,legendIntervals="data"
           ,legendMar = 2))


#plot maps - culture collections
#set color
oranges <- colorRampPalette(c("white", "#A45725"))
oranges_me<-oranges(6)
#observation
mapParams_culture <- mapCountryData(culture_collection_table_mapob
                                  ,nameColumnToPlot="Freq"
                                  ,addLegend=FALSE
                                  ,catMethod = culture_collection_table_mapob_classint_catMethod
                                  ,colourPalette=oranges_me,
                                  mapTitle = "Culture Collections")
#adding legend
do.call(addMapLegend
        ,c(mapParams_culture
           ,legendLabels="all"
           ,legendWidth=0.5
           ,legendIntervals="data"
           ,legendMar = 2))



##make bar graphs 
#format data
preserved_specimen_phylum<- data.frame(table(preserved_specimen$phylum))
human_observation_phylum<- data.frame(table(human_observation$phylum))
culture_collection_phylum<- data.frame(table(culture_collection$phylum))

par(mfrow=c(3,1))
#dev.off()
#par(mar=c(5,6,4,1)+.3)
#make bar graphs 

xx<- barplot(height = preserved_specimen_phylum$Freq,
        names = preserved_specimen_phylum$Var1, 
        col="black", 
        las = 2)
text(x = xx, y = preserved_specimen_phylum$Freq, label = preserved_specimen_phylum$Freq, pos = 3, cex = 0.8, col = "black", xpd=NA)


yy<- barplot(height = human_observation_phylum$Freq,
             names = human_observation_phylum$Var1, 
             col="black", 
             las = 2)
text(x = yy, y = human_observation_phylum$Freq, label = human_observation_phylum$Freq, pos = 3, cex = 0.8, col = "black", xpd=NA)


zz<- barplot(height = culture_collection_phylum$Freq,
             names = culture_collection_phylum$Var1, 
             col="black", 
             las = 2)
text(x = zz, y = culture_collection_phylum$Freq, label = culture_collection_phylum$Freq, pos = 3, cex = 0.8, col = "black", xpd=NA)

ggsave("CB_review_diversity.pdf", width=6, height=4, units="in")

