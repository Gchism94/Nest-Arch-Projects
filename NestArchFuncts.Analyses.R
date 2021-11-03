#############################################
## Autor: Greg CHISM
## Date: Aug 2021
## email: gchism@email.arizona.edu
## Project: Nest shape influences colony organization in ants
## Title: Raw coordinate transformations: Scaled cartesian coordinates & nest sections proportions 
#############################################

#This code is to replicate the analyses and figures for the following in my first chapter:
#Density of colony members in nest sections
#Netlogo simulation & worker comparisons
#Density figures

install.packages("pacman") #Download package with function to load multiple packaged at once
pacman::p_load(tidyverse, #Loading required packages for code below. p_load() will download packages that aren't in system library
               forcats,
               ggpubr,
               Kendall,
               kuiper.2samp,
               lme4,
               lmerTest,
               magick,
               magrittr,
               MuMIn,
               RColorBrewer,
               tidyverse,
               wesanderson,
               assertthat,
               twosamples,
               RColorBrewer,
               ggpointdensity,
               readxl)

data_names <- c("NetlogoTestFull", "NetlogoTestFullStandard",
                "NestArchProjRD2ColorBinsSep1", "NestArchProjRD2ColorBinsQueen",
                "NestArchProjRD2ColorBinsAlates", "FullDataCoordRD2",
                "AntPropFullWorkers", "AntPropFullWorkersRD2",
                "WorkerDistScaled", "WorkerDistScaledRD2",
                "WorkerDistScaledRD1_RD2", "AntPropFullBrood",
                "AntPropFullBroodRD2", "AntPropFullBroodRD1_RD2",
                "AntPropFullQueen", "AntPropFullQueenRD2",
                "AntPropFullQueenRD1_RD2", "AntPropFullAlateRD2",
                "AntPropFullSim",
                "WorkerSimPropFullRD1",
                "WorkerSimPropFullRD2",
                "WorkerSimPropAllSimRD1", "WorkerSimPropAllSimRD2",
                "AntPropFullWorkersSimRD1_RD2", "FidelityZonesRedRD1",
                "WorkerDistScaledRD1_RD2SFZFull", "FidelityZonesDataRD1_RD2",
                "DistBinsFull", "DistBinsFullNetlogo",
                "BinsNullFull", "BinsNullNetlogo",
                "WorkerDistScaledRD1_RD2SFZFullFid", 
                "BroodCentDistWorkersSFZFid",
                "BroodCentDistWorkersSFZ",
                "FullDataCoord",
                "FullDataCoordBrood",
                "FullDataCoordQueen", "AntPropFullBroodRD1_RD2",
                "MaxPropWorkersFull", "WorkerSim1TubePropAnalysis",
                "WorkerSim2TubePropAnalysisRD2", "WorkerSim1CirclePropAnalysis",
                "WorkerSim2CirclePropAnalysisRD2", "WorkerTubePropAnalysis",
                "WorkerTubePropAnalysisRD2", "WorkerCirclePropAnalysis",
                "WorkerCirclePropAnalysisRD2", "BroodDistScaled",
                "BroodDistScaledRD2", "BroodDistScaledRD1_RD2",
                "QueenDistScaled", "QueenDistScaledRD2",
                "QueenDistScaledRD1_RD2", "AlateDistScaledRD2Plot",
                "AlateDistScaledRD2", "SimDistScaled",
                "WorkerDistScaledMeanDist", "BroodCentDistWorkersRD1",
                "BroodCentDistWorkersRD2", "BroodCentDistWorkersRD1_RD2",
                "BroodCentDistQueensRD1", "BroodCentDistQueensRD2",
                "BroodCentDistQueensRD1_RD2", "BroodCentDistAlatesRD2",
                "CornerFull", "CornerFullSim"
)

for(i in 1:length(data_names)) {                              # Head of for-loop
  write.csv(get(data_names[i]),                              # Write CSV files to folder
             paste0("/Users/gregchism/Desktop/ArchitectureExp_Fall2017_Working/ImportData/NewExport/",
                    data_names[i],
                    ".csv"),
             row.names = FALSE)
}

library(plyr)
setwd("/Users/gregchism/Desktop/ArchitectureExp_Fall2017_Working")
mydir = "ImportData"
myfiles = list.files(path=mydir, pattern="*csv", full.names=TRUE)
dat_csv = ldply(myfiles, read_csv)
dat_csv

temp = list.files(pattern="*")
for (i in 1:length(myfiles)) assign(myfiles[i], read.csv(myfiles[i]))

WorkerDistScaled <- WorkerDistScaled.csv
WorkerDistScaledRD2<-WorkerDistScaledRD2.csv
BroodDistScaled<-BroodDistScaled.csv
BroodDistScaledRD2<-BroodDistScaledRD2.csv
QueenDistScaled<-QueenDistScaled.csv
QueenDistScaledRD2<-QueenDistScaledRD2.csv
AlateDistScaledRD2<-AlateDistScaledRD2.csv
NestArchProjRD2BroodBinsGC<-NestArchProjRD2BroodBinsGC.csv
NestArchProjRD2ColorBinsWorkers<-NestArchProjRD2ColorBinsWorkers.csv
NestArchProjRD2ColorBinsQueen<-NestArchProjRD2ColorBinsQueen.csv
NestArchProjRD2ColorBinsAlatesRatio<-NestArchProjRD2ColorBinsAlatesRatio.csv
BroodCentDistWorkersRD1<-BroodCentDistWorkersRD1.csv
BroodCentDistWorkersRD2<-BroodCentDistWorkersRD2.csv
BroodCentDistQueensRD1<-BroodCentDistQueensRD1.csv 
BroodCentDistQueensRD2<-BroodCentDistQueensRD2.csv 
BroodCentDistAlatesRD2<-BroodCentDistAlatesRD2.csv 
AntPropFullWorkers<-AntPropFullWorkers.csv 
AntPropFullRD2Workers<-AntPropFullRD2Workers.csv 
AntPropFullQueen<-AntPropFullQueen.csv
AntPropFullQueenRD2<-AntPropFullQueenRD2.csv
AntPropFullBrood<-AntPropFullBrood.csv
AntPropFullBroodRD2<-AntPropFullBroodRD2.csv
AntPropFullAlateRD2<-AntPropFullAlateRD2.csv 
WorkerDistScaledRD1SFZ<-WorkerDistScaledRD1SFZ.csv 
WorkerDistScaledRD2SFZ<-WorkerDistScaledRD2SFZ.csv 
BroodCentDistWorkersSFZRD1<-BroodCentDistWorkersSFZRD1.csv 
BroodCentDistWorkersSFZRD2<-BroodCentDistWorkersSFZRD2.csv 
FidelityZonesDataRD1<-FidelityZonesDataRD1.csv 
FidelityZonesDataRD2<-FidelityZonesDataRD2.csv 
FidelityZonesRedRD1<-FidelityZonesRedRD1 
FidelityZonesRedRD2<-FidelityZonesRedRD2 


write.csv(WorkerDistScaled,"WorkerDistScaled.csv",row.names=FALSE)
write.csv(WorkerDistScaledRD2,"WorkerDistScaledRD2.csv",row.names=FALSE)
write.csv(BroodDistScaled,"BroodDistScaled.csv",row.names=FALSE)
write.csv(BroodDistScaledRD2,"BroodDistScaledRD2.csv",row.names=FALSE)
write.csv(QueenDistScaled,"QueenDistScaled.csv",row.names=FALSE)
write.csv(QueenDistScaledRD2,"QueenDistScaledRD2.csv",row.names=FALSE)
write.csv(AlateDistScaledRD2,"AlateDistScaledRD2.csv",row.names=FALSE)
write.csv(NestArchProjRD2BroodBinsGC,"NestArchProjRD2BroodBinsGC.csv",row.names=FALSE)
write.csv(NestArchProjRD2ColorBinsWorkers,"NestArchProjRD2ColorBinsWorkers.csv",row.names=FALSE)
write.csv(NestArchProjRD2ColorBinsQueen,"NestArchProjRD2ColorBinsQueen.csv",row.names=FALSE)
write.csv(NestArchProjRD2ColorBinsAlatesRatio,"NestArchProjRD2ColorBinsAlatesRatio.csv",row.names=FALSE)
write.csv(BroodCentDistWorkersRD1,"BroodCentDistWorkersRD1.csv",row.names=FALSE)
write.csv(BroodCentDistWorkersRD2,"BroodCentDistWorkersRD2.csv",row.names=FALSE)
write.csv(BroodCentDistQueensRD1,"BroodCentDistQueensRD1.csv",row.names=FALSE)
write.csv(BroodCentDistQueensRD2,"BroodCentDistQueensRD2.csv",row.names=FALSE)
write.csv(BroodCentDistAlatesRD2,"BroodCentDistAlatesRD2.csv",row.names=FALSE)
write.csv(AntPropFullWorkers,"AntPropFullWorkers.csv",row.names=FALSE)
write.csv(AntPropFullRD2Workers,"AntPropFullRD2Workers.csv",row.names=FALSE)
write.csv(AntPropFullQueen,"AntPropFullQueen.csv",row.names=FALSE)
write.csv(AntPropFullQueenRD2,"AntPropFullQueenRD2.csv",row.names=FALSE)
write.csv(AntPropFullBrood,"AntPropFullBrood.csv",row.names=FALSE)
write.csv(AntPropFullBroodRD2,"AntPropFullBroodRD2.csv",row.names=FALSE)
write.csv(AntPropFullAlateRD2,"AntPropFullAlateRD2.csv",row.names=FALSE)
write.csv(WorkerDistScaledRD1SFZ,"WorkerDistScaledRD1SFZ.csv",row.names=FALSE)
write.csv(WorkerDistScaledRD2SFZ,"WorkerDistScaledRD2SFZ.csv",row.names=FALSE)
write.csv(BroodCentDistWorkersSFZRD1,"BroodCentDistWorkersSFZRD1.csv",row.names=FALSE)
write.csv(BroodCentDistWorkersSFZRD2,"BroodCentDistWorkersSFZRD2.csv",row.names=FALSE)
write.csv(FidelityZonesDataRD1,"FidelityZonesDataRD1.csv",row.names=FALSE)
write.csv(FidelityZonesDataRD2,"FidelityZonesDataRD2.csv",row.names=FALSE)
write.csv(FidelityZonesRedRD1,"FidelityZonesRedRD1",row.names=FALSE)
write.csv(FidelityZonesRedRD2,"FidelityZonesRedRD2",row.names=FALSE)
write.csv(WorkerSimPropFullRD1,"WorkerSimPropFullRD1.csv",row.names=FALSE)
write.csv(WorkerSimPropFullRD2,"WorkerSimPropFullRD2.csv",row.names=FALSE)
write.csv(WorkerDistScaledMeanDist,"WorkerDistScaledMeanDist.csv",row.names=FALSE)

save.image(file='NestArchEnvt_22_Apr_20.RData')

#SCALING COLONY MEMBER X AND Y COORDINATES
#This function is designed for imageJ or Fiji (just imageJ)
#The database used has the columns "Colony","Date","Day","Time","Nest", and "CoordID"
#as identifiers (see explaination in Chism_example_XYCorrected)
#Columns can be taken out of the function as needed (mainly in group_by())
#This function used a column called "CoordType" in the beginning
#In this column, each X and Y coordinate taken from imageJ is categorized as "ref1", "ref2", "coord"
#"ref1" corresponds to the first reference point you will want to take in imageJ (bottom left of nest)
#Whereas "ref1" corresponds to the second reference point (top left of nest)
#See example Chism_Examplephoto_XYCorrected
#The puspose is to use these reference coordinates with known distances (e.g. 7.5cm x 5.0cm)
#To scale all of the empirical XY marked objects (e.g. ants)
#The order of the reference coordinates NEED to be the same, else the function won't produce the correct points.
XYCorrected <- function(data_table) {
#First we need to assign the reference distance. In this example, its 5 for the 5cm width of the example nest.
  RefDistance = 5
#Here is a database management pipeline that removes any duplicate values of "CoordID" or any analagous column.
#The rationale behind this pipeline is to remove duplicate entries for an analyzed photo (from multiple people...)
  data_table <- data_table %>%
    group_by(Colony, Nest, Day) %>%
    distinct()
#Adding a new column and filling it with the condition, if ref1 then call the row xref1,
#Else call is xref2. Its not necessary to worry when those conditions are met.
  data_table["XCoordType"] <- NA 
  data_table$XCoordType <-
      ifelse(
        data_table$CoordType == "REF1", "xref1", "xref2"
      ) 
#Adding a new column and filling it with the condition, if ref1 then call the row yref1,
#Else call is yref2. Its not necessary to worry when those conditions are met.
  data_table["YCoordType"] <- NA 
  data_table$YCoordType<-
    ifelse(
      data_table$CoordType == "REF1", "yref1", "yref2"
      )
#Creating a new datatable with only reference coordinates
  RefTable <- data_table %>%
    filter(CoordType == "REF1" | CoordType == "REF2")
#Creating a new datatable with only Xref1 coordinates and renaming X and Y to "xref1" and "yref1"
  Xref1Table <- RefTable %>% 
    filter(XCoordType == "xref1") %>%
    group_by(Colony, Date, Day, Nest) %>%
    rename(xref1 = X, yref1 = Y) %>%
#After new data table is made, we need to remove the excess columns that categorize each X and Y 
    select(-c(CoordID, CoordType, XCoordType, YCoordType))
#Creating a new datatable with only Xref1 coordinates and renaming X and Y to "xref2" and "yref2"
  Xref2Table <- RefTable %>% 
    filter(XCoordType == "xref2") %>%
    rename(xref2 = X, yref2 = Y) %>%
#After new data table is made, we need to remove the excess columns that categorize each X and Y 
    select(-c(CoordID, CoordType, XCoordType, YCoordType))
#Next we full join the two new tables to create a column for each xref and yref. Since the identifiers are
#Consistent across all data tables, this sets up the next join.
  RefTableCrop <- left_join (Xref1Table, Xref2Table, by = c("Colony", "Date", "Nest", "Day"))
#Next we use a left join, since our main data table (x in the join) is the one we want to have the 
#New reference coordinates merged to based on our join qualifiers (by=)
  FullDataCoord <- left_join(data_table, RefTableCrop, by = c("Colony", "Date", "Nest", "Day"))
#The last steps are to scale the coordinates
#To do this, we use mutate to create a new column for the scaled X and Y coordinates
  FullDataCoord <- FullDataCoord %>%
#The normal origin in imageJ (or Fiji) is the top left
#To make ours the bottom left and scaled to the nest start, we need to remove the excess
#Distance on the X axis (X-xref1). This will make zero the left of the nest. 
#We then scale the distance by the reference distance. Since we know reference distance (plugged in at the top)
#We can subtract the largest and smallest Y axis reference coordinates and divide this number by our reference distance
#The scaled y coordinates is the same, except that, since the origin is the inverse of what we want
#We need to subtract our empirical Y coordinate from the yref coordinate. 
    mutate(ScaledX = ((X - xref1) * (RefDistance / ( yref1 - yref2))),
           ScaledY = (yref1 - Y) * (RefDistance / ( yref1 - yref2)))
  Colony8_10ColorUsableCoord <<- FullDataCoord %>%
    filter(CoordType != "REF1") %>%
    filter(CoordType != "REF2") %>%
    select(c(Colony, Nest, Date, Day, CoordID, CoordType, ScaledX, ScaledY, CoordType)) %>%
    rename(ColorID = CoordType)
#Lastly, we tidy up the database by removing reference coordinates. 
#Note, <<- sends the data table to the global environment, and is no longer usable for further 
#Manipulation in this function.
}
XYCorrected(Colony8_10ColorUsableTest)

#NETLOGO DATA IMPORT & PROCESSING
#Random
ArchitectureMoveModelFull_GC_30_Sept_2021_TestExp_Table <- read_excel("~/Desktop/NetLogoGC/ArchitectureMoveModelFull_GC_30.Sept.2021_TestExp_Table.xlsx", 
                                                                     sheet = "Working")

#Removing brackets from xcor and ycor lists
#Random
ArchitectureMoveModelFull_GC_30_Sept_2021_TestExp_Table<-ArchitectureMoveModelFull_GC_30_Sept_2021_TestExp_Table%>%
 mutate(xcor=gsub("\\[|\\]", "",xcor),
        ycor=gsub("\\[|\\]", "",ycor)) 


#Keep desired columns
#Random
ArchitectureMoveModelFull_GC_30_Sept_2021_TestExp_TableRed<-ArchitectureMoveModelFull_GC_30_Sept_2021_TestExp_Table%>%
  select(c(RunNumber,NestSize,Nest,MovementRule,TimeStep))

#Keeping only X and Y coordinates
#X coordinate
#Random
ArchitectureMoveModelFull_GC_30_Sept_2021_TestExp_TableXcor<-ArchitectureMoveModelFull_GC_30_Sept_2021_TestExp_Table%>%
  select(-c(ycor))

#Y coordinate
#Random
ArchitectureMoveModelFull_GC_30_Sept_2021_TestExp_TableYcor<-ArchitectureMoveModelFull_GC_30_Sept_2021_TestExp_Table%>%
  select(-c(xcor))


#Splitting the xcor and ycor lists
#Random
XCoordCorrect <- strsplit(ArchitectureMoveModelFull_GC_30_Sept_2021_TestExp_TableXcor$xcor, split = " ") #xcor
YCoordCorrect <- strsplit(ArchitectureMoveModelFull_GC_30_Sept_2021_TestExp_TableYcor$ycor, split = " ") #ycor

#Create a column that assigns unique IDs to each xcor and ycor row within each simulation run
#Random
NetlogoTestX<-data.frame(RunNumber = rep(ArchitectureMoveModelFull_GC_30_Sept_2021_TestExp_TableXcor$RunNumber, sapply(XCoordCorrect, length)), xcor = unlist(XCoordCorrect)) %>%
  rowid_to_column(var="id")

NetlogoTestY<-data.frame(RunNumber = rep(ArchitectureMoveModelFull_GC_30_Sept_2021_TestExp_TableYcor$RunNumber, sapply(YCoordCorrect, length)), ycor = unlist(YCoordCorrect)) %>%
  rowid_to_column(var="id")


#Creating the working Netlogo datasets
#Random
NetlogoTestFull<-left_join(NetlogoTestX,NetlogoTestY) %>%
  mutate(xcor=as.numeric(xcor),
         ycor=as.numeric(ycor)) %>%
  select(-c(id)) %>%
  left_join(ArchitectureMoveModelFull_GC_30_Sept_2021_TestExp_TableRed) %>%
  mutate(MovementRule = "Random",
         xcor = xcor * 0.1,
         ycor = ycor * 0.1) %>% 
rename(ScaledX = xcor, ScaledY = ycor)

NestArchProjRD1ColorBinsSep2

#Random
write.csv(NestArchProjRD1ColorBinsSep2,"NestArchProjRD1ColorBinsSep2.csv",row.names = FALSE)

#Separate color/ marked individuals 
FullDataCoord <- NestArchProjRD1WorkerBinsSFZ1Test %>%
  mutate(ColorID = ifelse(is.na(ColorID), "X,X,X,X",ColorID)) %>%
  separate(ColorID, c("Head", "Thorax", "Abd1", "Abd2"), sep = ",", remove = FALSE) %>%
  filter(Head != "Q") %>%
  select(Colony, Nest, Day, ScaledX, ScaledY, Bin, ColorID, Zone) %>%
  distinct()
NestArchProjRD1WorkerBinsSFZ1Test
NestArchProjRD1AntsCompleteColors <- full_join(NestArchFullRD1SFZWorking1, FullDataCoordQueen)
FullDataCoord <- NestArchProjRD1ColorBinsSep2 %>%
  select(-c(Head, Thorax, Abd1, Abd2))

#Subset workers
FullDataCoord <- NestArchProjRD1ColorBinsSep2Test
  filter(Head != "Q") %>%
  select(Colony, Nest, Day, ScaledX, ScaledY, Bin, ColorID)
  
  #Subset queens
  view(NestArchProjRD1ColorBinsSep2Test) 
FullDataCoordQueen <- NestArchProjRD1ColorBinsSep2Test %>%
  filter(Head == "Q") %>%
  select(Colony, Nest, Day, ScaledX, ScaledY, Bin)


#Separate color/ marked individuals 
NestArchProjRD2ColorBinsSep1 <- NestArchProjRd2AntsCompleteColors %>%
  separate(ColorID, c("Head", "Thorax", "Abd1", "Abd2"), sep = ",", remove = FALSE) %>%
  distinct()
write.csv(NestArchProjRD2ColorBinsQueen,"NestArchProjRD2ColorBinsQueen_test.csv",row.names = F)
#Subset queens
NestArchProjRD2ColorBinsQueen <- NestArchProjRD2ColorBinsSep1 %>%
  filter(Head == "Q")
NestArchProjRD2ColorBinsQueen
QueenDistScaledRD2
#Subset alates (RD2 only)
NestArchProjRD2ColorBinsAlates <- NestArchProjRD2ColorBinsSep1 %>%
  filter(Head == "A")
QueenDistScaledRD1_RD2
NestArchProjRD2ColorBinsAlates
AlateDistScaledRD2
#Subset workers
NestArchProjRd2AntsCompleteColors
NestArchProjRd2AntsCompleteColors <- FullDataCoordRD2 %>%
  full_join(NestArchProjRD2ColorBinsQueen) %>%
  full_join(NestArchProjRD2ColorBinsAlates) %>%
  select(-c(Head, Thorax, Abd1, Abd2, Sex, SexNumber, TotalNumber))
FullDataCoordRD2 <- NestArchProjRD2ColorBinsSep1 %>%
  filter(Head != "Q") %>%
  filter(Head != "A") %>%
  select(Colony, Nest, Day, ScaledX, ScaledY, Bin, CoordID, ColorID) 
view(FullDataCoordRD2)
WorkerDistScaledRD1_RD2
##For spatial fidelity zone calulations, please see file SFZFunctions.R 

###NEST SECTION DENSITY CALCULATIONS
#The following scripts calculate the proportion of colony members in each nest section (Column Bin: 1-8), calculated in the file ___. 
#The script uses a null dataset that's just Colony, Nest, and Bin called BinsNullFull, which just shows bin 1-8 for each Colony and nest combination
#First we create the proportions dataset, then a null one 
#This approach allows zeros to be present in the proportions datasets, since no workers in the nest section is relevant data.

#Prop function for workers RD1
#High density treatment
Prop_functionWorker<-function(data.table) {
  AntProp <- data.table %>% #Creating the dataset of worker proportions in each nest section
    group_by(Colony, Nest, Day)%>% #Group by columns Colony, Nest, Day
    mutate(count = n()) %>% #Count total number of workers in each observation
    group_by(Colony, Nest, Day, Bin) %>% #Group by columns Colony, Nest, Day, Bin
    mutate(BinCount = n(), #Counting the number of each worker in each bin in each observation
           PropWorker = (BinCount / count)) %>% #Calculate the proportion of workers in each bin 
    select(Colony, Day, Nest, Bin, PropWorker) %>% #Select only the desired columns
    distinct() #Remove duplicate rows
  #Creating the dataset of null worker proportions in each nest section
  AntPropNull <- AntProp %>%  
    ungroup() %>% #Ungroup the dataset
    select(c(Colony, Nest, Day)) %>% #Select the desired columns
    distinct() #Remove duplicate rows
  NestArchNullBins <- full_join(AntPropNull, BinsNullFull) #Join the two null datasets
  #Joining the working dataset to the null one, which keeps the zeros in the final dataset
  AntPropFullWorkers <<- full_join(NestArchNullBins, AntProp) %>%  
  group_by(Colony, Nest, Day) %>% #Group by columns Colony, Nest, and Day
    mutate(CoordType = "Obsv", #Create a column filled with "Obsv" which identifies this data as the real one vs. the Netlogo simunlations 
           PropWorker = ifelse(is.na(PropWorker), 0, PropWorker),#NAs are produced in the join above, this makes them zeros
           Density = "High", #Creates a column that assigns this dataset as the High density treatment
           Binsum = sum(PropWorker)) %>% #Create a column that sums the proportions
    filter(Binsum != 0) %>% #Removes any rows with zeros from the Binsum column. This is a precaution only
    select(Colony, Day, Nest, Bin, PropWorker, Density, CoordType) %>% #Select only the desired columns
    left_join(CornerFull) %>% #Joins with a dataset that assigned corner presence to each nest section
    drop_na() #Remove any NAs, also a precaution only
}

Prop_functionWorker(WorkerDistScaled)

#Prop function for workers RD2
#Same as above, but this is the Low density treatment
Prop_functionWorker <- function(data.table) {
  AntProp <- data.table %>%
    group_by(Colony, Nest, Day) %>%
    mutate(count = n()) %>%
    group_by(Colony, Nest, Day, Bin) %>%
    mutate(BinCount = n(),
           PropWorker = (BinCount / count)) %>%
    select(Colony, Day, Nest, Bin, PropWorker) %>%
    distinct()
  AntPropNull <- AntProp %>%
    ungroup() %>%
    select(c(Colony, Nest, Day)) %>%
    distinct()
  NestArchNullBins <- full_join(AntPropNull, BinsNullRd2)
  AntPropFullWorkersRD2 <<- full_join(NestArchNullBins, AntProp) %>%
    group_by(Colony, Nest, Day) %>%
    mutate(CoordType = "Obsv",
           PropWorker = ifelse(is.na(PropWorker), 0, PropWorker),
           Density = "Low",
           Binsum = sum(PropWorker))%>%
    filter(Binsum != 0) %>%
    select(Colony, Day, Nest, Bin, PropWorker, Density, CoordType) %>%
    left_join(CornerFull) %>%
    drop_na()
}
Prop_functionWorker(WorkerDistScaledRD2)

#Join worker proportions in nest sections datasets
AntPropFullWorkersRD1_RD2<-full_join(AntPropFullWorkers,AntPropFullWorkersRD2)
view(AntPropFullWorkersRD1_RD2)
#Prop function for Brood RD1
#Same function as workers, except the column "CoordType" = "Brood
#High density treatment
Prop_functionBrood <- function(data.table) {
  BroodProp <- data.table %>%
    group_by(Colony, Nest, Day) %>%
    mutate(count = n()) %>%
    group_by(Colony, Nest, Day, Bin) %>%
    mutate(BinCount = n(),
           PropBrood = (BinCount / count)) %>%
    select(Colony, Day, Nest, Bin, PropBrood) %>%
    distinct()
  BroodPropNull <- BroodProp %>%
    ungroup() %>%
    select(c(Colony, Nest, Day)) %>%
    distinct()
  NestArchNullBins <- full_join(BroodPropNull, BinsNull)
  AntPropFullBrood <<- full_join(NestArchNullBins, BroodProp) %>%
    group_by(Colony, Nest, Day) %>%
    mutate(CoordType = "Brood",
           PropBrood = ifelse(is.na(PropBrood), 0, PropBrood),
           Density = "High",
           Binsum = sum(PropBrood)) %>%
    filter(Binsum != 0) %>%
    select(Colony, Day, Nest, Bin, PropBrood, Density, CoordType) %>%
    group_by(Colony, Nest, Day) %>%
    left_join(CornerFull) %>%
    drop_na() 
}
Prop_functionBrood(BroodDistScaled)
view(AntPropFullBroodRD1_RD2 %>% filter(Nest == "Circle"))
#Prop function for Brood RD2
#Same as above, except this is the Low density treatment
Prop_functionBrood <- function(data.table) {
  BroodProp<-data.table %>%
    group_by(Colony, Nest, Day) %>%
    mutate(count = n()) %>%
    group_by(Colony, Nest, Day, Bin) %>%
    mutate(BinCount = n(),
           PropBrood = (BinCount / count)) %>%
    select(Colony, Day, Nest, Bin, PropBrood) %>%
    distinct()
  BroodPropNull <- BroodProp %>%
    ungroup() %>%
    select(c(Colony, Nest, Day)) %>%
    distinct()
  NestArchNullBins <- full_join(BroodPropNull, BinsNullRd2)
  AntPropFullBroodRD2 <<- full_join(NestArchNullBins, BroodProp)%>%
    group_by(Colony, Nest, Day) %>%
    mutate(CoordType = "Brood", 
           PropBrood = ifelse(is.na(PropBrood), 0, PropBrood),
           Density="Low",
           Binsum = sum(PropBrood)) %>%
    filter(Binsum != 0) %>%
    select(Colony, Day, Nest, Bin, PropBrood, Density, CoordType) %>%
    left_join(CornerFull) %>%
    drop_na() 
}

Prop_functionBrood(BroodDistScaledRD2)

#Join brood proportions in nest sections datasets
AntPropFullBroodRD1_RD2 <- full_join(AntPropFullBrood, AntPropFullBroodRD2) 

#Prop function for Queen RD1
#Same proportions function, except for queens datasets
#Here the "CoordType" column = "Queen"
#High density treatment
Prop_functionQueen <- function(data.table) {
  QueenProp <- data.table %>%
    group_by(Colony, Nest, Day) %>%
    mutate(count = n()) %>%
    group_by(Colony, Nest, Day, Bin) %>%
    mutate(BinCount = n(),
           PropQueen = (BinCount / count)) %>%
    select(Colony, Day, Nest, Bin, PropQueen) %>%
    distinct()
  QueenPropNull <- QueenProp %>%
    ungroup() %>%
    select(c(Colony, Nest, Day)) %>%
    distinct()
  NestArchNullBins <- full_join(QueenPropNull, BinsNull)
  AntPropFullQueen <<- full_join(NestArchNullBins, QueenProp) %>%
    group_by(Colony, Nest, Day) %>%
    mutate(CoordType = "Queen",
           PropQueen = ifelse(is.na(PropQueen), 0, PropQueen),
           Density = "High",
           Binsum = sum(PropQueen)) %>%
    filter(Binsum != 0) %>%
    select(Colony, Day, Nest, Bin, PropQueen, Density, CoordType) %>%
    left_join(CornerFull) %>%
    drop_na()
}
Prop_functionQueen(QueenDistScaled)

#Prop function for Queen RD2
Prop_functionQueen <- function(data.table) {
  QueenProp <- data.table %>%
    group_by(Colony, Nest, Day) %>%
    mutate(count = n()) %>%
    group_by(Colony, Nest, Day, Bin) %>%
    mutate(BinCount = n(),
           PropQueen = (BinCount / count)) %>%
    select(Colony, Day, Nest, Bin ,PropQueen) %>%
    distinct()
  QueenPropNull <- QueenProp %>%
    ungroup() %>%
    select(c(Colony, Nest, Day)) %>%
    distinct()
  NestArchNullBins <- full_join(QueenPropNull, BinsNullRd2)
  AntPropFullQueenRD2 <<- full_join(NestArchNullBins, QueenProp) %>%
    group_by(Colony, Nest, Day) %>%
    mutate(CoordType = "Queen",
           PropQueen = ifelse(is.na(PropQueen), 0, PropQueen),
           Density = "Low",
           Binsum = sum(PropQueen)) %>%
    filter(Binsum != 0) %>%
    select(Colony, Day, Nest, Bin, PropQueen, Density, CoordType) %>%
    left_join(CornerFull) %>%
    drop_na()
}
Prop_functionQueen(NestArchProjRD2ColorBinsQueen)

#Join queen proportions in nest sections datasets
AntPropFullQueenRD1_RD2 <- full_join(AntPropFullQueen, AntPropFullQueenRD2)

#Prop function for alates
#Same proportions function, except for the alate dataset
#Here the "CoordType" column = "Alate"
Prop_functionAlate <- function(data.table) {
  AlateProp <<- data.table %>%
    group_by(Colony, Nest, Day) %>%
    mutate(count = n()) %>%
    group_by(Colony, Nest, Day, Bin) %>%
    mutate(BinCount = n(),
           PropAlate = (BinCount / count)) %>%
    select(Colony, Day, Nest, Bin, PropAlate, Ratio) %>%
    distinct()
  AlatePropNull <- AlateProp %>%
    ungroup() %>%
    select(c(Colony, Nest, Day)) %>%
    distinct()
  NestArchNullBins <- full_join(AlatePropNull, BinsNullRd2)
  AntPropFullAlateRD2 <<- full_join(NestArchNullBins, AlateProp)%>%
    group_by(Colony, Nest, Day) %>%
    mutate(CoordType = "Alate",
           PropAlate = ifelse(is.na(PropAlate), 0, PropAlate),
           Binsum = sum(PropAlate)) %>%
    filter(Binsum != 0) %>%
    select(Colony, Day, Nest, Bin, PropAlate, Ratio) %>%
    left_join(CornerFull) %>%
    drop_na()
}

Prop_functionAlate(NestArchProjRD2ColorBinsAlatesRatio)

#NETLOGO SIMULATION BIN
#Prop function for simulations 
BinCoordNetlogo <- read_excel("~/Desktop/NetLogoGC/ArchitectureMoveModelFull_GC_11.Aug.2021_TestExp_Table.xlsx", 
                                sheet = "Bins")

BinsNullNetlogo <-read_excel("~/Desktop/NetLogoGC/ArchitectureMoveModelFull_GC_11.Aug.2021_TestExp_Table.xlsx", 
                             sheet = "BinsNull")

Prop_functionWorkerSim<-function(data.table) {
  NetlogoProp <- data.table %>%
    group_by(RunNumber, Nest, NestSize) %>%
    mutate(count = n()) %>%
    group_by(RunNumber, Nest, NestSize, Bin) %>%
    mutate(BinCount = n(),
           PropWorker = (BinCount / count)) %>%
    select(NestSize, Nest, Bin, RunNumber, PropWorker,TimeStep) %>%
    distinct()
  NetlogoPropNull <- NetlogoProp %>%
    ungroup() %>%
    select(c(RunNumber, Nest, NestSize)) %>%
    distinct()
  NestArchNullBins <- full_join(NetlogoPropNull, BinsNullNetlogo)
  AntPropFullSim <<- full_join(NestArchNullBins, NetlogoProp)%>%
    group_by(RunNumber, Nest, NestSize) %>%
    mutate(PropWorker = ifelse(is.na(PropWorker), 0, PropWorker),
           Binsum = sum(PropWorker),
           CoordType = "RandSim") %>%
    filter(Binsum != 0) %>%
  select(RunNumber, NestSize, Nest, Bin, PropWorker, CoordType, TimeStep) %>%
    left_join(CornerFullSim) %>%
    drop_na() %>%
    distinct()
}

Prop_functionWorkerSim(NetlogoBinnedFull)

#Random
WorkerSim1PropAnalysisSmall<-AntPropFullSim%>%filter(NestSize=="Small")
WorkerSim2PropAnalysisLarge<-AntPropFullSim%>%filter(NestSize=="Large")

#Workers
WorkerTubePropAnalysis<-AntPropFullWorkers%>%filter(Nest=="Tube")
WorkerCirclePropAnalysis<-AntPropFullWorkers%>%filter(Nest=="Circle")
WorkerTubePropAnalysisRD2<-AntPropFullWorkersRD2%>%filter(Nest=="Tube")
WorkerCirclePropAnalysisRD2<-AntPropFullWorkersRD2%>%filter(Nest=="Circle")

#Random
WorkerSim1TubePropAnalysis<-WorkerSim1PropAnalysisSmall%>%filter(Nest=="Tube")
WorkerSim1CirclePropAnalysis<-WorkerSim1PropAnalysisSmall%>%filter(Nest=="Circle")
WorkerSim2TubePropAnalysisRD2<-WorkerSim2PropAnalysisLarge%>%filter(Nest=="Tube")
WorkerSim2CirclePropAnalysisRD2<-WorkerSim2PropAnalysisLarge%>%filter(Nest=="Circle")

#Workers high density
MaxWorkerPropRD1 <- AntPropFullWorkers %>% 
  group_by(Colony,Nest,Day) %>%
  mutate(MaxPropWorker = max(PropWorker),MaxBin=ifelse(MaxPropWorker==PropWorker,1,0)) %>%
  select(-c(PropWorker, MaxPropWorker)) 

MaxWorkerPropRD1Tube<-MaxWorkerPropRD1%>%
  filter(Nest=="Tube")%>%
  select(Colony,Day,Nest,CoordType,Bin)

MaxWorkerPropRD1Circle<-MaxWorkerPropRD1%>%
  filter(Nest=="Circle")%>%
  select(Colony,Day,Nest,CoordType,Bin)

#Random
MaxWorkerPropSimRD1 <- WorkerSim1PropAnalysisSmall %>% 
  group_by(RunNumber,NestSize,Nest,CoordType) %>%
  mutate(MaxPropWorker = max(PropWorker), MaxBin=ifelse(MaxPropWorker==PropWorker,1,0)) %>%
  select(-c(PropWorker, MaxPropWorker)) 

MaxWorkerPropSimRD1Tube<-MaxWorkerPropSimRD1%>%
  filter(Nest=="Tube")%>%
  select(RunNumber,Nest,CoordType,Bin,NestSize)

MaxWorkerPropSimRD1Circle<-MaxWorkerPropSimRD1%>%
  filter(Nest=="Circle")%>%
  select(RunNumber,Nest,CoordType,Bin,NestSize)

#Random
MaxWorkerPropSimRD1TubeFull<-full_join(MaxWorkerPropSimRD1Tube,MaxWorkerPropRD1Tube)
MaxWorkerPropSimRD1CircleFull<-full_join(MaxWorkerPropSimRD1Circle,MaxWorkerPropRD1Circle)

#Workers
AntPropFullWorkersRD2<-AntPropFullWorkersRD2%>%
  mutate(CoordType="Obsv")

MaxWorkerPropRD2 <- AntPropFullWorkersRD2 %>% 
  group_by(Colony,Nest,Day) %>%
  mutate(MaxPropWorker = max(PropWorker),MaxBin=ifelse(MaxPropWorker==PropWorker,1,0)) %>%
  select(-c(PropWorker, MaxPropWorker))  

MaxWorkerPropRD2Tube<-MaxWorkerPropRD2%>%
  filter(Nest=="Tube")%>%
  select(Colony,Day,Nest,CoordType,Bin)

MaxWorkerPropRD2Circle<-MaxWorkerPropRD2%>%
  filter(Nest=="Circle")%>%
  select(Colony,Day,Nest,CoordType,Bin)

#Random
MaxWorkerPropSimRD2 <- WorkerSim2PropAnalysisLarge %>% 
  group_by(RunNumber,Nest,CoordType,NestSize) %>%
  mutate(MaxPropWorker = max(PropWorker),MaxBin=ifelse(MaxPropWorker==PropWorker,1,0)) %>%
  select(-c(PropWorker, MaxPropWorker)) 

MaxWorkerPropSimRD2Tube<-MaxWorkerPropSimRD2%>%
  filter(Nest=="Tube")%>%
  select(RunNumber,Nest,CoordType,Bin,NestSize)

MaxWorkerPropSimRD2Circle<-MaxWorkerPropSimRD2%>%
  filter(Nest=="Circle")%>%
  select(RunNumber,Nest,CoordType,Bin,NestSize)

MaxWorkerPropSimRD2TubeFull<-full_join(MaxWorkerPropSimRD2Tube,MaxWorkerPropRD2Tube)
MaxWorkerPropSimRD2CircleFull<-full_join(MaxWorkerPropSimRD2Circle,MaxWorkerPropRD2Circle)

MaxPropWorkersSim <- full_join(MaxWorkerPropSimRD1, MaxWorkerPropSimRD2) %>%
  mutate(Density = ifelse(NestSize == "Small", "High", "Low"))

MaxPropWorkersFull <- full_join(MaxWorkerPropRD1, MaxWorkerPropRD2) %>%
  full_join(MaxPropWorkersSim)  


#Worker & Random
WorkerTubeTbl1=table(MaxWorkerPropSimRD1TubeFull$CoordType,MaxWorkerPropSimRD1TubeFull$Bin)
WorkerCircleTbl1=table(MaxWorkerPropSimRD1CircleFull$CoordType,MaxWorkerPropSimRD1CircleFull$Bin)
WorkerTubeTbl2=table(MaxWorkerPropSimRD2TubeFull$CoordType,MaxWorkerPropSimRD2TubeFull$Bin)
WorkerCircleTbl2=table(MaxWorkerPropSimRD2CircleFull$CoordType,MaxWorkerPropSimRD2CircleFull$Bin)

#Combine datasets
WorkerSimPropFullRD1<-full_join(AntPropFullWorkers,WorkerSim1PropAnalysisSmall)
WorkerSimPropFullRD2<-full_join(AntPropFullWorkersRD2,WorkerSim2PropAnalysisLarge)

#Simulation & experimental distribition comparisions - Kuiper 2-samp tests
#Random
cvm_test(WorkerTubePropAnalysis$PropWorker,WorkerSim1TubePropAnalysis$PropWorker)
cvm_test(WorkerTubePropAnalysisRD2$PropWorker,WorkerSim2TubePropAnalysisRD2$PropWorker)
cvm_test(WorkerCirclePropAnalysis$PropWorker,WorkerSim1CirclePropAnalysis$PropWorker)
cvm_test(WorkerCirclePropAnalysisRD2$PropWorker,WorkerSim2CirclePropAnalysisRD2$PropWorker)
cvm_test(WorkerTubePropAnalysis$PropWorker,WorkerSim2TubePropAnalysisRD2$PropWorker)
cvm_test(WorkerTubePropAnalysisRD2$PropWorker,WorkerSim1TubePropAnalysis$PropWorker)
cvm_test(WorkerCirclePropAnalysis$PropWorker,WorkerSim2CirclePropAnalysisRD2$PropWorker)
cvm_test(WorkerCirclePropAnalysisRD2$PropWorker,WorkerSim1CirclePropAnalysis$PropWorker)

#Small large comp
cvm_test(WorkerSim1TubePropAnalysis$PropWorker,WorkerSim2TubePropAnalysisRD2$PropWorker)
cvm_test(WorkerSim1CirclePropAnalysis$PropWorker,WorkerSim2CirclePropAnalysisRD2$PropWorker)


#PLOTS AND ANALYSES
#Sim proportion of workers RD1
#Tube nest
WorkerPlotRD1Tube<-ggplot(data = WorkerSimPropFullRD1 %>% filter(Nest == "Tube"),
                          aes(x = as.factor(Bin), y = PropWorker, 
                              fill = CoordType)) + 
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      position = position_dodge(1),
                      color = "grey25", 
                      alpha = 0.65) +
  xlab("Nest section") + 
  ylab("Proportions of workers") +
  ggtitle("High density") +
  theme_pubclean() +  
  theme(axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.87, vjust = -10),
        axis.title = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values = c("red", "white", "grey"), 
                    labels = c("Obsv", "RandWalk", "170°RandWalk")) +
  ylim(0, 0.8)

#Sim proportion of workers RD2
WorkerPlotRD2Tube<-ggplot(data = WorkerSimPropFullRD2 %>% filter(Nest == "Tube"),
                          aes(x = as.factor(Bin), y = PropWorker, 
                              fill = CoordType)) + 
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      position = position_dodge(1),
                      color = "grey25", 
                      alpha = 0.65) +
  xlab("Nest section") + 
  ylab("Proportions of workers") +
  ggtitle("Low density")+
  theme_pubclean() +  
  theme(axis.text.y = element_text(size = 18, family = "Arial", color = "white"),
        axis.text.x = element_text(size = 18, family = "Arial", color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.875, vjust = -10),
        legend.key=element_blank(),
        legend.justification = c(1, -0.7),
        legend.position = c(1, 0.7),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, family = "Arial", color = "black"),
        legend.title = element_text(size = 18, family = "Arial", color = "black"),
        legend.key.size = unit(1, 'cm')) +
  guides(fill = guide_legend(title = "Worker type", family = "Arial", color = "black")) +
  scale_fill_manual(values=c("red","white","grey"),
                    labels=c("Obsv","RandWalk","170°RandWalk"))+
  ylim(0, 0.8)

#Compiling the two above box plots 
WorkerPropPlot <- ggarrange(WorkerPlotRD1Tube, WorkerPlotRD2Tube,
                            labels = c("(a)", "(b)"),
                            font.label = list(size = 18, family = "Arial", face = "plain"),
                            label.x = 0.9,
                            label.y = 1,
                            ncol = 2, nrow = 1,
                            common.legend = FALSE)

#Annotating the compiled plot to include common axes
WorkerPropPlotAnnot <-annotate_figure(WorkerPropPlot,
                                      top = text_grob("Tube nest", color = "black",
                                                      size = 18, x = 0.08, y = -0.8, family = "Arial"),
                                      bottom = NULL,
                                      left = NULL,
                                      right = NULL, 
                                      fig.lab.size = 18
)

#CIRCLE NEST
#High density treatment
WorkerPlotRD1Circle<-ggplot(data = WorkerSimPropFullRD1 %>% filter(Nest == "Circle"),
                            aes(x = as.factor(Bin), y = PropWorker, 
                                fill = CoordType)) + 
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      position = position_dodge(1),
                      color = "grey25", 
                      alpha = 0.65) +
  xlab("Nest section") + 
  ylab("Proportions of workers") +
  ggtitle("High density")+
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "white", hjust = 0.875, vjust = -10),
        legend.position = "none") +
  scale_fill_manual(values=c("blue","white","grey"),
                    labels=c("Obsv","RandWalk","170°RandWalk"))+
  ylim(0, 0.5)

#Low density treatment
WorkerPlotRD2Circle<-ggplot(data = WorkerSimPropFullRD2 %>% filter(Nest == "Circle"),
                            aes(x = as.factor(Bin), y = PropWorker, 
                                fill = CoordType)) + 
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      position = position_dodge(1),
                      color = "grey25", 
                      alpha = 0.65) +
  xlab("Nest section") + 
  ylab("Proportions of workers") +
  ggtitle("Low density")+
  theme_pubclean() +  
  theme(axis.text.x = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "white", hjust = 0.875, vjust = -10),
        axis.text.y = element_text(size=18, color = "white",family="Arial"),
        legend.key=element_blank(),
        legend.justification = c(1, -0.75),
        legend.position = c(1, 0.7),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, family = "Arial", color = "black"),
        legend.title = element_text(size = 18, family = "Arial", color = "black"),
        legend.key.size = unit(1, 'cm')) +
  guides(fill = guide_legend(title = "Worker type", family = "Arial", color = "black")) +
  scale_fill_manual(values=c("blue","white","grey"),
                    labels=c("Obsv","RandWalk","170°RandWalk"))+
  ylim(0, 0.5)

#Compiling the two above box plots 
WorkerPropPlot2 <- ggarrange(WorkerPlotRD1Circle, WorkerPlotRD2Circle,
                           labels = c("(c)", "(d)"),
                           font.label = list(size = 18, family = "Arial", color = "black", face = "plain"),
                           label.x = 0.9,
                           label.y = 1,
                           ncol = 2, nrow = 1,
                           common.legend = FALSE)

#Annotating the compiled plot to include common axes
WorkerPropPlotAnnot2 <- annotate_figure(WorkerPropPlot2,
                                        top = text_grob("Circle nest", color = "black",
                                                        size = 18, x = 0.08, y = -0.8, family = "Arial"),
                                        bottom = NULL,
                                        left = NULL,
                                        right = NULL
)

#Compiling the two above box plots 
WorkerPropPlotAnnotFull <- ggarrange(WorkerPropPlotAnnot, WorkerPropPlotAnnot2,
                             ncol = 1, nrow = 2,
                             common.legend = FALSE)

annotate_figure(WorkerPropPlotAnnotFull,
                         top = NULL,
                         bottom = text_grob("Nest section", color = "black",
                                            size = 18, x = 0.5, family = "Arial"),
                         left = text_grob("Proportions of workers", color = "black",
                                          size = 18, rot = 90, family = "Arial"),
                         right = NULL)

#Linear mixed effects model: observed workers
summary(lmer(PropWorker ~ poly(Bin, degree = 2, raw = TRUE) * Nest * Density + Day + Corner + (1|Colony), data = AntPropFullWorkersRD1_RD2))

#Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(PropWorker ~ poly(Bin, degree = 2, raw = TRUE) * Nest * Density + Day + Corner + (1|Colony), data = AntPropFullWorkersRD1_RD2))

AntPropFullWorkersSimRD1_RD2 <- AntPropFullSim %>% 
  mutate(Density = ifelse(Nest == "Small", "High", "Low")) %>%
  full_join(AntPropFullWorkersRD1_RD2)

#Linear regression: observed & simulated workeres
summary(lm(PropWorker ~ poly(Bin, degree = 2, raw = TRUE) * Nest * CoordType + Density + Corner, data = AntPropFullWorkersSimRD1_RD2))

#Maximum worker proportions
MaxLog1<- MaxPropWorkersFull %>% 
  filter(CoordType == "Obsv") %>%
  arrange(Nest) %>%
  ggplot(aes(Bin, MaxBin)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), aes(color = Nest, linetype = Density), se = FALSE) +
  theme_pubclean() + 
  ggtitle("Observed") +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = -0.1, vjust = -10),
        legend.position = "none") +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(breaks = c("Circle", "Tube"), 
                     name = "Nest",
                     values = c("blue", "red"),
                     labels = c("Circle","Tube")) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8)) +
  ylim(0, 1)

MaxLog2 <- MaxPropWorkersFull %>% 
  filter(CoordType == "RandSim") %>%
  arrange(Nest) %>%
  ggplot(aes(Bin, MaxBin)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), aes(color = Nest, linetype = Density), se = FALSE) +
  theme_pubclean() +
  ggtitle("Random walk") +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(size = 18, family = "Arial", color = "black"),
        axis.text.y = element_text(size = 18, family = "Arial", color = "white"),
        axis.title = element_text(size = 18, family = "Arial", color = "black"),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = -0.1, vjust = -10),
        legend.key = element_blank(),
        legend.position = c(0.975, 0.95),
        legend.direction = "horizontal",
        legend.justification = c(1, 1),
        legend.text = element_text(size = 18, family = "Arial"),
        legend.title = element_text(size = 18, family = "Arial"),
        legend.key.size = unit(1, 'cm')) +
  xlab(NULL) +
  ylab(NULL) +
  guides(color = guide_legend(title = "Nest", family = "Arial",
                              order = 1),
         linetype = guide_legend(order = 2)) +
  scale_color_manual(breaks = c("Tube", "Circle"), 
                     name = "Nest",
                     values = c("red", "blue")) +
  guides(lty = guide_legend(override.aes = list(col = 'black'))) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8)) +
  ylim(0, 1)

MaxLogPlot <- ggarrange(MaxLog1, MaxLog2,
                        labels = c("(a)", "(b)"),
                        label.x = 0.9,
                        font.label = list(size = 18, family = "Arial", face = "plain"),
                        ncol = 2, nrow = 1,
                        common.legend = FALSE)

#Annotating the compiled plots
MaxLogPlotFull <- annotate_figure(MaxLogPlot,
                                  top = NULL,
                                  bottom = text_grob("Nest section", color = "black",
                                                     size = 18, family = "Arial"),
                                  left = text_grob("Max worker proportion", color = "black",
                                                   size = 18, rot = 90, family = "Arial"),
                                  right = NULL
)
MaxLogPlotFull

summary(glm(MaxBin ~ poly(Bin, degree = 2, raw = TRUE)*CoordType*Nest*Density, family = binomial, data = MaxPropWorkersFull))

#The  boxplots show the brood densities throughout the nest (via nest sections - "Bin")
#High density treatment
BroodProp1 <- ggplot(data = AntPropFullBrood %>% arrange(Nest), 
                     aes(x = as.factor(Bin), y = PropBrood)) + 
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      aes(fill = Nest),
                      color = "grey25", 
                      alpha = 0.65) +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("High density") +
  theme_pubclean() +  
  theme(axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.87, vjust = -10),
        axis.title = element_blank(),
        legend.position = "none") +
  guides(fill = guide_legend(title = "Nest", family = "Arial", color = "black")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  ylim(0, 1)

#Low density treatment
BroodProp2 <- ggplot(data = AntPropFullBroodRD2 %>% arrange(Nest), 
                     aes(x = as.factor(Bin), y = PropBrood)) + 
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      aes(fill = Nest),
                      color = "grey25", 
                      alpha = 0.65) + 
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("Low density") +
  theme_pubclean() +  
  theme(axis.text.y = element_text(size = 18, family = "Arial", color = "white"),
        axis.text.x = element_text(size = 18, family = "Arial", color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.875, vjust = -10),
        legend.key=element_blank(),
        legend.justification = c(1, -0.7),
        legend.position = c(1, 0.7),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, family = "Arial", color = "black"),
        legend.title = element_text(size = 18, family = "Arial", color = "black"),
        legend.key.size = unit(1, 'cm')) +
  guides(fill = guide_legend(title = "Nest", family = "Arial", color = "black")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name="Nest",
                    values=c("red", "blue")) +
  ylim(0, 1)


#Compiling the two above box plots 
BroodPropPlot <- ggarrange(BroodProp1, BroodProp2,
                           labels = c("(a)", "(b)"),
                           font.label = list(size = 18, family = "Arial", face = "plain"),
                           label.x = 0.9,
                           label.y = 1,
                           ncol = 2, nrow = 1,
                           common.legend = FALSE)

#Annotating the compiled plot to include common axes
BroodPropPlotFull<-annotate_figure(BroodPropPlot,
                                   top = text_grob("Brood", color = "black", size = 18, x = 0.055, y = -0.6, family = "Arial"),
                                   bottom = NULL,
                                   left = text_grob("Proportions of brood", color = "black",
                                                    size = 18, rot = 90, family = "Arial"),
                                   right = NULL
)
view(AntPropFullBroodRD1_RD2 %>% select(Colony, Nest, Day) %>% distinct())
#Linear mixed effects  
#How does nest section number, nest shape, density treatment, day in experiment, and the presence of corners
#affect the proportions of brood members found in each nest section? 
summary(lmer(PropBrood ~ poly(Bin, degree = 2, raw = TRUE) * Nest * Density + Day + Corner + (1|Colony), data = AntPropFullBroodRD1_RD2))
AntPropFullBroodRD1_RD2 %>%
ggplot() +
geom_boxplot(aes(Corner, PropBrood))
#Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(PropBrood ~ poly(Bin, degree = 2, raw = TRUE) * Nest * Density + Day + Corner + (1|Colony), data = AntPropFullBroodRD1_RD2))

#Same as the above brood boxplots, except for the queen datasets
#High density treatment
QueenProp1 <- ggplot(data = AntPropFullQueen %>% arrange(Nest), 
                   aes(x = as.factor(Bin), y = PropQueen)) + 
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      aes(fill = Nest),
                      color = "grey25", 
                      alpha = 0.65) +
  xlab(NULL) + 
  ylab(NULL) +
  theme_pubclean() +
  theme(axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = "none") +
  guides(fill = guide_legend(title = "Nest", family = "Arial", color = "black")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  ylim(0, 1)

#Low density treatment
QueenProp2 <- ggplot(data = AntPropFullQueenRD2 %>% arrange(Nest), 
                   aes(x = as.factor(Bin), y = PropQueen)) + 
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      aes(fill = Nest),
                      color = "grey25", 
                      alpha = 0.65) +
  xlab(NULL) + 
  ylab(NULL) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        axis.text.y = element_text(size=18, color = "white",family="Arial"),
        legend.position = "none") +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name="Nest",
                    values=c("red", "blue")) +
  ylim(0, 1)

QueenPropPlot <- ggarrange(QueenProp1, QueenProp2,
                           labels = c("(c)", "(d)"),
                           font.label = list(size = 18, family = "Arial", face = "plain"),
                           label.x = 0.9,
                           label.y = 1.045,
                           ncol = 2, nrow = 1)

QueenPropPlotFull<-annotate_figure(QueenPropPlot,
                top = text_grob("Queens", color = "black", size = 18, x = 0.0625, y = 0, family = "Arial"),
                bottom = text_grob("Nest section", color = "black",
                                   size = 18, x = 0.5, family = "Arial"),
                left = text_grob("Proportions of queens", color = "black",
                                 size = 18, rot = 90, family = "Arial"),
                right = NULL
)

#Brood, Queen, and Alate combined proportions plot
ggarrange(BroodPropPlotFull, QueenPropPlotFull,
          ncol = 1, nrow = 2,
          common.legend = TRUE)

#Linear mixed effects: Queen density throughout the nest 
#How does nest section number, nest shape, density treatment, day in experiment, and the presence of corners
#affect the proportions of queens found in each nest section? 
summary(lmer(PropQueen ~ poly(Bin, degree = 2, raw = TRUE) * Nest * Density + Day + Corner + (1|Colony), data = AntPropFullQueenRD1_RD2))

#Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(PropQueen ~ poly(Bin, degree = 2, raw = TRUE) * Nest * Density + Day + Corner + (1|Colony), data = AntPropFullQueenRD1_RD2))

#Same as above boxplots except for the alate dataset
AlatePropFig<-ggplot(data = AntPropFullAlateRD2 %>% arrange(Nest), 
       aes(x = as.factor(Bin), y = PropAlate)) + 
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      aes(fill = Nest), 
                      color = "grey25", 
                      alpha = 0.65) +
  xlab("Nest section") + 
  ylab("Proportions of alates") +
  theme_pubclean() +
  theme(axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 18, family = "Arial", color = "black"),
        plot.title = element_text(size = 18, face = "bold", family = "Arial", color = "black"),
        legend.position = "none") +
  guides(fill = guide_legend(title = "Nest", family = "Arial", color = "black")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  ylim(0, 1)

AlatePropPlot <- ggarrange(AlatePropFig,
                           labels = c("(e)"),
                           font.label = list(size = 18, family = "Arial", face = "plain"),
                           label.x = 0.9,
                           label.y = 1.0325,
                           ncol = 1, nrow = 1)

AlatePropPlotFull<-annotate_figure(AlatePropPlot,
                                   top = text_grob("Alates", color = "black", size = 18, x = 0.15, y = 0, family = "Arial"),
                                   bottom = NULL,
                                   left = NULL,
                                   right = NULL
)
AlatePropPlotFull
#Linear regression 
#How does nest section number, nest shape, day in experiment, and the presence of corners
#affect the proportions of alates found in each nest section? 
summary(lmer(PropAlate ~  poly(Bin, degree = 2, raw = TRUE) * Nest + Day + Corner + (1|Colony), data = AntPropFullAlateRD2))
r.squaredGLMM(lmer(PropAlate ~ poly(Bin, degree = 2, raw = TRUE) * Nest + Day + Corner + (1|Colony), data = AntPropFullAlateRD2))

#The following histograms show the distributions of worker scaled distances from nest entrances
#High density treatment
WorkerDist1 <- ggplot(WorkerDistScaled %>% arrange(Nest), 
                      aes(ScaledDist, 
                          fill = Nest)) + 
  geom_histogram(position = "identity",
                 alpha = 0.7,
                 binwidth = 0.0416666) +
  ggtitle("High density") +
  labs(color="Nest") +
  xlab(NULL) + 
  ylab(NULL) +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.875, vjust = -20),
        legend.position = "none") +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue"))+
  ylim(0,2500)

#Low density treatment
WorkerDist2 <- ggplot(WorkerDistScaledRD2 %>% arrange(Nest), 
                      aes(ScaledDist,
                          fill = Nest)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 binwidth = 0.0416666) +
  ggtitle("Low density") + 
  labs(color = "Nest") +
  xlab(NULL) + 
  ylab(NULL) +
  theme_pubclean() +  
  theme(axis.text.y = element_text(size = 18, family = "Arial", color = "white"),
        axis.text.x = element_text(size = 18, family = "Arial", color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.875, vjust = -10),
        legend.key=element_blank(),
        legend.justification = c(1, -0.7),
        legend.position = c(1.05, 0.7),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, family = "Arial", color = "black"),
        legend.title = element_text(size = 18, family = "Arial", color = "black"),
        legend.key.size = unit(1, 'cm')) +
  guides(fill = guide_legend(title = "Nest", family = "Arial", color = "black")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue"))+
  ylim(0,2500)

WorkerDistPlot<-ggarrange(WorkerDist1, WorkerDist2,
                          labels = c("(a)", "(b)"),
                          label.x = 0.9,
                          font.label = list(size = 18, family = "Arial", face = "plain"),
                          ncol = 2, nrow = 1,
                          common.legend = FALSE)

WorkerDistPlotFull <- annotate_figure(WorkerDistPlot,
                top = text_grob("Workers", color = "black",
                                size = 18, x = 0.055, y = -0.6, family = "Arial"),
                bottom = NULL,
                left =  NULL,
                right = NULL
)

SimDist1 <- ggplot(SimDistScaled %>% filter(NestSize == "Small") %>% arrange(Nest), 
                      aes(ScaledDist, 
                          fill = Nest)) + 
  geom_histogram(position = "identity",
                 alpha = 0.7,
                 binwidth = 0.0416666) +
  labs(color="Nest") +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("High density") +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, face = "bold", family = "Arial", color = "white", hjust = 0.75, vjust = -20),
        legend.position = "none") +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  xlim(0,1)+
  ylim(0,30000)

SimDist2 <- ggplot(SimDistScaled %>% filter(NestSize == "Large") %>% arrange(Nest), 
                   aes(ScaledDist, 
                       fill = Nest)) + 
  geom_histogram(position = "identity",
                 alpha = 0.7,
                 binwidth = 0.0416666) +
  labs(color="Nest") +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("Low density") +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, face = "bold", family = "Arial", color = "white", hjust = 0.75, vjust = -20),
        legend.position = "none") +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  xlim(0,1) +
  ylim(0,30000)

WorkerSimPlot<-ggarrange(SimDist1, SimDist2,
                          labels = c("(c)", "(d)"),
                          label.x = 0.9,
                          label.y = 0.99,
                          font.label = list(size = 18, family = "Arial", face = "plain"),
                          ncol = 2, nrow = 1)

WorkerSimPlotFull <- annotate_figure(WorkerSimPlot,
                top = text_grob("Random walk", color = "black",
                                size = 18, x = 0.089, y = -1, family = "Arial"),
                bottom = NULL,
                left =  NULL,
                right = NULL
)

FullDistPlot<-ggarrange(WorkerDistPlotFull, WorkerSimPlotFull,
                        ncol = 1, nrow = 2,
                        common.legend = FALSE)

annotate_figure(FullDistPlot,
                top = NULL,
                bottom = text_grob("Scaled distance to nest entrance", color = "black",
                                   size = 18, x = 0.525, family = "Arial"),
                left = text_grob("Obsv / sim worker count", color = "black",
                                 size = 18,  rot = 90, family = "Arial"),
                right = NULL
)

#Linear mixed effects model
#How does nest shape, density treatment, day in experiment, and the presence of corners
#affect how far from the nest entrance workers are found?  
#How much of this effect can be explained just by the random effect colony identification?
summary(lmer(ScaledDist ~ Nest * Density + Day + Corner + (1 | Colony), data = WorkerDistScaledRD1_RD2))

#Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(ScaledDist ~ Nest * Density + Day + Corner + (1 | Colony), data = WorkerDistScaledRD1_RD2))

AllDistScaledRD1_RD2 <- SimDistScaled %>%
  mutate(Density = ifelse(NestSize == "Small", "High", "Low")) %>%
  full_join(WorkerDistScaledRD1_RD2)

summary(lm(ScaledDist ~ Nest * ColonyMember + Density + Corner, AllDistScaledRD1_RD2))

#Same histograms as above except for brood scaled distance to the nest entrance
#High density treatment
BroodDist1 <- ggplot(BroodDistScaled %>% arrange(Nest),
                   aes(ScaledDist, 
                       fill = Nest)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 binwidth = 0.0416666) +  
  ggtitle("High density") +
  labs(color="Nest") +
  xlab(NULL) + 
  ylab(NULL) +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.875, vjust = -20),
        legend.position = "none") +
  guides(fill = guide_legend(title = "Nest", family = "Arial", color = "black")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue"))+
  ylim(0, 8000) +
  xlim(0, 1)

#Low density treatment
BroodDist2 <- ggplot(BroodDistScaledRD2 %>% arrange(Nest), 
                     aes(ScaledDist, 
                         fill = Nest)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 binwidth = 0.0416666) +
  ggtitle("Low density") +
  labs(color="Nest") +
  xlab(NULL) + 
  ylab(NULL) +
  theme_pubclean() +  
  theme(axis.text.y = element_text(size = 18, family = "Arial", color = "white"),
        axis.text.x = element_text(size = 18, family = "Arial", color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.875, vjust = -10),
        legend.key=element_blank(),
        legend.justification = c(1, -0.7),
        legend.position = c(1.05, 0.685),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, family = "Arial", color = "black"),
        legend.title = element_text(size = 18, family = "Arial", color = "black"),
        legend.key.size = unit(1, 'cm')) +
  guides(fill = guide_legend(title = "Nest", family = "Arial")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue"))+
  ylim(0, 8000)+
  xlim(0, 1)

#Compiling the brood distance to the nest entrance histograms
BroodDistPlot <- ggarrange(BroodDist1, BroodDist2,
                           labels = c("(a)", "(b)"),
                           label.x = 0.9,
                           font.label = list(size = 18, family = "Arial", face = "plain"),
                           ncol = 2, nrow = 1,
                           common.legend = FALSE)

#Annotating the above histograms
BroodFullDist <- annotate_figure(BroodDistPlot,
                                 top = text_grob("Brood", color = "black",
                                                 size = 18, x = 0.055, y = -0.6, family = "Arial"),
                                 bottom = NULL,
                                 left =  text_grob("Brood count", color = "black",
                                                   size = 18, x = 0.525, rot = 90, family = "Arial"),
                                 right = NULL
)

#Linear mixed effects model
#How does nest shape, density treatment, day in experiment, and the presence of corners
#affect how far from the nest entrance brood are found?  
#How much of this effect can be explained just by the random effect colony identification?
summary(lmer(ScaledDist ~ Nest * Density + Day + Corner + (1 | Colony), data = BroodDistScaledRD1_RD2))

#Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(ScaledDist ~ Nest * Density + Day + Corner + (1 | Colony), data = BroodDistScaledRD1_RD2))

#Same histograms as above except for queens scaled distance to the nest entrance
#High density treatment
QueenDist1 <- ggplot(QueenDistScaled %>% arrange(Nest),
                     aes(ScaledDist, 
                         fill = Nest)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 binwidth = 0.0416666) +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("High density") +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, face = "bold", family = "Arial", color = "white", hjust = 0.75, vjust = -20),
        legend.position = "none") +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue"))+
  ylim(0, 150) +
  xlim(0, 1)

#Low density treatment
QueenDist2 <- ggplot(QueenDistScaledRD2 %>% arrange(Nest),
                     aes(ScaledDist, 
                         fill= Nest)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 binwidth = 0.0416666) +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("Low density") +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, face = "bold", family = "Arial", color = "white", hjust = 0.75, vjust = -20),
        legend.position = "none") +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue"))+
  ylim(0, 150) +
  xlim(0, 1)

#Compiling the queens distance to the nest entrance histograms
QueenDistPlot <- ggarrange(QueenDist1, QueenDist2,
                           labels = c("(c)", "(d)"),
                           label.x = 0.9,
                           label.y = 0.99,
                           font.label = list(size = 18, family = "Arial", face = "plain"),
                           ncol = 2, nrow = 1)

#Annotating the above histograms
QueenFullDist<-annotate_figure(QueenDistPlot,
                               top = text_grob("Queens", color = "black",
                                               size = 18, x = 0.06, y = -1, family = "Arial"),
                               bottom = NULL,
                               left = text_grob("Queen count", color = "black",
                                                         size = 18, x = 0.525, rot = 90, family = "Arial"),
                               right = NULL
)

BroodQueenDist<-ggarrange(BroodFullDist, QueenFullDist,
          ncol = 1, nrow = 2)

annotate_figure(BroodQueenDist,
                top = NULL,
                bottom = text_grob("Scaled distance to nest entrance", color = "black",
                                   size = 18, x = 0.525, family = "Arial"),
                left = NULL,
                right = NULL
)
#Linear mixed effects model
#How does nest shape, density treatment, day in experiment, and the presence of corners
#affect how far from the nest entrance queens are found?  
#How much of this effect can be explained just by the random effect colony identification?
summary(lmer(ScaledDist ~ Nest * Density + Day + Corner + (1 | Colony), data = QueenDistScaledRD1_RD2))

#Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(ScaledDist ~ Nest * Density + Day + Corner + (1 | Colony), data = QueenDistScaledRD1_RD2))

#Removing individuals with unknown alate sex 
AlateDistScaledRD2Plot <- AlateDistScaledRD2 %>%
  filter(Sex != "?")

#Same histogram as above except for alate scaled distance to the nest entrance
AlateDist1 <- ggplot(AlateDistScaledRD2Plot %>% arrange(Nest), 
                     aes(ScaledDist, 
                         fill = Nest)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 binwidth = 0.0416666) +
  xlab("Scaled distance to nest entrance") +
  ylab("Alate count") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_text(size = 18, family = "Arial", color = "black"),
        legend.justification = c(0.5, 1),
        legend.text = element_text(size = 18, family = "Arial", color = "black"),
        legend.title = element_text(size = 18,family = "Arial", color = "black"),
        legend.key.size = unit(1, 'cm')) +
  guides(fill = guide_legend(title = "Nest", family = "Arial", color = "black")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  ylim(0, 110)
#Linear mixed effects model
#How does nest shape, day in experiment, and the presence of corners
#affect how far from the nest entrance alates are found?  
#How much of this effect can be explained just by the random effect colony identification?

#Only male and female
summary(lmer(ScaledDist ~ Nest + Sex + Corner + Day + Ratio + (1 | Colony), data = AlateDistScaledRD2Plot))

#Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(ScaledDist ~ Nest + Sex + Corner + Day + Ratio + (1 | Colony), data = AlateDistScaledRD2Plot))

#The following boxplot shows the relationship between alate sex and alate scaled distance from the nest entrance

AlateDist2 <- ggplot(AlateDistScaledRD2Plot %>% arrange(Nest),
                     aes(x = Sex, y = ScaledDist, 
                         fill = Nest)) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      aes(fill = Nest), 
                      color = "grey25", 
                      alpha = 0.65) + 
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_text(size = 18, family = "Arial", color = "black"),
        legend.key = element_blank(),
        legend.justification = c(0.5, 1),
        legend.text = element_text(size = 18, family = "Arial", color = "black"),
        legend.title = element_text(size = 18, family = "Arial", color = "black"),
        legend.key.size = unit(1, 'cm')) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  xlab("Alate sex") +
  ylab("Scaled distance to nest entrance")

#Compiling the alate distance to the entrance plots 
AlateDistPlot <- ggarrange(AlateDist1, AlateDist2,
                         labels = c("(e)", "(f)"),
                         label.x = 0.9,
                         label.y = 0.965, 
                         font.label = list(size = 18, face = "plain", family = "Arial"),
                         ncol = 2, nrow = 1,
                         common.legend = FALSE)

annotate_figure(AlateDistPlot,
                top = text_grob("Alates", color = "black",
                                      size = 18, x = 0.06, y = -1.30, family = "Arial"),
                bottom = NULL,
                left = NULL,
                right = NULL
)


#The following histogram shows the distributions of worker scaled distances from all nest sections other than their own
#High density treatment
WorkerMeanDist1 <- ggplot(WorkerDistScaledMeanDist %>% arrange(Nest) %>% filter (Density == "High"),
                          aes(ScaledDistMean, 
                              fill = Nest)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 binwidth = 0.0416666) +
  ggtitle("High density") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.875, vjust = -20),
        legend.position = "none") +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  xlab("Mean scaled dist to nest sections") +
  ylab("Worker count") +
  xlim(0, 0.6) +
  ylim(0, 10000)

WorkerMeanDist2 <- ggplot(WorkerDistScaledMeanDist %>% arrange(Nest) %>% filter (Density == "Low"),
                          aes(ScaledDistMean, 
                              fill = Nest)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 binwidth = 0.0416666) +
  ggtitle("Low density") +
  labs(color="Nest") +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("Low density") +
  theme_pubclean() +  
  theme(axis.text.y = element_text(size = 18, family = "Arial", color = "white"),
        axis.text.x = element_text(size = 18, family = "Arial", color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.875, vjust = -10),
        legend.key=element_blank(),
        legend.justification = c(1, -0.7),
        legend.position = c(1.05, 0.685),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, family = "Arial", color = "black"),
        legend.title = element_text(size = 18, family = "Arial", color = "black"),
        legend.key.size = unit(1, 'cm')) +
  guides(fill = guide_legend(title = "Nest", family = "Arial")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  xlim(0, 0.6) +
  ylim(0, 10000)

#The following regression line plots show the relationship between nest shape and 
#Worker scaled distances from all nest sections other than their own
WorkerMeanDist3 <- ggplot(WorkerDistScaledMeanDist %>% arrange(Nest),
                          aes(x = ScaledDist, y = ScaledDistMean,
                              color = Nest, 
                              linetype = Density)) +
  geom_smooth(method = lm, se = FALSE) +
  xlab("Scaled dist to the entrance") +
  ylab("Mean scaled dist to nest sections") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_text(size = 18, family = "Arial", color = "black"),
        plot.title = element_text(size = 18, face = "bold", family = "Arial"),
        legend.key = element_blank(),
        legend.justification = c(1, 0.5),
        legend.box = "verticle",
        legend.position = "right",
        legend.margin=margin(),
        legend.text = element_text(size = 18, family = "Arial"),
        legend.title = element_text(size = 18, family = "Arial"),
        legend.key.size = unit(1, 'cm')) +
  guides(color = guide_legend(title = "Nest", family = "Arial",
                              order = 1),
         linetype = guide_legend(order = 2)) +
  scale_color_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  guides(lty = guide_legend(override.aes = list(col = 'black'))) 

#Compiled worker distance to nest sections plots
WorkerMeanDistPlot <- ggarrange(WorkerMeanDist1, WorkerMeanDist2,
                         labels = c("(a)", "(b)"),
                         label.x = 0.9,
                         label.y = 1,    
                         font.label = list(size = 18, face = "plain", family = "Arial"),
                         ncol = 2, nrow = 1,
                         common.legend = FALSE)

annotate_figure(WorkerMeanDistPlot,
                top = NULL,
                bottom = text_grob("Mean scaled dist to nest sections", color = "black",
                                   size = 18, x = 0.525, family = "Arial"),
                left = text_grob("Worker count", color = "black",
                                 size = 18, x = 0.525, rot = 90, family = "Arial"),
                right = NULL
)

ggarrange(WorkerMeanDist3,
                                labels = c("(c)"),
                                label.x = 0.725,
                                label.y = 0.97,    
                                font.label = list(size = 18, face = "plain", family = "Arial"),
                                ncol = 1, nrow = 1,
                                common.legend = FALSE)
#Linear mixed effects model
#How does nest shape, density treatment, day in experiment, and the presence of corners
#affect how far from all other nest sections workers are found outside of their residing one?  
#How much of this effect can be explained just by the random effect colony identification?
summary(lmer(ScaledDistMean ~ Nest * Density * ScaledDist + Day + Corner + (1 | Colony), data = WorkerDistScaledMeanDist))
boxplot(WorkerDistScaledMeanDist$ScaledDistMean, WorkerDistScaledMeanDist$Day)

BroodCentDistWorkersRD1_RD2 %>%
  ggplot() + 
  geom_boxplot(aes(Corner, ToBrood))

plot(BroodCentDistQueensRD1_RD2$Day, BroodCentDistQueensRD1_RD2$ToBrood)
abline(lm(BroodCentDistQueensRD1_RD2$ToBrood~BroodCentDistQueensRD1_RD2$Day))
#Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(ScaledDistMean ~ Nest * Density * ScaledDist + Day + Corner + (1 | Colony), data = WorkerDistScaledMeanDist))

#The following histograms show the distributions of worker scaled distances to the brood center
#High density treatment
WorkerBroodDist1 <- ggplot(BroodCentDistWorkersRD1 %>% arrange(Nest),
                           aes(ToBrood, 
                               fill = Nest)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 binwidth = 0.0416666) +
  ggtitle("High density") +
  labs(color="Nest") +
  xlab(NULL) + 
  ylab(NULL) +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.875, vjust = -20),
        legend.position = "none") +
  guides(fill = guide_legend(title = "Nest", family = "Arial")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) + 
  xlim(0, 1) + 
  ylim(0, 4000)

#Low density treatment
WorkerBroodDist2 <- ggplot(BroodCentDistWorkersRD2 %>% arrange(Nest), 
                           aes(ToBrood, 
                               fill = Nest)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 binwidth = 0.0416666) +
  labs(color="Nest") +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("Low density") +
  theme_pubclean() +  
  theme(axis.text.y = element_text(size = 18, family = "Arial", color = "white"),
        axis.text.x = element_text(size = 18, family = "Arial", color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.875, vjust = -10),
        legend.key=element_blank(),
        legend.justification = c(1, -0.7),
        legend.position = c(1.05, 0.7),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, family = "Arial", color = "black"),
        legend.title = element_text(size = 18, family = "Arial", color = "black"),
        legend.key.size = unit(1, 'cm')) +
  guides(fill = guide_legend(title = "Nest", family = "Arial")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) + 
  xlim(0, 1) + 
  ylim(0, 4000)

#Compiling worker scaled distances from nest entrance plots
WorkerBroodDistPlot <- ggarrange(WorkerBroodDist1, WorkerBroodDist2,
                                 labels = c("(a)", "(b)"),
                                 label.x = 0.9,
                                 font.label = list(size = 18, family = "Arial", face = "plain"),
                                 ncol = 2, nrow = 1,
                                 common.legend = FALSE)

#Annotating the compiled plot
WorkerFullBroodDist <- annotate_figure(WorkerBroodDistPlot,
                                       top = text_grob("Workers", color = "black",
                                                       size = 18, x = 0.06, y = -0.675, family = "Arial"),
                                 bottom = NULL,
                                 left = text_grob("Worker count", color = "black",
                                                  size = 18, rot = 90, family = "Arial"),
                                 right = NULL
)

#Linear mixed effects model
#How does nest shape, density treatment, day in experiment, and the presence of corners
#affect how far from the center of the brood pile workers are found?  
#How much of this effect can be explained just by the random effect colony identification?
summary(lmer(ToBrood ~ Nest * Density + Day + Corner + (1 | Colony), data = BroodCentDistWorkersRD1_RD2))

#Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(ToBrood ~ Nest * Density + Day + Corner + (1 | Colony), data = BroodCentDistWorkersRD1_RD2))

#Same histograms as above except for queens scaled distances to the brood center
#High density treatment
QueenBroodDist1 <- ggplot(BroodCentDistQueensRD1 %>% arrange(Nest),
                        aes(ToBrood, 
                            fill = Nest)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 binwidth = 0.0416666) +
  ggtitle("High density") +
  labs(color="Nest") +
  xlab(NULL) + 
  ylab(NULL) +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, face = "bold", family = "Arial", color = "white", hjust = 0.75, vjust = -20),
        legend.position = "none") +
  guides(fill = guide_legend(title = "Nest", family = "Arial")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) 

#Low density treatment
QueenBroodDist2 <- ggplot(BroodCentDistQueensRD2 %>% arrange(Nest),
                          aes(ToBrood, fill = Nest)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 binwidth = 0.0416666) +
  ggtitle("Low density") +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, face = "bold", family = "Arial", color = "white", hjust = 0.75, vjust = -20),
        legend.position = "none") +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) 

#Compiling queens scaled distances to the brood center plots
QueenBroodDistPlot <- ggarrange(QueenBroodDist1, QueenBroodDist2,
                                labels = c("(c)", "(d)"),
                                label.x = 0.9,
                                font.label = list(size = 18, family = "Arial", face = "plain"),
                                ncol = 2, nrow = 1,
                                common.legend = FALSE)

#Annotating the compiled plots
QueenBroodFullDist <- annotate_figure(QueenBroodDistPlot,
                                      top = text_grob("Queens", color = "black",
                                                      size = 18, x = 0.06, y = -0.75, family = "Arial"),
                                 bottom = NULL,
                                 left = text_grob("Queen count", color = "black",
                                                  size = 18, rot = 90, family = "Arial"),
                                 right = NULL
)

WorkerQueenBroodDist<-ggarrange(WorkerFullBroodDist, QueenBroodFullDist,
                          ncol = 1, nrow = 2)

annotate_figure(WorkerQueenBroodDist,
                top = NULL,
                bottom = text_grob("Scaled distance to brood center", color = "black",
                                   size = 18, x = 0.525, family = "Arial"),
                left = NULL,
                right = NULL
)

#Linear mixed effects model
#How does nest shape, density treatment, day in experiment, and the presence of corners
#affect how far from the center of the brood pile queens are found?  
#How much of this effect can be explained just by the random effect colony identification?
summary(lmer(ToBrood ~ Nest * Density + Day + Corner + (1 | Colony), data = BroodCentDistQueensRD1_RD2))

#Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(ToBrood ~ Nest * Density + Day + Corner + (1 | Colony), data = BroodCentDistQueensRD1_RD2))

BroodCentDistAlatesRD2Plot <- BroodCentDistAlatesRD2 %>% filter(Sex != "?")

#Same histograms as above except for alate scaled distances to the brood center
AlateBroodPlot <- ggplot(BroodCentDistAlatesRD2Plot %>% arrange(Nest),
                         aes(ToBrood, 
                             fill = Nest)) + 
  geom_histogram(position = "identity", 
                 alpha = 0.7, 
                 binwidth = 0.0416666) +
  xlab("Scaled distance to brood center") +
  ylab("Alate count") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_text(size = 18, family = "Arial"),
        legend.key = element_blank(),
        legend.justification = c(0.5, 1),
        legend.text = element_text(size = 18, family = "Arial", color = "black"),
        legend.title = element_text(size = 18, family = "Arial", color = "black"),
        legend.key.size = unit(1, 'cm')) +
  guides(fill = guide_legend(title = "Nest", family = "Arial")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue"))

AlateBroodPlot2 <- ggplot(BroodCentDistAlatesRD2Plot %>% arrange(Nest),
                     aes(x = Sex, y = ToBrood, 
                         fill = Nest)) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      aes(fill = Nest), 
                      color = "grey25", 
                      alpha = 0.65) + 
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_text(size = 18, family = "Arial", color = "black"),
        legend.key = element_blank(),
        legend.justification = c(0.5, 1),
        legend.text = element_text(size = 18, family = "Arial", color = "black"),
        legend.title = element_text(size = 18, family = "Arial", color = "black"),
        legend.key.size = unit(1, 'cm')) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  xlab("Alate sex") +
  ylab("Scaled distance to brood center")

#Compiling the alate distance to the the brood center plots 
AlateToBroodPlot <- ggarrange(AlateBroodPlot, AlateBroodPlot2,
                           labels = c("(e)", "(f)"),
                           label.x = 0.9,
                           label.y = 0.965, 
                           font.label = list(size = 18, face = "plain", family = "Arial"),
                           ncol = 2, nrow = 1,
                           common.legend = FALSE)

annotate_figure(AlateToBroodPlot,
                top = text_grob("Alates", color = "black",
                                size = 18, x = 0.06, y = -1.30, family = "Arial"),
                bottom = NULL,
                left = NULL,
                right = NULL
)

#Linear mixed effects model
#How does nest shape, day in experiment, and the presence of corners
#affect how far from the center of the brood pile alates are found?  
#How much of this effect can be explained just by the random effect colony identification?
summary(lmer(ToBrood ~ Nest + Sex + Ratio + Day + Corner + (1 | Colony), data = BroodCentDistAlatesRD2Plot))

#Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(ToBrood ~ Nest + Sex + Ratio + Day + Corner + (1 | Colony), data = BroodCentDistAlatesRD2Plot))

#The following boxplots show worker spatial fidelity zone size across experimental colonies 
#High density treatment
FidZone.1 <- ggplot(data = WorkerDistScaledRD1_RD2SFZWorkingFid %>%
                      filter(Colony < 11) %>% arrange(Nest), 
                    aes(x = as.factor(Colony), y = SFZ),
                    position = position_dodge(2)) + 
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      aes(fill = Nest), 
                      color = "grey25", 
                      alpha = 0.65) +  
  xlab(NULL) + 
  ylab("Fidelity zone size") +
  ggtitle("High density") +
  theme_pubclean() +  
  theme(axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.875, vjust = -10),
        legend.position = "none") +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  ylim(0, 0.5)

#Spatial fidelity zones - Treatment 2
FidZone.2 <- ggplot(data = WorkerDistScaledRD1_RD2SFZWorkingFid %>%
                      filter(Colony > 10) %>% arrange(Nest), 
                    aes(x = as.factor(Colony), y = SFZ),
                    position = position_dodge(2)) + 
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      aes(fill = Nest), 
                      color = "grey25", 
                      alpha = 0.65) +  
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("Low density") +
  theme_pubclean() +  
  theme(axis.text.y = element_text(size = 18, family = "Arial", color = "white"),
        axis.text.x = element_text(size = 18, family = "Arial", color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.875, vjust = -10),
        legend.key=element_blank(),
        legend.justification = c(1, -0.7),
        legend.position = c(1.05, 0.725),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, family = "Arial", color = "black"),
        legend.title = element_text(size = 18, family = "Arial", color = "black"),
        legend.key.size = unit(1, 'cm')) +
  guides(fill = guide_legend(title = "Nest", family = "Arial", color = "black")) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  ylim(0, 0.5)

#The following boxplots show worker total nest space usage (Occurrence zone size) across experimental colonies 
#High density treatment
Occur.1 <- ggplot(data = WorkerDistScaledRD1_RD2SFZFull %>%
                    filter(Colony < 11) %>% arrange(Nest), 
                  aes(x = as.factor(Colony), y = SFZ_Full),
                  position = position_dodge(2)) + 
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      aes(fill = Nest), 
                      color = "grey25", 
                      alpha = 0.65) +  
  xlab(NULL) + 
  ylab("Occurrence zone size") +
  ggtitle("Low density") +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, face = "bold", family = "Arial", color = "white", hjust = 0.75, vjust = -20),
        legend.position = "none",
        legend.key=element_blank()) +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  ylim(0, 0.5)

#Low density treatment
Occur.2 <- ggplot(data = WorkerDistScaledRD1_RD2SFZFull %>%
                    filter(Colony > 10) %>% arrange(Nest), 
                  aes(x = as.factor(Colony), y = SFZ_Full),
                  position = position_dodge(2)) + 
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00),
                      aes(fill = Nest), 
                      color = "grey25", 
                      alpha = 0.65) +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("Low density") +
  theme_pubclean() +  
  theme(axis.text.x = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 18, face = "bold", family = "Arial", color = "white", hjust = 0.75, vjust = -20),
        axis.text.y = element_text(size=18, color = "white",family="Arial"),
        legend.position = "none") +
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name = "Nest",
                    values = c("red", "blue")) +
  ylim(0, 0.5)

#Compiling worker fidelity zone size v. worker scaled distance to the nest entrance plots
FidZonePlot <- ggarrange(FidZone.1, FidZone.2,
                         labels = c("(a)", "(b)"),
                         label.x = 0.9,
                         font.label = list(size = 18, family = "Arial", face = "plain"),
                         ncol = 2, nrow = 1,
                         common.legend = FALSE)
#Annotating the compiled plots
FidZonePlotFull <- annotate_figure(FidZonePlot,
                                   top = NULL,
                                   bottom = NULL,
                                   left = text_grob("Fidelity zone size", color = "black",
                                                    size = 18, rot = 90, family = "Arial"),
                                   right = NULL
)

#Compiling worker occurrence zone size v. worker scaled distance to the nest entrance plots
OccurZonePlot <- ggarrange(Occur.1, Occur.2,
                           labels = c("(c)", "(d)"),
                           label.x = 0.9,
                           font.label = list(size = 18, family = "Arial", face = "plain"),
                           ncol = 2, nrow = 1,
                           common.legend = FALSE)

OccurZonePlotFull <- annotate_figure(OccurZonePlot,
                                     top = NULL,
                                     bottom = NULL,
                                     left = text_grob("Occurrence zone size", color = "black",
                                                      size = 18, rot = 90, family = "Arial"),
                                     right = NULL
)

FidOccurPlot<-ggarrange(FidZonePlotFull, OccurZonePlotFull,
                            ncol = 1, nrow = 2,
                            common.legend = TRUE)

#Annotating the compiled plots
annotate_figure(FidOccurPlot,
                top = NULL,
                bottom = text_grob("Colony identification", color = "black",
                                   size = 18, x = 0.525, family = "Arial"),
                left = NULL,
                right = NULL
)



#Linear mixed effects model
#How does nest shape, density treatment, and colony ID affect worker spatial fidelity zone size? 
#How much of this effect can be explained just by the random effect ant color identification?
summary(lmer(SFZ ~ Nest * Density + Colony + (1|ColorID), data = WorkerDistScaledRD1_RD2SFZWorkingFid))

summary(lmer(SFZArea ~ Nest * Density + Colony + (1|ColorID), data = WorkerDistScaledRD1_RD2SFZWorkingFid))


BroodCentDistWorkersSFZ %>% filter(Nest == "Circle", Density == "Low") %>%
  ggplot(aes(MeanToBrood, SFZ_Full)) +
  geom_point() +
  geom_smooth(method = lm) 
  
#Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(SFZ ~ Nest * Density + Colony + (1 | ColorID), data = WorkerDistScaledRD1_RD2SFZWorkingFid))

r.squaredGLMM(lmer(SFZArea ~ Nest * Density + Colony + (1|ColorID), data = WorkerDistScaledRD1_RD2SFZWorkingFid))

#Linear mixed effects model
#How does nest shape, density treatment, and colony ID affect worker total nest space useage (occurrence zone size)? 
#How much of this effect can be explained just by the random effect ant color identification?
summary(lmer(SFZ_Full ~ Nest * Density + Colony + (1|ColorID), data = WorkerDistScaledRD1_RD2SFZWorking))

summary(lmer(SFZ_FullArea ~ Nest * Density + Colony + (1|ColorID), data = WorkerDistScaledRD1_RD2SFZWorking))

#Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(SFZ_Full ~ Nest * Density + Colony + (1 | ColorID), data = WorkerDistScaledRD1_RD2SFZWorking))

r.squaredGLMM(lmer(SFZ_FullArea ~ Nest * Density + Colony + (1|ColorID), data = WorkerDistScaledRD1_RD2SFZWorking))

#Function to create large points in a geom_point legend
large_points <- function(data, params, size) {
  # Multiply by some number, it doesn't matter what value, but larger numbers = large sized points in the legend
  data$size <- data$size * 2.5
  draw_key_point(data = data, params = params, size = size)
}

#The following plots show the relationship between worker spatial fidelity zone size
#and worker scaled distance to the nest entrance
#High density treatment
SFZDist1 <- ggplot(data = WorkerDistScaledRD1_RD2SFZWorkingFid %>% filter(Density == "High") %>% arrange(Nest), 
                   aes(x = MeanScaledDist, y=SFZ, 
                       linetype = Nest,
                       color = Nest, 
                       shape = Nest)) +
  ggtitle("High density") +
  geom_point(key_glyph = large_points, size = 3, alpha = 0.33) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.25, color = "black") +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.875, vjust = -20),
        legend.position = "none") +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(breaks = c("Circle", "Tube"), 
                     name = "Nest",
                     values = c("blue", "red"),
                     labels = c("Circle","Tube")) +
  ylim(0.05, 0.5) +
  xlim(0, 0.75)

#Low density treatment
SFZDist2<-ggplot(data = WorkerDistScaledRD1_RD2SFZWorkingFid %>% filter(Density == "Low") %>% arrange(Nest),
                 aes(x = MeanScaledDist, y = SFZ, linetype = Nest,
                     color = Nest,
                     shape = Nest)) +
  ggtitle("Low density") +
  geom_point(key_glyph = large_points, size = 3, alpha = 0.33) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.25, color = "black") +
  theme_pubclean() +  
  theme(axis.text.y = element_text(size = 18, family = "Arial", color = "white"),
        axis.text.x = element_text(size = 18, family = "Arial", color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.875, vjust = -10),
        legend.key=element_blank(),
        legend.justification = c(1, -0.7),
        legend.position = c(1, 0.71),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, family = "Arial", color = "black"),
        legend.title = element_text(size = 18, family = "Arial", color = "black"),
        legend.key.size = unit(1, 'cm')) +
  labs(color = "Nest", linetype = "Nest", shape = "Nest") +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(breaks = c("Circle", "Tube"), 
                     name = "Nest",
                     values = c("blue", "red"),
                     labels = c("Circle","Tube")) +
  guides(shape = guide_legend(override.aes = list(alpha = 0.75))) +
  ylim(0.05, 0.5) +
  xlim(0, 0.75)

ggplot(data = WorkerDistScaledRD1_RD2SFZWorkingFid %>% filter(Density == "High") %>% arrange(Nest), 
                   aes(x = DistanceTotal, y=SFZArea, 
                       linetype = Nest,
                       color = Nest, 
                       shape = Nest)) +
  ggtitle("High density") +
  geom_point(key_glyph = large_points, size = 3, alpha = 0.33) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.25, color = "black") +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.875, vjust = -20),
        legend.position = "none") +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(breaks = c("Circle", "Tube"), 
                     name = "Nest",
                     values = c("blue", "red"),
                     labels = c("Circle","Tube")) 

#Low density treatment
ggplot(data = WorkerDistScaledRD1_RD2SFZWorkingFid %>% filter(Density == "Low") %>% arrange(Nest),
                 aes(x = MeanScaledDist, y=SFZArea,  linetype = Nest,
                     color = Nest,
                     shape = Nest)) +
  ggtitle("Low density") +
  geom_point(key_glyph = large_points, size = 3, alpha = 0.33) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.25, color = "black") +
  theme_pubclean() +  
  theme(axis.text.y = element_text(size = 18, family = "Arial", color = "white"),
        axis.text.x = element_text(size = 18, family = "Arial", color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.875, vjust = -10),
        legend.key=element_blank(),
        legend.justification = c(1, -0.7),
        legend.position = c(1, 0.71),
        legend.direction = "horizontal",
        legend.text = element_text(size = 18, family = "Arial", color = "black"),
        legend.title = element_text(size = 18, family = "Arial", color = "black"),
        legend.key.size = unit(1, 'cm')) +
  labs(color = "Nest", linetype = "Nest", shape = "Nest") +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(breaks = c("Circle", "Tube"), 
                     name = "Nest",
                     values = c("blue", "red"),
                     labels = c("Circle","Tube")) +
  guides(shape = guide_legend(override.aes = list(alpha = 0.75))) 

#The following plots show the relationship between worker total nest space usage (Occurrence zone size)
#and worker scaled distance to the nest entrance
#High density treatment

OccurDist1<-ggplot(data = WorkerDistScaledRD1_RD2SFZWorking %>% filter(Density == "High") %>% arrange(Nest),
                   aes(x = MeanScaledDist, y = SFZ_Full,
                       color = Nest,
                       linetype = Nest,
                       shape = Nest)) +
  geom_point(key_glyph = large_points, size = 3, alpha = 0.33) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.25, color = "black") +
  ggtitle("High density") +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, face = "bold", family = "Arial", color = "white", hjust = 0.75, vjust = -20),
        legend.position = "none") +
  labs(color = "Nest", linetype = "Nest", shape = "Nest") +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(breaks = c("Circle", "Tube"), 
                     name = "Nest",
                     values = c("blue", "red"),
                     labels = c("Circle","Tube")) +
  ylim(0.05, 0.5) +
  xlim(0, 0.75)


#Low density treatment
OccurDist2 <- ggplot(data = WorkerDistScaledRD1_RD2SFZWorking %>% filter(Density == "Low") %>% arrange(Nest),
                     aes(x = MeanScaledDist, y = SFZ_Full,
                         color = Nest, linetype = Nest,
  shape = Nest)) +
  geom_point(key_glyph = large_points, size = 3, alpha = 0.33) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.25, color = "black") +
  ggtitle("Low density") +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, face = "bold", family = "Arial", color = "white", hjust = 0.75, vjust = -20),
        legend.position = "none") +
  labs(color = "Nest", linetype = "Nest", shape = "Nest") +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(breaks = c("Circle", "Tube"), 
                     name = "Nest",
                     values = c("blue", "red"),
                     labels = c("Circle","Tube")) +
  ylim(0.05, 0.5) +
  xlim(0, 0.75)

#Compiling worker fidelity zone size v. worker scaled distance to the nest entrance plots
SFZDistPlot <- ggarrange(SFZDist1, SFZDist2,
                         labels = c("(a)", "(b)"),
                         label.x = 0.9,
                         font.label = list(size = 18, family = "Arial", face = "plain"),
                         ncol = 2, nrow = 1,
                         common.legend = FALSE)
#Annotating the compiled plots
SFZFullDistPlot <- annotate_figure(SFZDistPlot,
                                       top = NULL,
                                       bottom = NULL,
                                       left = text_grob("Fidelity zone size", color = "black",
                                                        size = 18, rot = 90, family = "Arial"),
                                       right = NULL
)

#Compiling worker occurrence zone size v. worker scaled distance to the nest entrance plots
OccurDistPlot <- ggarrange(OccurDist1, OccurDist2,
                           labels = c("(c)", "(d)"),
                           label.x = 0.9,
                           font.label = list(size = 18, family = "Arial", face = "plain"),
                           ncol = 2, nrow = 1,
                           common.legend = FALSE)

OccurFullDistPlot <- annotate_figure(OccurDistPlot,
                                       top = NULL,
                                       bottom = NULL,
                                       left = text_grob("Occurrence zone size", color = "black",
                                                        size = 18, rot = 90, family = "Arial"),
                                       right = NULL
)

FidOccurDistPlot<-ggarrange(SFZFullDistPlot, OccurFullDistPlot,
                                ncol = 1, nrow = 2,
                            common.legend = TRUE)

#Annotating the compiled plots
annotate_figure(FidOccurDistPlot,
                top = NULL,
                bottom = text_grob("Scaled distance to nest entrance", color = "black",
                                   size = 18, x = 0.525, family = "Arial"),
                left = NULL,
                right = NULL
)

#Linear mixed effects model
#How does mean scaled distance to the nest entrance, nest shape, density treatment affect worker spatial fidelity zone size?  
#How much of this effect can be explained just by the random effect colony identification and worker color identification?
summary(lmer(SFZ ~ MeanScaledDist * Nest * Density + (1 | Colony) + (1|ColorID), data = WorkerDistScaledRD1_RD2SFZWorkingFid))

summary(lmer(SFZArea ~ MeanScaledDist * Nest * Density + (1 | Colony) + (1|ColorID), data = WorkerDistScaledRD1_RD2SFZWorkingFid))

#Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(SFZ ~ MeanScaledDist * Nest * Density + (1 | Colony) + (1|ColorID), data = WorkerDistScaledRD1_RD2SFZWorkingFid))

r.squaredGLMM(lmer(SFZArea ~ MeanScaledDist * Nest * Density + (1 | Colony) + (1|ColorID), data = WorkerDistScaledRD1_RD2SFZWorkingFid))

#Linear mixed effects model
#How does mean scaled distance to the nest entrance, nest shape, density treatment affect worker total nest space usage (occurrence zone)?  
#How much of this effect can be explained just by the random effect colony identification and worker color identification?
summary(lmer(SFZ_Full ~ MeanScaledDist * Nest * Density + (1 | Colony) + (1|ColorID), data = WorkerDistScaledRD1_RD2SFZWorking))

summary(lmer(SFZ_FullArea ~ MeanScaledDist * Nest * Density + (1 | Colony) + (1|ColorID), data = WorkerDistScaledRD1_RD2SFZWorking))

#Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(SFZ_Full ~ MeanScaledDist * Nest * Density + (1 | Colony) + (1|ColorID), data = WorkerDistScaledRD1_RD2SFZWorking))

r.squaredGLMM(lmer(SFZ_FullArea ~ MeanScaledDist * Nest * Density + (1 | Colony) + (1|ColorID), data = WorkerDistScaledRD1_RD2SFZWorking))

#The following plots show the relationship between worker fidelity zone size
#and worker scaled distance to the brood center
#High density treatment
SFZBroodDist1 <- ggplot(data = BroodCentDistWorkersSFZFid %>% filter(Density == "High") %>% arrange(Nest), 
                        aes(x = MeanToBrood, y = SFZ, 
                            color = Nest,
                            linetype = Nest,
                            shape = Nest)) +
  ggtitle("High density") +
  geom_point(key_glyph = large_points, size = 3, alpha = 0.33) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.25, color = "black") +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.875, vjust = -20),
        legend.key=element_blank(),
        legend.justification = c(1, 1),
        legend.text = element_text(size = 18, family = "Arial", color = "black"),
        legend.title = element_text(size = 18, family = "Arial", color = "black"),
        legend.key.size = unit(1, 'cm')) +
  xlab(NULL) +
  ylab(NULL) +
  labs(color = "Nest", linetype = "Nest", shape = "Nest") +
  scale_color_manual(breaks = c("Circle", "Tube"), 
                     name = "Nest",
                     values = c("blue", "red"),
                     labels = c("Circle","Tube")) +
  guides(shape = guide_legend(override.aes = list(alpha = 0.75))) +
  ylim(0.05, 0.4) +
  xlim(0, 0.5)

#Low density treatment
SFZBroodDist2 <- ggplot(data = BroodCentDistWorkersSFZFid %>% filter(Density == "Low") %>% arrange(Nest),
                        aes(x = MeanToBrood, y = SFZ, 
                            color = Nest,
                            linetype = Nest,
                            shape = Nest)) +
  ggtitle("Low density") +
  geom_point(key_glyph = large_points, size = 3, alpha = 0.33) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.25, color = "black") +
  theme_pubclean() +  
  theme(axis.text.y = element_text(size = 18, family = "Arial", color = "white"),
        axis.text.x = element_text(size = 18, family = "Arial", color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.875, vjust = -10),
        legend.key=element_blank(),
        legend.justification = c(1, 1),
        legend.text = element_text(size = 18, family = "Arial", color = "black"),
        legend.title = element_text(size = 18, family = "Arial", color = "black"),
        legend.key.size = unit(1, 'cm')) +
  labs(color = "Nest", linetype = "Nest", shape = "Nest") +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(breaks = c("Circle", "Tube"), 
                     name = "Nest",
                     values = c("blue", "red"),
                     labels = c("Circle","Tube")) +
  guides(shape = guide_legend(override.aes = list(alpha = 0.75))) +
  ylim(0.05, 0.4) +
  xlim(0, 0.5)

ggplot(data = BroodCentDistWorkersSFZFid %>% filter(Density == "High") %>% arrange(Nest), 
                        aes(x = BroodDist, y = SFZArea, 
                            color = Nest,
                            linetype = Nest,
                            shape = Nest)) +
  ggtitle("High density") +
  geom_point(key_glyph = large_points, size = 3, alpha = 0.33) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.25, color = "black") +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.875, vjust = -20),
        legend.key=element_blank(),
        legend.justification = c(1, 1),
        legend.text = element_text(size = 18, family = "Arial", color = "black"),
        legend.title = element_text(size = 18, family = "Arial", color = "black"),
        legend.key.size = unit(1, 'cm')) +
  xlab(NULL) +
  ylab(NULL) +
  labs(color = "Nest", linetype = "Nest", shape = "Nest") +
  scale_color_manual(breaks = c("Circle", "Tube"), 
                     name = "Nest",
                     values = c("blue", "red"),
                     labels = c("Circle","Tube")) +
  guides(shape = guide_legend(override.aes = list(alpha = 0.75))) 

#Low density treatment
ggplot(data = BroodCentDistWorkersSFZFid %>% filter(Density == "Low") %>% arrange(Nest),
                        aes(x = BroodDist, y = SFZArea, 
                            color = Nest,
                            linetype = Nest,
                            shape = Nest)) +
  ggtitle("Low density") +
  geom_point(key_glyph = large_points, size = 3, alpha = 0.33) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.25, color = "black") +
  theme_pubclean() +  
  theme(axis.text.y = element_text(size = 18, family = "Arial", color = "white"),
        axis.text.x = element_text(size = 18, family = "Arial", color = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, family = "Arial", color = "black", hjust = 0.875, vjust = -10),
        legend.key=element_blank(),
        legend.justification = c(1, 1),
        legend.text = element_text(size = 18, family = "Arial", color = "black"),
        legend.title = element_text(size = 18, family = "Arial", color = "black"),
        legend.key.size = unit(1, 'cm')) +
  labs(color = "Nest", linetype = "Nest", shape = "Nest") +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(breaks = c("Circle", "Tube"), 
                     name = "Nest",
                     values = c("blue", "red"),
                     labels = c("Circle","Tube")) +
  guides(shape = guide_legend(override.aes = list(alpha = 0.75))) 


#The following plots show the relationship between worker total nest space usage (Occurrence zone size)
#and worker scaled distance to the brood center
#High density treatment
OccurBroodDist1 <- ggplot(data = BroodCentDistWorkersSFZ %>% filter(Density == "High") %>% arrange(Nest),
                          aes(x = MeanToBrood, y = SFZ_Full,
                              color = Nest,
                              linetype = Nest,
                              shape = Nest)) +
  geom_point(key_glyph = large_points, size = 3, alpha = 0.33) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.25, color = "black") +
  ggtitle("High density") +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, face = "bold", family = "Arial", color = "white", hjust = 0.75, vjust = -20),
        legend.position = "none") +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(breaks = c("Circle", "Tube"), 
                     name = "Nest",
                     values = c("blue", "red"),
                     labels = c("Circle","Tube")) +
  ylim(0.1, 0.5) +
  xlim(0, 0.5)

#Low density treatment
OccurBroodDist2 <- ggplot(data = BroodCentDistWorkersSFZ %>% filter(Density == "Low") %>% arrange(Nest),
                          aes(x = MeanToBrood, y = SFZ_Full, 
                              color = Nest, 
                              linetype = Nest,
                              shape = Nest)) +
  geom_point(key_glyph = large_points, size = 3, alpha = 0.33) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.25, color = "black") +
  ggtitle("Low density") +
  theme_pubclean() +  
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 18, family = "Arial", color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, face = "bold", family = "Arial", color = "white", hjust = 0.75, vjust = -20),
        legend.position = "none") +
  xlab(NULL) +
  ylab(NULL) +
  scale_color_manual(breaks = c("Circle", "Tube"), 
                     name = "Nest",
                     values = c("blue", "red"),
                     labels = c("Circle","Tube")) +
  ylim(0.1, 0.5) +
  xlim(0, 0.5)
#Compiling worker fidelity zone size v. worker scaled distance to the brood center plots
SFZBroodDistPlot <- ggarrange(SFZBroodDist1, SFZBroodDist2,
                              labels = c("(a)", "(b)"),
                              label.x = 0.9,
                              font.label = list(size = 18, family = "Arial", face = "plain"),
                              ncol = 2, nrow = 1,
                              common.legend = TRUE)
#Annotating the compiled plots
SFZFullBroodDistPlot <- annotate_figure(SFZBroodDistPlot,
                                   top = NULL,
                                   bottom = NULL,
                                   left = text_grob("Fidelity zone size", color = "black",
                                                    size = 18, rot = 90, family = "Arial"),
                                   right = NULL
)

#Compiling worker occurrence zone size v. worker scaled distance to the nest entrance plots
OccurBroodDistPlot <- ggarrange(OccurBroodDist1, OccurBroodDist2,
                                labels = c("(c)", "(d)"),
                                label.x = 0.9,
                                font.label = list(size = 18, family = "Arial", face = "plain"),
                                ncol = 2, nrow = 1,
                                common.legend = FALSE)

OccurFullBroodDistPlot <- annotate_figure(OccurBroodDistPlot,
                                     top = NULL,
                                     bottom = NULL,
                                     left = text_grob("Occurrence zone size", color = "black",
                                                      size = 18, rot = 90, family = "Arial"),
                                     right = NULL
)

FidOccurBroodDistPlot<-ggarrange(SFZFullBroodDistPlot, OccurFullBroodDistPlot,
                            ncol = 1, nrow = 2,
                            common.legend = TRUE)

#Annotating the compiled plots
annotate_figure(FidOccurBroodDistPlot,
                top = NULL,
                bottom = text_grob("Scaled distance to brood center", color = "black",
                                   size = 18, x = 0.525, family = "Arial"),
                left = NULL,
                right = NULL
)

#Linear mixed effects model
#How does mean scaled distance to the brood center, nest shape, density treatment affect worker spatial fidelity zone size?  
#How much of this effect can be explained just by the random effect colony identification and worker color identification?
summary(lmer(SFZ ~ BroodDist * Nest * Density + Colony + (1|ColorID), data = BroodCentDistWorkersSFZFid))

summary(lmer(SFZArea ~ MeanToBrood * Nest * Density + Colony + (1|ColorID), data = BroodCentDistWorkersSFZFid))

summary(lmer(SFZArea ~ MeanToBrood * Nest  +  Colony + (1|ColorID), data = BroodCentDistWorkersSFZFid %>% filter(Density == "Low")))

#Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(SFZ ~ MeanToBrood * Nest * Density + Colony + (1|ColorID), data = BroodCentDistWorkersSFZFid))

r.squaredGLMM(lmer(SFZArea ~ MeanToBrood * Nest * Density + (1 | Colony) + (1|ColorID), data = BroodCentDistWorkersSFZFid))

#Linear mixed effects model
#How does mean scaled distance to the nest entrance, nest shape, density treatment affect worker total nest space usage (occurrence zone)?  
#How much of this effect can be explained just by the random effect colony identification and worker color identification?
summary(lmer(SFZ_Full ~ MeanToBrood * Nest * Density + (1 | Colony) + (1|ColorID), data = BroodCentDistWorkersSFZ))
summary(lmer(SFZ_Full ~ MeanToBrood * Nest * Density + (1 | Colony) + (1|ColorID), data = BroodCentDistWorkersSFZ))

summary(lmer(SFZ_FullArea ~ MeanToBrood * Nest * Density + (1 | Colony) + (1|ColorID), data = BroodCentDistWorkersSFZ))

#Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(SFZ_Full ~ MeanToBrood * Nest * Density + (1 | Colony) + (1|ColorID), data = BroodCentDistWorkersSFZ))

#EXTRA to check
ggplot(data=BroodCentDistWorkersSFZ,aes(x=Nest,y=MeanToBrood,fill=fct_rev(Nest))) +
  ylab("Mean scaled distance to brood center") +
  xlab("Nest") +
  geom_boxplot(coef=200)+
  theme_classic()+
  theme(axis.line = element_line(color="black", size = 0.3))+
  theme(axis.text=element_text(size=12,family="Arial"),axis.title = element_text(size = 16,family="Arial")) + 
  theme(plot.title=element_text(size=16,face="bold",family="Arial"))+
  theme(legend.justification = c(1, 1))+
  guides(fill=guide_legend(title="Nest",family="Arial"))+
  theme(legend.text=element_text(size = 12,family="Arial"),
        legend.title=element_text(size=16,family="Arial"))+
  labs(color="Nest",linetype="Nest")+
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                     name="Nest",
                     values=c("red", "blue"),
                     labels=c("Tube","Circle"))

summary(lmer(MeanToBrood~Nest*Density+(1|Colony),data=BroodCentDistWorkersSFZ))
r.squaredGLMM(lmer(MeanToBrood~Nest*Density+(1|Colony),data=BroodCentDistWorkersSFZ))

WorkerDistScaledMeanDistRound<-WorkerDistScaledMeanDist%>%
  mutate_at(vars(ScaledDist,ScaledX,ScaledY), funs(round(., 2)))%>%
  select(-c(ScaledDist))
WorkerDistScaledRD1SFZFull2<-WorkerDistScaledRD1SFZFull1%>%
  mutate_at(vars(ScaledX,ScaledY), funs(round(., 2)))%>%
  select(-c(ScaledDist))
view(BroodCentDistWorkersSFZRD1_RD2Brood)
WorkerDistScaledRD1SFZFullMean<-left_join(WorkerDistScaledRD1SFZFull2,WorkerDistScaledMeanDistRound)

ggplot(BroodCentDistWorkersSFZFid,aes(MeanToBrood,fill=fct_rev(Nest)))+
  geom_histogram(binwidth=0.041666,alpha=0.5)+
  theme_pubclean()+
  theme(axis.text=element_text(size=12,family="Arial"),axis.title = element_text(size = 16,family="Arial")) + 
  theme(plot.title=element_text(size=16,face="bold",family="Arial"))+
  theme(legend.justification = c(1, 1))+
  guides(fill=guide_legend(title="Nest",family="Arial"))+
  theme(legend.text=element_text(size = 12,family="Arial"),
        legend.title=element_text(size=16,family="Arial"))+
  theme(panel.spacing = unit(1, "lines"))+
  theme(strip.text = element_text(size = 16,face="bold",family="Arial"))+
  labs(color="Density",fill="Nest")+
  xlab("Mean scaled distance to brood")+
  ylab("Worker zones count")+
  facet_wrap(~Colony)+
  scale_fill_manual(breaks = c("Tube", "Circle"), 
                    name="Nest",
                    values=c("red", "blue"))+
  xlim(0,1)
summary(lmer(MeanToBrood~Nest*Density*SFZ+(1|Colony)+(1|ColorID),data=BroodCentDistWorkersSFZFid))
r.squaredGLMM(lmer(MeanToBrood~Nest*Density*SFZ+(1|Colony)+(1|ColorID),data=BroodCentDistWorkersSFZFid))

# fit model with 3-way-interaction
fit <- lmer(SFZ_Full~MeanToBrood*Nest*Density+(1|Colony)+(1|ColorID),data=BroodCentDistWorkersSFZRD1_RD2)
mydf <- ggpredict(fit, terms = c("MeanToBrood", "Nest", "Density"))
#Plot
ggplot(mydf, aes(x, predicted, colour = group)) + 
  xlab("Mean scaled distance to the brood") +
  ylab("Predicted effect on occurrence zone size") +
  theme_set+
  geom_line() + 
  facet_wrap(~facet)+
  theme(axis.line = element_line(color="black", size = 0.2))+
  theme(axis.text=element_text(size=12,family="Arial"),axis.title = element_text(size = 16,family="Arial")) + 
  theme(plot.title=element_text(size=16,face="bold",family="Arial"))+
  theme(legend.justification = c(1, 1))+
  guides(fill=guide_legend(title="Nest",family="Arial"))+
  theme(legend.text=element_text(size = 12,family="Arial"),
        legend.title=element_text(size=16,family="Arial"))+
  theme(panel.spacing = unit(1, "lines"))+
  theme(strip.text = element_text(size = 16,face="bold",family="Arial"))+
  scale_color_manual(breaks = c("Tube", "Circle"), 
                     name="Nest",
                     values=c("red", "blue"),
                     labels=c("Tube","Circle"))

#DENSITY PLOTS
install.packages("ggpointdensity")
library(ggpointdensity)

install.packages("RANN")
library(RANN)


colorscaleWorkers = scale_color_gradientn(
  colors = rev(brewer.pal(9, "RdYlBu")),
  values = c(0, exp(seq(-3, 0, length.out = 100))))

colorscaleBrood = scale_color_gradientn(
  colors = rev(brewer.pal(9, "YlGnBu")),
  values = c(0, exp(seq(-3, 0, length.out = 100))))

colorscaleAlate = scale_color_gradientn(
  colors = rev(brewer.pal(9, "PuOr")),
  values = c(0, exp(seq(-3, 0, length.out = 100))))

colorscaleSmall = scale_color_gradientn(
  colors = rev(brewer.pal(9, "BuPu")),
  values = c(0, exp(seq(-3, 0, length.out = 100))))

colorscaleLarge = scale_color_gradientn(
  colors = rev(brewer.pal(9, "BuPu")),
  values = c(0, exp(seq(-3, 0, length.out = 100))))

AlateDistScaledRD2 %>% select(Colony) %>% distinct()

#WORKERS
WorkerDensityColony<- ggplot(data=WorkerDistScaledRD1_RD2%>%filter(Colony == 20) %>% filter (ScaledDist < 0.9),
       aes(ScaledX, ScaledY)) +
  geom_pointdensity(alpha = 0.75) +
  colorscaleWorkers +
  coord_fixed() +
  theme_pubclean()+
  xlab("X (cm)")+
  ylab("Y (cm)")+
  ggtitle("Workers") +
  theme(axis.text=element_text(size=18, family = "Arial", color = "black"),
        axis.title=element_blank(),
        legend.text=element_text(size = 18, family = "Arial", color = "black"),
        legend.title=element_text(size=18, family = "Arial", color = "black"),
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title=element_text(size=18,family = "Arial", color = "black", vjust = -6.75, hjust = 0.01))+
  labs(color=" KNN")+
  facet_wrap(~Nest) +
  guides(color = guide_colorsteps(barheight = unit(0.5, "cm"),
                                  barwidth = unit(7.5, "cm"),
                                 even.steps = FALSE,
                                 frame.colour = "black"))+
  xlim(0,7.5) +
  ylim(-0.25,5) 

#BROOD w/ QUEENS
BroodQueenDensityColony<- ggplot(data=BroodDistScaledRD1_RD2%>%filter(Colony == 20) %>% filter (ScaledDist < 0.9),
       aes(ScaledX,ScaledY))+
  geom_pointdensity(alpha = 0.75) +
  geom_point(data=QueenDistScaledRD1_RD2%>%filter(Colony == 20) %>% filter (ScaledDist < 0.9),
             aes(ScaledX,ScaledY),  alpha=0.85, color="black", size = 2, shape = 17) +
  colorscaleBrood +
  coord_fixed() +
  theme_pubclean()+
  xlab("X (cm)")+
  ylab("Y (cm)")+
  ggtitle("Brood & Queens") +
  theme(axis.text=element_text(size = 18, family = "Arial", color = "black"),
        axis.title=element_blank(),
        legend.text=element_text(size = 18, family = "Arial", color = "black"),
        legend.title=element_text(size = 18, family = "Arial", color = "black"),
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title=element_text(size= 18, family = "Arial", color = "black", vjust = -6.75, hjust = 0.01))+
  labs(color=" KNN")+
  facet_wrap(~Nest) +
  guides(color = guide_colorsteps(barheight = unit(0.5, "cm"),
                                 barwidth = unit(7.5, "cm"),
                                 even.steps = FALSE,
                                 frame.colour = "black"))+
  xlim(0,7.5)+
  ylim(-0.25,5) 


#ALATE DENSITY PLOTS
#     Colony
#     11
#     12
#     13
#     14
#     15
#     18
#     19
AlateDistScaledRD2 %>% select(Colony, Nest) %>% distinct()
AlateDensity <- ggplot(data=AlateDistScaledRD2%>%filter(Colony == 19) %>% filter (ScaledDist < 0.9),
                                   aes(ScaledX,ScaledY))+
  geom_pointdensity(alpha = 0.75) +
  colorscaleAlate +
  coord_fixed() +
  theme_pubclean()+
  xlab("X (cm)")+
  ylab("Y (cm)")+
  ggtitle("Alates") +
  theme(axis.text=element_text(size = 18, family = "Arial", color = "black"),
        axis.title=element_blank(),
        legend.text=element_text(size = 18, family = "Arial", color = "black"),
        legend.title=element_text(size = 18, family = "Arial", color = "black"),
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title=element_text(size = 18, family = "Arial", color = "black", vjust = -6.75, hjust = 0.01))+
  facet_wrap(~Nest) +
  labs(color=" KNN")+
  guides(color = guide_colorsteps(barheight = unit(0.5, "cm"),
                                  barwidth = unit(7.5, "cm"),
                                 even.steps = FALSE,
                                 frame.colour = "black"))+
  xlim(0,7.5)+
  ylim(-0.25,5) 

#SIMULATION DENSITY PLOTS
SmallRandWalkDensity <- ggplot(data=SimDistScaled%>%filter(NestSize == "Small") %>% filter (ScaledDist < 0.9),
       aes(ScaledX,ScaledY))+
  geom_pointdensity() +
  colorscaleSmall +
  coord_fixed() +
  theme_pubclean()+
  xlab("X (cm)")+
  ylab("Y (cm)")+
  ggtitle("Random walk") +
  theme(axis.text=element_text(size=18, family = "Arial", color = "black"),
        axis.title=element_blank(),
        legend.text=element_text(size = 18, family = "Arial", color = "black"),
        legend.title=element_text(size=20, family = "Arial", color = "black"),
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title=element_text(size = 18, family = "Arial", color = "black", vjust = -6.75, hjust = 0.01))+
  facet_wrap(~Nest) +
  labs(color="KNN")+
  guides(color = guide_colorsteps(barheight = unit(0.5, "cm"),
                                  barwidth = unit(7.5, "cm"),
                                 even.steps = FALSE,
                                 frame.colour = "black"))+
  xlim(0,7.5)+
  ylim(-0.25,5) 

LargeRandWalkDensity<- ggplot(data=SimDistScaled%>%filter(NestSize == "Large") %>% filter (ScaledDist < 0.9),
       aes(ScaledX,ScaledY))+
  geom_pointdensity(alpha = 0.75) +
  colorscaleLarge +
  coord_fixed() +
  theme_pubclean()+
  xlab("X (cm)")+
  ylab("Y (cm)")+
  ggtitle("Random walk") +
  theme(axis.text=element_text(size=18, family = "Arial", color = "black"),
        axis.title=element_blank(),
        legend.text=element_text(size = 18, family = "Arial", color = "black"),
        legend.title=element_text(size = 18, family = "Arial", color = "black"),
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.title=element_text(size = 18, family = "Arial", color = "black", vjust = -6.75, hjust = 0.01))+
  facet_wrap(~Nest) +
  labs(color=" KNN")+
  guides(color = guide_colorsteps(barheight = unit(0.5, "cm"),
                                  barwidth = unit(7.5, "cm"),
                                 even.steps = FALSE,
                                 frame.colour = "black"))+
  xlim(0,7.5)+
  ylim(-0.25,5) 


FullDensityPlot <- ggarrange(WorkerDensityColony, BroodQueenDensityColony,
          LargeRandWalkDensity,
          font.label = list(size = 18, face = "plain", family = "Arial"),
          ncol = 2, nrow = 2,
          widths = 0.05,
          vjust = 1.85,
          label.x = 0.85)

DensityPlotColony20 <- FullDensityPlot

annotate_figure(FullDensityPlot,
                top = text_grob("Colony 20 Member Densities", color = "black",
                                size = 18, x = 0.2, family = "Arial"),
                bottom = text_grob("X (cm)", color = "black",
                                   size = 18, x = 0.5, y = 1, family = "Arial"),
                left = text_grob("Y (cm)", color = "black",
                                 size = 18, rot = 90, y = 0.55, x= 1, family = "Arial"),
                right = NULL
)

view(QueenDistScaled %>% filter(Colony == 10 & Nest == "Circle")) 

#Colony ensity plots
DensityPlotColony1
DensityPlotColony2
DensityPlotColony3
DensityPlotColony4
DensityPlotColony5
DensityPlotColony6
DensityPlotColony7
DensityPlotColony8
DensityPlotColony9
DensityPlotColony10
DensityPlotColony11
DensityPlotColony12
DensityPlotColony13
DensityPlotColony14
DensityPlotColony15
DensityPlotColony16
DensityPlotColony17
DensityPlotColony18
DensityPlotColony19
DensityPlotColony20