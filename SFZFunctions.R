#############################################
## Autor: Greg CHISM
## Date: Aug 2021
## email: gchism@email.arizona.edu
## Project: Nest shape influences colony organization in ants
## Title: Site fidelity zone assignment, calculations, and distance functions
#############################################

#This code is to replicate the analyses and figures for the following in my first chapter:
#Zone assignment for site fidelity
#Fidelity zone calculations
#Fidelity zone distance functions

#SITE FIDELITY ASSIGNMENT FUNCTION
#The code below bins x and y coordinate colony data into twenty-four even area nest sections
#Note that the code is specifically used below for the low density treatment colonies (11-20)
#This is because the high density treatment colonies (1-10) were done through excel but can be run through this script as well
#To do this, a reference dataset of bin coordinates is used and coordinates are run through a series of conditional statements
#Where each conditional statement checks whether the coordinate is in one of twenty-four bins sequentially
#The reference dataset for tube nest bins was used here as well, since the bins were divided by three to achieve the desired nest sections here
#The circle nest shape has a different set of reference coordinates

#Note that you have to change the value of "Colony" before each iteration.
#A for loop could have accomplished to avoid changing the value, but computational power was a concern

BinCoord24Circ <- ColonySize %>%
  mutate(Radius = Diameter / 2) %>%
  left_join(Scaling) %>%
  mutate(AddValue = Radius * Scaling) %>%
  distinct()
write.csv(BinCoordRD1Full,"BinCoordRD1Full.csv", row.names = FALSE)

BinCoordRD1 <- read_excel("BinCoordFull.xlsx", 
                          sheet = "Sheet2")
view(BinCoord)
BinCoordFull
CoordBinnedSFZ <- function(data_table){
  #Filtering the desired colony from the reference datasets
  SFZBinCoordAssign <- BinCoord %>%
    filter(Colony == "13") 
  BinCoordAssign <- BinCoord %>%
    filter(Colony == "13") 
  BinCoord24Circ <- BinCoord24Circ %>%
    filter(Colony == "13")
  
  #Filtering the desired colony from the worker tube nest datasets
  #Note that the site fidelity Bins will be referred to as "Zones" from here on
  #Tube nest zones
  Colony13RD2BinnedTubeSFZ <- data_table %>%
    filter(Colony == "13" & Nest == "Tube") %>%
    #The following columns create reference points from the eight even area bin reference dataset
    #The goal is to produce a segment length that is along one of the eight bins, each encompassing three of the twenty-four zones
    #Since these bins were cut into thirds, it is more accurate to simply use these segments than to create a whole new set of reference coordinates
    mutate(Bin1.3 = SFZBinCoordAssign$ScaledY[2], #Length from Zone 1 to 3
           Bin4.6 = abs(SFZBinCoordAssign$ScaledX[2] - SFZBinCoordAssign$ScaledX[3]), #Length from Zone 4 to 6
           Bin7.9 = abs(SFZBinCoordAssign$ScaledY[4] - SFZBinCoordAssign$ScaledY[3]), #Length from Zone 7 to 9
           Bin10.12 = abs(SFZBinCoordAssign$ScaledX[5] - SFZBinCoordAssign$ScaledX[4]), #Length from Zone 10 to 12
           Bin13.15 = abs(SFZBinCoordAssign$ScaledX[6] - SFZBinCoordAssign$ScaledX[5]), #Length from Zone 13 to 15
           Bin16.18 = abs(SFZBinCoordAssign$ScaledX[7] - SFZBinCoordAssign$ScaledX[6]), #Length from Zone 16 to 18
           Bin19.21 = abs(SFZBinCoordAssign$ScaledX[7] - SFZBinCoordAssign$ScaledX[9]), #Length from Zone 19 to 21
           Bin22.24 = abs(SFZBinCoordAssign$ScaledX[8] - SFZBinCoordAssign$ScaledX[9]), #Length from Zone 22 to 24
           #Binning the worker coordinates into a Zone
           Zone =
             #Zone 1
             if_else(ScaledX >= SFZBinCoordAssign$ScaledX[2] & 
                       ScaledX <= SFZBinCoordAssign$ScaledX[1] &
                       ScaledY <= (1/3) * (Bin1.3), 1, 
                     #Zone 2
                     if_else(ScaledX >= SFZBinCoordAssign$ScaledX[2] & 
                               ScaledX <= SFZBinCoordAssign$ScaledX[1] &
                               ScaledY >= (1/3) * (Bin1.3) &
                               ScaledY <= (2/3) * (Bin1.3), 2,
                             #Zone 3
                             if_else(ScaledX >= SFZBinCoordAssign$ScaledX[2] & 
                                       ScaledX <= SFZBinCoordAssign$ScaledX[1] &
                                       ScaledY >= (2/3) * (Bin1.3) &
                                       ScaledY <= (Bin1.3), 3, 
                                     #Zone 4
                                     if_else(ScaledX >= BinCoordAssign$ScaledX[3] + ((2/3) * (Bin4.6)) & 
                                               ScaledX <= BinCoordAssign$ScaledX[3] + (Bin4.6) &
                                               ScaledY >= BinCoordAssign$ScaledY[3] &
                                               ScaledY <= BinCoordAssign$ScaledY[2], 4,
                                             #Zone 5
                                             if_else(ScaledX >= BinCoordAssign$ScaledX[3] + ((1/3) * (Bin4.6)) & 
                                                       ScaledX <= BinCoordAssign$ScaledX[3] + ((2/3) * (Bin4.6)) &
                                                       ScaledY >= BinCoordAssign$ScaledY[3] &
                                                       ScaledY <= BinCoordAssign$ScaledY[2], 5,
                                                     #Zone 6
                                                     if_else(ScaledX <= BinCoordAssign$ScaledX[3] + ((1/3) * (Bin4.6)) &
                                                               ScaledX >= BinCoordAssign$ScaledX[3] &
                                                               ScaledY >= BinCoordAssign$ScaledY[3] &
                                                               ScaledY <= BinCoordAssign$ScaledY[2], 6,
                                                             #Zone 7
                                                             if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                                                       ScaledX <= BinCoordAssign$ScaledX[3] &
                                                                       ScaledY <= BinCoordAssign$ScaledY[3] + ((1/3) * (Bin7.9)), 7,
                                                                     #Zone 8
                                                                     if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                                                               ScaledX <= BinCoordAssign$ScaledX[3] &
                                                                               ScaledY >= BinCoordAssign$ScaledY[3] + ((1/3) * (Bin7.9)) &
                                                                               ScaledY <= BinCoordAssign$ScaledY[3] + ((2/3) * (Bin7.9)), 8,
                                                                             #Zone 9
                                                                             if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                                                                       ScaledX <= BinCoordAssign$ScaledX[3] &
                                                                                       ScaledY >= BinCoordAssign$ScaledY[3] + ((2/3) * (Bin7.9)) & 
                                                                                       ScaledY <= BinCoordAssign$ScaledY[3] + (Bin7.9), 9,
                                                                                     #Zone 10
                                                                                     if_else(ScaledX <= BinCoordAssign$ScaledX[4] + ((1/3) * Bin10.12) & 
                                                                                               ScaledY >= BinCoordAssign$ScaledY[4] &
                                                                                               ScaledY <= BinCoordAssign$ScaledY[5], 10,
                                                                                             #Zone 11
                                                                                             if_else(ScaledX >= BinCoordAssign$ScaledX[4] + ((1/3) * Bin10.12) & 
                                                                                                       ScaledX <= BinCoordAssign$ScaledX[4] + ((2/3) * Bin10.12) & 
                                                                                                       ScaledY >= BinCoordAssign$ScaledY[4] &
                                                                                                       ScaledY <= BinCoordAssign$ScaledY[5], 11,
                                                                                                     #Zone 12
                                                                                                     if_else(ScaledX <= BinCoordAssign$ScaledX[4] + Bin10.12 & 
                                                                                                               ScaledX >= BinCoordAssign$ScaledX[4] + ((2/3) * Bin10.12) &
                                                                                                               ScaledY >= BinCoordAssign$ScaledY[4] &
                                                                                                               ScaledY <= BinCoordAssign$ScaledY[5], 12,
                                                                                                             #Zone13
                                                                                                             if_else(ScaledX >= BinCoordAssign$ScaledX[4] + Bin10.12 &
                                                                                                                       ScaledX <= BinCoordAssign$ScaledX[5] + ((1/3) * Bin13.15) &
                                                                                                                       ScaledY >= BinCoordAssign$ScaledY[4] &
                                                                                                                       ScaledY <= BinCoordAssign$ScaledY[5], 13,
                                                                                                                     #Zone 14
                                                                                                                     if_else(ScaledX >= BinCoordAssign$ScaledX[5] + ((1/3) * Bin13.15) & 
                                                                                                                               ScaledX <= BinCoordAssign$ScaledX[5] + ((2/3) * Bin13.15) & 
                                                                                                                               ScaledY >= BinCoordAssign$ScaledY[4] &
                                                                                                                               ScaledY <= BinCoordAssign$ScaledY[5], 14,
                                                                                                                             #Zone 15
                                                                                                                             if_else( ScaledX >= BinCoordAssign$ScaledX[5] + ((2/3) * Bin13.15) &
                                                                                                                                        ScaledX <= BinCoordAssign$ScaledX[5] + Bin13.15 & 
                                                                                                                                        ScaledY >= BinCoordAssign$ScaledY[4] &
                                                                                                                                        ScaledY <= BinCoordAssign$ScaledY[5], 15,
                                                                                                                                      #Zone 16
                                                                                                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] + Bin13.15 & 
                                                                                                                                                 ScaledX <= BinCoordAssign$ScaledX[6] + ((1/3) * Bin16.18) & 
                                                                                                                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                                                                                                                 ScaledY <= BinCoordAssign$ScaledY[5], 16,
                                                                                                                                               #Zone 17
                                                                                                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] + ((1/3) * Bin16.18) & 
                                                                                                                                                         ScaledX <= BinCoordAssign$ScaledX[6] + ((2/3) * Bin16.18) & 
                                                                                                                                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                                                                                                                                         ScaledY <= BinCoordAssign$ScaledY[5], 17,
                                                                                                                                                       #Zone 18
                                                                                                                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[6] + ((2/3) * Bin16.18) &
                                                                                                                                                                 ScaledX <= BinCoordAssign$ScaledX[6] + Bin16.18 &
                                                                                                                                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                                                                                                                                 ScaledY <= BinCoordAssign$ScaledY[5], 18,
                                                                                                                                                               #Zone 19
                                                                                                                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                                                                                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                                                                                                                         ScaledY <= BinCoordAssign$ScaledY[7] & 
                                                                                                                                                                         ScaledY >= BinCoordAssign$ScaledY[7] - (1/3) * (Bin19.21), 19,
                                                                                                                                                                       #Zone 20
                                                                                                                                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                                                                                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                                                                                                                                 ScaledY <= BinCoordAssign$ScaledY[7] - (1/3) * (Bin19.21) & 
                                                                                                                                                                                 ScaledY >= BinCoordAssign$ScaledY[7] - (2/3) * (Bin19.21), 20,
                                                                                                                                                                               #Zone 21
                                                                                                                                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                                                                                                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                                                                                                                                         ScaledY <= BinCoordAssign$ScaledY[7] - (2/3) * (Bin19.21) & 
                                                                                                                                                                                         ScaledY >= BinCoordAssign$ScaledY[7] - (Bin19.21), 21,
                                                                                                                                                                                       #Zone 22
                                                                                                                                                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[9] + ((2/3) * (Bin22.24)) & 
                                                                                                                                                                                                 ScaledX <= BinCoordAssign$ScaledX[9] + (Bin22.24) &
                                                                                                                                                                                                 ScaledY <= BinCoordAssign$ScaledY[9], 22,
                                                                                                                                                                                               #Zone 23
                                                                                                                                                                                               if_else(ScaledX <= BinCoordAssign$ScaledX[9] + ((2/3) * (Bin22.24)) & 
                                                                                                                                                                                                         ScaledX >= BinCoordAssign$ScaledX[9] + ((1/3) * (Bin22.24)) &
                                                                                                                                                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 23,
                                                                                                                                                                                                       #Zone 24
                                                                                                                                                                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                                                                                                                                                                 ScaledX <= BinCoordAssign$ScaledX[9] + (1/3) * (Bin22.24) &
                                                                                                                                                                                                                 ScaledY <= BinCoordAssign$ScaledY[9], 24, 0
                                                                                                                                                                                                       )))))))))))))))))))))))))
  #Circle nest zones
  Colony13RD2BinnedCircleSFZ <- data_table %>%
  filter(Colony == "13" & Nest == "Circle") %>%
  mutate(Zone =
           #Zone 1
           if_else(ScaledX <= 3.75 - BinCoord24Circ$AddValue[1] &
                     ScaledY <= BinCoordAssign$ScaledY[10], 1,
                   #Zone 2
                   if_else(ScaledX >= 3.75 - BinCoord24Circ$AddValue[1] &
                             ScaledX <= 3.75 + BinCoord24Circ$AddValue[1] &
                             ScaledY <= BinCoordAssign$ScaledY[10], 2,
                           #Zone 3
                           if_else(ScaledX >= 3.75 + BinCoord24Circ$AddValue[1] &
                                     ScaledY <= BinCoordAssign$ScaledY[10], 3,
                                   #Zone 4
                                   if_else(ScaledX <= 3.75 - BinCoord24Circ$AddValue[2] &
                                             ScaledY >= BinCoordAssign$ScaledY[10] & 
                                             ScaledY <= BinCoordAssign$ScaledY[11], 4,
                                           #Zone 5
                                           if_else(ScaledX >= 3.75 - BinCoord24Circ$AddValue[2] &
                                                     ScaledX <= 3.75 + BinCoord24Circ$AddValue[2] &
                                                     ScaledY >= BinCoordAssign$ScaledY[10] &
                                                     ScaledY <= BinCoordAssign$ScaledY[11], 5,
                                                   #Zone 6
                                                   if_else(ScaledX >= 3.75 + BinCoord24Circ$AddValue[2] &
                                                             ScaledY >= BinCoordAssign$ScaledY[10] & 
                                                             ScaledY <= BinCoordAssign$ScaledY[11], 6,
                                                           #Zone 7
                                                           if_else(ScaledX <= 3.75 - BinCoord24Circ$AddValue[3] &
                                                                     ScaledY >= BinCoordAssign$ScaledY[11] & 
                                                                     ScaledY <= BinCoordAssign$ScaledY[12], 7,
                                                                   #Zone 8
                                                                   if_else(ScaledX >= 3.75 - BinCoord24Circ$AddValue[3] &
                                                                             ScaledX <= 3.75 + BinCoord24Circ$AddValue[3] &
                                                                             ScaledY >= BinCoordAssign$ScaledY[11] & 
                                                                             ScaledY <= BinCoordAssign$ScaledY[12], 8,
                                                                           #Zone 9
                                                                           if_else(ScaledX >= 3.75 + BinCoord24Circ$AddValue[3] &
                                                                                     ScaledY >= BinCoordAssign$ScaledY[11] & 
                                                                                     ScaledY <= BinCoordAssign$ScaledY[12], 9, 
                                                                                   #Zone 10
                                                                                   if_else(ScaledX <= 3.75 - BinCoord24Circ$AddValue[4] &
                                                                                             ScaledY >= BinCoordAssign$ScaledY[12] & 
                                                                                             ScaledY <= BinCoordAssign$ScaledY[13], 10,
                                                                                           #Zone 11
                                                                                           if_else(ScaledX >= 3.75 - BinCoord24Circ$AddValue[4] &
                                                                                                     ScaledX <= 3.75 + BinCoord24Circ$AddValue[4] &
                                                                                                     ScaledY >= BinCoordAssign$ScaledY[12] & 
                                                                                                     ScaledY <= BinCoordAssign$ScaledY[13], 11,
                                                                                                   #Zone 12
                                                                                                   if_else(ScaledX >= 3.75 + BinCoord24Circ$AddValue[4] &
                                                                                                             ScaledY >= BinCoordAssign$ScaledY[12] & 
                                                                                                             ScaledY <= BinCoordAssign$ScaledY[13], 12,
                                                                                                           #Zone 13
                                                                                                           if_else(ScaledX <= 3.75 - BinCoord24Circ$AddValue[4] &
                                                                                                                     ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                                                                                     ScaledY <= BinCoordAssign$ScaledY[14], 13,
                                                                                                                   #Zone 14
                                                                                                                   if_else(ScaledX >= 3.75 - BinCoord24Circ$AddValue[4] &
                                                                                                                             ScaledX <= 3.75 + BinCoord24Circ$AddValue[4] &
                                                                                                                             ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                                                                                             ScaledY <= BinCoordAssign$ScaledY[14], 14,
                                                                                                                           #Zone 15
                                                                                                                           if_else(ScaledX >= 3.75 + BinCoord24Circ$AddValue[4] &
                                                                                                                                     ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                                                                                                     ScaledY <= BinCoordAssign$ScaledY[14], 15,
                                                                                                                                   #Zone 16
                                                                                                                                   if_else(ScaledX <= 3.75 - BinCoord24Circ$AddValue[3] &
                                                                                                                                             ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                                                                                                             ScaledY <= BinCoordAssign$ScaledY[15], 16,
                                                                                                                                           #Zone 17
                                                                                                                                           if_else(ScaledX >= 3.75 - BinCoord24Circ$AddValue[3] &
                                                                                                                                                     ScaledX <= 3.75 + BinCoord24Circ$AddValue[3] &
                                                                                                                                                     ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                                                                                                                     ScaledY <= BinCoordAssign$ScaledY[15], 17,
                                                                                                                                                   #Zone 18
                                                                                                                                                   if_else(ScaledX >= 3.75 + BinCoord24Circ$AddValue[3] &
                                                                                                                                                             ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                                                                                                                             ScaledY <= BinCoordAssign$ScaledY[15], 18,
                                                                                                                                                           #Zone 19
                                                                                                                                                           if_else(ScaledX <= 3.75 - BinCoord24Circ$AddValue[2] &
                                                                                                                                                                     ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                                                                                                                     ScaledY <= BinCoordAssign$ScaledY[16], 19,
                                                                                                                                                                   #Zone 20
                                                                                                                                                                   if_else(ScaledX >= 3.75 - BinCoord24Circ$AddValue[2] &
                                                                                                                                                                             ScaledX <= 3.75 + BinCoord24Circ$AddValue[2] &
                                                                                                                                                                             ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                                                                                                                             ScaledY <= BinCoordAssign$ScaledY[16], 20,
                                                                                                                                                                           #Zone 21
                                                                                                                                                                           if_else(ScaledX >= 3.75 + BinCoord24Circ$AddValue[2] &
                                                                                                                                                                                     ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                                                                                                                                     ScaledY <= BinCoordAssign$ScaledY[16], 21,
                                                                                                                                                                                   #Zone 22
                                                                                                                                                                                   if_else(ScaledX <= 3.75 - BinCoord24Circ$AddValue[1] &
                                                                                                                                                                                             ScaledY >= BinCoordAssign$ScaledY[16], 22,
                                                                                                                                                                                           #Zone 23
                                                                                                                                                                                           if_else(ScaledX >= 3.75 - BinCoord24Circ$AddValue[1] &
                                                                                                                                                                                                     ScaledX <= 3.75 + BinCoord24Circ$AddValue[1] &
                                                                                                                                                                                                     ScaledY >= BinCoordAssign$ScaledY[16], 23,
                                                                                                                                                                                                   #Zone 24
                                                                                                                                                                                                   if_else(ScaledX >= 3.75 + BinCoord24Circ$AddValue[1] &
                                                                                                                                                                                                             ScaledY >= BinCoordAssign$ScaledY[16], 24, 0
                                                                                                                                                                                                           )))))))))))))))))))))))))
  
  #Join the site fidelity zone datasets
  Colony13RD2BinnedSFZ <<- full_join(Colony13RD2BinnedTubeSFZ, Colony13RD2BinnedCircleSFZ) %>%
    select(Colony, Nest, Day, Bin, ScaledX, ScaledY, Zone, ColorID)
}
Colony6RD2BinnedSFZ1
Colony6RD2BinnedSFZ
CoordBinnedSFZ(FullDataCoordRD2)

NestArchFullRD1SFZWorking1
SFZRD2DataFull2.1 <- SFZRD2DataFull2 %>%
  select(-c(Zone))
SFZRD2DataFull2

view(SFZRD2DataFull2)
NestArchFullRD1SFZWorking1 <-NestArchFullRD1SFZWorking1 %>%
  filter(Colony !=6) %>%
  full_join(Colony6RD2BinnedSFZ1)
view(Colony14RD2BinnedSFZ)
#Checking for workers with no zone assignment
#This also checks whether the correct colony assignment was used throughout
FullDataCoord <- FullDataCoord %>%
  filter(Colony != 6) %>%
  full_join(Colony6RD2BinnedSFZ1)
FullDataCoord
view(Colony1RD2BinnedSFZ%>%
  filter(Zone=="0")) 

Colony15RD2BinnedSFZ%>%
  filter(Zone=="0")%>%
  group_by(Zone)%>%
  summarise(n=n())

 
Colony9RD2BinnedSFZ1%>%
  filter(Zone=="0")%>%
  group_by(Zone)%>%
  summarise(n=n())

Colony3RD2BinnedSFZ 
Colony3RD2BinnedSFZ%>%
  filter(Zone=="0")%>%
  group_by(Zone)%>%
  summarise(n=n())

Colony4RD2BinnedSFZ 
Colony4RD2BinnedSFZ%>%
  filter(Zone=="0")%>%
  group_by(Zone)%>%
  summarise(n=n())

Colony5RD2BinnedSFZ 
Colony5RD2BinnedSFZ%>%
  filter(Zone=="0")%>%
  group_by(Zone)%>%
  summarise(n=n())

Colony6RD2BinnedSFZ 
Colony6RD2BinnedSFZ%>%
  filter(Zone=="0")%>%
  group_by(Zone)%>%
  summarise(n=n())

Colony7RD2BinnedSFZ 
Colony7RD2BinnedSFZ%>%
  filter(Zone=="0")%>%
  group_by(Zone)%>%
  summarise(n=n())

Colony8RD2BinnedSFZ 
Colony8RD2BinnedSFZ%>%
  filter(Zone=="0")%>%
  group_by(Zone)%>%
  summarise(n=n())

Colony9RD2BinnedSFZ%>%
  filter(Zone=="0") 
Colony9RD2BinnedSFZ%>%
  filter(Zone=="0")%>%
  group_by(Zone)%>%
  summarise(n=n())

Colony10RD2BinnedSFZ 
SFZRD2DataFull%>%
  filter(Zone=="0")%>%
  group_by(Zone)%>%
  summarise(n=n())

view(SFZRD2DataFullZone)
SFZRD2DataFullZone
SFZRD2DataFullZone
SFZRD2DataFullZone<- full_join(Colony11RD2BinnedSFZ, Colony13RD2BinnedSFZ) %>%
  full_join(Colony12RD2BinnedSFZ) %>%
  full_join(Colony14RD2BinnedSFZ) %>%
  full_join(Colony15RD2BinnedSFZ) %>%
  full_join(Colony16RD2BinnedSFZ) %>%
  full_join(Colony17RD2BinnedSFZ) %>%
  full_join(Colony18RD2BinnedSFZ) %>%
  full_join(Colony19RD2BinnedSFZ) %>%
  full_join(Colony20RD2BinnedSFZ) %>%
  distinct()
  select(-c(CoordID))

#Joining all of the separate site fidelity zone datasets
  NestArchProjRD1WorkerBinsSFZ1
NestArchProjRD1WorkerBinsSFZ1<-full_join(Colony1RD2BinnedSFZ1, Colony2RD2BinnedSFZ1) %>%
  full_join(Colony3RD2BinnedSFZ1) %>%
  full_join(Colony4RD2BinnedSFZ1) %>%
  full_join(Colony5RD2BinnedSFZ1) %>%
  full_join(Colony6RD2BinnedSFZ1) %>%
  full_join(Colony7RD2BinnedSFZ1) %>%
  full_join(Colony8RD2BinnedSFZ1) %>%
  full_join(Colony9RD2BinnedSFZ1) %>%
  full_join(Colony10RD2BinnedSFZ1) %>%
  filter(Zone != 0)

ColorRefRD1
#CORRECTING COLOR COORDINATES
#The following function corrects viable worker color identification
#This means that there is a missing color and only one possible combination
#The function uses the worker color combinations for each colony as a reference
#First the worker dataset is set so that missing colors are "X"
#Then the colors are filled by looking at all possible combinations of one missing color mark for each colony
#The function then checks for any rows that didn't have a corrected combination, resulting most likely from the color being misinterpreted
ColorCoords <- function(data_table){
  #Fixing missing marks to only by "X" 
  ColorSetting <- data_table %>%
    #Separate the ColorID column into 4 columns, one for each mark 
    separate(ColorID, c("Head", "Thorax", "Abd1", "Abd2"), sep = ",", remove = FALSE) %>% 
    #Changing "?" values to "X" in each column
    mutate(Head = ifelse(Head == "?", "X", Head),
           Thorax = ifelse(Thorax == "?", "X", Thorax),
           Abd1 = ifelse(Abd1 == "?", "X", Abd1),
           Abd2 = ifelse(Abd2 == "?", "X", Abd2),
           Head = ifelse(Head == "Lb", "LB", Head),
           Thorax = ifelse(Thorax == "Lb", "LB", Thorax),
           Abd1 = ifelse(Abd1 == "Lb", "LB", Abd1),
           Abd2 = ifelse(Abd2 == "Lb", "LB", Abd2),
           Head = ifelse(Head == "Db", "DB", Head),
           Thorax = ifelse(Thorax == "Db", "DB", Thorax),
           Abd1 = ifelse(Abd1 == "Db", "DB", Abd1),
           Abd2 = ifelse(Abd2 == "Db", "DB", Abd2)) %>%
    #Uniting the 4 columns to the original ColorID column
    unite(ColorID, c("Head", "Thorax", "Abd1", "Abd2"), sep = ",", remove = TRUE)
  #Creating a reference dataset of all possible combinations for one missing color mark
  ColorRefRD1 <<- ColorRefRD1%>%
    #Unite the color reference columns
    unite(ColorIDRef, c("Head", "Thorax", "Abd1", "Abd2"), sep = ",", remove = FALSE) %>%
    distinct()
  #Creating individual datasets to create one column full of "X"
  #Filling one of the reference columns
  X1 <- ColorRefRD1 %>% 
    mutate(Head = "X")
  X2 <- ColorRefRD1%>% 
    mutate(Thorax = "X")
  X3 <- ColorRefRD1%>% 
    mutate(Abd1 = "X")
  X4 <- ColorRefRD1%>% 
    mutate(Abd2 = "X")
  #Joing the reference datasets
  FullColorCoordRD1Ref <- full_join(X1,X2) %>%
    full_join(X3) %>%
    full_join(X4) %>%
    #Uniting the reference columns
    #This new column is the same name as in the real dataset
    #So any column with the same missing value will now have the true color combination in the ColorIDRef column
    unite(ColorID, c("Head", "Thorax", "Abd1", "Abd2"), sep = ",", remove = TRUE)
  #Creating the final dataset with the original combination and true color combination
  SFZRD1DataFull <<- left_join(ColorSetting, FullColorCoordRD1Ref) %>%
    #Separating the true ColorID column and removing rows with more than one "X"
    #This is done by creating a column that assigns a 1 when two columns are "X"
    separate(ColorID, c("Head", "Thorax", "Abd1", "Abd2"), sep = ",", remove = FALSE) %>%
    mutate(XCount = ifelse(Head == "X" & Thorax == "X" | 
                             Abd1 == "X" & Abd2 == "X" | 
                             Head == "X" & Abd1 == "X" |
                             Head == "X" & Abd2 == "X" | 
                             Thorax == "X" & Abd1 == "X"|
                             Thorax == "X" & Abd2 == "X", 1, 0),
           #Changing NA values in ColorIDRef to 0, which occurs when a ColorID doesn't have just one missing color
           #The objective is to remove both rows with more than two missing colors and keep all fully marked workers 
           ColorIDRef = ifelse(is.na(ColorIDRef), 0, ColorIDRef),
           ColorID = ifelse(ColorIDRef == 0, ColorID, ColorIDRef)) %>%
    #Filtering out rows with more than one "X", or missing color
    filter(XCount != 1) %>%
    #Selecting the desired columns 
    select(Colony, Nest, Day, ScaledX, ScaledY, Zone, Bin, ColorID)
}
ColorCoords(FullDataCoord)
view(SFZRD1DataFullTest)
SFZRD1DataFull
NestArchFullRD1SFZWorking <- FullDataCoord
FullDataCoordRD2Join <- FullDataCoordRD2 %>%
  select(-c(ColorID)) %>%
  distinct() %>%
  left_join(FullDataCoordRD2) %>%
  rename(ColorID1 = ColorID) 
FullDataCoordTesting
FullDataCoord <- FullDataCoord %>%
  distinct()
view(FullDataCoordTesting)
FullDataCoordRD2Join <- left_join()
FullDataCoordTesting
FullDataCoordRD2
SFZRD2DataFull1
FullDataCoordRD2
FullDataCoordTesting <- SFZRD2DataFull1 %>%
  rename(ColorID1 = ColorID) %>%
  left_join(SFZRD2DataFull) %>%
      mutate(Remove = ifelse(is.na(Remove), "YES", Remove)) %>%
      filter(Remove == "NO")
    view(FullDataCoordTesting)
    FullDataCoordRD2
    view(SFZRD2DataFull2)
    SFZRD2DataFull2
    SFZRD2DataFull
    view(FullDataCoordTesting)
    
FullDataCoordTesting <- left_join(SFZRD2DataFull2, SFZRD2DataFull1) %>%
  mutate(Remove = ifelse(is.na(Remove), "YES", Remove)) %>%
  rename(ColorID1 = ColorID)
  mutate(Remove = ifelse(ColorID != ColorID2, ))
view(FullDataCoordRD2)
FullDataCoordTesting
SFZRD2DataFull
view(FullDataCoordTesting1)
FullDataCoordTesting
FullDataCoordTesting
FullDataCoordTesting2 <- FullDataCoordTesting1
view(FullDataCoordTesting1)
FullDataCoordTesting3
FullDataCoordTesting1

FullDataCoordTesting1 
FullDataCoordTesting
FullDataCoordTesting2
FullDataCoordTesting2 <- full_join(FullDataCoordTesting1, SFZRD2DataFull)
FullDataCoordTesting3 <- left_join(FullDataCoordRD2, FullDataCoordTesting) %>%
  mutate(Remove = ifelse(is.na(Remove), "NO", Remove)) %>%
  filter(Remove == "NO")
view(FullDataCoordTesting1)
FullDataCoordTesting1<-FullDataCoordRD2 %>%
  separate(ColorID, c("Head", "Thorax", "Abd1", "Abd2"), sep = ",", remove = FALSE) %>%
  #Changing "?" values to "X" in each column
  mutate(Head = ifelse(Head == "?", "X", Head),
         Thorax = ifelse(Thorax == "?", "X", Thorax),
         Abd1 = ifelse(Abd1 == "?", "X", Abd1),
         Abd2 = ifelse(Abd2 == "?", "X", Abd2)) %>%
  mutate(ColorID = ifelse(is.na(ColorID), 0, ColorID)) %>%
  mutate(XCount = ifelse(Head == "X" & Thorax == "X" | 
                           Abd1 == "X" & Abd2 == "X" | 
                           Head == "X" & Abd1 == "X" |
                           Head == "X" & Abd2 == "X" | 
                           Thorax == "X" & Abd1 == "X"|
                           Thorax == "X" & Abd2 == "X", 1, 0)) %>%
  filter(XCount != 0) %>%
  select(-c(XCount, Head, Thorax, Abd1, Abd2))
SFZRD2DataFull1
write.csv(FullDataCoordTesting1, "FullDataCoordTesting1.csv", row.names = FALSE)
view(ColorRefRD2)
#CORRECTING COLOR COORDINATES
#The following function corrects viable worker color identification
#This means that there is a missing color and only one possible combination
#The function uses the worker color combinations for each colony as a reference
#First the worker dataset is set so that missing colors are "X"
#Then the colors are filled by looking at all possible combinations of one missing color mark for each colony
#The function then checks for any rows that didn't have a corrected combination, resulting most likely from the color being misinterpreted
ColorCoords <- function(data_table){
  #Fixing missing marks to only by "X" 
  ColorSetting <- data_table %>%
    #Separate the ColorID column into 4 columns, one for each mark 
    separate(ColorID, c("Head", "Thorax", "Abd1", "Abd2"), sep = ",", remove = FALSE) %>% 
    #Changing "?" values to "X" in each column
    mutate(Head = ifelse(Head == "?", "X", Head),
           Thorax = ifelse(Thorax == "?", "X", Thorax),
           Abd1 = ifelse(Abd1 == "?", "X", Abd1),
           Abd2 = ifelse(Abd2 == "?", "X", Abd2)) %>%
    #Uniting the 4 columns to the original ColorID column
    unite(ColorID, c("Head", "Thorax", "Abd1", "Abd2"), sep = ",", remove = TRUE)
  #Creating a reference dataset of all possible combinations for one missing color mark
  ColorRefRD2 <- ColorRefRD2%>%
    #Unite the color reference columns
    unite(ColorIDRef, c("Head", "Thorax", "Abd1", "Abd2"), sep = ",", remove = FALSE)
  #Creating individual datasets to create one column full of "X"
  X1 <- ColorRefRD2
  X2 <- ColorRefRD2
  X3 <- ColorRefRD2
  X4 <- ColorRefRD2
  #Filling one of the reference columns
  X1[3] <- "X"
  X2[4] <- "X"
  X3[5] <- "X"
  X4[6] <- "X"
  #Joing the reference datasets
  FullColorCoordRD2Ref <- full_join(X1,X2) %>%
    full_join(X3) %>%
    full_join(X4) %>%
    #Uniting the reference columns
    #This new column is the same name as in the real dataset
    #So any column with the same missing value will now have the true color combination in the ColorIDRef column
    unite(ColorID, c("Head", "Thorax", "Abd1", "Abd2"), sep = ",", remove = TRUE)
  #Creating the final dataset with the original combination and true color combination
  SFZRD2DataFull2 <<- left_join(ColorSetting, FullColorCoordRD2Ref) %>%
    #Separating the true ColorID column and removing rows with more than one "X"
    #This is done by creating a column that assigns a 1 when two columns are "X"
    separate(ColorID, c("Head", "Thorax", "Abd1", "Abd2"), sep = ",", remove = FALSE) %>%
    mutate(XCount = ifelse(Head == "X" & Thorax == "X" | 
                             Abd1 == "X" & Abd2 == "X" | 
                             Head == "X" & Abd1 == "X" |
                             Head == "X" & Abd2 == "X" | 
                             Thorax == "X" & Abd1 == "X"|
                             Thorax == "X" & Abd2 == "X", 1, 0),
           #Changing NA values in ColorIDRef to 0, which occurs when a ColorID doesn't have just one missing color
           #The objective is to remove both rows with more than two missing colors and keep all fully marked workers 
           ColorIDRef = ifelse(is.na(ColorIDRef), 0, ColorIDRef),
           ColorID = ifelse(ColorIDRef == "0", ColorID, ColorIDRef)) %>%
    #Filtering out rows with more than one "X", or missing color
    filter(XCount != 1) %>%
    #Selecting the desired columns 
    select(Colony, Nest, Day, ScaledX, ScaledY, Bin, ColorID, Zone) %>%
    distinct()
}
ColorCoords(FullDataCoordRD2)
FullDataCoordRD2
view(SFZRD2DataFull%>%select(c(Colony,Nest,Day))%>%distinct())
SFZRD2DataFull
SFZRD2DataFull2
view(SFZRD2DataFull2)
FullDataCoordTesting1
SFZRD2DataFull2
view(SFZRD2DataFull2)
SFZRD2DataFull3 <- SFZRD2DataFull2 %>%
  rename(ColorID1 = ColorID) %>%
  left_join(SFZRD2DataFull)
view(SFZRD2DataFull2)
FullDataCoordRD2 <- FullDataCoordTesting
write.csv(SFZRD2DataFull,"SFZRD2DataFull.csv",row.names = FALSE)
FullDataCoordTesting
SFZRD2DataFull2
#SPATIAL FIDELITY & OCCURRENCE ZONE CALCULATIONS
#The following functions are the site fidelity calulations
#The function uses a nest area reference and the color coordinate dataset created above
#First, the reference datset is joined and a new column is created with a 1
#Then, we find the frequency of the color coordinate for the colony in each nest
#We require at least 3 observations of the color ID
#Next, we categorize spatial fidelity zones as zones having at least 15% of the total observations, where all obversations are included in the occurrence zone
#Last, each zone size is calculated by adding the number of zones in each category and multiplying that value by 1/24 the area of the nest 
FidelityZones <- function(data_table){
  #Joining the area reference dataset
  FidelityZonesDataRD1 <<- left_join(data_table, NestAreaFull) %>%
    #Creating a column filled with 1s
    mutate(ColorIDNum = 1) %>%
    #Group by the Colony, Nest, Day, and ColorID columns
    group_by(Colony,Nest,Day,ColorID) %>%
    #Count the number of each group
    mutate(count = n()) %>%
    #There should only be one observation of each color ID
    #If this isn't true we don't want the color ID in case of error
    filter(count == 1) %>%
    #Group by the Colony, Nest, and ColorID columns
    group_by(Colony, Nest, ColorID) %>%
    #Finding the freauency of the color ID in each group
    mutate(Freq = sum(ColorIDNum),
           #Count the number of observations in each group
           count = n()) %>%
    #Removing all color IDs with fewer than three observations
    filter(Freq > 2) %>%
    #Group by the Colony, Nest, ColorID, and Zone columns
    group_by(Colony,Nest,ColorID,Zone)%>%
    #Count the number of observations in each group
    mutate(ZoneCount = n(),
           #Calculating the proportion of color ID observations in each observed zone 
           PropSFZ = (ZoneCount / (count))) %>%
    #Remove any duplicates
    distinct() %>%
    #Determining whether the zone has at least 15% of total observations or not
    #If so, the zone will be included in fidelity zone
    mutate(FullZone = 1, FidZone = ifelse(PropSFZ >= 0.15, 1, 0)) %>%
    #Group by the Colony, Nest, and ColorID columns
    group_by(Colony, Nest, ColorID) %>%
    #Calculating the zone sizes
    mutate(SFZ_Full = ((Area / 24) * sum(FullZone)) / Area, SFZ = ((Area / 24) * sum(FidZone)) / Area,
           #Adding a column for the density treatment
           Density = "High") %>%
    #Select the desired columns
    select(Colony, Nest, ColorID, SFZ, SFZ_Full, Area, ScaledX, ScaledY, Density, Day, Bin)
}

FidelityZones(SFZRD1DataFull)
SFZRD1DataFull
#Same as above except for the low density treatment
FidelityZonesRD2<-function(data_table){
  FidelityZonesDataRD2 <<- left_join(data_table, NestAreaFull) %>%
    mutate(ColorIDNum = 1) %>%
    group_by(Colony,Nest,Day,ColorID) %>%
    mutate(count = n()) %>%
    filter(count == 1) %>%
    group_by(Colony, Nest, ColorID) %>%
    mutate(Freq = sum(ColorIDNum),
           count = n()) %>%
    filter(Freq>2) %>%
    group_by(Colony,Nest,ColorID,Zone)%>%
    mutate(ZoneCount = n(),
           PropSFZ = (ZoneCount / (count))) %>%
    distinct() %>%
    mutate(FullZone = 1, FidZone = ifelse(PropSFZ >= 0.15, 1, 0)) %>%
    group_by(Colony, Nest, ColorID) %>%
    mutate(SFZ_Full = ((Area / 24) * sum(FullZone)) / Area, SFZ = ((Area / 24) * sum(FidZone)) / Area,
           Density = "Low") %>%
    select(Colony, Nest, ColorID, SFZ, SFZ_Full, Area, ScaledX, ScaledY, Density, Day, Bin)
}
SFZRD2DataFull
FidelityZonesRD2(SFZRD2DataFull)
SFZRD2DataFull <- SFZRD2DataFull %>%
  distinct()
FidelityZonesDataRD21
FidelityZonesDataRD2
FidelityZonesDataRD2

FidelityZonesAreaDataRD1_RD2 <- FidelityZonesDataRD1_RD2 %>%
  group_by(Colony, Nest) %>%
  mutate(SFZArea = SFZ * Area, SFZ_FullArea = SFZ_Full * Area) 
FidelityZonesAreaDataRD1_RD2
SFZRD2DataFull
#Combining the site fidelity zone datasets
FidelityZonesDataRD1_RD2 <- full_join(FidelityZonesDataRD1, FidelityZonesDataRD2)
view(FidelityZonesDataRD1%>%filter(Colony==6 & Nest == "Tube"))
#SITE FIDELITY ZONE DISTANCE TO ENTRANCE
#The following function calculates the mean distance to the entrance for each worker to correspond with their site fidelity
#The function is very similar to the distance to the entrance functions used in the "DistanceFunctions.R" script. 
#Please see the corresponding code in this .R file for a detailed explanation. Here we will only highlight differences from those functions

#In sum, the function uses reference shortest distances to the nest entrance for each of the eight nest sections
#The shortest distance to the nest entrance is then determined by finding the shortest pythagorean distance for each worker
#Which is then added to the appropriate reference distance
DistanceCoordsFunctionSFZ<-function(data.table){
  Bin1 <- DistBinsFull %>%
    filter(Bin == 1) %>%
    group_by(Colony, Nest) %>%
    mutate(BinX1 = BinX, LeftCorner = BinX1 - 0.1, RightCorner = BinX1 + 0.1) %>%
    select(Colony, Nest, BinX1, LeftCorner, RightCorner)
  Bin4<-DistBinsFull %>%
    filter(Nest == "Tube" & Bin == 3) %>%
    group_by(Colony) %>%
    mutate(BinY4 = BinY,
           Distance4 = Distance) %>%
    select(Colony, BinY4 ,Distance4)
  Bin7 <- DistBinsFull %>%
    filter(Nest == "Tube" & Bin == 4) %>%
    group_by(Colony) %>%
    mutate(BinX7 = BinX,
           Distance7 = Distance) %>%
    select(Colony, BinX7, Distance7)
  Bin3 <- DistBinsFull %>%
    filter(Nest == "Tube" & Bin == 2) %>%
    group_by(Colony) %>%
    mutate(BinX3 = BinX,
           BinY3 = BinY,
           Distance3 = Distance) %>%
    select(Colony, BinY3, BinX3, Distance3)
  SegEntLeft = sqrt((3.75 - 3.65)^2 + (0.2 - 0)^2)
  SegEntRight = sqrt((3.85 - 3.75)^2 + (0.2 - 0)^2)
  AngleEnt1 = tan((0.2 - 0) / (3.75 - 3.65))
  AngleEnt2 = tan((0.2 - 0) / (3.85 - 3.75))
  DistBinsFull <- left_join(DistBinsFull, Bin4) %>%
    left_join(Bin7) %>%
    left_join(Bin3) %>%
    left_join(Bin1)
  DistBins.tube <- DistBinsFull %>%
    filter(Nest == "Tube")
  DistBins.circle <- DistBinsFull %>%
    filter(Nest == "Circle")
  BinsTube <- data.table %>%
    filter(Nest == "Tube")
  BinsCircle <- data.table %>%
    filter(Nest == "Circle")
  DistBinsTube <- left_join(BinsTube, DistBins.tube) %>%
    group_by(Colony, Bin) %>%
    filter(Bin != 1) %>%
    mutate(DistanceX = ScaledX - BinX,
           DistanceY = ScaledY - BinY,
           DistanceX7 = ScaledX - BinX7,
           DistanceY4 = ScaledY - BinY4,
           DistanceX3 = ScaledX - BinX3,
           DistanceY3 = ScaledY - BinY3,
           PythagDist = ifelse(Bin == 4 & Nest == "Tube" & ScaledX < BinX,
                               sqrt((DistanceX^2) + (DistanceY4^2)),
                               ifelse(Bin == 7 & Nest == "Tube" & ScaledY > BinY,
                                      sqrt((DistanceX7^2) + ((DistanceY^2))),
                                      ifelse(Bin == 3 & Nest == "Tube" & ScaledY < BinY4,
                                             sqrt((DistanceX3^2) + (DistanceY3^2)),
                                             sqrt((DistanceX^2) + (DistanceY^2))))),
           DistanceTotal = ifelse(Bin == 4 & Nest == "Tube" & ScaledX < BinX,
                                  PythagDist + Distance4,
                                  ifelse(Bin == 7 & Nest == "Tube" & ScaledY > BinY,
                                         PythagDist + Distance7,
                                         ifelse(Bin == 3 & Nest == "Tube" & ScaledY < BinY4,
                                                PythagDist + Distance3, 
                                                PythagDist + Distance))))
  DistBinsTubeBin1 <- left_join(BinsTube, DistBins.tube) %>%
    group_by(Colony, Bin) %>%
    filter(Bin == 1) %>%
    mutate(DistanceX = ScaledX - BinX,
           DistanceY = ScaledY - BinY,
           PythagDistEnt = ifelse(ScaledX < LeftCorner, 
                                  sqrt((LeftCorner - ScaledX)^2 + (ScaledY - 0.2)^2),
                                  sqrt((ScaledX - RightCorner)^2 + (ScaledY - 0.2)^2)), 
           AngleEntExp = ifelse(ScaledX < LeftCorner,
                                cos(abs(LeftCorner - ScaledX) / PythagDistEnt),
                                cos(abs(ScaledX - RightCorner) / PythagDistEnt)),
           AngleEntExp = ifelse(ScaledX >= LeftCorner & ScaledX <= RightCorner,
                                180, AngleEntExp),
           AngleCheckEnt = ifelse(((AngleEnt1 + AngleEntExp + 90) < 180) | ((AngleEnt2 + AngleEntExp + 90) < 180), "Yes", "No"),
           DistanceTotal = ifelse(ScaledX <= BinX,
                                  ifelse(AngleCheckEnt == "Yes",  PythagDistEnt + SegEntLeft, 
                                         sqrt((DistanceX^2) + (DistanceY^2))),
                                  ifelse(AngleCheckEnt == "Yes",  PythagDistEnt + SegEntRight, 
                                         sqrt((DistanceX^2) + (DistanceY^2)))))
  DistBinsTube <- full_join(DistBinsTube, DistBinsTubeBin1)
  DistBinsCircle <-left_join(BinsCircle, DistBins.circle) %>%
    group_by(Colony) %>%
    mutate(DistanceX = ScaledX - BinX,
           DistanceY = ScaledY - BinY,
           PythagDistEnt = ifelse(ScaledX < LeftCorner, 
                                  sqrt((LeftCorner - ScaledX)^2 + (ScaledY - 0.2)^2),
                                  sqrt((ScaledX - RightCorner)^2 + (ScaledY - 0.2)^2)), 
           AngleEntExp = ifelse(ScaledX < LeftCorner,
                                cos(abs(LeftCorner - ScaledX) / PythagDistEnt),
                                cos(abs(ScaledX - RightCorner) / PythagDistEnt)),
           AngleEntExp = ifelse(ScaledX >= LeftCorner & ScaledX <= RightCorner,
                                180, AngleEntExp),
           AngleCheckEnt = ifelse(((AngleEnt1 + AngleEntExp + 90) < 180) | ((AngleEnt2 + AngleEntExp + 90) < 180), "Yes", "No"),
           DistanceTotal = ifelse(ScaledX <= BinX,
                                  ifelse(AngleCheckEnt == "Yes",  PythagDistEnt + SegEntLeft, 
                                         sqrt((DistanceX^2) + (DistanceY^2))),
                                  ifelse(AngleCheckEnt == "Yes",  PythagDistEnt + SegEntRight, 
                                         sqrt((DistanceX^2) + (DistanceY^2)))))
  
  WorkerDistScaledRD1_RD2SFZFull <<- full_join(DistBinsTube, DistBinsCircle) %>%
    #Group by the columns Colony, Nest and ColorID
    group_by(Colony, Nest, Day, ColorID) %>%
    #Finding the mean distance of workers within the groupings
    mutate(ScaledDist = DistanceTotal / (MaxDist),
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist))%>%
    ungroup()%>%
    select(Colony, Nest, ColorID, SFZ, SFZ_Full, ScaledDist, Density, Day, SFZArea, SFZ_FullArea, DistanceTotal) %>%
    distinct()
  
  WorkerDistScaledRD1_RD2SFZFullMeanDist <<- full_join(DistBinsTube, DistBinsCircle) %>%
    #Group by the columns Colony, Nest and ColorID
    group_by(Colony, Nest, Day, ColorID) %>%
    #Finding the mean distance of workers within the groupings
    mutate(ScaledDist = DistanceTotal / (MaxDist),
           ScaledDist = ifelse(ScaledDist > 1, 1, ScaledDist))%>%
    ungroup()%>%
    select(Colony, Nest, ColorID, SFZ, SFZ_Full, ScaledDist, Density, Day, ScaledX, ScaledY, Bin, SFZArea, SFZ_FullArea, DistanceTotal)%>%
    distinct()
  
  WorkerDistScaledRD1_RD2SFZWorking <<- WorkerDistScaledRD1_RD2SFZFull %>%
    group_by(Colony, Nest, ColorID) %>%
    mutate(MeanScaledDist = mean(ScaledDist)) %>%
    select(Colony, Nest, ColorID, SFZ, SFZ_Full, MeanScaledDist, Density, SFZArea, SFZ_FullArea, DistanceTotal)%>%
    distinct()
  
  WorkerDistScaledRD1_RD2SFZWorkingFid <<- WorkerDistScaledRD1_RD2SFZWorking %>%
    filter(SFZ > 0) 
}

DistanceCoordsFunctionSFZ(FidelityZonesAreaDataRD1_RD2)
WorkerDistScaledRD1_RD2SFZWorking
WorkerDistScaledRD1_RD2SFZWorking %>% filter(Density == "Low") %>% select(Colony, Nest, ColorID) %>% distinct()

#DISTANCE TO BROOD CENTER CALCULATION SFZ
#NOTE that observations for brood don't line up perfectly with the workers, queens, and alates
#This means that brood centers do not exist for those instances 
#When this was found, a different day was used to make the sample size 10, but another observation for workers was not created 
#The following calculates colony member distances from the brood center
#Worker to brood distance RD1
#High density treatment

MeanBroodCoordProps <- AntPropFullBroodRD1_RD2 %>%
  group_by(Colony, Nest, Day) %>%
  mutate(MaxBrood = max(PropBrood), MaxBin = ifelse(PropBrood == MaxBrood, Bin, NA)) %>%
  ungroup() %>%
  drop_na() %>%
  select(Colony, Nest, Density, Day, Bin)

AntPropFullBroodRD1_RD2 %>% filter(Density == "Low")

MeanBroodCoordFull <- left_join(MeanBroodCoordProps, BroodDistScaledRD1_RD2) %>%
  drop_na() %>%
  group_by(Colony, Nest, Day) %>%
  mutate(BroodX = mean(ScaledX), BroodY = mean(ScaledY)) %>%
  rename(BroodBin = Bin) %>%
  select(Colony, Nest, Day, BroodBin, BroodX, BroodY, Density) %>%
  distinct()

TubeDistances <- DistBinsFull %>% #Filtering out the Tube reference distances
  filter(Nest == "Tube")

#Filtering out only distances for nest section 1
TubeDistances
DistBin1 <- TubeDistances %>% 
  filter(Bin == 1) %>%
  mutate(Dist1 = Distance) %>% #Set reference distance
  select(Colony, Nest ,Dist1) #Select desired columns

#Filtering and creating reference distances for nest sections 2 - 8
DistBin2 <- TubeDistances %>%
  filter(Bin == 2) %>%
  mutate(Dist2 = Distance) %>%
  select(Colony, Nest, Dist2)

DistBin3 <- TubeDistances %>%
  filter(Bin == 3) %>%
  mutate(Dist3 = Distance) %>%
  select(Colony, Nest, Dist3)

DistBin4 <- TubeDistances %>%
  filter(Bin == 4) %>%
  mutate(Dist4 = Distance) %>%
  select(Colony, Nest, Dist4)

DistBin5 <- TubeDistances %>%
  filter(Bin == 5) %>%
  mutate(Dist5 = Distance) %>%
  select(Colony, Nest, Dist5)

DistBin6 <- TubeDistances %>%
  filter(Bin == 6) %>%
  mutate(Dist6 = Distance) %>%
  select(Colony, Nest, Dist6)

DistBin7 <- TubeDistances %>%
  filter(Bin == 7) %>%
  mutate(Dist7 = Distance) %>%
  select(Colony, Nest, Dist7)

DistBin8 <- TubeDistances %>%
  filter(Bin == 8) %>%
  mutate(Dist8 = Distance) %>%
  select(Colony, Nest, Dist8)

#Create a combined dataset of reference coordinates and distances
DistBin1_8Full <- full_join(DistBin1, DistBin2) %>%
  full_join(DistBin3) %>%
  full_join(DistBin4) %>%
  full_join(DistBin5) %>%
  full_join(DistBin6) %>%
  full_join(DistBin7) %>%
  full_join(DistBin8) %>%
  mutate(
    Distance7_0 = DistBin1_8$Dist7, #Distance from bin 7 to 0
    Distance3_0 = DistBin1_8$Dist3, #Distance from bin 3 to 0 
    Distance8_7 = abs(DistBin1_8$Dist8 - DistBin1_8$Dist7), #Distance from bin 8 to 6 or 4 to 2 or 4 to 6  
    Distance8_1 = abs(DistBin1_8$Dist8 - DistBin1_8$Dist1), #Distance from bin 8 to 1
    Distance7_1 = abs(DistBin1_8$Dist7 - DistBin1_8$Dist1), #Distance from bin 7 to 1
    Distance3_1 = abs(DistBin1_8$Dist3 - DistBin1_8$Dist1), #Distance from bin 4 to 1
    Distance2_1 = abs(DistBin1_8$Dist2 - DistBin1_8$Dist1), #Distance from bin 3 to 1
    Distance8_2 = abs(DistBin1_8$Dist8 - DistBin1_8$Dist2), #Distance from bin 8 to 2
    Distance7_2 = abs(DistBin1_8$Dist7 - DistBin1_8$Dist2), #Distance from bin 7 to 2
    Distance7_3 = abs(DistBin1_8$Dist7 - DistBin1_8$Dist3), #Distance from bin 7 to 3
    Distance8_4 = abs(DistBin1_8$Dist8-DistBin1_8$Dist3), #Distance from bin 8 to 4
    Distance7_4 = abs(DistBin1_8$Dist7 - DistBin1_8$Dist3 - Distance8_7) #Distance from bin 7 to 3
  )
DistBin1_8
#Bin 4 and 7 has two possible shortest distances, one on the right past the farthest x coordinate of bin 3, and one to the left,
#which we account for this with two sets of possible reference coordinates

#Creating the second nest section 4 reference y coordinate
Bin4 <- DistBinsFull %>%
  filter(Nest == "Tube" & Bin==3) %>%
  group_by(Colony) %>%
  mutate(BinY4 = BinY) %>%
  select(Colony, BinY4)

#Creating the second nest section 7 reference y coordinate
Bin6 <- DistBinsFull %>%
  filter(Nest == "Tube" & Bin == 8) %>%
  group_by(Colony)%>%
  mutate(BinY7 = BinY) %>%
  select(Colony, BinY7)

#Creating the second nest section 4 reference x coordinate
Bin7 <- DistBinsFull %>%
  filter(Nest == "Tube" & Bin == 4) %>%
  group_by(Colony) %>%
  mutate(BinX7 = BinX) %>%
  select(Colony, BinX7)

#Creating the second nest section 6 reference x coordinate
Bin4.2 <- DistBinsFull %>%
  filter(Nest == "Tube" & Bin == 6) %>%
  group_by(Colony) %>%
  mutate(BinX6 = BinX,
         Distance6 = Distance) %>%
  select(Colony, BinX6)

#Creating the second nest section 7 reference x coordinate
Bin7.2 <- DistBinsFull %>%
  filter(Nest == "Tube" & Bin == 7) %>%
  group_by(Colony) %>%
  mutate(BinX7.2 = BinX) %>%
  select(Colony, BinX7.2)

#Creating the second nest section 6 reference x coordinate 
Bin3 <- DistBinsFull %>%
  filter(Nest == "Tube" & Bin == 3) %>%
  group_by(Colony) %>%
  mutate(BinX3 = BinX,
         Distance6 = Distance) %>%
  select(Colony, BinX3)

#Creating columns for nest sections 2 and 3 to create reference distances in the function below
Bin2X <- DistBinsFull %>%
  filter(Nest == "Tube" & Bin == 2) %>%
  group_by(Colony) %>%
  mutate(BinX.2 = BinX) %>%
  select(Colony, BinX.2)

Bin3X <- DistBinsFull %>%
  filter(Nest == "Tube" & Bin == 3) %>%
  group_by(Colony) %>%
  mutate(BinX3 = BinX) %>%
  select(Colony, BinX3)

Bin1X <- DistBinsFull %>%
  filter(Nest == "Tube" & Bin == 1) %>%
  group_by(Colony) %>%
  mutate(BinX.1 = BinX) %>%
  select(Colony, BinX.1)

#Combining the two diffrent sets of reference coordinates
#they are for separate purposes so they will remain separate
Bin4.6 <- full_join(Bin4, Bin6) %>%
  full_join(Bin7) %>%
  full_join(Bin4.2) %>%
  full_join(Bin7.2) %>%
  full_join(Bin3)
Bin2.3 <- full_join(Bin2X, Bin3X) %>%
  full_join(Bin1X)

#Function that uses the datasets of worker or simulation distances to the entrance
#The function uses these distances and either adds or subtracts reference distances to obtain the shortest distance to each nest section

DistBinsTubeRefFull <- DistBinsFull %>%
  filter(Nest == "Tube") %>%
  select(-c(Distance, Xmax, Ymax, TubeRatio))

DistBinsTubeRef1 <- DistBinsTubeRefFull %>%
  filter(Nest == "Tube" & Bin == 1) %>%
  rename(BinXRef1 = BinX, BinYRef1 = BinY) %>%
  select(-c(Bin))

DistBinsTubeRef2 <- DistBinsTubeRefFull %>%
  filter(Nest == "Tube" & Bin == 2) %>%
  rename(BinXRef2 = BinX, BinYRef2 = BinY) %>%
  select(-c(Bin))

DistBinsTubeRef3 <- DistBinsTubeRefFull %>%
  filter(Nest == "Tube" & Bin == 3) %>%
  rename(BinXRef3 = BinX, BinYRef3 = BinY)%>%
  select(-c(Bin))

DistBinsTubeRef4_6 <- DistBinsTubeRefFull %>%
  filter(Nest == "Tube" & Bin == 4) %>%
  rename(BinXRef4_6 = BinX, BinYRef4_6 = BinY)%>%
  select(-c(Bin))

DistBinsTubeRef7 <- DistBinsTubeRefFull %>%
  filter(Nest == "Tube" & Bin == 7) %>%
  rename(BinXRef7 = BinX, BinYRef7 = BinY)%>%
  select(-c(Bin))

DistBinsTubeRef8 <- DistBinsTubeRefFull %>%
  filter(Nest == "Tube" & Bin == 8) %>%
  rename(BinXRef8 = BinX, BinYRef8 = BinY)%>%
  select(-c(Bin))

DistBinsTubeRef <- left_join(DistBinsTubeRefFull,DistBinsTubeRef1) %>%
  left_join(DistBinsTubeRef2) %>%
  left_join(DistBinsTubeRef3) %>%
  left_join(DistBinsTubeRef4_6) %>%
  left_join(DistBinsTubeRef7) %>%
  left_join(DistBinsTubeRef8)

#Filtering tube nest data and joining reference coordinate and distance columns
MeanBroodCoordFullTube <- MeanBroodCoordFull %>% 
  filter(Nest == "Tube") %>%
  left_join(DistBinsTubeRef)


DistanceToBroodSFZTube <- FidelityZonesAreaDataRD1_RD2 %>% 
  filter(Nest == "Tube") %>%
  left_join(MeanBroodCoordFullTube) %>%
  left_join(DistBin1_8Full) %>%
  drop_na() %>%
  group_by(Colony, Day) %>%
  #Creating columns of reference distances from both the created distances and reference coordinates above 
  mutate(Distance3_4 = abs(BinXRef3 - BinXRef4_6), #Shortest distance from Bin 3 to 4
         #Creating the x and y distances from each worker to all Bin x and y references
         #Distances from the worker to the occupied Bins edge towards the nest entrance
         DistanceX = ScaledX - BinX, #Distance from each worker x coordinate to the Bin x reference towards the entrance
         DistanceY = ScaledY - BinY, #Distance from each worker y coordinate to the Bin y reference towards the entrance
         DistanceY4 = ScaledY - BinYRef3, 
         #Calculating the shortest distance from each worker to each nest section
         #This uses the pythagorean theorem, which finds the hypotenuse through the formula sqrt((DistanceX^2) + (DistanceY^2))
         #A set of shortest distances are calculated for Bins towards and away from the nest entrance from the reference of each worker
         #Shortest distances from each worker to Bins towards the nest entrance
         #If the worker is in Bin 4 but to the left of the x reference, use the first formula, else use the second
         PythagDist = ifelse(Bin == 4 & ScaledX < BinX, 
                             sqrt((DistanceX^2) + (DistanceY4^2)),
                             sqrt((DistanceX^2) + (DistanceY^2))),
         #Calculating the average distance of each worker to all Bins
         BroodDist = 
           #Distances from workers in Bin 1 to all other Bins divided by 7
           ifelse(BroodBin == 1,
                  ifelse(Bin == 1, sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                         ifelse(Bin == 2, 
                                ifelse(BroodY > BinYRef2, 
                                       sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                       PythagDist + sqrt(((BroodX - BinXRef2)^2) + ((BroodY - BinYRef2)^2))),
                                ifelse(Bin == 3, 
                                       ifelse(ScaledY < BinYRef3,
                                              ifelse(BroodY > BinYRef2, 
                                                     sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                                     sqrt(((ScaledX - BinXRef2)^2) + ((ScaledY - BinYRef2)^2)) + sqrt(((BroodX - BinXRef2)^2) + ((BroodY - BinYRef2)^2))),
                                              ifelse(BroodY > BinYRef2,
                                                     sqrt(((ScaledX - BinXRef3)^2) + ((ScaledY - BinYRef3)^2)) + sqrt(((BroodX - BinXRef3)^2) + ((BroodY - BinYRef3)^2)),
                                                     sqrt(((ScaledX - BinXRef3)^2) + ((ScaledY - BinYRef3)^2)) + sqrt(((BroodX - BinXRef2)^2) + ((BroodY - BinYRef2)^2)) + Distance2_1)),
                                       ifelse(Bin == 4,
                                              ifelse(ScaledX < BinXRef4_6,
                                                     ifelse(BroodY > BinYRef2,
                                                            PythagDist + sqrt(((BinXRef3 - BroodX)^2) + ((BinYRef3 - BroodY)^2)),
                                                            PythagDist + sqrt(((BinXRef2 - BroodX)^2) + ((BinYRef2 - BroodY)^2)) + Distance2_1),
                                                     ifelse(BroodY > BinYRef2,
                                                            PythagDist + Distance3_4 + sqrt(((BinXRef3 - BroodX)^2) + ((BinYRef3 - BroodY)^2)),
                                                            PythagDist + Distance3_4 + sqrt(((BinXRef2 - BroodX)^2) + ((BinYRef2 - BroodY)^2)) + Distance2_1)),
                                              ifelse(Bin == 5,
                                                     ifelse(BroodY > BinYRef2,
                                                            PythagDist + Distance3_4 + sqrt(((BinXRef3 - BroodX)^2) + ((BinYRef3 - BroodY)^2)),
                                                            PythagDist + Distance3_4 + sqrt(((BinXRef2 - BroodX)^2) + ((BinYRef2 - BroodY)^2)) + Distance2_1),
                                                     ifelse(Bin == 6,
                                                            ifelse(BroodY > BinYRef2,
                                                                   PythagDist + Distance3_4 + sqrt(((BinXRef3 - BroodX)^2) + ((BinYRef3 - BroodY)^2)),
                                                                   PythagDist + Distance3_4 + sqrt(((BinXRef2 - BroodX)^2) + ((BinYRef2 - BroodY)^2)) + Distance2_1),
                                                            ifelse(Bin == 7,
                                                                   ifelse(ScaledY > BinYRef7,
                                                                          ifelse(BroodY > BinYRef2,
                                                                                 PythagDist + Distance3_4 + sqrt(((BinXRef3 - BroodX)^2) + ((BinYRef3 - BroodY)^2)),
                                                                                 PythagDist + Distance3_4 + sqrt(((BinXRef2 - BroodX)^2) + ((BinYRef2 - BroodY)^2)) + Distance2_1),
                                                                          ifelse(BroodY > BinYRef2,
                                                                                 PythagDist + Distance7_2 + sqrt(((BinXRef3 - BroodX)^2) + ((BinYRef3 - BroodY)^2)),
                                                                                 PythagDist + Distance7_1 + sqrt(((BinXRef2 - BroodX)^2) + ((BinYRef2 - BroodY)^2)))),
                                                                   ifelse(Bin == 8,
                                                                          ifelse(BroodY > BinYRef2,
                                                                                 PythagDist + Distance8_2 + sqrt(((BinXRef3 - BroodX)^2) + ((BinYRef3 - BroodY)^2)),
                                                                                 PythagDist + Distance8_2 + sqrt(((BinXRef2 - BroodX)^2) + ((BinYRef2 - BroodY)^2))),
                                                                          NA)
                                                            ))))))),
                  ifelse(BroodBin == 2,
                         ifelse(Bin == 1, 
                                ifelse(ScaledY > BinYRef2,
                                       sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                       sqrt(((ScaledX - BinXRef2)^2) + ((ScaledY - BinYRef2)^2)) + sqrt(((BroodX - BinXRef2)^2) + ((BroodY - BinYRef2)^2))),
                                ifelse(Bin == 2,
                                       sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                       ifelse(Bin == 3,
                                              ifelse(ScaledY < BinYRef3,
                                                     sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                                     sqrt(((ScaledX - BinXRef3)^2) + ((ScaledY - BinYRef3)^2)) + sqrt(((BroodX - BinXRef3)^2) + ((BroodY - BinYRef3)^2))),
                                              ifelse(Bin == 4,
                                                     ifelse(ScaledX < BinXRef4_6,
                                                            PythagDist + sqrt(((BinXRef3 - BroodX)^2) + ((BinYRef3 - BroodY)^2)),
                                                            PythagDist + Distance3_4 + sqrt(((BinXRef3 - BroodX)^2) + ((BinYRef3 - BroodY)^2))),
                                                     ifelse(Bin == 5,
                                                            PythagDist + Distance3_4 + sqrt(((BinXRef3 - BroodX)^2) + ((BinYRef3 - BroodY)^2)),
                                                            ifelse(Bin == 6,
                                                                   PythagDist + Distance3_4 + sqrt(((BinXRef3 - BroodX)^2) + ((BinYRef3 - BroodY)^2)),
                                                                   ifelse(Bin == 7,
                                                                          ifelse(ScaledY > BinYRef7,
                                                                                 PythagDist + Distance3_4 + sqrt(((BinXRef3 - BroodX)^2) + ((BinYRef3 - BroodY)^2)),
                                                                                 PythagDist + Distance7_2 + sqrt(((BinXRef3 - BroodX)^2) + ((BinYRef3 - BroodY)^2))),
                                                                          ifelse(Bin == 8,
                                                                                 PythagDist + Distance8_2 + sqrt(((BinXRef3 - BroodX)^2) + ((BinYRef3 - BroodY)^2)),
                                                                                 NA)))))))),
                         ifelse(BroodBin == 3,
                                ifelse(Bin == 1,
                                       ifelse(BroodY < BinYRef3,
                                              ifelse(ScaledY > BinYRef2,
                                                     sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                                     sqrt(((ScaledX - BinXRef2)^2) + ((ScaledY - BinYRef2)^2)) + sqrt(((BroodX - BinXRef2)^2) + ((BroodY - BinYRef2)^2))),
                                              ifelse(ScaledY > BinYRef2,
                                                     sqrt(((ScaledX - BinXRef3)^2) + ((ScaledY - BinYRef3)^2)) + sqrt(((BroodX - BinXRef3)^2) + ((BroodY - BinYRef3)^2)),
                                                     sqrt(((ScaledX - BinXRef2)^2) + ((ScaledY - BinYRef2)^2)) + sqrt(((BroodX - BinXRef3)^2) + ((BroodY - BinYRef3)^2)) + Distance2_1)),
                                       ifelse(Bin == 2,
                                              ifelse(BroodY < BinYRef3,
                                                     sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                                     sqrt(((ScaledX - BinXRef3)^2) + ((ScaledY - BinYRef3)^2)) + sqrt(((BroodX - BinXRef3)^2) + ((BroodY - BinYRef3)^2))),
                                              ifelse(Bin == 3,
                                                     sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                                     ifelse(Bin == 4,
                                                            ifelse(ScaledX < BinXRef4_6,
                                                                   sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                                                   PythagDist + sqrt(((BroodX - BinXRef4_6)^2) + ((BroodY - BinYRef4_6)^2))),
                                                            ifelse(Bin == 5,
                                                                   PythagDist + sqrt(((BroodX - BinXRef4_6)^2) + ((BroodY - BinYRef4_6)^2)),
                                                                   ifelse(Bin == 6,
                                                                          PythagDist + sqrt(((BroodX - BinXRef4_6)^2) + ((BroodY - BinYRef4_6)^2)),
                                                                          ifelse(Bin == 7,
                                                                                 ifelse(ScaledY > BinYRef7,
                                                                                        PythagDist + sqrt(((BinXRef4_6 - BroodX)^2) + ((BinYRef4_6 - BroodY)^2)),
                                                                                        PythagDist + Distance7_4 + sqrt(((BinXRef4_6 - BroodX)^2) + ((BinYRef4_6 - BroodY)^2))),
                                                                                 ifelse(Bin == 8,
                                                                                        PythagDist + Distance8_4 + sqrt(((BinXRef4_6 - BroodX)^2) + ((BinYRef4_6 - BroodY)^2)),
                                                                                        NA)))))))),
                                ifelse(BroodBin == 4,
                                       ifelse(Bin == 1,
                                              ifelse(
                                                BroodX < BinXRef4_6,
                                                ifelse(ScaledY > BinYRef2, 
                                                       sqrt(((ScaledX - BinXRef3)^2) + ((ScaledY - BinYRef3)^2)) + sqrt(((BroodX - BinXRef3)^2) + ((BroodY - BinYRef3)^2)),
                                                       sqrt(((ScaledX - BinXRef2)^2) + ((ScaledY - BinYRef2)^2)) + sqrt(((BroodX - BinXRef3)^2) + ((BroodY - BinYRef3)^2)) + Distance2_1),
                                                ifelse(ScaledY > BinYRef2,
                                                       sqrt(((ScaledX - BinXRef3)^2) + ((ScaledY - BinYRef3)^2)) + sqrt(((BroodX - BinXRef4_6)^2) + ((BroodY - BinYRef4_6)^2)) + Distance3_4,
                                                       sqrt(((ScaledX - BinXRef2)^2) + ((ScaledY - BinYRef2)^2)) + sqrt(((BroodX - BinXRef4_6)^2) + ((BroodY - BinYRef4_6)^2)) + Distance3_1)),
                                              ifelse(Bin == 2,
                                                     ifelse(
                                                       BroodX < BinXRef4_6,
                                                       sqrt(((ScaledX - BinXRef3)^2) + ((ScaledY - BinYRef3)^2)) + sqrt(((BroodX - BinXRef3)^2) + ((BroodY - BinYRef3)^2)),
                                                       sqrt(((ScaledX - BinXRef3)^2) + ((ScaledY - BinYRef3)^2)) + sqrt(((BroodX - BinXRef4_6)^2) + ((BroodY - BinYRef4_6)^2)) + Distance3_4),
                                                     ifelse(Bin == 3,
                                                            ifelse(BroodX < BinXRef4_6,
                                                                   sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                                                   sqrt(((ScaledX - BinXRef4_6)^2) + ((ScaledY - BinYRef4_6)^2)) + sqrt(((BroodX - BinXRef4_6)^2) + ((BroodY - BinYRef4_6)^2))),
                                                            ifelse(Bin == 4,
                                                                   sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                                                   ifelse(Bin == 5,
                                                                          sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                                                          ifelse(Bin == 6,
                                                                                 sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                                                                 ifelse(Bin == 7,
                                                                                        ifelse(ScaledY > BinYRef7,
                                                                                               sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                                                                               PythagDist + sqrt(((BroodX - BinXRef7)^2) + ((BroodY - BinYRef7)^2))),
                                                                                        ifelse(Bin == 8,
                                                                                               PythagDist + Distance8_7 + sqrt(((BinXRef7 - BroodX)^2) + ((BinYRef7 - BroodY)^2)), #OKAY
                                                                                               NA)))))))),
                                       ifelse(BroodBin == 5,
                                              ifelse(Bin == 1,
                                                     ifelse(ScaledY > BinYRef2,
                                                            sqrt(((ScaledX - BinXRef3)^2) + ((ScaledY - BinYRef3)^2)) + sqrt(((BroodX - BinXRef4_6)^2) + ((BroodY - BinYRef4_6)^2)) + Distance3_4,
                                                            sqrt(((ScaledX - BinXRef2)^2) + ((ScaledY - BinYRef2)^2)) + sqrt(((BroodX - BinXRef4_6)^2) + ((BroodY - BinYRef4_6)^2)) + Distance3_1),
                                                     ifelse(Bin == 2,
                                                            sqrt(((ScaledX - BinXRef3)^2) + ((ScaledY - BinYRef3)^2)) + sqrt(((BroodX - BinXRef4_6)^2) + ((BroodY - BinYRef4_6)^2)) + Distance3_4,
                                                            ifelse(Bin == 3,
                                                                   sqrt(((ScaledX - BinXRef4_6)^2) + ((ScaledY - BinYRef4_6)^2)) + sqrt(((BroodX - BinXRef4_6)^2) + ((BroodY - BinYRef4_6)^2)),
                                                                   ifelse(Bin == 4,
                                                                          sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                                                          ifelse(Bin == 5,
                                                                                 sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                                                                 ifelse(Bin == 6,
                                                                                        sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                                                                        ifelse(Bin == 7,
                                                                                               ifelse(ScaledY > BinYRef7,
                                                                                                      sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                                                                                      PythagDist + sqrt(((BroodX - BinXRef7)^2) + ((BroodY - BinYRef7)^2))),
                                                                                               ifelse(Bin == 8,
                                                                                                      PythagDist + Distance8_7 + sqrt(((BinXRef7 - BroodX)^2) + ((BinYRef7 - BroodY)^2)),
                                                                                                      NA)))))))),
                                              ifelse(BroodBin == 6,
                                                     ifelse(Bin == 1,
                                                            ifelse(ScaledY > BinYRef2,
                                                                   sqrt(((ScaledX - BinXRef3)^2) + ((ScaledY - BinYRef3)^2)) + sqrt(((BroodX - BinXRef4_6)^2) + ((BroodY - BinYRef4_6)^2)) + Distance3_4,
                                                                   sqrt(((ScaledX - BinXRef2)^2) + ((ScaledY - BinYRef2)^2)) + sqrt(((BroodX - BinXRef4_6)^2) + ((BroodY - BinYRef4_6)^2)) + Distance3_1),
                                                            ifelse(Bin == 2,
                                                                   sqrt(((ScaledX - BinXRef3)^2) + ((ScaledY - BinYRef3)^2)) + sqrt(((BroodX - BinXRef4_6)^2) + ((BroodY - BinYRef4_6)^2)) + Distance3_4,
                                                                   ifelse(Bin == 3,
                                                                          sqrt(((ScaledX - BinXRef4_6)^2) + ((ScaledY - BinYRef4_6)^2)) + sqrt(((BroodX - BinXRef4_6)^2) + ((BroodY - BinYRef4_6)^2)),
                                                                          ifelse(Bin == 4,
                                                                                 sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                                                                 ifelse(Bin == 5,
                                                                                        sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                                                                        ifelse(Bin == 6,
                                                                                               sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                                                                               ifelse(Bin == 7,
                                                                                                      ifelse(ScaledY > BinYRef7,
                                                                                                             sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                                                                                             PythagDist + sqrt(((BroodX - BinXRef7)^2) + ((BroodY - BinYRef7)^2))),
                                                                                                      ifelse(Bin == 8,
                                                                                                             PythagDist + Distance8_7 + sqrt(((BinXRef7 - BroodX)^2) + ((BinYRef7 - BroodY)^2)),
                                                                                                             NA)))))))),
                                                     ifelse(BroodBin == 8,
                                                            ifelse(Bin == 1,
                                                                   ifelse(ScaledY > BinYRef2,
                                                                          sqrt(((ScaledX - BinXRef3)^2) + ((ScaledY - BinYRef3)^2)) + sqrt(((BroodX - BinXRef8)^2) + ((BroodY - BinYRef8)^2)) + Distance8_2,
                                                                          sqrt(((ScaledX - BinXRef2)^2) + ((ScaledY - BinYRef2)^2)) + sqrt(((BroodX - BinXRef8)^2) + ((BroodY - BinYRef8)^2)) + Distance8_1),
                                                                   ifelse(Bin == 2,
                                                                          sqrt(((ScaledX - BinXRef3)^2) + ((ScaledY - BinYRef3)^2)) + sqrt(((BroodX - BinXRef8)^2) + ((BroodY - BinYRef8)^2)) + Distance8_2,
                                                                          ifelse(Bin == 3,
                                                                                 sqrt(((ScaledX - BinXRef4_6)^2) + ((ScaledY - BinYRef4_6)^2)) + sqrt(((BroodX - BinXRef8)^2) + ((BroodY - BinYRef8)^2)) + Distance8_4,
                                                                                 ifelse(Bin == 4,
                                                                                        sqrt(((ScaledX - BinXRef7)^2) + ((ScaledY - BinYRef7)^2)) + sqrt(((BroodX - BinXRef8)^2) + ((BroodY - BinYRef8)^2)) + Distance8_7,
                                                                                        ifelse(Bin == 5,
                                                                                               sqrt(((ScaledX - BinXRef7)^2) + ((ScaledY - BinYRef7)^2)) + sqrt(((BroodX - BinXRef8)^2) + ((BroodY - BinYRef8)^2)) + Distance8_7,
                                                                                               ifelse(Bin == 6,
                                                                                                      sqrt(((ScaledX - BinXRef7)^2) + ((ScaledY - BinYRef7)^2)) + sqrt(((BroodX - BinXRef8)^2) + ((BroodY - BinYRef8)^2)) + Distance8_7,
                                                                                                      ifelse(Bin == 7,
                                                                                                             ifelse(ScaledY < BinYRef8,
                                                                                                                    sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                                                                                                    sqrt(((ScaledX - BinXRef8)^2) + ((ScaledY - BinYRef8)^2)) + sqrt(((BroodX - BinXRef8)^2) + ((BroodY - BinYRef8)^2))),
                                                                                                             ifelse(Bin == 8,
                                                                                                                    sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
                                                                                                                    NA)
                                                                                                      )
                                                                                               )
                                                                                        )
                                                                                 )
                                                                          )
                                                                   )
                                                            ),
                                                            NA
                                                     )))))))) %>%
  group_by(Colony) %>%
  mutate(ToBrood = BroodDist / MaxDist,
         ToBrood = ifelse(ToBrood > 1, 1, ToBrood)) %>%
  select(Colony, Nest, Day, ScaledX, ScaledY, Density, ToBrood, ColorID, SFZ, SFZ_Full, SFZArea, SFZ_FullArea, BroodDist) %>%
  distinct() %>%
  drop_na()


MeanBroodCoordFullCircle <- MeanBroodCoordFull %>% 
  filter(Nest == "Circle") %>%
  left_join(DistBinsFull)

DistanceToBroodSFZCircle <- FidelityZonesAreaDataRD1_RD2 %>% 
  filter(Nest == "Circle") %>%
  left_join(MeanBroodCoordFullCircle) %>%
  group_by(Colony, Day) %>% 
  mutate(BroodDist = sqrt(((ScaledX - BroodX)^2) + ((ScaledY - BroodY)^2)),
         ToBrood = BroodDist / MaxDist) %>%
  select(Colony, Nest, Day, ScaledX, ScaledY, Density, ToBrood, ColorID, SFZ, SFZ_Full, SFZArea, SFZ_FullArea, BroodDist) %>%
  distinct() %>%
  drop_na()

BroodCentDistWorkersSFZ <- full_join(DistanceToBroodSFZTube, DistanceToBroodSFZCircle) %>%
  group_by(Colony, Nest, ColorID) %>%
  mutate(MeanToBrood = mean(ToBrood)) %>%
  select(-c(Day, ScaledX, ScaledY, ToBrood)) %>%
  distinct()

BroodCentDistWorkersSFZFid <- BroodCentDistWorkersSFZ %>%
  filter(SFZ > 0) 
  


  