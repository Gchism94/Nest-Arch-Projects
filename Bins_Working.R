
#The below function is a simplified version of XYCorrected, where we aren't concerned about the Date and Day.
XYCorrectedBin <- function(data_table){
  #First we need to assign the reference distance. In this example, its 5 for the 5cm width of the example nest.
  RefDistance = 5
  #Here is a database management pipeline that removes any duplicate values of "CoordID" or any analagous column.
  #The rationale behind this pipeline is to remove duplicate entries for an analyzed photo (from multiple people...)
  data_table <- data_table %>%
    group_by(Colony, Nest) %>%
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
  data_table$YCoordType <-
    ifelse(
      data_table$CoordType == "REF1", "yref1", "yref2"
    )
  #Creating a new datatable with only reference coordinates
  RefTable <- data_table %>%
    filter(CoordType == "REF1" | CoordType == "REF2")
  #Creating a new datatable with only Xref1 coordinates and renaming X and Y to "xref1" and "yref1"
  Xref1Table <- RefTable %>% 
    filter(XCoordType == "xref1") %>%
    group_by(Colony, Nest) %>%
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
  RefTableCrop <- left_join(Xref1Table, Xref2Table, by = c("Colony", "Nest"))
  #Next we use a left join, since our main data table (x in the join) is the one we want to have the 
  #New reference coordinates merged to based on our join qualifiers (by=)
  FullDataCoord <- left_join(data_table, RefTableCrop, by = c("Colony", "Nest"))
  #The last steps are to scale the coordinates
  #To do this, we use mutate to create a new column for the scaled X and Y coordinates
  FullDataCoord <- FullDataCoord %>%
    #The normal origin in imageJ (or Fiji - REFERENCE) is the top left
    #To make ours the bottom left and scaled to the nest start, we need to remove the excess
    #Distance on the X axis (X - xref1). This will make zero the left of the nest. 
    #We then scale the distance by the reference distance. Since we know reference distance (plugged in at the top)
    #We can subtract the largest and smallest Y axis reference coordinates and divide this number by our reference distance
    #The scaled y coordinates is the same, except that, since the origin is the inverse of what we want
    #We need to subtract our empirical Y coordinate from the yref coordinate. 
    mutate(ScaledX = ((X - xref1) * (RefDistance / (yref1 - yref2))),
           ScaledY = (yref1 - Y) * (RefDistance / (yref1 - yref2)))
  BinCoord24Circ <<- FullDataCoord %>% #Change name accordingly
    filter(CoordType != "REF1") %>%
    filter(CoordType != "REF2") %>%
    select(c(Colony, Nest, CoordID, ScaledX, ScaledY))
  #Lastly, we tidy up the database by removing reference coordinates. 
  #Note, <<- sends the data table to the global environment, and is no longer usable for further manipulation in this function
}

XYCorrectedBin() #Bin references dataset

#BIN ASSIGNMENT FUNCTION
#The code below bins x and y coordinate colony data into eight even area nest sections
#Note that the code is specifically used below for the low density treatment colonies (11-20)
#This is because the high density treatment colonies (1-10) were done through excel but can be run through this script as well
#The code for Netlogo simulated x and y coordinate results is separate and below
#To do this, a reference dataset of bin coordinates is used and coordinates are run through a series of conditional statements
#Where each conditional statement checks whether the coordinate is in one of eight bins sequentially

#Note that you have to change the value of "Colony" before each iteration.
#A for loop could have accomplished to avoid changing the value, but computational power was a concern
CoordBinned<-function(data_table){
  #Selecting the colony bin references
  BinCoordAssign <- BinCoordVGA %>%
    filter(Colony == "13")
  
  #Binning the tube colony member coordinates
  #Filtering out the tube data for the selected colony
  Colony13RD2BinnedTube <- data_table %>%
    filter(Colony == "13" & Nest == "Tube") %>%
    #Ifelse conditional statements for each bin
    mutate(Bin =
             #The bins determine whether the x and y coordinates are within a set of relevant bounds for each bin
             #The order is always x coordinates first, then y
             if_else(
               ScaledX >= BinCoordAssign$ScaledX[2] & 
                 ScaledX <= BinCoordAssign$ScaledX[1] &
                 ScaledY <= BinCoordAssign$ScaledY[2], 1,
               #Bin 2
               if_else(ScaledX >= BinCoordAssign$ScaledX[3] & 
                         ScaledX <= BinCoordAssign$ScaledX[2] &
                         ScaledY >= BinCoordAssign$ScaledY[3] &
                         ScaledY <= BinCoordAssign$ScaledY[2], 2,
                       #Bin 3
                       if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                 ScaledX <= BinCoordAssign$ScaledX[3] &
                                 ScaledY >= BinCoordAssign$ScaledY[3] &
                                 ScaledY <= BinCoordAssign$ScaledY[4], 3,
                               #Bin 4
                               if_else(ScaledX >= BinCoordAssign$ScaledX[4] & 
                                         ScaledX <= BinCoordAssign$ScaledX[5] &
                                         ScaledY >= BinCoordAssign$ScaledY[4] &
                                         ScaledY <= BinCoordAssign$ScaledY[5], 4,
                                       #Bin 5
                                       if_else(ScaledX >= BinCoordAssign$ScaledX[5] & 
                                                 ScaledX <= BinCoordAssign$ScaledX[6] &
                                                 ScaledY >= BinCoordAssign$ScaledY[4] &
                                                 ScaledY <= BinCoordAssign$ScaledY[5], 5,
                                               #Bin 6
                                               if_else(ScaledX >= BinCoordAssign$ScaledX[6] & 
                                                         ScaledX <= BinCoordAssign$ScaledX[7] &
                                                         ScaledY >= BinCoordAssign$ScaledY[7] &
                                                         ScaledY <= BinCoordAssign$ScaledY[6], 6,
                                                       #Bin 7
                                                       if_else(ScaledX >= BinCoordAssign$ScaledX[8] & 
                                                                 ScaledX <= BinCoordAssign$ScaledX[7] &
                                                                 ScaledY <= BinCoordAssign$ScaledY[7], 7,
                                                               #Bin 8
                                                               if_else(ScaledX >= BinCoordAssign$ScaledX[9] & 
                                                                         ScaledX <= BinCoordAssign$ScaledX[8] &
                                                                         ScaledY <= BinCoordAssign$ScaledY[9], 8, 0
                                                                       )))))))))
  #Binning circle nest coordinates 
  #Filtering out the circle data for the selected colony
  #The code is the same as above, except its only y coordinates since xlims don't matter for the binning to work 
  Colony13RD2BinnedCircle <- data_table %>%
    filter(Colony == "13" & Nest == "Circle") %>%
    mutate(Bin =
             #Bin 1
             if_else(ScaledY <= BinCoordAssign$ScaledY[10], 1,
                     #Bin 2
                     if_else(ScaledY >= BinCoordAssign$ScaledY[10] & 
                               ScaledY <= BinCoordAssign$ScaledY[11], 2,
                             #Bin 3
                             if_else(ScaledY >= BinCoordAssign$ScaledY[11] & 
                                       ScaledY <= BinCoordAssign$ScaledY[12], 3,
                                     #Bin 4
                                     if_else(ScaledY >= BinCoordAssign$ScaledY[12] & 
                                               ScaledY <= BinCoordAssign$ScaledY[13], 4,
                                             #Bin 5
                                             if_else(ScaledY >= BinCoordAssign$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssign$ScaledY[14], 5,
                                                     #Bin 6
                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssign$ScaledY[15], 6,
                                                             #Bin 7
                                                             if_else(ScaledY >= BinCoordAssign$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssign$ScaledY[16], 7,
                                                                     #Bin 8
                                                                     if_else(ScaledY >= BinCoordAssign$ScaledY[16], 8, 0
                                                                             )))))))))
  Colony13RD2Binned <<- full_join(Colony13RD2BinnedTube, Colony13RD2BinnedCircle)
}

CoordBinned() #Colony member dataset

#The following series of code check how many coordinates have zero bin values, meaning they werent' placed in a bin
#These coordinates need to be removed as they are most likely an error (a coordinate where no ant exists)
#Each issue was examined in Excel 

#Colony 11
Colony11RD2Binned 
Colony11RD2Binned %>%
  filter(Bin == "0") %>%
  group_by(Bin) %>%
  summarise(n = n())

#Colony12
Colony12RD2Binned 
Colony12RD2Binned %>%
  filter(Bin == "0") %>%
  group_by(Bin) %>%
  summarise(n = n())

#Colony13
Colony13RD2Binned 
Colony13RD2Binned %>%
  filter(Bin == "0") %>%
  group_by(Bin) %>%
  summarise(n = n())

#Colony14
Colony14RD2Binned 
Colony14RD2Binned %>%
  filter(Bin == "0") %>%
  group_by(Bin) %>%
  summarise(n = n())

#Colony15
Colony15RD2Binned  
Colony15RD2Binned %>%
  filter(Bin == "0") %>%
  group_by(Bin) %>%
  summarise(n = n())

#Colony16
Colony16RD2Binned 
Colony16RD2Binned %>%
  filter(Bin == "0") %>%
  group_by(Bin) %>%
  summarise(n = n())

#Colony17
Colony17RD2Binned 
Colony17RD2Binned %>%
  filter(Bin == "0") %>%
  group_by(Bin) %>%
  summarise(n = n())

#Colony18
Colony18RD2Binned 
Colony18RD2Binned %>%
  filter(Bin == "0") %>%
  group_by(Bin) %>%
  summarise(n = n())

#Colony19
Colony19RD2Binned 
Colony19RD2Binned %>%
  filter(Bin == "0") %>%
  group_by(Bin) %>%
  summarise(n = n())

#Colony20
Colony20RD2Binned 
Colony20RD2Binned %>%
  filter(Bin == "0") %>%
  group_by(Bin) %>%
  summarise(n = n())

#Remove all of the zeros, since some coordinates exist that may be accidental or error. 
NestArchProjRD2AntsColorBinsGC <- full_join(Colony11RD2Binned, Colony12RD2Binned) %>%
  full_join(Colony13RD2Binned, Colony14RD2Binned) %>%
  full_join(Colony15RD2Binned, Colony16RD2Binned) %>%
  full_join(Colony17RD2Binned, Colony18RD2Binned) %>%
  full_join(Colony19RD2Binned, Colony20RD2Binned) %>%
  filter(Bin != 0)

#NETLOGO SIMULATION RESULTS BIN FUNCTION
#The below code is the same as for the experimental coordinates
#There is also a bin coordinate reference dataset used here 
#Except for a renaming line that changes "x" and "y" columns to "ScaledX" and "ScaledY" in order to work with the function
#This also makes later joining real data and simulated results easier 
#There is a separate set of code for each combination of nest shape (Tube, Circle) and size (Small, Large)
CoordBinnedNetlogo <- function(data_table){
  #Separating out the 
  BinCoordAssignSmall <- BinCoordNetlogo %>%
    filter(Size == "Small")
  
  BinCoordAssignLarge <- BinCoordNetlogo %>%
    filter(Size == "Large")
  
  #Binning small tube nest coordinates 
  NetlogoBinnedTubeSmall <- data_table %>%
    rename(ScaledX = x, ScaledY = y) %>%
    filter(Size == "Small" & Nest == "Tube") %>%
    mutate(Bin =
             #Bin 1
             if_else(ScaledX >= BinCoordAssignSmall$ScaledX[2] & 
                       ScaledX <= BinCoordAssignSmall$ScaledX[1] &
                       ScaledY <= BinCoordAssignSmall$ScaledY[2], 1,
                     #Bin 2
                     if_else(ScaledX >= BinCoordAssignSmall$ScaledX[3] & 
                               ScaledX <= BinCoordAssignSmall$ScaledX[2] &
                               ScaledY >= BinCoordAssignSmall$ScaledY[3] &
                               ScaledY <= BinCoordAssignSmall$ScaledY[2], 2,
                             #Bin 3
                             if_else(ScaledX >= BinCoordAssignSmall$ScaledX[4] & 
                                       ScaledX <= BinCoordAssignSmall$ScaledX[3] &
                                       ScaledY >= BinCoordAssignSmall$ScaledY[3] &
                                       ScaledY <= BinCoordAssignSmall$ScaledY[4], 3,
                                     #Bin 4
                                     if_else(ScaledX >= BinCoordAssignSmall$ScaledX[4] & 
                                               ScaledX <= BinCoordAssignSmall$ScaledX[5] &
                                               ScaledY >= BinCoordAssignSmall$ScaledY[4] &
                                               ScaledY <= BinCoordAssignSmall$ScaledY[5], 4,
                                             #Bin 5
                                             if_else(ScaledX >= BinCoordAssignSmall$ScaledX[5] & 
                                                       ScaledX <= BinCoordAssignSmall$ScaledX[6] &
                                                       ScaledY >= BinCoordAssignSmall$ScaledY[4] &
                                                       ScaledY <= BinCoordAssignSmall$ScaledY[5], 5,
                                                     #Bin 6
                                                     if_else(ScaledX >= BinCoordAssignSmall$ScaledX[6] & 
                                                               ScaledX <= BinCoordAssignSmall$ScaledX[7] &
                                                               ScaledY >= BinCoordAssignSmall$ScaledY[7] &
                                                               ScaledY <= BinCoordAssignSmall$ScaledY[6], 6,
                                                             #Bin 7
                                                             if_else(ScaledX >= BinCoordAssignSmall$ScaledX[8] & 
                                                                       ScaledX <= BinCoordAssignSmall$ScaledX[7] &
                                                                       ScaledY <= BinCoordAssignSmall$ScaledY[7], 7,
                                                                     #Bin 8
                                                                     if_else(ScaledX >= BinCoordAssignSmall$ScaledX[9] & 
                                                                               ScaledX <= BinCoordAssignSmall$ScaledX[8] &
                                                                               ScaledY <= BinCoordAssignSmall$ScaledY[9], 8, 0
                                                                             )))))))))
  #Binning large tube nest coordinates 
  NetlogoBinnedTubeLarge <- data_table %>%
    rename(ScaledX = x, ScaledY = y) %>%
    filter(Size == "Large" & Nest == "Tube") %>%
    mutate(Bin =
             #Bin 1
             if_else(ScaledX >= BinCoordAssignLarge$ScaledX[2] & 
                       ScaledX <= BinCoordAssignLarge$ScaledX[1] &
                       ScaledY <= BinCoordAssignLarge$ScaledY[2], 1,
                     #Bin 2
                     if_else(ScaledX >= BinCoordAssignLarge$ScaledX[3] & 
                               ScaledX <= BinCoordAssignLarge$ScaledX[2] &
                               ScaledY >= BinCoordAssignLarge$ScaledY[3] &
                               ScaledY <= BinCoordAssignLarge$ScaledY[2], 2,
                             #Bin 3
                             if_else(ScaledX >= BinCoordAssignLarge$ScaledX[4] & 
                                       ScaledX <= BinCoordAssignLarge$ScaledX[3] &
                                       ScaledY >= BinCoordAssignLarge$ScaledY[3] &
                                       ScaledY <= BinCoordAssignLarge$ScaledY[4], 3,
                                     #Bin 4
                                     if_else(ScaledX >= BinCoordAssignLarge$ScaledX[4] & 
                                               ScaledX <= BinCoordAssignLarge$ScaledX[5] &
                                               ScaledY >= BinCoordAssignLarge$ScaledY[4] &
                                               ScaledY <= BinCoordAssignLarge$ScaledY[5], 4,
                                             #Bin 5
                                             if_else(ScaledX >= BinCoordAssignLarge$ScaledX[5] & 
                                                       ScaledX <= BinCoordAssignLarge$ScaledX[6] &
                                                       ScaledY >= BinCoordAssignLarge$ScaledY[4] &
                                                       ScaledY <= BinCoordAssignLarge$ScaledY[5], 5,
                                                     #Bin 6
                                                     if_else(ScaledX >= BinCoordAssignLarge$ScaledX[6] & 
                                                               ScaledX <= BinCoordAssignLarge$ScaledX[7] &
                                                               ScaledY >= BinCoordAssignLarge$ScaledY[7] &
                                                               ScaledY <= BinCoordAssignLarge$ScaledY[6], 6,
                                                             #Bin 7
                                                             if_else(ScaledX >= BinCoordAssignLarge$ScaledX[8] & 
                                                                       ScaledX <= BinCoordAssignLarge$ScaledX[7] &
                                                                       ScaledY <= BinCoordAssignLarge$ScaledY[7], 7,
                                                                     #Bin 8
                                                                     if_else(ScaledX >= BinCoordAssignLarge$ScaledX[9] & 
                                                                               ScaledX <= BinCoordAssignLarge$ScaledX[8] &
                                                                               ScaledY <= BinCoordAssignLarge$ScaledY[9], 8, 0
                                                                     )))))))))
  #Binning small circle nest coordinates 
  NetlogoBinnedCircleSmall <- data_table %>%
    filter(Size == "Small" & Nest == "Circle") %>%
    mutate(Bin =
             #Bin 1
             if_else(ScaledY <= BinCoordAssignSmall$ScaledY[10], 1,
                     #Bin 2
                     if_else(ScaledY >= BinCoordAssignSmall$ScaledY[10] & 
                               ScaledY <= BinCoordAssignSmall$ScaledY[11], 2,
                             #Bin 3
                             if_else(ScaledY >= BinCoordAssignSmall$ScaledY[11] & 
                                       ScaledY <= BinCoordAssignSmall$ScaledY[12], 3,
                                     #Bin 4
                                     if_else(ScaledY >= BinCoordAssignSmall$ScaledY[12] & 
                                               ScaledY <= BinCoordAssignSmall$ScaledY[13], 4,
                                             #Bin 5
                                             if_else(ScaledY >= BinCoordAssignSmall$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssignSmall$ScaledY[14], 5,
                                                     #Bin 6
                                                     if_else(ScaledY >= BinCoordAssignSmall$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssignSmall$ScaledY[15], 6,
                                                             #Bin 7
                                                             if_else(ScaledY >= BinCoordAssignSmall$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssignSmall$ScaledY[16], 7,
                                                                     #Bin 8
                                                                     if_else(ScaledY >= BinCoordAssignSmall$ScaledY[16], 8, 0
                                                                             )))))))))
  #Binning large circle nest coordinates 
  NetlogoBinnedCircleLarge <- data_table %>%
    filter(Size == "Large" & Nest == "Circle") %>%
    mutate(Bin =
             #Bin 1
             if_else(ScaledY <= BinCoordAssignLarge$ScaledY[10], 1,
                     #Bin 2
                     if_else(ScaledY >= BinCoordAssignLarge$ScaledY[10] & 
                               ScaledY <= BinCoordAssignLarge$ScaledY[11], 2,
                             #Bin 3
                             if_else(ScaledY >= BinCoordAssignLarge$ScaledY[11] & 
                                       ScaledY <= BinCoordAssignLarge$ScaledY[12], 3,
                                     #Bin 4
                                     if_else(ScaledY >= BinCoordAssignLarge$ScaledY[12] & 
                                               ScaledY <= BinCoordAssignLarge$ScaledY[13], 4,
                                             #Bin 5
                                             if_else(ScaledY >= BinCoordAssignLarge$ScaledY[13] & 
                                                       ScaledY <= BinCoordAssignLarge$ScaledY[14], 5,
                                                     #Bin 6
                                                     if_else(ScaledY >= BinCoordAssignLarge$ScaledY[14] & 
                                                               ScaledY <= BinCoordAssignLarge$ScaledY[15], 6,
                                                             #Bin 7
                                                             if_else(ScaledY >= BinCoordAssignLarge$ScaledY[15] & 
                                                                       ScaledY <= BinCoordAssignLarge$ScaledY[16], 7,
                                                                     #Bin 8
                                                                     if_else(ScaledY >= BinCoordAssignLarge$ScaledY[16], 8, 0
                                                                     )))))))))
  
  NetlogoBinnedFull <<- full_join(NetlogoBinnedTubeSmall, NetlogoBinnedTubeCircle) %>%
    full_join(NetlogoBinnedCircleSmall) %>% 
    full_join(NetlogoBinnedCircleLarge)
}

CoordBinnedNetlogo() #NetlogoDataset