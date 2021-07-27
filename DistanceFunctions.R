#The following code is used to calculate the shortest distance each worker is from all nest sections but their own
#To do this, we use a reference dataset that defines the shortest distance from the beginning of each nest section to the nest entrance

#Tube nest shape 
#First we make datasets of distance segments either between a nest section and the entrance or between two sections
#This is done outside of the actual function below to take less computing time
TubeDistances <- DistBinsFull %>% #Filtering out the Tube reference distances
  filter(Nest == "Tube")

#Filtering out only distances for nest section 1
DistBin1 <- TubeDistances %>% 
  filter(Bin == 1) %>%
  mutate(BinX = 3.75, #Create x reference column
         BinY = 0, #Create y reference column
         Dist1 = Distance) %>% #Set reference distance
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
Distance8_0 = DistBin1_8$Dist8, #Distance from bin 8 to 0
Distance7_0 = DistBin1_8$Dist7, #Distance from bin 7 to 0
Distance6_0 = DistBin1_8$Dist6, #Distance from bin 4 to 0
Distance5_0 = DistBin1_8$Dist5, #Distance from bin 4 to 0 
Distance4_0 = DistBin1_8$Dist4, #Distance from bin 4 to 0 
Distance3_0 = DistBin1_8$Dist3, #Distance from bin 4 to 0 
Distance2_0 = DistBin1_8$Dist2, #Distance from bin 3 to 0
Distance1_0 = DistBin1_8$Dist1, #Distance brom bin 2 to 0
Distance8_7 = abs(DistBin1_8$Dist8 - DistBin1_8$Dist7), #Distance from bin 8 to 6 or 4 to 2 or 4 to 5  
Distance8_1 = abs(DistBin1_8$Dist8 - DistBin1_8$Dist1), #Distance from bin 8 to 1
Distance7_1 = abs(DistBin1_8$Dist7 - DistBin1_8$Dist1), #Distance from bin 7 to 1
Distance3_1 = abs(DistBin1_8$Dist3 - DistBin1_8$Dist1), #Distance from bin 4 to 1
Distance2_1 = abs(DistBin1_8$Dist2 - DistBin1_8$Dist1), #Distance from bin 3 to 1
Distance8_2 = abs(DistBin1_8$Dist8 - DistBin1_8$Dist2), #Distance from bin 8 to 2
Distance7_2 = abs(DistBin1_8$Dist7 - DistBin1_8$Dist2), #Distance from bin 7 to 2
Distance8_3 = abs(DistBin1_8$Dist8 - DistBin1_8$Dist3), #Distance from bin 8 to 3
Distance7_3 = abs(DistBin1_8$Dist7 - DistBin1_8$Dist3), #Distance from bin 7 to 3
Distance8_4 = abs(DistBin1_8$Dist8-DistBin1_8$Dist3), #Distance from bin 8 to 4
Distance7_4 = abs(DistBin1_8$Dist7 - DistBin1_8$Dist3 - Distance8_7) #Distance from bin 7 to 4
)

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
  filter(Nest == "Tube" & Bin == 7) %>%
  group_by(Colony)%>%
  mutate(BinY7 = BinY) %>%
  select(Colony, BinY7)

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

#Combining the two diffrent sets of reference coordinates
#they are for separate purposes so they will remain separate
Bin4.6 <- full_join(Bin4, Bin6)
Bin2.3 <- full_join(Bin2X, Bin3X)

#Function that uses the datasets of worker or simulation distances to the entrance
#The function uses these distances and either adds or subtracts reference distances to obtain the shortest distance to each nest section
MinDistanceTube <- function(data_table) {
#Filtering tube nest data and joining reference coordinate and distance columns
DistanceToBins <<- data_table %>% 
  filter(Nest == "Tube") %>%
  left_join(DistBinsFull) %>%
  left_join(Bin4.6) %>%
  left_join(Bin2.3) %>%
  left_join(DistBin1_8Full) %>%
  #Creating columns of reference distances from both the created distances and reference coordinates above 
  mutate(Distance3_4=abs(BinX3-BinX.2), #Shortest distance from Bin 3 to 4
         Distance3_6=abs(Distance8_7+Distance3_4), #Shortest distance from Bin 3 to 6
         Distance7_5=abs(Distance7_0-Distance3_0-Distance3_6), #Shortest distance from Bin 7 to 5
    #Creating the second x and y coordinate used to find shortest distances to each Bin
    #These reference coordinates representing distances away from the entrance
    #Second x reference coordinates 
    BinX2 = ifelse(
    #If Bin 1, use the same x reference
    Bin == 1, BinX, 
    #If Bin 2, BinX2 = the Bin 2 x reference - the distance from Bin 3 to 4
    ifelse(Bin == 2, BinX - Distance3_4, 
           #If Bin 3, use the same x reference 
           ifelse(Bin == 3, BinX,
                  #If Bin 4, BinX2 = the Bin 4 x reference + the distance from the back of the nest to Bin 7
                  ifelse(Bin == 4, BinX + Distance8_7, 
                         #If Bin 5, BinX2 = the Bin 5 x reference + the distance from Bin 3 to 4
                         ifelse(Bin == 5, BinX + Distance3_4,
                                #If Bin 6, BinX2 = the Bin 6 x reference + the distance from the back of the nest to Bin 7
                                ifelse(Bin == 6, BinX + Distance8_7,
                                       #If Bin 7, use the same x reference
                                       ifelse(Bin == 7, BinX,
                                              #If Bin 8, use the same x reference
                                              ifelse(Bin == 8, BinX,
                                                     #All others would be 0, but they don't exist
                                                     0)))))))),
    #Second y reference coordinates
    BinY2 = ifelse(
      #If Bin 1, BinY2 = the Bin 1 y reference - the distance from Bin 3 to 4 - the width of the bin
      #The width of tube nest bins are 0.2723 * the length of the bin
      #Since the distance from Bin 3 to 4 is the length of the tube nest Bin, multiplying this by 0.2723 gives us the width. 
      Bin == 1, BinY + Distance3_4 - (0.2723 * Distance3_4),
      #If Bin 2, BinY2 = the Bin 2 y reference + the width of tube nest Bins
      ifelse(Bin == 2, BinY + (0.2723 * Distance3_4),
             #If Bin 3, BinY2 = the Bin 3 y reference + the distance from the back of the nest to Bin 7
             ifelse(Bin == 3, BinY + Distance8_7,
                    #For Bin 4, 5, and 6, use the same y reference
                    ifelse(Bin == 4, BinY,
                           ifelse(Bin == 5, BinY,
                                  ifelse(Bin == 6, BinY,
                                         #For Bin 7, BinY2 = the Bin 7 y reference - the distance from the back of the nest to Bin 7
                                         ifelse(Bin == 7, BinY - Distance8_7,
                                                #If Bin 8, use the same y reference
                                                ifelse(Bin == 8, BinY,
                                                       0)))))))),
    #Creating the x and y distances from each worker to all Bin x and y references
    #Distances from the worker to the occupied Bins edge towards the nest entrance
    DistanceX = ScaledX - BinX, #Distance from each worker x coordinate to the Bin x reference towards the entrance
    DistanceY = ScaledY - BinY, #Distance from each worker y coordinate to the Bin y reference towards the entrance
    DistanceY4 = ScaledY - BinY4, #Distance from each worker x coordinate to the Bin x reference towards the back of the nest
    DistanceY6 = ScaledY - BinY7, #Distance from each worker y coordinate to the Bin y reference towards the entrance
    #Calculating the shortest distance from each worker to each nest section
    #This uses the pythagorean theorem, which finds the hypotenuse through the formula sqrt((DistanceX^2) + (DistanceY^2))
    #A set of shortest distances are calculated for Bins towards and away from the nest entrance from the reference of each worker
    #Shortest distances from each worker to Bins towards the nest entrance
    #If the worker is in Bin 4 but to the left of the x reference, use the first formula, else use the second
    PythagDist = ifelse(Bin == 4 & ScaledX < BinX, 
                        sqrt((DistanceX^2) + (DistanceY4^2)),
                        sqrt((DistanceX^2) + (DistanceY^2))),
    
    #Distances from the worker to the occupied Bins edge towards the back of the nest 
    DistanceX2 = ScaledX - BinX2,
    DistanceY2 = ScaledY - BinY2,
    #If the worker is in Bin 7 but to the left of the x reference, use the first formula, else use the second
    PythagDist2 = ifelse(Bin == 7 & ScaledX > BinX, sqrt((DistanceX^2) + (DistanceY6^2)),
                          sqrt((DistanceX2^2) +(DistanceY2^2))),
    #Calculating the average distance of each worker to all Bins
    MeanDist = 
      #Distances from workers in Bin 1 to all other Bins divided by 7
      ifelse(Bin == 1, (((Distance8_1 + PythagDist2) + #To Bin 8
                               (Distance7_1 + PythagDist2) + #To Bin 7
                               (Distance3_1 + (Distance3_4 * 2) + PythagDist2) + #To Bin 6
                               (Distance3_1 + Distance8_7 + PythagDist2) + #To Bin 5
                               (Distance3_1 + PythagDist2) + #To Bin 4
                               (Distance2_1 + PythagDist2) + #To Bin 3
                               (PythagDist2)) / 7), #To Bin 2
             #Distances from workers in Bin 2 to all other Bins divided by 7
             ifelse(Bin == 2, (((Distance8_2 + PythagDist2) + #To Bin 8
                                  ((Distance7_2 + PythagDist2)) + #To Bin 7
                                  (Distance7_2 - Distance7_5 + PythagDist2) + #To Bin 6
                                  (Distance8_7*2 + PythagDist2) + #To Bin 5
                                  (Distance8_7 + PythagDist2) + #To Bin 4
                                  (PythagDist2) + #To Bin 3
                                  (PythagDist)) / 7), #To Bin 1
                    #Distances from workers in Bin 3 to all other Bins divided by 7
                    ifelse(Bin == 3, (((Distance8_4 + PythagDist2) + #To Bin 8
                                         (Distance7_4 + PythagDist2) + #To Bin 7
                                         (Distance8_7 + Distance3_4 + PythagDist2) + #To Bin 6
                                         (Distance8_7 + PythagDist2) + #To Bin 5
                                         (PythagDist2) + #To Bin 4
                                         (PythagDist) + #To Bin 2
                                         (PythagDist + Distance2_1)) / 7), #To Bin 1
                           #Distances from workers in Bin 4 to all other Bins divided by 7
                           ifelse(Bin == 4, (((Distance8_4 - Distance8_7 + PythagDist2) + #To Bin 8
                                                (Distance7_4 - Distance8_7 + PythagDist2) + #To Bin 7
                                                (PythagDist2 + Distance3_4) + #To Bin 6
                                                (PythagDist2) + #To Bin 5
                                                (PythagDist) + #To Bin 3
                                                (PythagDist + Distance8_7) + #To Bin 2
                                                (PythagDist + Distance3_1)) / 7), #To Bin 1
                                  #Distances from workers in Bin 5 to all other Bins divided by 7
                                  ifelse(Bin == 5, (((Distance8_7 + Distance7_5 + PythagDist2) + #To Bin 8
                                                    (Distance7_5 + PythagDist2) + #To Bin 7
                                                    (PythagDist2) + #To Bin 6
                                                    (PythagDist) + #To Bin 4
                                                    (PythagDist + Distance7_5) + #To Bin 3
                                                    (PythagDist + Distance7_5 * 2) + #To Bin 2
                                                    (PythagDist + Distance7_4 + Distance3_1)) / 7), #To Bin 1
                                         #Distances from workers in Bin 6 to all other Bins divided by 7
                                         ifelse(Bin == 6, (((PythagDist2 + Distance8_7) + #To Bin 8
                                                           (PythagDist2) + #To Bin 7
                                                           (PythagDist) + #To Bin 5
                                                           (PythagDist + Distance3_4) + #To Bin 4
                                                           (PythagDist + Distance3_4 + Distance8_7) + #To Bin 3
                                                           (PythagDist + Distance3_4 + Distance8_7 * 2) + #To Bin 2
                                                           (PythagDist + Distance3_4 + Distance8_7 + Distance3_1)) / 7), #To Bin 1
                                                #Distances from workers in Bin 7 to all other Bins divided by 7
                                                ifelse(Bin == 7,(((PythagDist2) + #To Bin 8
                                                                  (PythagDist) + #To Bin 6
                                                                  (PythagDist + Distance7_5) + #To Bin 5
                                                                  (PythagDist + Distance7_4) + #To Bin 4
                                                                  (PythagDist + Distance7_3) + #To Bin 3
                                                                  (PythagDist + Distance7_2) + #To Bin 2
                                                                  (PythagDist + Distance7_1)) / 7), #To Bin 1
                                                       #Distances from workers in Bin 8 to all other Bins divided by 7
                                                       ifelse(Bin == 8, (((PythagDist) + #To Bin 7
                                                                           (PythagDist + Distance8_7) + #To Bin 6
                                                                           (PythagDist + Distance7_5 + Distance8_7) + #To Bin 5
                                                                           (PythagDist + Distance7_4) + #To Bin 4
                                                                           (PythagDist + Distance7_4) + #To Bin 3
                                                                           (PythagDist + Distance7_4 + Distance8_7) + #To Bin 2
                                                                           (PythagDist + Distance7_4 + Distance3_1)) / 7), #To Bin 1
                                                                           0
                                                              ))))))))) %>%
  mutate(ScaledDistMean = MeanDist / (MaxDist), #Scaling worker average distance to all other bins
         #In case ScaledDistMean is greater than 1, change to 1 because the values shouldn't exist
         ScaledDistMean = ifelse(ScaledDistMean > 1, 1, ScaledDistMean)) %>% 
  select(Colony, Nest, Day, ScaledX, ScaledY, Bin, ScaledDist, Corner, ScaledDistMean, Density) #Select desired columns
}

MinDistanceTube(WorkerDistScaledRD1_RD2)

#Creating the reference x and y coordinates for the sequence and joining
#First we get the circle Bin reference coordinates
#Then we mirror the reference x coordinate by the center of the nest, which is 7.5cm long, therefore the center is (7.5 / 2)
#Finally we round the x and y values to the 0.01 to make joining possible later
BinCoordCircRadRedTest <- NestBinCircle %>%
  mutate(XSub = BinX - (7.5 / 2),
        BinX2 = (7.5 / 2) - XSub) %>%
  mutate_at(vars(Colony,BinID,BinX, BinX2, BinY,BinY2), funs(round(., 2)))%>%
  select(-c(Bin))
BinCoordCircRadRedTest
NestBinFull
#Create a sequence of numbers along each bin, for every 0.05 cm
#The function calls a row bind that is applied to the rows of circle nest worker coordinate dataset created above 
SequencesDistances <- do.call(rbind, apply(BinCoordCircRadRedTest, 1, function(x) 
  #This creates a new dataframe thatcreates a sequence of numbers (every 0.05cm) between Scaled X2 and Scaled X taken from the above dataset,
  #Colony is also taken from above as a reference key for future joining
  data.frame(Colony = x[1], BinX2 = x[8], BinX = x[4], Sequence = seq(x[8], x[4],by=0.05))))
SequencesDistances
#Common columns in both the SequencesDistances and BinCoordCircRadRedTest datasets need to be the same data (numeric) type for joining
#SequencesDistances
SequencesDistances$Colony<-as.numeric(SequencesDistances$Colony)
SequencesDistances$BinX2<-as.numeric(SequencesDistances$BinX2)
SequencesDistances$BinX<-as.numeric(SequencesDistances$BinX)

#BinCoordCircRadRedTest
BinCoordCircRadRedTest$Colony<-as.numeric(BinCoordCircRadRedTest$Colony)
BinCoordCircRadRedTest$BinX2<-as.numeric(BinCoordCircRadRedTest$BinX2)
BinCoordCircRadRedTest$BinX<-as.numeric(BinCoordCircRadRedTest$BinX)

#Joinging the final reference coordinate dataset for worker shortest distance to all nest sections but their own in circle nests
BinCoordCircRadRedTestGood<-full_join(BinCoordCircRadRedTest,SequencesDistances)%>%distinct()

#Calculating the average worker shortest distances to all other nest sections 
#This function uses the worker distance to the nest entrance dataset, the sequences dataset created above, and two reference datasets:
#The first has the x and y reference coordinates from which we use to calculate shortest distances
#The second provides the max shortest distance possible from the back of the nest to the front of the nest, allowing us to scale the shortest distances
WorkerDistScaledRD1_RD2Seq <- WorkerDistScaledRD1_RD2 %>%
  #First we filter circle nest coordinates from the worker distance to the nest entrance dataset
  filter(Nest == "Circle") %>%
  group_by(Colony) %>% #Group by the Colony column
  left_join(BinCoordCircRadRedTestGood) %>% #Join the sequences dataset 
  group_by(Colony, Bin, BinID) %>% #Group by the Colony, Bin and BinID columns
  #Calculating the x and y distances from each worker to every sequence point
  #There are two y reference points, because workers in their Bins have both y values above and below their Bin
  #Using a conditional Ifelse(), we can use either the first or second y reference to calculate these y-axis distances 
  #This prevents dublicate distances, and overall the approach works best for how the reference datasheet is formatted
  mutate(DistanceXSeq = ScaledX - Sequence, #X distance 
         DistanceYSeq = ScaledY - BinY, #First y distance
         DistanceYSeq2 = ScaledY - BinY2, #Second y distance
         #Conditional statement that has pythagorean distances calculated for Bins below the one the worker resides in first
         #And then calculates those distances above 
         #Absolute values of both x and y distances are used in each formula to prevent negative distances
         PythagDistSeq = ifelse (BinID - Bin < 2, 
                                 sqrt(abs(DistanceXSeq^2) + abs(DistanceYSeq^2)), 
                                 sqrt(abs(DistanceXSeq^2) + abs(DistanceYSeq2^2))),
         #Finding the smallest distance along each sequence
         MinDist = min(PythagDistSeq)) %>%
  ungroup() %>% #Ungroup the dataset
  select(Colony, Nest, Day, ScaledX, ScaledY, Bin, ScaledDist, Density, Corner, MinDist) %>% #Select desired columns
  distinct() %>% #Remove duplicates to avoid skewing the average calculations below
  left_join(DistBinsFull) %>% #Join the distance reference dataset used above throughout
  group_by(Colony, Bin) %>% #Group by the Colony and Bin columns
  #Calculated the worker scaled average distances to all other nest sections but their own
  #The average is calculated first
  #Then these averages are scaled using the max possible distance in each nest
  #Finally, as a check, all values greater than 1 are made 1. This is impossible, but keeps consistency with the methods used for the tube nest shape
  mutate(MeanDist = mean(MinDist), 
           ScaledDistMean = MeanDist / MaxDist,
           ScaledDistMean = ifelse(ScaledDistMean > 1, 1, ScaledDistMean)) %>%
  ungroup() %>% #Ungroup the dataset
  select(Colony, Nest, Day, ScaledX, ScaledY, Bin, ScaledDist, Density, Corner, ScaledDistMean) %>% #Select desired columns
  distinct() #Remove any duplicates

#Join the final worker mean scaled distance to all nest sections but their own dataset
WorkerDistScaledMeanDist<-full_join(DistanceToBins,WorkerDistScaledRD1_RD2Seq)
