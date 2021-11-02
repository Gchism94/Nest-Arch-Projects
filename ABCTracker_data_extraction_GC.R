#############################################
## Autor: Greg CHISM & Alann RATHERY
## Date: May 2019 - Current
## email: gchism@email.arizona.edu
## Project: Impact of nest architecture on communication network in the ant Temnothorax rugatulus 
## Title: Script for data transformation 
#############################################

##The aim of this script will be to transform the data I have collected from ABCTracker. 
##Indeed, the tracker record and give us the complete tracks for each ant and I will search for 
##contact between ants. 
##I will be interested during this script to 2 types of interaction and for each of those, I'll
##take the number of interaction event between two ants and the durtion od this one.
#This script suggest that there's any hole in the ID column of dataset (this is why 
#I change it on the Word file)

#First of all, I set the working directory manually
pacman::p_load(assertthat, #Loading required packages for code below. p_load() will download packages that aren't in system library
               data.table,
               forcats,
               gganimate,
               ggpubr,
               janitor,
               lme4,
               lmerTest,
               magrittr,
               MuMIn,
               purrr,
               rbenchmark,
               readr,
               tidyverse,
               tidyr,
               wesanderson
               )

#IMPORTING THE RAW ABCTRACKER DATA
#Below is the code I used to import all of the datasets from my working directory
##COLONY 5
#Colony 5 Circle Pre
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony5_2_Oct_17_AggnPreAM2.csv")#introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 5 Circle Aggn
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony5_2CircleOct_18AggnTest.csv")  #introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 5 Tube Pre
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony5_24Tube_Oct_17_AggnExpPM.csv")#introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 5 Tube Aggn
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony5TubeAug_17AggnTNew.csv")  #introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

##COLONY 6
#Colony 6 Circle Pre
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony6_3_Circle_Oct_17_AggnPreAM.csv")#introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 6 Circle Aggn
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony6_3CircleOct_18AggnT.csv")  #introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 6 Tube Pre
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony6_27_Oct_17_AggnPreAM.csv")#introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 6 Tube Aggn
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony6TubeOct_17AggnT.csv")  #introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

##COLONY 7
#Colony 7 Circle Pre
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony7_Circle_5_Oct_17_VidAMPre.csv")#introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 7 Circle Aggn
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony7Circle_AggnT.csv")  #introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 7 Tube Pre
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony7_23_Oct_17_AggnPreAM.csv")#introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 7 Tube Aggn
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony7Tube_AggnT.csv")  #introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

##COLONY 8
#Colony 8 Circle Pre
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony8_25_June_18_AggnPreAM.csv")#introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 8 Circle Aggn
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony8CircleJune_18AggnT.csv")  #introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 8 Tube Pre
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony8_7_June_18_AggnPreAM.csv")#introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 8 Tube Aggn
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony8TubeJune_18AggnT.csv")  #introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

##COLONY 9
#Colony 9 Circle Pre
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony9CirclePre.csv")#introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 9 Circle Aggn
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony9CircleJune_18AggnT.csv")  #introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 9 Tube Pre
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony9_Tube_6_June_18_AggnP.csv")#introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 9 Tube Aggn
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony9TubeJune_18AggnT.csv")  #introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

##COLONY 11
#Colony 11 Circle Pre
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony11_Circle_28_June_18_AggnPreAM.csv")#introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 11 Circle Aggn
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony11CircleJune_18AggnT.csv")  #introduction of the dataset
data <- data %>%
  select(-c(Tag, Completed))
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 11 Tube Pre
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony11_10_June_18_AggnPreAM_2.csv")#introduction of the dataset
data <- Colony11TubePreData
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 11 Tube Aggn
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony11TubeJune_18AggnT.csv")  #introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

##COLONY 13
#Colony 13 Circle Pre
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony13CirclePre.csv")#introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 13 Circle Aggn
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony13CircleJune_18AggnT.csv")  #introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 13 Tube Pre
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony13_Tube_29_June_18_AggnPre.csv")#introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 13 Tube Aggn
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony13TubeJune_18AggnT.csv")  #introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#NOTE: id 1 in this dataset does not produce a TRUE/FALSE for if(cond4 && cond5 | cond1 && cond5) and therefore causes an error
#This is (most likely) because the individual doesn't interact with any individual, but I am not certain
#To counter this I manually added the column and row for this id after it was completed
data <- data %>%
  filter(id > 1)

##COLONY 17
#Colony 17 Circle Pre
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony17_Circle_12_Aug_18_AggnPreAM.csv")#introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 17 Circle Aggn
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony17CircleAug_18AggnT.csv")  #introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 17 Tube Pre
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony17_21_July_18_AggnPrePM_2.csv")#introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 17 Tube Aggn
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony17TubeJuly_18AggnT.csv")  #introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))


##COLONY 18
#Colony 18 Circle Pre
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony18_Circle_14_Aug_18_AggnPreAM.csv")#introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 18 Circle Aggn
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony18CircleAug_18AggnT.csv")  #introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 18 Tube Pre
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony18TubePreVideo.csv")#introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 18 Tube Aggn
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony18TubeJuly_18AggnT.csv")  #introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))


##COLONY 20
#Colony 20 Circle Pre
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony20_18Aug20_CircPre.csv")#introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 20 Circle Aggn
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony20CircleAug_18AggnT.csv")  #introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 20 Tube Pre
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony20_Tube_1_Aug_18_AggnPreAM.csv")#introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#Colony 20 Tube Aggn
data <- read_csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/FinalCorrectedData/Colony20TubeAug_18AggnT.csv")  #introduction of the dataset
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))
#NOTE: id 1 in this dataset does not produce a TRUE/FALSE for if(cond4 && cond5 | cond1 && cond5) and therefore causes an error
#This is (most likely) because the individual doesn't interact with any individual, but I am not certain
#To counter this I manually added the column and row for this id after it was completed
data <- data %>%
  filter(id > 1)


##BEGIN HERE

#Assign the imported dataset to this object
data <- #Assign the imported dataset to this object

#Set the column names to those needed for the script below    
data <- setNames(data, c("id", "Frames", "locX", "locY", "orientation", "SizeWidth.px", "SizeLeng.px", "Speed.Px.s", "Interpolated", "headLocX", "headLocY"))

#REFERENCES FOR LOOPS BELOW 
#Set references to be used for matrix column and row names 
DataReference<- data %>%
  #Arranging the data from smallest to largest values in the "id" column
  arrange(id) %>%
  #Only keep the id column
  select(id) %>%
  #Remove duplicates
  distinct() %>%
  #Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var='obs')

#Joining the reference dataset with the raw ABCTracker output
#This gives us the sequential numbers instead of the true ID values
#The matrix filling loops below use sequential ID numbers
data1<-data %>%
  arrange(id) %>%
  ungroup() %>%
  left_join(DataReference) %>%
  select(-c(Interpolated,id))%>%
  rename(id = obs)

nbr_ID<-length(unique(data1$id)) #nbr_ID will be the number of individual IDs of my dataset
#We create a vector that will contain all the value of size to work on it
v<-vector("numeric", nbr_ID) 
for (i in 1:length(unique(data1$id))) {
  id <- unique(data1$id)[i] 
  v[i] <- data$SizeLeng.px[data1$id == id][1] # the [1] ensures that only one value is retrieved
} #This for loop is just to have all values of size 2 for all different ID. 
distance <- 0.3*mean(v)
v=mean(v)

#We create a vector that will contain all the value of size to work on it
g<-vector("numeric", nbr_ID) 
for (i in 1:length(unique(data1$id))) {
  id <- unique(data1$id)[i] 
  g[i] <- data1$SizeWidth.px[id][1] # the [1] ensures that only one value is retrieved
}
g=mean(g)

#This vector include the values of ID used for matrices
A<-c(1: nbr_ID) 

##FOR MATRIX GENERATION TOWARDS STATIC NETWORKS (SKIP IF ONLY CONCERNED WITH PAIRWISE DATASET PRODUCTION)

##GENERATE EMPTY MATRICES
#Building of the four matrices
##Non-reciprocal matrix
#The matrix size is determined by the number of IDs
NonR_matrix <- matrix(data=NA, nrow = nbr_ID , ncol = nbr_ID , byrow = F, dimnames = list(A)) 
#Filled with zeros
NonR_matrix [is.na(NonR_matrix)]<- 0
#Row and column names are the sequential IDs
rownames(NonR_matrix)<-A
colnames(NonR_matrix)<-A
##Reciprocal matrix
#Same as for the non-reciprocal matrix
R_matrix <- matrix(data=NA, nrow = nbr_ID , ncol = nbr_ID , byrow = F, dimnames = list(A)) 
R_matrix [is.na(R_matrix)]<- 0
rownames(R_matrix)<-A
colnames(R_matrix)<-A
#Now, all our matrices are full with O in each cell


#Creating empty lists to fill with the the pairwise interactions from each frame
R_data = list() 
NR_data = list()

#Here the nested for loops go through each ID and determine whether they interact with all other IDs
#This nested loop both fills the matrices for our overall networks and the lists that we rbind to form the pairwise interaction dataframes for our time ordered networks
#This is done every frame - necessarily so - which is why it is so computationally expensive and the largest processes take several hours
system.time(for (i in unique(data1$id)){ #Do it for all the id in the big data set I have
  cat("starting id ", i, " ")
  id.1 <- i
  frames_id<-data1[data1$id==i,]
  for (f in frames_id$Frames){
    frames.match <- data1[data1$Frames==f,] #First, we create a dataframe with only rows where frames are equal
    for(r in 1:nrow(frames.match)) { #Comparison of each row in the new dataframe
      r.id<-frames.match$id[r] #Pull out the value of id column for row r 
      headX_r<- frames.match$headLocX[r] #Definitions of X coordinates of head for the r individual
      headY_r<- frames.match$headLocY[r] #Definitions of Y coordinates of head for the r individual
      headX_i<- frames.match$headLocX[frames.match$id==i] #Definitions of X coordinates of head for the i individual
      headY_i<- frames.match$headLocY[frames.match$id==i] #Definitions of Y coordinates of head for the i individual
      middleY_r <- frames.match$locY[r] #Definitions of X coordinates of middle of the square for the r individual
      middleX_r <- frames.match$locX[r] #Definitions of Y coordinates of middle of the square for the r individual
      middleX_i <- frames.match$locX[frames.match$id==i] #Definitions of X coordinates of middle of the square for the i individual
      middleY_i <- frames.match$locY[frames.match$id==i] #Definitions of Y coordinates of middle of the square for the i individual
      # Introduction of all my conditions
      #For non-reciprocal matrices, the conditions are to determine if ants are in our proximity window (1/3 of an ant-length)
      #We also determine if the head of the target ant is close to the head of the other ant (condition for reciprocal interactions)
      #Is the head close to the middle of the other ant
      cond4 <- sqrt((headX_r-middleX_i)^2+(headY_r-middleY_i)^2) <= 1.5*mean(v) 
      #Are the two ant bodies close to each other
      cond5 <- sqrt((middleX_r-middleX_i)^2+(middleY_r-middleY_i)^2) > 0.5*mean(g)
      cond1 <- sqrt((headX_r-headX_i)^2+(headY_r-headY_i)^2) <= distance #If the distance between two heads is within the threshold
      #Cond2 and cond3 ensures that we look only on individuals that are diametrically opposite
      cond2 <- frames.match$orientation[r]>=frames.match$orientation[frames.match$id==i]+160 && frames.match$orientation[r]<=frames.match$orientation[frames.match$id==i] +200
      cond3 <- frames.match$orientation[r]>=frames.match$orientation[frames.match$id==i]-200 && frames.match$orientation[r]<=frames.match$orientation[frames.match$id==i] -160
      
      #If the row of r is different of the row of i 
      if (frames.match$id[r]!=i) 
      { #If the ants are in proximity
        if(cond4 && cond5 | cond1 && cond5) {
          #If the head is close to the head of the other ant (reciprocal interaction)
          if(cond1 && cond2 | cond1 && cond3)
          { #Fill the reciprocal matrix + 1 for that reciprocal interaction
            R_matrix [frames.match$id[r],i]<- R_matrix [frames.match$id[r],i] + 1 
            #Fill the reciprocal list of pairwise interactions for reciprocal interactions
            R_data <- data.frame(VertexFrom = id.1, #Ant 1
                                 VertexTo = r.id, #Ant 2
                                 TimeStart = f, #Current frame 
                                 TimeStop = as.numeric(f) + 1) #The next frame, because the interaction at least goes from the current to the next
            R_datalist <- rbind(R_datalist, R_data) #Adding the dataframe for this frame to the list of pairwise interactions
            } #Add 1 to the value in the cell of the matrix
          else {
            #Else fill the non-reciprocal matrix
            NonR_matrix [frames.match$id[r],i] <- NonR_matrix [frames.match$id[r],i]+1
            #Pairwise interaction dataframe for non-reciprocal interactions
            #The method is the same as above
            NR_data <- data.frame(VertexFrom = id.1,
                                  VertexTo = r.id, 
                                  TimeStart = f,
                                  TimeStop = as.numeric(f) + 1)
            NonR_datalist <- rbind(NR_data, NonR_datalist)
            }
          }  
        }
      }
    }
  }
)


#PAIRWISE DATASET PRODUCTION
#Creating empty lists to fill with the the pairwise interactions from each frame
Full_datalist = list()
Full_data = list()

#NESTED LOOPS: IN SUM, FOR EACH UNIQUE ANT ID IN THE DATA, FOR EVERY FRAME, SEARCH ACROSS EVERY OTHER ANT TO SEE IF AN INTERACTION OCCURS BASED ON PROXIMITY AND ORIENTATION 
system.time(for (i in unique(data1$id)){ #Do it for all the id in the big data set I have
  cat("starting id ", i, " ") #Just shows the progress
  id.1 <- i
  frames_id<-data1[data1$id==i,]
  for (f in frames_id$Frames){
    frames.match <- data1[data1$Frames==f,] #First, we create a dataframe with only rows where frames are equal
    for(r in 1:nrow(frames.match)) { #Comparison of each row in the new dataframe
      r.id<-frames.match$id[r] #Pull out the value of id column for row r 
      headX_r<- frames.match$headLocX[r] #Definitions of X coordinates of head for the r individual
      headY_r<- frames.match$headLocY[r] #Definitions of Y coordinates of head for the r individual
      headX_i<- frames.match$headLocX[frames.match$id==i] #Definitions of X coordinates of head for the i individual
      headY_i<- frames.match$headLocY[frames.match$id==i] #Definitions of Y coordinates of head for the i individual
      middleY_r <- frames.match$locY[r] #Definitions of X coordinates of middle of the square for the r individual
      middleX_r <- frames.match$locX[r] #Definitions of Y coordinates of middle of the square for the r individual
      middleX_i <- frames.match$locX[frames.match$id==i] #Definitions of X coordinates of middle of the square for the i individual
      middleY_i <- frames.match$locY[frames.match$id==i] #Definitions of Y coordinates of middle of the square for the i individual
      
      # Introduction of all my conditions
      #For non-reciprocal interactions, the conditions are to determine if ants are in our proximity window (1/3 of an ant-length)
      #We also determine if the head of the target ant is close to the head of the other ant (condition for reciprocal interactions)
      
      #Conditions:
      #Is the head close to the middle of the other ant
      cond4 <- sqrt((headX_r-middleX_i)^2+(headY_r-middleY_i)^2) <= 1.5*mean(v) 
      
      #Are the two ant bodies close to each other
      cond5 <- sqrt((middleX_r-middleX_i)^2+(middleY_r-middleY_i)^2) > 0.5*mean(g)
      
      #Are the two ant heats close to each other 
      cond1 <- sqrt((headX_r-headX_i)^2+(headY_r-headY_i)^2) <= distance 
      
      #If the row of r is different of the row of i (are the IDs different)
      if (frames.match$id[r]!=i) 
      { #If the ants are in proximity
        if(cond4 && cond5 | cond1 && cond5) 
        {#Fill the reciprocal list of pairwise interactions for reciprocal interactions
          Full_data <- data.frame(VertexFrom = id.1, #Ant 1
                                  VertexTo = r.id, #Ant 2
                                  TimeStart = f, #Current frame
                                  TimeStop = f + 1)  #The next frame, because the interaction at least goes from the current to the next
          Full_datalist <- rbind(Full_datalist, Full_data) #Row binding the data for each frame to the complete list of pairwise interactions
        } 
        else {Full_datalist <- Full_datalist #If the conditions aren't met, the dataframe remains the same
          }
        }  
      }
    }
  }
)

#Shortcuts 
#3D data structure, ant-ant matrix, frame

pacman::p_load("timeordered") #I did not load above because plyr is a dependency for the package and this messes with all of the dplyr functions. I will fix this issue later. 

detach("package:timeordered", unload = TRUE) #Use when you don't want timeordered loaded anymore

data(ants) #Reference dataset for timeordered

#THE REMAINING CODE ISN'T CLEAN AND IS USED FOR SAVING MATRICES AND DATAFRAMES

#Same the matrices as data.frames in an object
R <- as.data.frame(R_matrix)
NR<- as.data.frame(NonR_matrix)


# Extract CSV file of the link between nodes 
#Create a dataset that is the true IDs of the ants
ColumnNamesMatrix<- DataReference$id
#Make all row and column names the IDs
colnames(R) = rownames(R) = colnames(NR) = rownames(NR) = ColumnNamesMatrix
#Add a character to the names to prevent .CSV exports from adding an "X" to the column values
colnames(R) <- paste("id", colnames(R), sep = "_")
rownames(R) <- paste("id", rownames(R), sep = "_")
#Make sure they are all the same
colnames(NR) = rownames(NR) = colnames(R) = rownames(R)
R_datalist <- left_join(R_datalist, DataReference) 
R_datalist
#Export the matrices as .CSVs and add "id" to the top left corner to name the rows, and for reference when constructing the networks

#COLONY 5
Colony5CirclePreR <- R
Colony5CirclePreNR <- NR
write.table(data.frame("id"=rownames(Colony5CirclePreR),Colony5CirclePreR),"Colony5CirclePreRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony5CirclePreNR),Colony5CirclePreNR),"Colony5CirclePreNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony5TubePreRTimeOrder <- R_datalist %>%
  mutate(Colony = 5, Nest = "Tube", Trial = "Pre", Intn = "Direct")
write.csv(Colony5TubePreRTimeOrder, "Colony5TubePreRTimeOrder.csv", row.names = FALSE)

Colony5TubePreNRTimeOrder <- NonR_datalist %>%
  mutate(Colony = 5, Nest = "Tube", Trial = "Pre", Intn = "Indirect")
write.csv(NonR_datalist, "Colony5TubePreNRTimeOrder.csv", row.names = FALSE)

Colony5CircleAggnR <- R
Colony5CircleAggnNR <- NR
write.table(data.frame("id"=rownames(Colony5CircleAggnR),Colony5CircleAggnR),"Colony5CircleAggnRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony5CircleAggnNR),Colony5CircleAggnNR),"Colony5CircleAggnNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony5CirclePreRTimeOrder <- R_datalist %>%
  mutate(Colony = 5, Nest = "Circle", Trial = "Pre", Intn = "Direct")
write.csv(Colony5CirclePreRTimeOrder, "Colony5CirclePreRTimeOrder.csv", row.names = FALSE)

Colony5CirclePreNRTimeOrder <- NonR_datalist %>%
  mutate(Colony = 5, Nest = "Circle", Trial = "Pre", Intn = "Indirect")
write.csv(Colony5CirclePreNRTimeOrder, "Colony5CirclePreNRTimeOrder.csv", row.names = FALSE)


Colony5TubePreR <- R
Colony5TubePreNR <- NR
write.table(data.frame("id"=rownames(Colony5TubePreR),Colony5TubePreR),"Colony5TubePreRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony5TubePreNR),Colony5TubePreNR),"Colony5TubePreNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony5TubeAggnR <- R
Colony5TubeAggnNR <- NR
write.table(data.frame("id"=rownames(Colony5TubeAggnR),Colony5TubeAggnR),"Colony5TubeAggnRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony5TubeAggnNR),Colony5TubeAggnNR),"Colony5TubeAggnNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

#COLONY 6
Colony6CirclePreR <- R
Colony6CirclePreNR <- NR
write.table(data.frame("id"=rownames(Colony6CirclePreR),Colony6CirclePreR),"Colony6CirclePreRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony6CirclePreNR),Colony6CirclePreNR),"Colony6CirclePreNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony6CircleAggnR <- R
Colony6CircleAggnNR <- NR
write.table(data.frame("id"=rownames(Colony6CircleAggnR),Colony6CircleAggnR),"Colony6CircleAggnRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony6CircleAggnNR),Colony6CircleAggnNR),"Colony6CircleAggnNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony6TubePreR <- R
Colony6TubePreNR <- NR
write.table(data.frame("id"=rownames(Colony6TubePreR),Colony6TubePreR),"Colony6TubePreRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony6TubePreNR),Colony6TubePreNR),"Colony6TubePreNRMatrix.csv", row.names=FALSE,sep=",",dec=",")
Colony6TubePreRTimeOrder

Col6TubeAggnTimeOrder <- Full_datalist %>%
  mutate(Colony = 6, Nest = "Tube", Trial = "Aggn")

write.csv(Col6TubeAggnTimeOrder, "Col6TubeAggnTimeOrder.csv", row.names = FALSE)

Colony6TubePreNRTimeOrder
Colony6TubePreNRTimeOrder <- NonR_datalist %>%
  mutate(Colony = 6, Nest = "Tube", Trial = "Pre", Intn = "Indirect")
write.csv(NonR_datalist, "Colony6TubePreNRTimeOrder.csv", row.names = FALSE)



Colony6TubeAggnR <- R
Colony6TubeAggnNR <- NR
write.table(data.frame("id"=rownames(Colony6TubeAggnR),Colony6TubeAggnR),"Colony6TubeAggnRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony6TubeAggnNR),Colony6TubeAggnNR),"Colony6TubeAggnNRMatrix.csv", row.names=FALSE,sep=",",dec=",")
Colony6TubeAggnRTimeOrder
Colony6TubeAggnRTimeOrder <- R_datalist %>%
  mutate(Colony = 6, Nest = "Tube", Trial = "Aggn", Intn = "Direct")
write.csv(Colony6TubeAggnRTimeOrder, "Colony6TubeAggnRTimeOrder.csv", row.names = FALSE)

rbind(Full_TimeOrderCol5CircleAggn)

Colony6TubeAggnNRTimeOrder <- NonR_datalist %>%
  mutate(Colony = 6, Nest = "Tube", Trial = "Aggn", Intn = "Indirect")
write.csv(NonR_datalist, "Colony6TubeAggnNRTimeOrder.csv", row.names = FALSE)

#COLONY 7
Colony7CirclePreR <- R
Colony7CirclePreNR <- NR
write.table(data.frame("id"=rownames(Colony7CirclePreR),Colony7CirclePreR),"Colony7CirclePreRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony7CirclePreNR),Colony7CirclePreNR),"Colony7CirclePreNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony7CircleAggnR <- R
Colony7CircleAggnNR <- NR
write.table(data.frame("id"=rownames(Colony7CircleAggnR),Colony7CircleAggnR),"Colony7CircleAggnRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony7CircleAggnNR),Colony7CircleAggnNR),"Colony7CircleAggnNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony7TubePreR <- R
Colony7TubePreNR <- NR
write.table(data.frame("id"=rownames(Colony7TubePreR),Colony7TubePreR),"Colony7TubePreRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony7TubePreNR),Colony7TubePreNR),"Colony7TubePreNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony7TubeAggnR <- R
Colony7TubeAggnNR <- NR
write.table(data.frame("id"=rownames(Colony7TubeAggnR),Colony7TubeAggnR),"Colony7TubeAggnRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony7TubeAggnNR),Colony7TubeAggnNR),"Colony7TubeAggnNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

#COLONY 8
Colony8CirclePreR <- R
Colony8CirclePreNR <- NR
write.table(data.frame("id"=rownames(Colony8CirclePreR),Colony8CirclePreR),"Colony8CirclePreRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony8CirclePreNR),Colony8CirclePreNR),"Colony8CirclePreNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony8CircleAggnR <- R
Colony8CircleAggnNR <- NR
write.table(data.frame("id"=rownames(Colony8CircleAggnR),Colony8CircleAggnR),"Colony8CircleAggnRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony8CircleAggnNR),Colony8CircleAggnNR),"Colony8CircleAggnNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony8TubePreR <- R
Colony8TubePreNR <- NR
write.table(data.frame("id"=rownames(Colony8TubePreR),Colony8TubePreR),"Colony8TubePreRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony8TubePreNR),Colony8TubePreNR),"Colony8TubePreNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony8TubeAggnR <- R
Colony8TubeAggnNR <- NR
write.table(data.frame("id"=rownames(Colony8TubeAggnR),Colony8TubeAggnR),"Colony8TubeAggnRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony8TubeAggnNR),Colony8TubeAggnNR),"Colony8TubeAggnNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

#COLONY 9
Colony9CirclePreR <- R
Colony9CirclePreNR <- NR
write.table(data.frame("id"=rownames(Colony9CirclePreR),Colony9CirclePreR),"Colony9CirclePreRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony9CirclePreNR),Colony9CirclePreNR),"Colony9CirclePreNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony9CircleAggnR <- R
Colony9CircleAggnNR <- NR
write.table(data.frame("id"=rownames(Colony9CircleAggnR),Colony9CircleAggnR),"Colony9CircleAggnRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony9CircleAggnNR),Colony9CircleAggnNR),"Colony9CircleAggnNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony9TubePreR <- R
Colony9TubePreNR <- NR
write.table(data.frame("id"=rownames(Colony9TubePreR),Colony9TubePreR),"Colony9TubePreRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony9TubePreNR),Colony9TubePreNR),"Colony9TubePreNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony9TubeAggnR <- R
Colony9TubeAggnNR <- NR
write.table(data.frame("id"=rownames(Colony9TubeAggnR),Colony9TubeAggnR),"Colony9TubeAggnRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony9TubeAggnNR),Colony9TubeAggnNR),"Colony9TubeAggnNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

#COLONY 11
Colony11CirclePreR <- R
Colony11CirclePreNR <- NR
write.table(data.frame("id"=rownames(Colony11CirclePreR),Colony11CirclePreR),"Colony11CirclePreRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony11CirclePreNR),Colony11CirclePreNR),"Colony11CirclePreNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony11CircleAggnR <- R
Colony11CircleAggnNR <- NR
write.table(data.frame("id"=rownames(Colony11CircleAggnR),Colony11CircleAggnR),"Colony11CircleAggnRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony11CircleAggnNR),Colony11CircleAggnNR),"Colony11CircleAggnNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony11TubePreR <- R
Colony11TubePreNR <- NR
write.table(data.frame("id"=rownames(Colony11TubePreR),Colony11TubePreR),"Colony11TubePreRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony11TubePreNR),Colony11TubePreNR),"Colony11TubePreNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony11TubeAggnR <- R
Colony11TubeAggnNR <- NR
write.table(data.frame("id"=rownames(Colony11TubeAggnR),Colony11TubeAggnR),"Colony11TubeAggnRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony11TubeAggnNR),Colony11TubeAggnNR),"Colony11TubeAggnNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

#COLONY 13
Colony13CirclePreR <- R
Colony13CirclePreNR <- NR
write.table(data.frame("id"=rownames(Colony13CirclePreR),Colony13CirclePreR),"Colony13CirclePreRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony13CirclePreNR),Colony13CirclePreNR),"Colony13CirclePreNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony13CircleAggnR <- R
Colony13CircleAggnNR <- NR
write.table(data.frame("id"=rownames(Colony13CircleAggnR),Colony13CircleAggnR),"Colony13CircleAggnRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony13CircleAggnNR),Colony13CircleAggnNR),"Colony13CircleAggnNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony13TubePreR <- R
Colony13TubePreNR <- NR
write.table(data.frame("id"=rownames(Colony13TubePreR),Colony13TubePreR),"Colony13TubePreRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony13TubePreNR),Colony13TubePreNR),"Colony13TubePreNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony13TubeAggnR <- R
Colony13TubeAggnNR <- NR
#Needs manual correction (can code later...)
write.table(data.frame("id"=rownames(Colony13TubeAggnR),Colony13TubeAggnR),"Colony13TubeAggnRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony13TubeAggnNR),Colony13TubeAggnNR),"Colony13TubeAggnNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

#COLONY 17
Colony17CirclePreR <- R
Colony17CirclePreNR <- NR
write.table(data.frame("id"=rownames(Colony17CirclePreR),Colony17CirclePreR),"Colony17CirclePreRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony17CirclePreNR),Colony17CirclePreNR),"Colony17CirclePreNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony17CircleAggnR <- R
Colony17CircleAggnNR <- NR
write.table(data.frame("id"=rownames(Colony17CircleAggnR),Colony17CircleAggnR),"Colony17CircleAggnRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony17CircleAggnNR),Colony17CircleAggnNR),"Colony17CircleAggnNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony17TubePreR <- R
Colony17TubePreNR <- NR
write.table(data.frame("id"=rownames(Colony17TubePreR),Colony17TubePreR),"Colony17TubePreRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony17TubePreNR),Colony17TubePreNR),"Colony17TubePreNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony17TubeAggnR <- R
Colony17TubeAggnNR <- NR
write.table(data.frame("id"=rownames(Colony17TubeAggnR),Colony17TubeAggnR),"Colony17TubeAggnRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony17TubeAggnNR),Colony17TubeAggnNR),"Colony17TubeAggnNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

#COLONY 18
Colony18CirclePreR <- R
Colony18CirclePreNR <- NR
write.table(data.frame("id"=rownames(Colony18CirclePreR),Colony18CirclePreR),"Colony18CirclePreRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony18CirclePreNR),Colony18CirclePreNR),"Colony18CirclePreNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony18CircleAggnR <- R
Colony18CircleAggnNR <- NR
write.table(data.frame("id"=rownames(Colony18CircleAggnR),Colony18CircleAggnR),"Colony18CircleAggnRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony18CircleAggnNR),Colony18CircleAggnNR),"Colony18CircleAggnNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony18TubePreR <- R
Colony18TubePreNR <- NR
write.table(data.frame("id"=rownames(Colony18TubePreR),Colony18TubePreR),"Colony18TubePreRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony18TubePreNR),Colony18TubePreNR),"Colony18TubePreNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony18TubeAggnR <- R
Colony18TubeAggnNR <- NR
write.table(data.frame("id"=rownames(Colony18TubeAggnR),Colony18TubeAggnR),"Colony18TubeAggnRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony18TubeAggnNR),Colony18TubeAggnNR),"Colony18TubeAggnNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

#COLONY 20
Colony20CirclePreR <- R
Colony20CirclePreNR <- NR
write.table(data.frame("id"=rownames(Colony20CirclePreR),Colony20CirclePreR),"Colony20CirclePreRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony20CirclePreNR),Colony20CirclePreNR),"Colony20CirclePreNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony20CircleAggnR <- R
Colony20CircleAggnNR <- NR
write.table(data.frame("id"=rownames(Colony20CircleAggnR),Colony20CircleAggnR),"Colony20CircleAggnRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony20CircleAggnNR),Colony20CircleAggnNR),"Colony20CircleAggnNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony20TubePreR <- R
Colony20TubePreNR <- NR
write.table(data.frame("id"=rownames(Colony20TubePreR),Colony20TubePreR),"Colony20TubePreRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony20TubePreNR),Colony20TubePreNR),"Colony20TubePreNRMatrix.csv", row.names=FALSE,sep=",",dec=",")

Colony20TubeAggnR <- R
Colony20TubeAggnNR <- NR
#Needs manual correction (can code later...)
write.table(data.frame("id"=rownames(Colony20TubeAggnR),Colony20TubeAggnR),"Colony20TubeAggnRMatrix.csv", row.names=FALSE,sep=",",dec=",")
write.table(data.frame("id"=rownames(Colony20TubeAggnNR),Colony20TubeAggnNR),"Colony20TubeAggnNRMatrix.csv", row.names=FALSE,sep=",",dec=",")


library(timeordered)
Col11TubeAggnTimeOrder <- Col11TubeAggnTimeOrder %>% select(VertexFrom) %>% distinct()
Col11TubeAggnTimeOrder
Col11TubeAggnTimeOrder <- Col11TubeAggnTimeOrder %>% mutate(TimeStop = TimeStart + 1)
TimeOrderTest<-generatetonetwork(Col11TubeAggnTimeOrder)
plottonet(TimeOrderTest,edgecolor="black")
td100 <- generatetimedeltas(0, 7500, 100)
ns100 <- generatenetworkslices(TimeOrderTest, td100)
plotnetworkslices(ns100, td100)

td500 <- generatetimedeltas(0, 7500, 500)
ns500 <- generatenetworkslices(TimeOrderTest, td500)
plotnetworkslices(ns500, td500)

md100 <- applynetworkfunction(ns100, function(x) {diameter(x)})
md500 <- applynetworkfunction(ns500, function(x) {diameter(x)})
plot(midpoints(td500),unlist(md500),col="red")
plot(midpoints(td500),unlist(md500),type="l",col="red",xlab="Time",ylab="Network diameter")
lines(midpoints(td100),unlist(md100),type="l",col="blue",xlab="Time",ylab="Network diameter")
legend("topright",c("100 window size", "500 window size"),bty="n",lwd=2,col=c("blue","red"))

tl100 <- generatetimelags(0,7500,100)
nl100 <- generatenetworkslices(TimeOrderTest, tl100)
ml100 <- applynetworkfunction(nl100, function(x){diameter(x)})
plot(maxpoints(tl100),unlist(ml100),type="l",xlab="Aggregation time",ylab="Diameter",col="blue")

rt <- randomizetimes(Colony6TubeAggnRTimeOrder,withinvertexfrom=T,byvertexfrom=T,withreplacement=T)
plottonet(generatetonetwork(rt),edgecolor="black")

gshort <- generatetonetwork(rarefy(Colony6TubeAggnRTimeOrder, 0.05))
plottonet(gshort)

##IMPORTANT 

sa <- spreadanalysis(TimeOrderTest, seq(0,7500,by=500), 50)
boxplot(sa[,-1],xlab="Time delay",ylab="Fraction reached")
b <- transformspreadbyindividual(sa)
plot(ts(b,delta=50),plot.type="single",col=rainbow(ncol(b)),xlab="Time",ylab="Fraction reached")
legend("bottomright",colnames(b),lwd=1,col=rainbow(ncol(b)),bg="white")
b
b%>%filt
TimeOrderFract <- as.data.frame(b)
TimeOrderFract
TimeOrderFract1 <- rownames_to_column(TimeOrderFract, "Frame") %>% filter(Frame == 7500) %>% select(-c(Frame))
TimeOrderFractData <- list()
TimeOrderFractData <- cbind(t(TimeOrderFract1))
TimeOrderFractData
TimeOrderFractData <- rownames_to_column(as.data.frame(TimeOrderFractData), "id") 
TimeOrderFractData <- TimeOrderFractData %>% mutate(Fraction = V1) %>% select(-c(V1))
TimeOrderFractData
Colony6TubePreDist1 <- Colony6TubePreDist1 %>% mutate(ID = as.character(ID))
view(Colony6TubePreDist1)

Col6TubeDataReference<- Colony6TubePreDist1 %>%
  #Arranging the data from smallest to largest values in the "id" column
  arrange(ID) %>%
  ungroup() %>%
  #Only keep the id column
  select(ID) %>%
  #Remove duplicates
  distinct() %>%
  #Create a new column of sequential numbers, starting from 1 and going until the final row
  rowid_to_column(var='obs') %>% drop_na()
Col6TubeDataReference
Colony6TubePreDist1
Colony11TubeAggnDist1
DataReference1 <- DataReference %>% dplyr::rename(ID = id)
Colony11TubeAggnDist1
Colony11TubeAggnDist1 <- left_join(Colony11TubeAggnDist1, DataReference1) %>% drop_na() %>% dplyr::rename(id = obs) %>% ungroup() %>% select(-c(ID))
Colony11TubeAggnDist1
Colony11TubeAggnDist1.2 <- Colony11TubeAggnDist1 %>% ungroup() %>% mutate(id = as.character(id)) %>% filter(Seconds < 0.05 )
Colony11TubePreDist1
Colony11TubeAggnDist1.2
TimeOrderFractData
Col11DistFract <- left_join(TimeOrderFractData, Colony11TubeAggnDist1.2) %>% drop_na()
TimeOrderFractData
Col11DistFract
shp <- shortesthoppath(g,"WRRY", 243,  "GBGR", 1186)
plottonet(g, shp)

l <- generatelatencies(Col11TubeAggnTimeOrder,allindivs = union(Col11TubeAggnTimeOrder$VertexFrom, Col11TubeAggnTimeOrder$VertexTo))
image(l[,,1000],axes=F,frame=T,col=rainbow(100))
axis(1, at = (1:ncol(l))/ncol(l), labels=colnames(l),tick=F,las=2,cex.axis=0.2)
axis(2, at = (1:nrow(l))/nrow(l), labels=rownames(l),tick=F,las=2,cex.axis=0.2)

summary(lm(Fraction ~ ScaledDist, Col11DistFract))

ggplot(data=Col11DistFract,
                       aes(x=ScaledDist, y=Fraction)) +
  ggtitle("Col 11 tube nest, aggn")+
  geom_point(key_glyph = large_points,size=3,alpha=0.75,color="blue") +
  theme_pubclean()+
  theme(axis.text.x = element_text(size=16,family = "Arial", colour = "black"),
        axis.title = element_text(size = 18,family="Arial"),
        axis.text.y = element_text(size=16,family="Arial",colour = "black"),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 24, family = "Arial", colour = "black"),
        legend.key = element_blank(),
        legend.justification = c(1,1),
        legend.text=element_text(size = 16,family="Arial"),
        legend.title=element_text(size=18,family="Arial")) + 
  xlab("Scaled distance to entrance, frame 1") +
  ylab("Fraction reached by info from an indiv") 
