#######DESCRIPTION OF THE SCRIPT########
#This script is to create a graph of network from a matrix of contact. In the first time the aim is to 
#transform the matrix into the 2 dataset that we will use, the nodes dataset and the edge dataset
#Once these datasets are done, we will create the network

#####
##NAMES GIVEN HERE:
#fakematrix is the matrix of contact
#trynodes is the dataset of the nodes
#trylinks is the dataset of the links
#net is the network 

#Introduction of the libraries needed
pacman::p_load(reshape2,
               tidyverse,
               igraph,
               ggraph,
               graphlayouts,
               matrixStats,
               CINNA,
               lme4,
               lmerTest,
               MuMIn)

###### 
#FIRST PART: Transformation (UNNEEDED)
setwd("~/Desktop/Work Arizona/Datasets Alann Stage")
view(fakematrix)
fakematrix
fakematrix<-read.csv("Colony5",sep=',',h=T) #Introduction of the dataset 
TestMatrix <- Colony5CirclePreRMatrix
TestMatrix <- TestMatrix[,-1] #These 2 lines are for setting up the first column as row names
rownames(fakematrix) <- fakematrix[,1]
fakematrix[upper.tri(fakematrix)] <- 0 #If the matrix is a reciprocal one we will have 2 links per combination
fakematrix[upper.tri(R)] <- 0 #same here
Colony5CircleAggnRMatrix
is.square.matrix(as.matrix(NetworkData))
NetworkData <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony11CircleAggnNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
NetworkData <- NetworkData %>% column_to_rownames(var = "id") %>% select(-c(id))
view(NetworkData)
NetworkData <- as.matrix(NetworkData)
rownames(NetworkData) <- NetworkData[,1]

#IMPORTING DATABASES & GENERATING NETWORKS
## READ ME: The goal of this section is to: 
##(1) Import and combine all matrices, while also removing imported database after they are combined (they are redundant and take up memory)
##(2) Create matrices from the combined datasets
##COLONY 5
##AGGRESSION
#Tube nest
Colony5TubeAggnNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony5TubeAggnNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony5TubeAggnRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony5TubeAggnRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony5TubeAggnMatrix <- as.matrix(Colony5TubeAggnNRMatrix + Colony5TubeAggnRMatrix)

rm("Colony5TubeAggnNRMatrix", "Colony5TubeAggnRMatrix")

Colony5TubeAggnNetwork<-graph_from_adjacency_matrix(Colony5TubeAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony5TubeAggnNetworkUnweighted <- ifelse(Colony5TubeAggnMatrix > 1, 1, Colony5TubeAggnMatrix)


#Circle nest
Colony5CircleAggnNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony5CircleAggnNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony5CircleAggnRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony5CircleAggnRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony5CircleAggnMatrix <- as.matrix(Colony5CircleAggnNRMatrix + Colony5CircleAggnRMatrix)

rm("Colony5CircleAggnNRMatrix", "Colony5CircleAggnRMatrix")

Colony5CircleAggnNetwork<-graph_from_adjacency_matrix(Colony5CircleAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony5CircleAggnNetworkUnweighted <- ifelse(Colony5CircleAggnMatrix > 1, 1, Colony5TubeAggnMatrix)

##BASELINE
#Tube nest
Colony5TubePreNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony5TubePreNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony5TubePreRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony5TubePreRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony5TubePreMatrix <- as.matrix(Colony5TubePreNRMatrix + Colony5TubePreRMatrix)

rm("Colony5TubePreNRMatrix", "Colony5TubePreRMatrix")

#Circle nest
Colony5CirclePreNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony5CirclePreNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony5CirclePreRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony5CirclePreRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony5CirclePreMatrix <- as.matrix(Colony5CirclePreNRMatrix + Colony5CirclePreRMatrix)

rm("Colony5CirclePreNRMatrix", "Colony5CirclePreRMatrix")

##COLONY 6
##AGGRESSION
#Tube nest
Colony6TubeAggnNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony6TubeAggnNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony6TubeAggnRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony6TubeAggnRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony6TubeAggnMatrix <- as.matrix(Colony6TubeAggnNRMatrix + Colony6TubeAggnRMatrix)

rm("Colony6TubeAggnNRMatrix", "Colony6TubeAggnRMatrix")

#Circle nest
Colony6CircleAggnNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony6CircleAggnNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony6CircleAggnRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony6CircleAggnRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony6CircleAggnMatrix <- as.matrix(Colony6CircleAggnNRMatrix + Colony6CircleAggnRMatrix)

rm("Colony6CircleAggnNRMatrix", "Colony6CircleAggnRMatrix")

##BASELINE
#Tube nest
Colony6TubePreNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony6TubePreNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony6TubePreRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony6TubePreRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony6TubePreMatrix <- as.matrix(Colony6TubePreNRMatrix + Colony6TubePreRMatrix)

rm("Colony6TubePreNRMatrix", "Colony6TubePreRMatrix")

#Circle nest
Colony6CirclePreNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony6CirclePreNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony6CirclePreRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony6CirclePreRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony6CirclePreMatrix <- as.matrix(Colony6CirclePreNRMatrix + Colony6CirclePreRMatrix)

rm("Colony6CirclePreNRMatrix", "Colony6CirclePreRMatrix")


##COLONY 7
##AGGRESSION
#Tube nest
Colony7TubeAggnNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony7TubeAggnNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony7TubeAggnRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony7TubeAggnRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony7TubeAggnMatrix <- as.matrix(Colony7TubeAggnNRMatrix + Colony7TubeAggnRMatrix)

rm("Colony7TubeAggnNRMatrix", "Colony7TubeAggnRMatrix")

#Circle nest
Colony7CircleAggnNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony7CircleAggnNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony7CircleAggnRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony7CircleAggnRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony7CircleAggnMatrix <- as.matrix(Colony7CircleAggnNRMatrix + Colony7CircleAggnRMatrix)

rm("Colony7CircleAggnNRMatrix", "Colony7CircleAggnRMatrix")

##BASELINE
#Tube nest
Colony7TubePreNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony7TubePreNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony7TubePreRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony7TubePreRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony7TubePreMatrix <- as.matrix(Colony7TubePreNRMatrix + Colony7TubePreRMatrix)

rm("Colony7TubePreNRMatrix", "Colony7TubePreRMatrix")

#Circle nest
Colony7CirclePreNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony7CirclePreNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony7CirclePreRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony7CirclePreRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony7CirclePreMatrix <- as.matrix(Colony7CirclePreNRMatrix + Colony7CirclePreRMatrix)

rm("Colony7CirclePreNRMatrix", "Colony7CirclePreRMatrix")

##COLONY 8
##AGGRESSION
#Tube nest
Colony8TubeAggnNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony8TubeAggnNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony8TubeAggnRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony8TubeAggnRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony8TubeAggnMatrix <- as.matrix(Colony8TubeAggnNRMatrix + Colony8TubeAggnRMatrix)

rm("Colony8TubeAggnNRMatrix", "Colony8TubeAggnRMatrix")

#Circle nest
Colony8CircleAggnNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony8CircleAggnNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony8CircleAggnRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony8CircleAggnRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony8CircleAggnMatrix <- as.matrix(Colony8CircleAggnNRMatrix + Colony8CircleAggnRMatrix)

rm("Colony8CircleAggnNRMatrix", "Colony8CircleAggnRMatrix")

##BASELINE
#Tube nest
Colony8TubePreNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony8TubePreNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony8TubePreRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony8TubePreRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony8TubePreMatrix <- as.matrix(Colony8TubePreNRMatrix + Colony8TubePreRMatrix)

rm("Colony8TubePreNRMatrix", "Colony8TubePreRMatrix")

#Circle nest
Colony8CirclePreNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony8CirclePreNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony8CirclePreRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony8CirclePreRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony8CirclePreMatrix <- as.matrix(Colony8CirclePreNRMatrix + Colony8CirclePreRMatrix)

rm("Colony8CirclePreNRMatrix", "Colony8CirclePreRMatrix")

##COLONY 9
##AGGRESSION
#Tube nest
Colony9TubeAggnNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony9TubeAggnNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony9TubeAggnRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony9TubeAggnRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony9TubeAggnMatrix <- as.matrix(Colony9TubeAggnNRMatrix + Colony9TubeAggnRMatrix)

rm("Colony9TubeAggnNRMatrix", "Colony9TubeAggnRMatrix")

#Circle nest
Colony9CircleAggnNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony9CircleAggnNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony9CircleAggnRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony9CircleAggnRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony9CircleAggnMatrix <- as.matrix(Colony9CircleAggnNRMatrix + Colony9CircleAggnRMatrix)

rm("Colony9CircleAggnNRMatrix", "Colony9CircleAggnRMatrix")

##BASELINE
#Tube nest
Colony9TubePreNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony9TubePreNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony9TubePreRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony9TubePreRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony9TubePreMatrix <- as.matrix(Colony9TubePreNRMatrix + Colony9TubePreRMatrix)

rm("Colony9TubePreNRMatrix", "Colony9TubePreRMatrix")

#Circle nest
Colony9CirclePreNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony9CirclePreNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony9CirclePreRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony9CirclePreRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony9CirclePreMatrix <- as.matrix(Colony9CirclePreNRMatrix + Colony9CirclePreRMatrix)

rm("Colony9CirclePreNRMatrix", "Colony9CirclePreRMatrix")

##COLONY 11
##AGGRESSION
#Tube nest
Colony11TubeAggnNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony11TubeAggnNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony11TubeAggnRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony11TubeAggnRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony11TubeAggnMatrix <- as.matrix(Colony11TubeAggnNRMatrix + Colony11TubeAggnRMatrix)

rm("Colony11TubeAggnNRMatrix", "Colony11TubeAggnRMatrix")

#Circle nest
Colony11CircleAggnNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony11CircleAggnNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony11CircleAggnRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony11CircleAggnRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony11CircleAggnMatrix <- as.matrix(Colony11CircleAggnNRMatrix + Colony11CircleAggnRMatrix)

rm("Colony11CircleAggnNRMatrix", "Colony11CircleAggnRMatrix")

##BASELINE
#Tube nest
Colony11TubePreNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony11TubePreNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony11TubePreRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony11TubePreRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony11TubePreMatrix <- as.matrix(Colony11TubePreNRMatrix + Colony11TubePreRMatrix)

rm("Colony11TubePreNRMatrix", "Colony11TubePreRMatrix")

#Circle nest
Colony11CirclePreNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony11CirclePreNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony11CirclePreRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony11CirclePreRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony11CirclePreMatrix <- as.matrix(Colony11CirclePreNRMatrix + Colony11CirclePreRMatrix)

rm("Colony11CirclePreNRMatrix", "Colony11CirclePreRMatrix")

##COLONY 13
##AGGRESSION
#Tube nest
Colony13TubeAggnNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony13TubeAggnNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony13TubeAggnRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony13TubeAggnRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony13TubeAggnMatrix <- as.matrix(Colony13TubeAggnNRMatrix + Colony13TubeAggnRMatrix)

rm("Colony13TubeAggnNRMatrix", "Colony13TubeAggnRMatrix")

#Circle nest
Colony13CircleAggnNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony13CircleAggnNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony13CircleAggnRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony13CircleAggnRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony13CircleAggnMatrix <- as.matrix(Colony13CircleAggnNRMatrix + Colony13CircleAggnRMatrix) 

Colony13CircleAggnMatrixUnweighted <- ifelse(Colony13CircleAggnMatrix > 1, 1, Colony13CircleAggnMatrix)

rm("Colony13CircleAggnNRMatrix", "Colony13CircleAggnRMatrix")

##BASELINE
#Tube nest
Colony13TubePreNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony13TubePreNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony13TubePreRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony13TubePreRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony13TubePreMatrix <- as.matrix(Colony13TubePreNRMatrix + Colony13TubePreRMatrix)

rm("Colony13TubePreNRMatrix", "Colony13TubePreRMatrix")

#Circle nest
Colony13CirclePreNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony13CirclePreNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony13CirclePreRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony13CirclePreRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony13CirclePreMatrix <- as.matrix(Colony13CirclePreNRMatrix + Colony13CirclePreRMatrix)

rm("Colony13CirclePreNRMatrix", "Colony13CirclePreRMatrix")

##COLONY 17
##AGGRESSION
#Tube nest
Colony17TubeAggnNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony17TubeAggnNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony17TubeAggnRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony17TubeAggnRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony17TubeAggnMatrix <- as.matrix(Colony17TubeAggnNRMatrix + Colony17TubeAggnRMatrix)

rm("Colony17TubeAggnNRMatrix", "Colony17TubeAggnRMatrix")

#Circle nest
Colony17CircleAggnNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony17CircleAggnNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony17CircleAggnRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony17CircleAggnRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony17CircleAggnMatrix <- as.matrix(Colony17CircleAggnNRMatrix + Colony17CircleAggnRMatrix)

rm("Colony17CircleAggnNRMatrix", "Colony17CircleAggnRMatrix")

##BASELINE
#Tube nest
Colony17TubePreNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony17TubePreNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony17TubePreRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony17TubePreRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony17TubePreMatrix <- as.matrix(Colony17TubePreNRMatrix + Colony17TubePreRMatrix)

rm("Colony17TubePreNRMatrix", "Colony17TubePreRMatrix")

#Circle nest
Colony17CirclePreNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony17CirclePreNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony17CirclePreRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony17CirclePreRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony17CirclePreMatrix <- as.matrix(Colony17CirclePreNRMatrix + Colony17CirclePreRMatrix)

rm("Colony17CirclePreNRMatrix", "Colony17CirclePreRMatrix")

##COLONY 18
##AGGRESSION
#Tube nest
Colony18TubeAggnNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony18TubeAggnNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony18TubeAggnRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony18TubeAggnRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony18TubeAggnMatrix <- as.matrix(Colony18TubeAggnNRMatrix + Colony18TubeAggnRMatrix)

rm("Colony18TubeAggnNRMatrix", "Colony18TubeAggnRMatrix")

#Circle nest
Colony18CircleAggnNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony18CircleAggnNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony18CircleAggnRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony18CircleAggnRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony18CircleAggnMatrix <- as.matrix(Colony18CircleAggnNRMatrix + Colony18CircleAggnRMatrix)

rm("Colony18CircleAggnNRMatrix", "Colony18CircleAggnRMatrix")

##BASELINE
#Tube nest
Colony18TubePreNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony18TubePreNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony18TubePreRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony18TubePreRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony18TubePreMatrix <- as.matrix(Colony18TubePreNRMatrix + Colony18TubePreRMatrix)

rm("Colony18TubePreNRMatrix", "Colony18TubePreRMatrix")

#Circle nest
Colony18CirclePreNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony18CirclePreNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony18CirclePreRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony18CirclePreRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony18CirclePreMatrix <- as.matrix(Colony18CirclePreNRMatrix + Colony18CirclePreRMatrix)

rm("Colony18CirclePreNRMatrix", "Colony18CirclePreRMatrix")

##COLONY 20
##AGGRESSION
#Tube nest
Colony20TubeAggnNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony20TubeAggnNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony20TubeAggnRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony20TubeAggnRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony20TubeAggnMatrix <- as.matrix(Colony20TubeAggnNRMatrix + Colony20TubeAggnRMatrix)

rm("Colony20TubeAggnNRMatrix", "Colony20TubeAggnRMatrix")

#Circle nest
Colony20CircleAggnNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony20CircleAggnNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony20CircleAggnRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony20CircleAggnRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony20CircleAggnMatrix <- as.matrix(Colony20CircleAggnNRMatrix + Colony20CircleAggnRMatrix)

rm("Colony20CircleAggnNRMatrix", "Colony20CircleAggnRMatrix")

##BASELINE
#Tube nest
Colony20TubePreNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony20TubePreNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony20TubePreRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony20TubePreRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony20TubePreMatrix <- as.matrix(Colony20TubePreNRMatrix + Colony20TubePreRMatrix)

rm("Colony20TubePreNRMatrix", "Colony20TubePreRMatrix")

#Circle nest
Colony20CirclePreNRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony20CirclePreNRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset
Colony20CirclePreRMatrix <- read.csv("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Matrices/Colony20CirclePreRMatrix.csv", row.names = 1, header = TRUE)#introduction of the dataset

Colony20CirclePreMatrix <- as.matrix(Colony20CirclePreNRMatrix + Colony20CirclePreRMatrix)

rm("Colony20CirclePreNRMatrix", "Colony20CirclePreRMatrix")


#GENERATE NETWORKS
#COLONY 5
#AGGRESSION
Colony5TubeAggnNetwork <- graph_from_adjacency_matrix(Colony5TubeAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above
Colony5CircleAggnNetwork <- graph_from_adjacency_matrix(Colony5CircleAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony5TubeAggnUnweighted <- ifelse(Colony5TubeAggnMatrix > 1, 1, Colony5TubeAggnMatrix) #Unweighted matrix, replacing numbers greater than 1 with 1
Colony5CircleAggnUnweighted <- ifelse(Colony5CircleAggnMatrix > 1, 1, Colony5CircleAggnMatrix) #Unweighted matrix, replacing numbers greater than 1 with 1

Colony5TubeAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony5TubeAggnUnweighted) #Make a network from adjacency matrix above
Colony5CircleAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony5CircleAggnUnweighted) #Make a network from adjacency matrix above

#BASELINE
Colony5TubePreNetwork <- graph_from_adjacency_matrix(Colony5TubePreMatrix, weighted=TRUE) #Make a network from adjacency matrix above
Colony5CirclePreNetwork <- graph_from_adjacency_matrix(Colony5CirclePreMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony5TubePreUnweighted <- ifelse(Colony5TubePreMatrix > 1, 1, Colony5TubePreMatrix) #Unweighted matrix, replacing numbers greater than 1 with 1
Colony5CirclePreUnweighted <- ifelse(Colony5CirclePreMatrix > 1, 1, Colony5CirclePreMatrix) #Unweighted matrix, replacing numbers greater than 1 with 1

Colony5TubePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony5TubePreUnweighted) #Make a network from adjacency matrix above
Colony5CirclePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony5CirclePreUnweighted) #Make a network from adjacency matrix above

#COLONY 6
#AGGRESSION
Colony6TubeAggnNetwork<-graph_from_adjacency_matrix(Colony6TubeAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above
Colony6CircleAggnNetwork<-graph_from_adjacency_matrix(Colony6CircleAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony6TubeAggnUnweighted <- ifelse(Colony6TubeAggnMatrix > 1, 1, Colony6TubeAggnMatrix) #Unweighted matrix, replacing numbers greater than 1 with 1
Colony6CircleAggnUnweighted <- ifelse(Colony6CircleAggnMatrix > 1, 1, Colony6CircleAggnMatrix) #Unweighted matrix, replacing numbers greater than 1 with 1

Colony6TubeAggnNetworkUnweighted<-graph_from_adjacency_matrix(Colony6TubeAggnUnweighted) #Make a network from adjacency matrix above
Colony6CircleAggnNetworkUnweighted<-graph_from_adjacency_matrix(Colony6CircleAggnUnweighted) #Make a network from adjacency matrix above

#BASELINE
Colony6TubePreNetwork<-graph_from_adjacency_matrix(Colony6TubePreMatrix, weighted=TRUE) #Make a network from adjacency matrix above
Colony6CirclePreNetwork<-graph_from_adjacency_matrix(Colony6CirclePreMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony6TubePreUnweighted <- ifelse(Colony6TubePreMatrix > 1, 1, Colony6TubePreMatrix) #Unweighted matrix, replacing numbers greater than 1 with 1
Colony6CirclePreUnweighted <- ifelse(Colony6CirclePreMatrix > 1, 1, Colony6CirclePreMatrix) #Unweighted matrix, replacing numbers greater than 1 with 1

Colony6TubePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony6TubePreUnweighted) #Make a network from adjacency matrix above
Colony6CirclePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony6CirclePreUnweighted) #Make a network from adjacency matrix above

#COLONY 7
#AGGRESSION
Colony7TubeAggnNetwork<-graph_from_adjacency_matrix(Colony7TubeAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above
Colony7CircleAggnNetwork<-graph_from_adjacency_matrix(Colony7CircleAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony7TubeAggnUnweighted <- ifelse(Colony7TubeAggnMatrix > 1, 1, Colony7TubeAggnMatrix) #Unweighted matrix, replacing numbers greater than 1 with 1
Colony7CircleAggnUnweighted <- ifelse(Colony7CircleAggnMatrix > 1, 1, Colony7CircleAggnMatrix) #Unweighted matrix, replacing numbers greater than 1 with 1

Colony7TubeAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony7TubeAggnUnweighted) #Make a network from adjacency matrix above
Colony7CircleAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony7CircleAggnUnweighted) #Make a network from adjacency matrix above

#BASELINE
Colony7TubePreNetwork<-graph_from_adjacency_matrix(Colony7TubePreMatrix, weighted=TRUE) #Make a network from adjacency matrix above
Colony7CirclePreNetwork<-graph_from_adjacency_matrix(Colony7CirclePreMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony7TubePreNetworkUnweighted <- ifelse(Colony7TubePreMatrix > 1, 1, Colony7TubePreMatrix) #Unweighted matrix, replacing numbers greater than 1 with 1
Colony7CirclePreNetworkUnweighted <- ifelse(Colony7CirclePreMatrix > 1, 1, Colony7CirclePreMatrix) #Unweighted matrix, replacing numbers greater than 1 with 1

Colony7TubePreUnweighted <- ifelse(Colony7TubePreMatrix > 1, 1, Colony7TubePreMatrix) #Unweighted matrix, replacing numbers greater than 1 with 1
Colony7CirclePreUnweighted <- ifelse(Colony7CirclePreMatrix > 1, 1, Colony7CirclePreMatrix) #Unweighted matrix, replacing numbers greater than 1 with 1

Colony7TubePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony7TubePreUnweighted) #Make a network from adjacency matrix above
Colony7CirclePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony7CirclePreUnweighted) #Make a network from adjacency matrix above

#COLONY 8
#AGGRESSION
Colony8TubeAggnNetwork<-graph_from_adjacency_matrix(Colony8TubeAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above
Colony8CircleAggnNetwork<-graph_from_adjacency_matrix(Colony8CircleAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony8TubeAggnUnweighted <- ifelse(Colony8TubeAggnMatrix > 1, 1, Colony8TubeAggnMatrix) #Unweighted matrix, replacing numbers greater than 1 with 1
Colony8CircleAggnUnweighted <- ifelse(Colony8CircleAggnMatrix > 1, 1, Colony8CircleAggnMatrix) #Unweighted matrix, replacing numbers greater than 1 with 1

Colony8TubeAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony8TubeAggnUnweighted) #Make a network from adjacency matrix above
Colony8CircleAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony8CircleAggnUnweighted) #Make a network from adjacency matrix above

#BASELINE
Colony8TubePreNetwork<-graph_from_adjacency_matrix(Colony8TubePreMatrix, weighted=TRUE) #Make a network from adjacency matrix above
Colony8CirclePreNetwork<-graph_from_adjacency_matrix(Colony8CirclePreMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony8TubePreUnweighted <- ifelse(Colony8TubePreMatrix > 1, 1, Colony8TubePreMatrix)
Colony8CirclePreUnweighted <- ifelse(Colony8CirclePreMatrix > 1, 1, Colony8CirclePreMatrix)

Colony8TubePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony8TubePreUnweighted) #Make a network from adjacency matrix above
Colony8CirclePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony8CirclePreUnweighted) #Make a network from adjacency matrix above

#COLONY 9
#AGGRESSION
Colony9TubeAggnNetwork<-graph_from_adjacency_matrix(Colony9TubeAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above
Colony9CircleAggnNetwork<-graph_from_adjacency_matrix(Colony9CircleAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony9TubeAggnUnweighted <- ifelse(Colony9TubeAggnMatrix > 1, 1, Colony9TubeAggnMatrix)
Colony9CircleAggnUnweighted <- ifelse(Colony9CircleAggnMatrix > 1, 1, Colony9CircleAggnMatrix)

Colony9TubeAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony9TubeAggnUnweighted) #Make a network from adjacency matrix above
Colony9CircleAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony9CircleAggnUnweighted) #Make a network from adjacency matrix above

#BASELINE
Colony9TubePreNetwork<-graph_from_adjacency_matrix(Colony9TubePreMatrix, weighted=TRUE) #Make a network from adjacency matrix above
Colony9CirclePreNetwork<-graph_from_adjacency_matrix(Colony9CirclePreMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony9TubePreUnweighted <- ifelse(Colony9TubePreMatrix > 1, 1, Colony9TubePreMatrix)
Colony9CirclePreUnweighted <- ifelse(Colony9CirclePreMatrix > 1, 1, Colony9CirclePreMatrix)

Colony9TubePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony9TubePreUnweighted) #Make a network from adjacency matrix above
Colony9CirclePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony9CirclePreUnweighted) #Make a network from adjacency matrix above

#COLONY 11
#AGGRESSION
Colony11TubeAggnNetwork<-graph_from_adjacency_matrix(Colony11TubeAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above
Colony11CircleAggnNetwork<-graph_from_adjacency_matrix(Colony11CircleAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony11TubeAggnUnweighted <- ifelse(Colony11TubeAggnMatrix > 1, 1, Colony11TubeAggnMatrix)
Colony11CircleAggnUnweighted <- ifelse(Colony11CircleAggnMatrix > 1, 1, Colony11CircleAggnMatrix)

Colony11TubeAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony11TubeAggnUnweighted) #Make a network from adjacency matrix above
Colony11CircleAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony11CircleAggnUnweighted) #Make a network from adjacency matrix above

#BASELINE
Colony11TubePreNetwork<-graph_from_adjacency_matrix(Colony11TubePreMatrix, weighted=TRUE) #Make a network from adjacency matrix above
Colony11CirclePreNetwork<-graph_from_adjacency_matrix(Colony11CirclePreMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony11TubePreUnweighted <- ifelse(Colony11TubePreMatrix > 1, 1, Colony11TubePreMatrix)
Colony11CirclePreUnweighted <- ifelse(Colony11CirclePreMatrix > 1, 1, Colony11CirclePreMatrix)

Colony11TubePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony11TubePreUnweighted) #Make a network from adjacency matrix above
Colony11CirclePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony11CirclePreUnweighted) #Make a network from adjacency matrix above

#COLONY 13
#AGGRESSION
Colony13TubeAggnNetwork<-graph_from_adjacency_matrix(Colony13TubeAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above
Colony13CircleAggnNetwork<-graph_from_adjacency_matrix(Colony13CircleAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony13TubeAggnUnweighted <- ifelse(Colony13TubeAggnMatrix > 1, 1, Colony13TubeAggnMatrix)
Colony13CircleAggnUnweighted <- ifelse(Colony13CircleAggnMatrix > 1, 1, Colony13CircleAggnMatrix)

Colony13TubeAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony13TubeAggnUnweighted) #Make a network from adjacency matrix above
Colony13CircleAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony13CircleAggnUnweighted) #Make a network from adjacency matrix above

#BASELINE
Colony13TubePreNetwork<-graph_from_adjacency_matrix(Colony13TubePreMatrix, weighted=TRUE) #Make a network from adjacency matrix above
Colony13CirclePreNetwork<-graph_from_adjacency_matrix(Colony13CirclePreMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony13TubePreUnweighted <- ifelse(Colony13TubePreMatrix > 1, 1, Colony13TubePreMatrix)
Colony13CirclePreUnweighted <- ifelse(Colony13CirclePreMatrix > 1, 1, Colony13CirclePreMatrix)

Colony13TubePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony13TubePreUnweighted) #Make a network from adjacency matrix above
Colony13CirclePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony13CirclePreUnweighted) #Make a network from adjacency matrix above

#COLONY 17
#AGGRESSION
Colony17TubeAggnNetwork<-graph_from_adjacency_matrix(Colony17TubeAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above
Colony17CircleAggnNetwork<-graph_from_adjacency_matrix(Colony17CircleAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony17TubeAggnUnweighted <- ifelse(Colony17TubeAggnMatrix > 1, 1, Colony17TubeAggnMatrix)
Colony17CircleAggnUnweighted <- ifelse(Colony17CircleAggnMatrix > 1, 1, Colony17CircleAggnMatrix)

Colony17TubeAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony17TubeAggnUnweighted) #Make a network from adjacency matrix above
Colony17CircleAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony17CircleAggnUnweighted) #Make a network from adjacency matrix above

#BASELINE
Colony17TubePreNetwork<-graph_from_adjacency_matrix(Colony17TubePreMatrix, weighted=TRUE) #Make a network from adjacency matrix above
Colony17CirclePreNetwork<-graph_from_adjacency_matrix(Colony17CirclePreMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony17TubePreUnweighted <- ifelse(Colony17TubePreMatrix > 1, 1, Colony17TubePreMatrix)
Colony17CirclePreUnweighted <- ifelse(Colony17CirclePreMatrix > 1, 1, Colony17CirclePreMatrix)

Colony17TubePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony17TubePreUnweighted) #Make a network from adjacency matrix above
Colony17CirclePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony17CirclePreUnweighted) #Make a network from adjacency matrix above

#COLONY 18
#AGGRESSION
Colony18TubeAggnNetwork<-graph_from_adjacency_matrix(Colony18TubeAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above
Colony18CircleAggnNetwork<-graph_from_adjacency_matrix(Colony18CircleAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony18TubeAggnUnweighted <- ifelse(Colony18TubeAggnMatrix > 1, 1, Colony18TubeAggnMatrix)
Colony18CircleAggnUnweighted <- ifelse(Colony18CircleAggnMatrix > 1, 1, Colony18CircleAggnMatrix)

Colony18TubeAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony18TubeAggnUnweighted) #Make a network from adjacency matrix above
Colony18CircleAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony18CircleAggnUnweighted) #Make a network from adjacency matrix above

#BASELINE
Colony18TubePreNetwork<-graph_from_adjacency_matrix(Colony18TubePreMatrix, weighted=TRUE) #Make a network from adjacency matrix above
Colony18CirclePreNetwork<-graph_from_adjacency_matrix(Colony18CirclePreMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony18TubePreUnweighted <- ifelse(Colony18TubePreMatrix > 1, 1, Colony18TubePreMatrix)
Colony18CirclePreUnweighted <- ifelse(Colony18CirclePreMatrix > 1, 1, Colony18CirclePreMatrix)

Colony18TubePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony18TubePreUnweighted) #Make a network from adjacency matrix above
Colony18CirclePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony18CirclePreUnweighted) #Make a network from adjacency matrix above

#COLONY 20
#AGGRESSION
Colony20TubeAggnNetwork<-graph_from_adjacency_matrix(Colony20TubeAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above
Colony20CircleAggnNetwork<-graph_from_adjacency_matrix(Colony20CircleAggnMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony20TubeAggnUnweighted <- ifelse(Colony20TubeAggnMatrix > 1, 1, Colony20TubeAggnMatrix)
Colony20CircleAggnUnweighted <- ifelse(Colony20CircleAggnMatrix > 1, 1, Colony20CircleAggnMatrix)

Colony20TubeAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony20TubeAggnUnweighted) #Make a network from adjacency matrix above
Colony20CircleAggnNetworkUnweighted <- graph_from_adjacency_matrix(Colony20CircleAggnUnweighted) #Make a network from adjacency matrix above

#BASELINE
Colony20TubePreNetwork<-graph_from_adjacency_matrix(Colony20TubePreMatrix, weighted=TRUE) #Make a network from adjacency matrix above
Colony20CirclePreNetwork<-graph_from_adjacency_matrix(Colony20CirclePreMatrix, weighted=TRUE) #Make a network from adjacency matrix above

Colony20TubePreUnweighted <- ifelse(Colony20TubePreMatrix > 1, 1, Colony20TubePreMatrix)
Colony20CirclePreUnweighted <- ifelse(Colony20TubePreMatrix > 1, 1, Colony20TubePreMatrix)

Colony20TubePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony20TubePreUnweighted) #Make a network from adjacency matrix above
Colony20CirclePreNetworkUnweighted <- graph_from_adjacency_matrix(Colony20CirclePreUnweighted) #Make a network from adjacency matrix above

#NETWORK EFFICIENCY AND HARMONIC CENTRALITY
##READ ME: The purpose of this section is to generate dataframes composed of (1) network efficiency or (2) Node Harmonic Centrality, and then combine them for analyses

##COLONY 5
#TUBE NEST
##AGGRESSION

#Weighted
Colony5TubeAggnNetwork

#Unweighted
Colony5TubeAggnNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony5TubeAggnNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony5TubeAggnNetworkUnweighted)

#Network degree
d<-degree(Colony5TubeAggnNetworkUnweighted)

#Network characteristics
E(Colony5TubeAggnNetworkUnweighted)$width <- log(E(Colony5TubeAggnNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony5TubeAggnNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony5TubeAggnNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony5TubeAggnNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony5TubeAggnNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony5TubeAggnNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony5TubeAggnNetworkUnweighted,dim = 3, weights=NULL)
Colony5TubeAggnNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony5TubeAggnNetworkUnweighted)$width)
shortest_path_net<-distances(Colony5TubeAggnNetworkUnweighted, v = V(Colony5TubeAggnNetworkUnweighted), to = V(Colony5TubeAggnNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

#Harmonic centrality 
HC <- harmonic_centrality(Colony5TubeAggnNetworkUnweighted, mode=c("all"))

Colony5TubeAggnHC <- as.data.frame(cbind(HC))

Colony5TubeAggnHC <- rownames_to_column(Colony5TubeAggnHC, "ID") 

Colony5TubeAggnDist1 <- Colony5TubeAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony5TubeAggnDist1$ID <- interaction( "id", Colony5TubeAggnDist1$ID, sep = "_")

Colony5TubeAggnHCDist<-left_join(Colony5TubeAggnHC,Colony5TubeAggnDist1)%>%drop_na()

Colony5TubeAggnHCDistFinal <- as.data.frame(lapply(Colony5TubeAggnHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

##Value of efficiency of the real network
shortest_path_1<-distances(Colony5TubeAggnNetworkUnweighted, v = V(Colony5TubeAggnNetworkUnweighted), to = V(Colony5TubeAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

#Global efficiency
Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)
 
Colony5TubeAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 5, Nest = "Tube", Trial = "Aggn")

##BASELINE

#Weighted
Colony5TubePreNetwork

#Unweighted
Colony5TubePreNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony5TubePreNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony5TubePreNetworkUnweighted)

#Network degree
d<-degree(Colony5TubePreNetworkUnweighted)

#Network characteristics
E(Colony5TubePreNetworkUnweighted)$width <- log(E(Colony5TubePreNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony5TubePreNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony5TubePreNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony5TubePreNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony5TubePreNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony5TubePreNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony5TubePreNetworkUnweighted,dim = 3, weights=NULL)
Colony5TubePreNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony5TubePreNetworkUnweighted)$width)
shortest_path_net<-distances(Colony5TubePreNetworkUnweighted, v = V(Colony5TubePreNetworkUnweighted), to = V(Colony5TubePreNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

HC <- harmonic_centrality(Colony5TubePreNetworkUnweighted, mode=c("all"))

Colony5TubePreHC <- as.data.frame(cbind(HC))

Colony5TubePreHC <- rownames_to_column(Colony5TubePreHC, "ID") 

Colony5TubePreDist1 <- Colony5TubePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony5TubePreDist1$ID <- interaction( "id", Colony5TubePreDist1$ID, sep = "_")

Colony5TubePreHCDist<-left_join(Colony5TubePreHC,Colony5TubePreDist1)%>%drop_na()

Colony5TubePreHCDistFinal <- as.data.frame(lapply(Colony5TubePreHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

Colony5TubeHCDistFinal <- full_join(Colony5TubeAggnHCDistFinal, Colony5TubePreHCDistFinal)

##Value of efficiency of the real network
shortest_path_1<-distances(Colony5TubePreNetworkUnweighted, v = V(Colony5TubePreNetworkUnweighted), to = V(Colony5TubePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony5TubePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 5, Nest = "Tube", Trial = "Pre")

#CIRCLE NEST
##AGGRESSION

#Weighted
Colony5CircleAggnNetwork

#Unweighted
Colony5CircleAggnNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony5CircleAggnNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony5CircleAggnNetworkUnweighted)

#Network degree
d<-degree(Colony5CircleAggnNetworkUnweighted)

#Network characteristics
E(Colony5CircleAggnNetworkUnweighted)$width <- log(E(Colony5CircleAggnNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony5CircleAggnNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony5CircleAggnNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony5CircleAggnNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony5CircleAggnNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony5CircleAggnNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony5CircleAggnNetworkUnweighted,dim = 3, weights=NULL)
Colony5CircleAggnNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony5CircleAggnNetworkUnweighted)$width)
shortest_path_net<-distances(Colony5CircleAggnNetworkUnweighted, v = V(Colony5CircleAggnNetworkUnweighted), to = V(Colony5CircleAggnNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

#Harmonic centrality 
HC <- harmonic_centrality(Colony5CircleAggnNetworkUnweighted, mode=c("all"))

Colony5CircleAggnHC <- as.data.frame(cbind(HC))

Colony5CircleAggnHC <- rownames_to_column(Colony5CircleAggnHC, "ID") 

Colony5CircleAggnDist1 <- Colony5CircleAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony5CircleAggnDist1$ID <- interaction( "id", Colony5CircleAggnDist1$ID, sep = "_")

Colony5CircleAggnHCDist<-left_join(Colony5CircleAggnHC,Colony5CircleAggnDist1)%>%drop_na()

Colony5CircleAggnHCDistFinal <- as.data.frame(lapply(Colony5CircleAggnHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

##Value of efficiency of the real network
shortest_path_1<-distances(Colony5CircleAggnNetworkUnweighted, v = V(Colony5CircleAggnNetworkUnweighted), to = V(Colony5CircleAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony5CircleAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 5, Nest = "Circle", Trial = "Aggn")


##BASELINE

#Weighted
Colony5CirclePreNetwork

#Unweighted
Colony5CirclePreNetworkUnweighted 

#Number of edges in the matrix
m<-gorder(Colony5CirclePreNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony5CirclePreNetworkUnweighted)

#Network degree
d<-degree(Colony5CirclePreNetworkUnweighted)

#Network characteristics
E(Colony5CirclePreNetworkUnweighted)$width <- log(E(Colony5CirclePreNetworkUnweighted)$weight) #To plot the width of arrows according to their weight (of the relation)
E(Colony5CirclePreNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony5CirclePreNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony5CirclePreNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony5CirclePreNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony5CirclePreNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony5CirclePreNetworkUnweighted,dim = 3, weights=NULL)
Colony5CirclePreNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony5CirclePreNetworkUnweighted)$width)
shortest_path_net<-distances(Colony5CirclePreNetworkUnweighted, v = V(Colony5CirclePreNetworkUnweighted), to = V(Colony5CirclePreNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network
E(Colony5CirclePreNetworkUnweighted)$weight
HC <- harmonic_centrality(Colony5CirclePreNetworkUnweighted, mode=c("all"), weights = dist_net)

Colony5CirclePreHC <- as.data.frame(cbind(HC))

Colony5CirclePreHC <- rownames_to_column(Colony5CirclePreHC, "ID") 

Colony5CirclePreDist1 <- Colony5CirclePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony5CirclePreDist1$ID <- interaction( "id", Colony5CirclePreDist1$ID, sep = "_")

Colony5CirclePreHCDist<-left_join(Colony5CirclePreHC,Colony5CirclePreDist1)%>%drop_na()

Colony5CirclePreHCDistFinal <- as.data.frame(lapply(Colony5CirclePreHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

Colony5CircleHCDistFinal <- full_join(Colony5CircleAggnHCDistFinal, Colony5CirclePreHCDistFinal)

##Value of efficiency of the real network
shortest_path_1<-distances(Colony5CirclePreNetworkUnweighted, v = V(Colony5CirclePreNetworkUnweighted), to = V(Colony5CirclePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony5CirclePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 5, Nest = "Circle", Trial = "Pre")

#FULL DATASETS
#Harmonic centrality
Colony5HCDistFinal <- full_join(Colony5TubeHCDistFinal, Colony5CircleHCDistFinal)

#Network efficiency
Colony5NetEfficiency <- full_join(Colony5TubeAggnEfficiency, Colony5TubePreEfficiency) %>%
  full_join(Colony5CircleAggnEfficiency) %>%
  full_join(Colony5CirclePreEfficiency)

##COLONY 6
#TUBE NEST
##AGGRESSION

#Weighted
Colony6TubeAggnNetwork

#Unweighted
Colony6TubeAggnNetworkUnweighted 

#Number of edges in the matrix
m<-gorder(Colony6TubeAggnNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony6TubeAggnNetworkUnweighted)

#Network degree
d<-degree(Colony6TubeAggnNetworkUnweighted)

#Network characteristics
E(Colony6TubeAggnNetworkUnweighted)$width <- log(E(Colony6TubeAggnNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony6TubeAggnNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony6TubeAggnNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony6TubeAggnNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony6TubeAggnNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony6TubeAggnNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony6TubeAggnNetworkUnweighted,dim = 3, weights=NULL)
Colony6TubeAggnNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony6TubeAggnNetworkUnweighted)$width)
shortest_path_net<-distances(Colony6TubeAggnNetworkUnweighted, v = V(Colony6TubeAggnNetworkUnweighted), to = V(Colony6TubeAggnNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

#Harmonic centrality 
HC <- harmonic_centrality(Colony6TubeAggnNetworkUnweighted, mode=c("all") )

Colony6TubeAggnHC <- as.data.frame(cbind(HC))

Colony6TubeAggnHC <- rownames_to_column(Colony6TubeAggnHC, "ID") 

Colony6TubeAggnDist1 <- Colony6TubeAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony6TubeAggnDist1$ID <- interaction( "id", Colony6TubeAggnDist1$ID, sep = "_")

Colony6TubeAggnHCDist<-left_join(Colony6TubeAggnHC,Colony6TubeAggnDist1)%>%drop_na()

Colony6TubeAggnHCDistFinal <- as.data.frame(lapply(Colony6TubeAggnHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

##Value of efficiency of the real network
shortest_path_1<-distances(Colony6TubeAggnNetworkUnweighted, v = V(Colony6TubeAggnNetworkUnweighted), to = V(Colony6TubeAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony6TubeAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 6, Nest = "Tube", Trial = "Aggn")

##BASELINE

#Weighted
Colony6TubePreNetwork

#Unweighted
Colony6TubePreNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony6TubePreNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony6TubePreNetworkUnweighted)

#Network degree
d<-degree(Colony6TubePreNetworkUnweighted)

#Network characteristics
E(Colony6TubePreNetworkUnweighted)$width <- log(E(Colony6TubePreNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony6TubePreNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony6TubePreNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony6TubePreNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony6TubePreNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony6TubePreNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony6TubePreNetworkUnweighted,dim = 3, weights=NULL)
Colony6TubePreNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony6TubePreNetworkUnweighted)$width)
shortest_path_net<-distances(Colony6TubePreNetworkUnweighted, v = V(Colony6TubePreNetworkUnweighted), to = V(Colony6TubePreNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

HC <- harmonic_centrality(Colony6TubePreNetworkUnweighted, mode=c("all") )

Colony6TubePreHC <- as.data.frame(cbind(HC))

Colony6TubePreHC <- rownames_to_column(Colony6TubePreHC, "ID") 

Colony6TubePreDist1 <- Colony6TubePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony6TubePreDist1$ID <- interaction( "id", Colony6TubePreDist1$ID, sep = "_")

Colony6TubePreHCDist<-left_join(Colony6TubePreHC,Colony6TubePreDist1)%>%drop_na()

Colony6TubePreHCDistFinal <- as.data.frame(lapply(Colony6TubePreHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

Colony6TubeHCDistFinal <- full_join(Colony6TubeAggnHCDistFinal, Colony6TubePreHCDistFinal)

##Value of efficiency of the real network
shortest_path_1<-distances(Colony6TubePreNetworkUnweighted, v = V(Colony6TubePreNetworkUnweighted), to = V(Colony6TubePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony6TubePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 6, Nest = "Tube", Trial = "Pre")

#CIRCLE NEST
##AGGRESSION

#Weighted
Colony6CircleAggnNetwork

#Unweighted
Colony6CircleAggnNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony6CircleAggnNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony6CircleAggnNetworkUnweighted)

#Network degree
d<-degree(Colony6CircleAggnNetworkUnweighted)

#Network characteristics
E(Colony6CircleAggnNetworkUnweighted)$width <- log(E(Colony6CircleAggnNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony6CircleAggnNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony6CircleAggnNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony6CircleAggnNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony6CircleAggnNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony6CircleAggnNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony6CircleAggnNetworkUnweighted,dim = 3, weights=NULL)
Colony6CircleAggnNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony6CircleAggnNetworkUnweighted)$width)
shortest_path_net<-distances(Colony6CircleAggnNetworkUnweighted, v = V(Colony6CircleAggnNetworkUnweighted), to = V(Colony6CircleAggnNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

#Harmonic centrality 
HC <- harmonic_centrality(Colony6CircleAggnNetworkUnweighted, mode=c("all") )

Colony6CircleAggnHC <- as.data.frame(cbind(HC))

Colony6CircleAggnHC <- rownames_to_column(Colony6CircleAggnHC, "ID") 

Colony6CircleAggnDist1 <- Colony6CircleAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony6CircleAggnDist1$ID <- interaction( "id", Colony6CircleAggnDist1$ID, sep = "_")

Colony6CircleAggnHCDist<-left_join(Colony6CircleAggnHC,Colony6CircleAggnDist1)%>%drop_na()

Colony6CircleAggnHCDistFinal <- as.data.frame(lapply(Colony6CircleAggnHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

##Value of efficiency of the real network
shortest_path_1<-distances(Colony6CircleAggnNetworkUnweighted, v = V(Colony6CircleAggnNetworkUnweighted), to = V(Colony6CircleAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony6CircleAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 6, Nest = "Circle", Trial = "Aggn")

##BASELINE

#Weighted
Colony6CirclePreNetwork

#Unweighted
Colony6CirclePreNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony6CirclePreNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony6CirclePreNetworkUnweighted)

#Network degree
d<-degree(Colony6CirclePreNetworkUnweighted)

#Network characteristics
E(Colony6CirclePreNetworkUnweighted)$width <- log(E(Colony6CirclePreNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony6CirclePreNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony6CirclePreNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony6CirclePreNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony6CirclePreNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony6CirclePreNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony6CirclePreNetworkUnweighted,dim = 3, weights=NULL)
Colony6CirclePreNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony6CirclePreNetworkUnweighted)$width)
shortest_path_net<-distances(Colony6CirclePreNetworkUnweighted, v = V(Colony6CirclePreNetworkUnweighted), to = V(Colony6CirclePreNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

HC <- harmonic_centrality(Colony6CirclePreNetworkUnweighted, mode=c("all") )

Colony6CirclePreHC <- as.data.frame(cbind(HC))

Colony6CirclePreHC <- rownames_to_column(Colony6CirclePreHC, "ID") 

Colony6CirclePreDist1 <- Colony6CirclePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony6CirclePreDist1$ID <- interaction( "id", Colony6CirclePreDist1$ID, sep = "_")

Colony6CirclePreHCDist<-left_join(Colony6CirclePreHC,Colony6CirclePreDist1)%>%drop_na()

Colony6CirclePreHCDistFinal <- as.data.frame(lapply(Colony6CirclePreHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

Colony6CircleHCDistFinal <- full_join(Colony6CircleAggnHCDistFinal, Colony6CirclePreHCDistFinal)

##Value of efficiency of the real network
shortest_path_1<-distances(Colony6CirclePreNetworkUnweighted, v = V(Colony6CirclePreNetworkUnweighted), to = V(Colony6CirclePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony6CirclePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 6, Nest = "Circle", Trial = "Pre")

#FULL DATASET
#Harmonic centrality
Colony6HCDistFinal <- full_join(Colony6TubeHCDistFinal, Colony6CircleHCDistFinal)

#Network efficiency
Colony6NetEfficiency <- full_join(Colony6TubeAggnEfficiency, Colony6TubePreEfficiency) %>%
  full_join(Colony6CircleAggnEfficiency) %>%
  full_join(Colony6CirclePreEfficiency)

##COLONY 7
#TUBE NEST
##AGGRESSION

#Weighted
Colony7TubeAggnNetwork

#Unweighted
Colony7TubeAggnNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony7TubeAggnNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony7TubeAggnNetworkUnweighted)

#Network degree
d<-degree(Colony7TubeAggnNetworkUnweighted)

#Network characteristics
E(Colony7TubeAggnNetworkUnweighted)$width <- log(E(Colony7TubeAggnNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony7TubeAggnNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony7TubeAggnNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony7TubeAggnNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony7TubeAggnNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony7TubeAggnNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony7TubeAggnNetworkUnweighted,dim = 3, weights=NULL)
Colony7TubeAggnNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony7TubeAggnNetworkUnweighted)$width)
shortest_path_net<-distances(Colony7TubeAggnNetworkUnweighted, v = V(Colony7TubeAggnNetworkUnweighted), to = V(Colony7TubeAggnNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

#Harmonic centrality 
HC <- harmonic_centrality(Colony7TubeAggnNetworkUnweighted, mode=c("all") )

Colony7TubeAggnHC <- as.data.frame(cbind(HC))

Colony7TubeAggnHC <- rownames_to_column(Colony7TubeAggnHC, "ID") 

Colony7TubeAggnDist1 <- Colony7TubeAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony7TubeAggnDist1$ID <- interaction( "id", Colony7TubeAggnDist1$ID, sep = "_")

Colony7TubeAggnHCDist<-left_join(Colony7TubeAggnHC,Colony7TubeAggnDist1)%>%drop_na()

Colony7TubeAggnHCDistFinal <- as.data.frame(lapply(Colony7TubeAggnHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

##Value of efficiency of the real network
shortest_path_1<-distances(Colony7TubeAggnNetworkUnweighted, v = V(Colony7TubeAggnNetworkUnweighted), to = V(Colony7TubeAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony7TubeAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 7, Nest = "Tube", Trial = "Aggn")

##BASELINE

#Weighted
Colony7TubePreNetwork

#Unweighted
Colony7TubePreNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony7TubePreNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony7TubePreNetworkUnweighted)

#Network degree
d<-degree(Colony7TubePreNetworkUnweighted)

#Network characteristics
E(Colony7TubePreNetworkUnweighted)$width <- log(E(Colony7TubePreNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony7TubePreNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony7TubePreNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony7TubePreNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony7TubePreNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony7TubePreNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony7TubePreNetworkUnweighted,dim = 3, weights=NULL)
Colony7TubePreNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony7TubePreNetworkUnweighted)$width)
shortest_path_net<-distances(Colony7TubePreNetworkUnweighted, v = V(Colony7TubePreNetworkUnweighted), to = V(Colony7TubePreNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

HC <- harmonic_centrality(Colony7TubePreNetworkUnweighted, mode=c("all") )

Colony7TubePreHC <- as.data.frame(cbind(HC))

Colony7TubePreHC <- rownames_to_column(Colony7TubePreHC, "ID") 

Colony7TubePreDist1 <- Colony7TubePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony7TubePreDist1$ID <- interaction( "id", Colony7TubePreDist1$ID, sep = "_")

Colony7TubePreHCDist<-left_join(Colony7TubePreHC,Colony7TubePreDist1)%>%drop_na()

Colony7TubePreHCDistFinal <- as.data.frame(lapply(Colony7TubePreHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

Colony7TubeHCDistFinal <- full_join(Colony7TubeAggnHCDistFinal, Colony7TubePreHCDistFinal)

##Value of efficiency of the real network
shortest_path_1<-distances(Colony7TubePreNetworkUnweighted, v = V(Colony7TubePreNetworkUnweighted), to = V(Colony7TubePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony7TubePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 7, Nest = "Tube", Trial = "Pre")

#CIRCLE NEST

#Weighted
Colony7CircleAggnNetwork

#Unweighted
Colony7CircleAggnNetworkUnweighted

##AGGRESSION
#Number of edges in the matrix
m<-gorder(Colony7CircleAggnNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony7CircleAggnNetworkUnweighted)

#Network degree
d<-degree(Colony7CircleAggnNetworkUnweighted)

#Network characteristics
E(Colony7CircleAggnNetworkUnweighted)$width <- log(E(Colony7CircleAggnNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony7CircleAggnNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony7CircleAggnNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony7CircleAggnNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony7CircleAggnNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony7CircleAggnNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony7CircleAggnNetworkUnweighted,dim = 3, weights=NULL)
Colony7CircleAggnNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony7CircleAggnNetworkUnweighted)$width)
shortest_path_net<-distances(Colony7CircleAggnNetworkUnweighted, v = V(Colony7CircleAggnNetworkUnweighted), to = V(Colony7CircleAggnNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

#Harmonic centrality 
HC <- harmonic_centrality(Colony7CircleAggnNetworkUnweighted, mode=c("all") )

Colony7CircleAggnHC <- as.data.frame(cbind(HC))

Colony7CircleAggnHC <- rownames_to_column(Colony7CircleAggnHC, "ID") 

Colony7CircleAggnDist1 <- Colony7CircleAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony7CircleAggnDist1$ID <- interaction( "id", Colony7CircleAggnDist1$ID, sep = "_")

Colony7CircleAggnHCDist<-left_join(Colony7CircleAggnHC,Colony7CircleAggnDist1)%>%drop_na()

Colony7CircleAggnHCDistFinal <- as.data.frame(lapply(Colony7CircleAggnHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

##Value of efficiency of the real network
shortest_path_1<-distances(Colony7CircleAggnNetworkUnweighted, v = V(Colony7CircleAggnNetworkUnweighted), to = V(Colony7CircleAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony7CircleAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 7, Nest = "Circle", Trial = "Aggn")

##BASELINE

#Weighted
Colony7CirclePreNetwork

#Unweighted
Colony7CirclePreNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony7CirclePreNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony7CirclePreNetworkUnweighted)

#Network degree
d<-degree(Colony7CirclePreNetworkUnweighted)

#Network characteristics
E(Colony7CirclePreNetworkUnweighted)$width <- log(E(Colony7CirclePreNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony7CirclePreNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony7CirclePreNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony7CirclePreNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony7CirclePreNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony7CirclePreNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony7CirclePreNetworkUnweighted,dim = 3, weights=NULL)
Colony7CirclePreNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony7CirclePreNetworkUnweighted)$width)
shortest_path_net<-distances(Colony7CirclePreNetworkUnweighted, v = V(Colony7CirclePreNetworkUnweighted), to = V(Colony7CirclePreNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

HC <- harmonic_centrality(Colony7CirclePreNetworkUnweighted, mode=c("all") )

Colony7CirclePreHC <- as.data.frame(cbind(HC))

Colony7CirclePreHC <- rownames_to_column(Colony7CirclePreHC, "ID") 

Colony7CirclePreDist1 <- Colony7CirclePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony7CirclePreDist1$ID <- interaction( "id", Colony7CirclePreDist1$ID, sep = "_")

Colony7CirclePreHCDist<-left_join(Colony7CirclePreHC,Colony7CirclePreDist1)%>%drop_na()

Colony7CirclePreHCDistFinal <- as.data.frame(lapply(Colony7CirclePreHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

Colony7CircleHCDistFinal <- full_join(Colony7CircleAggnHCDistFinal, Colony7CirclePreHCDistFinal)

##Value of efficiency of the real network
shortest_path_1<-distances(Colony7CirclePreNetworkUnweighted, v = V(Colony7CirclePreNetworkUnweighted), to = V(Colony7CirclePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony7CirclePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 7, Nest = "Circle", Trial = "Pre")

#FULL DATASET
Colony7HCDistFinal <- full_join(Colony7TubeHCDistFinal, Colony7CircleHCDistFinal)

#Network efficiency
Colony7NetEfficiency <- full_join(Colony7TubeAggnEfficiency, Colony7TubePreEfficiency) %>%
  full_join(Colony7CircleAggnEfficiency) %>%
  full_join(Colony7CirclePreEfficiency)

##COLONY 8
#TUBE NEST
##AGGRESSION

#Weighted
Colony8TubeAggnNetwork

#Unweighted
Colony8TubeAggnNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony8TubeAggnNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony8TubeAggnNetworkUnweighted)

#Network degree
d<-degree(Colony8TubeAggnNetworkUnweighted)

#Network characteristics
E(Colony8TubeAggnNetworkUnweighted)$width <- log(E(Colony8TubeAggnNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony8TubeAggnNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony8TubeAggnNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony8TubeAggnNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony8TubeAggnNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony8TubeAggnNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony8TubeAggnNetworkUnweighted,dim = 3, weights=NULL)
Colony8TubeAggnNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony8TubeAggnNetworkUnweighted)$width)
shortest_path_net<-distances(Colony8TubeAggnNetworkUnweighted, v = V(Colony8TubeAggnNetworkUnweighted), to = V(Colony8TubeAggnNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

#Harmonic centrality 
HC <- harmonic_centrality(Colony8TubeAggnNetworkUnweighted, mode=c("all") )

Colony8TubeAggnHC <- as.data.frame(cbind(HC))

Colony8TubeAggnHC <- rownames_to_column(Colony8TubeAggnHC, "ID") 

Colony8TubeAggnDist1 <- Colony8TubeAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony8TubeAggnDist1$ID <- interaction( "id", Colony8TubeAggnDist1$ID, sep = "_")

Colony8TubeAggnHCDist<-left_join(Colony8TubeAggnHC,Colony8TubeAggnDist1)%>%drop_na()

Colony8TubeAggnHCDistFinal <- as.data.frame(lapply(Colony8TubeAggnHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

##Value of efficiency of the real network
shortest_path_1<-distances(Colony8TubeAggnNetworkUnweighted, v = V(Colony8TubeAggnNetworkUnweighted), to = V(Colony8TubeAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony8TubeAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 8, Nest = "Tube", Trial = "Aggn")

##BASELINE

#Weighted
Colony8TubePreNetwork

#Unweighted
Colony8TubePreNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony8TubePreNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony8TubePreNetworkUnweighted)

#Network degree
d<-degree(Colony8TubePreNetworkUnweighted)

#Network characteristics
E(Colony8TubePreNetworkUnweighted)$width <- log(E(Colony8TubePreNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony8TubePreNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony8TubePreNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony8TubePreNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony8TubePreNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony8TubePreNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony8TubePreNetworkUnweighted,dim = 3, weights=NULL)
Colony8TubePreNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony8TubePreNetworkUnweighted)$width)
shortest_path_net<-distances(Colony8TubePreNetworkUnweighted, v = V(Colony8TubePreNetworkUnweighted), to = V(Colony8TubePreNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

HC <- harmonic_centrality(Colony8TubePreNetworkUnweighted, mode=c("all") )

Colony8TubePreHC <- as.data.frame(cbind(HC))

Colony8TubePreHC <- rownames_to_column(Colony8TubePreHC, "ID") 

Colony8TubePreDist1 <- Colony8TubePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony8TubePreDist1$ID <- interaction( "id", Colony8TubePreDist1$ID, sep = "_")

Colony8TubePreHCDist<-left_join(Colony8TubePreHC,Colony8TubePreDist1)%>%drop_na()

Colony8TubePreHCDistFinal <- as.data.frame(lapply(Colony8TubePreHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

Colony8TubeHCDistFinal <- full_join(Colony8TubeAggnHCDistFinal, Colony8TubePreHCDistFinal)

##Value of efficiency of the real network
shortest_path_1<-distances(Colony8TubePreNetworkUnweighted, v = V(Colony8TubePreNetworkUnweighted), to = V(Colony8TubePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony8TubePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 8, Nest = "Tube", Trial = "Pre")

#CIRCLE NEST
##AGGRESSION

#Weighted
Colony8CircleAggnNetwork

#Unweighted
Colony8CircleAggnNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony8CircleAggnNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony8CircleAggnNetworkUnweighted)

#Network degree
d<-degree(Colony8CircleAggnNetworkUnweighted)

#Network characteristics
E(Colony8CircleAggnNetworkUnweighted)$width <- log(E(Colony8CircleAggnNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony8CircleAggnNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony8CircleAggnNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony8CircleAggnNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony8CircleAggnNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony8CircleAggnNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony8CircleAggnNetworkUnweighted,dim = 3, weights=NULL)
Colony8CircleAggnNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony8CircleAggnNetworkUnweighted)$width)
shortest_path_net<-distances(Colony8CircleAggnNetworkUnweighted, v = V(Colony8CircleAggnNetworkUnweighted), to = V(Colony8CircleAggnNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

#Harmonic centrality 
HC <- harmonic_centrality(Colony8CircleAggnNetworkUnweighted, mode=c("all") )

Colony8CircleAggnHC <- as.data.frame(cbind(HC))

Colony8CircleAggnHC <- rownames_to_column(Colony8CircleAggnHC, "ID") 

Colony8CircleAggnDist1 <- Colony8CircleAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony8CircleAggnDist1$ID <- interaction( "id", Colony8CircleAggnDist1$ID, sep = "_")

Colony8CircleAggnHCDist<-left_join(Colony8CircleAggnHC,Colony8CircleAggnDist1)%>%drop_na()

Colony8CircleAggnHCDistFinal <- as.data.frame(lapply(Colony8CircleAggnHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

##Value of efficiency of the real network
shortest_path_1<-distances(Colony8CircleAggnNetworkUnweighted, v = V(Colony8CircleAggnNetworkUnweighted), to = V(Colony8CircleAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony8CircleAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 8, Nest = "Circle", Trial = "Aggn")

##BASELINE

#Weighted
Colony8CirclePreNetwork

#Unweighted
Colony8CirclePreNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony8CirclePreNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony8CirclePreNetworkUnweighted)

#Network degree
d<-degree(Colony8CirclePreNetworkUnweighted)

#Network characteristics
E(Colony8CirclePreNetworkUnweighted)$width <- log(E(Colony8CirclePreNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony8CirclePreNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony8CirclePreNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony8CirclePreNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony8CirclePreNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony8CirclePreNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony8CirclePreNetworkUnweighted,dim = 3, weights=NULL)
Colony8CirclePreNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony8CirclePreNetworkUnweighted)$width)
shortest_path_net<-distances(Colony8CirclePreNetworkUnweighted, v = V(Colony8CirclePreNetworkUnweighted), to = V(Colony8CirclePreNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

HC <- harmonic_centrality(Colony8CirclePreNetworkUnweighted, mode=c("all") )

Colony8CirclePreHC <- as.data.frame(cbind(HC))

Colony8CirclePreHC <- rownames_to_column(Colony8CirclePreHC, "ID") 

Colony8CirclePreDist1 <- Colony8CirclePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony8CirclePreDist1$ID <- interaction( "id", Colony8CirclePreDist1$ID, sep = "_")

Colony8CirclePreHCDist<-left_join(Colony8CirclePreHC,Colony8CirclePreDist1)%>%drop_na()

Colony8CirclePreHCDistFinal <- as.data.frame(lapply(Colony8CirclePreHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

Colony8CircleHCDistFinal <- full_join(Colony8CircleAggnHCDistFinal, Colony8CirclePreHCDistFinal)

##Value of efficiency of the real network
shortest_path_1<-distances(Colony8CirclePreNetworkUnweighted, v = V(Colony8CirclePreNetworkUnweighted), to = V(Colony8CirclePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony8CirclePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 8, Nest = "Circle", Trial = "Pre")

#FULL DATASET
#Harmonic centrality
Colony8HCDistFinal <- full_join(Colony8TubeHCDistFinal, Colony8CircleHCDistFinal)

#Network efficiency
Colony8NetEfficiency <- full_join(Colony8TubeAggnEfficiency, Colony8TubePreEfficiency) %>%
  full_join(Colony8CircleAggnEfficiency) %>%
  full_join(Colony8CirclePreEfficiency)


##COLONY 9
#TUBE NEST
##AGGRESSION

#Weighted
Colony9TubeAggnNetwork

#Unweighted
Colony9TubeAggnNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony9TubeAggnNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony9TubeAggnNetworkUnweighted)

#Network degree
d<-degree(Colony9TubeAggnNetworkUnweighted)

#Network characteristics
E(Colony9TubeAggnNetworkUnweighted)$width <- log(E(Colony9TubeAggnNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony9TubeAggnNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony9TubeAggnNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony9TubeAggnNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony9TubeAggnNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony9TubeAggnNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony9TubeAggnNetworkUnweighted,dim = 3, weights=NULL)
Colony9TubeAggnNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony9TubeAggnNetworkUnweighted)$width)
shortest_path_net<-distances(Colony9TubeAggnNetworkUnweighted, v = V(Colony9TubeAggnNetworkUnweighted), to = V(Colony9TubeAggnNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

#Harmonic centrality 
HC <- harmonic_centrality(Colony9TubeAggnNetworkUnweighted, mode=c("all") )

Colony9TubeAggnHC <- as.data.frame(cbind(HC))

Colony9TubeAggnHC <- rownames_to_column(Colony9TubeAggnHC, "ID") 

Colony9TubeAggnDist1 <- Colony9TubeAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony9TubeAggnDist1$ID <- interaction( "id", Colony9TubeAggnDist1$ID, sep = "_")

Colony9TubeAggnHCDist<-left_join(Colony9TubeAggnHC,Colony9TubeAggnDist1)%>%drop_na()

Colony9TubeAggnHCDistFinal <- as.data.frame(lapply(Colony9TubeAggnHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

##Value of efficiency of the real network
shortest_path_1<-distances(Colony9TubeAggnNetworkUnweighted, v = V(Colony9TubeAggnNetworkUnweighted), to = V(Colony9TubeAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony9TubeAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 9, Nest = "Tube", Trial = "Aggn")

##BASELINE

#Weighted
Colony9TubePreNetwork

#Unweighted
Colony9TubePreNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony9TubePreNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony9TubePreNetworkUnweighted)

#Network degree
d<-degree(Colony9TubePreNetworkUnweighted)

#Network characteristics
E(Colony9TubePreNetworkUnweighted)$width <- log(E(Colony9TubePreNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony9TubePreNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony9TubePreNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony9TubePreNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony9TubePreNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony9TubePreNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony9TubePreNetworkUnweighted,dim = 3, weights=NULL)
Colony9TubePreNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony9TubePreNetworkUnweighted)$width)
shortest_path_net<-distances(Colony9TubePreNetworkUnweighted, v = V(Colony9TubePreNetworkUnweighted), to = V(Colony9TubePreNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

HC <- harmonic_centrality(Colony9TubePreNetworkUnweighted, mode=c("all") )

Colony9TubePreHC <- as.data.frame(cbind(HC))

Colony9TubePreHC <- rownames_to_column(Colony9TubePreHC, "ID") 

Colony9TubePreDist1 <- Colony9TubePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony9TubePreDist1$ID <- interaction( "id", Colony9TubePreDist1$ID, sep = "_")

Colony9TubePreHCDist<-left_join(Colony9TubePreHC,Colony9TubePreDist1)%>%drop_na()

Colony9TubePreHCDistFinal <- as.data.frame(lapply(Colony9TubePreHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

Colony9TubeHCDistFinal <- full_join(Colony9TubeAggnHCDistFinal, Colony9TubePreHCDistFinal)

##Value of efficiency of the real network
shortest_path_1<-distances(Colony9TubePreNetworkUnweighted, v = V(Colony9TubePreNetworkUnweighted), to = V(Colony9TubePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony9TubePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 9, Nest = "Tube", Trial = "Pre")

#CIRCLE NEST
##AGGRESSION

#Weighted
Colony9CircleAggnNetwork

#Unweighted
Colony9CircleAggnNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony9CircleAggnNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony9CircleAggnNetworkUnweighted)

#Network degree
d<-degree(Colony9CircleAggnNetworkUnweighted)

#Network characteristics
E(Colony9CircleAggnNetworkUnweighted)$width <- log(E(Colony9CircleAggnNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony9CircleAggnNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony9CircleAggnNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony9CircleAggnNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony9CircleAggnNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony9CircleAggnNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony9CircleAggnNetworkUnweighted,dim = 3, weights=NULL)
Colony9CircleAggnNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony9CircleAggnNetworkUnweighted)$width)
shortest_path_net<-distances(Colony9CircleAggnNetworkUnweighted, v = V(Colony9CircleAggnNetworkUnweighted), to = V(Colony9CircleAggnNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

#Harmonic centrality 
HC <- harmonic_centrality(Colony9CircleAggnNetworkUnweighted, mode=c("all") )

Colony9CircleAggnHC <- as.data.frame(cbind(HC))

Colony9CircleAggnHC <- rownames_to_column(Colony9CircleAggnHC, "ID") 

Colony9CircleAggnDist1 <- Colony9CircleAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony9CircleAggnDist1$ID <- interaction( "id", Colony9CircleAggnDist1$ID, sep = "_")

Colony9CircleAggnHCDist<-left_join(Colony9CircleAggnHC,Colony9CircleAggnDist1)%>%drop_na()

Colony9CircleAggnHCDistFinal <- as.data.frame(lapply(Colony9CircleAggnHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

##Value of efficiency of the real network
shortest_path_1<-distances(Colony9CircleAggnNetworkUnweighted, v = V(Colony9CircleAggnNetworkUnweighted), to = V(Colony9CircleAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony9CircleAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 9, Nest = "Circle", Trial = "Aggn")

##BASELINE

#Weighted
Colony9CirclePreNetwork

#Unweighted
Colony9CirclePreNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony9CirclePreNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony9CirclePreNetworkUnweighted)

#Network degree
d<-degree(Colony9CirclePreNetworkUnweighted)

#Network characteristics
E(Colony9CirclePreNetworkUnweighted)$width <- log(E(Colony9CirclePreNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony9CirclePreNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony9CirclePreNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony9CirclePreNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony9CirclePreNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony9CirclePreNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony9CirclePreNetworkUnweighted,dim = 3, weights=NULL)
Colony9CirclePreNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony9CirclePreNetworkUnweighted)$width)
shortest_path_net<-distances(Colony9CirclePreNetworkUnweighted, v = V(Colony9CirclePreNetworkUnweighted), to = V(Colony9CirclePreNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

HC <- harmonic_centrality(Colony9CirclePreNetworkUnweighted, mode=c("all") )

Colony9CirclePreHC <- as.data.frame(cbind(HC))

Colony9CirclePreHC <- rownames_to_column(Colony9CirclePreHC, "ID") 

Colony9CirclePreDist1 <- Colony9CirclePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony9CirclePreDist1$ID <- interaction( "id", Colony9CirclePreDist1$ID, sep = "_")

Colony9CirclePreHCDist<-left_join(Colony9CirclePreHC,Colony9CirclePreDist1)%>%drop_na()

Colony9CirclePreHCDistFinal <- as.data.frame(lapply(Colony9CirclePreHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

Colony9CircleHCDistFinal <- full_join(Colony9CircleAggnHCDistFinal, Colony9CirclePreHCDistFinal)

##Value of efficiency of the real network
shortest_path_1<-distances(Colony9CirclePreNetworkUnweighted, v = V(Colony9CirclePreNetworkUnweighted), to = V(Colony9CirclePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony9CirclePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 9, Nest = "Circle", Trial = "Pre")

#FULL DATASET
#Harmonic centrality
Colony9HCDistFinal <- full_join(Colony9TubeHCDistFinal, Colony9CircleHCDistFinal)

#Network efficiency
Colony9NetEfficiency <- full_join(Colony9TubeAggnEfficiency, Colony9TubePreEfficiency) %>%
  full_join(Colony9CircleAggnEfficiency) %>%
  full_join(Colony9CirclePreEfficiency)

##COLONY 11
#TUBE NEST
##AGGRESSION

#Weighted
Colony11TubeAggnNetwork

#Unweighted
Colony11TubeAggnNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony11TubeAggnNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony11TubeAggnNetworkUnweighted)

#Network degree
d<-degree(Colony11TubeAggnNetworkUnweighted)

#Network characteristics
E(Colony11TubeAggnNetworkUnweighted)$width <- log(E(Colony11TubeAggnNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony11TubeAggnNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony11TubeAggnNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony11TubeAggnNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony11TubeAggnNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony11TubeAggnNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony11TubeAggnNetworkUnweighted,dim = 3, weights=NULL)
Colony11TubeAggnNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony11TubeAggnNetworkUnweighted)$width)
shortest_path_net<-distances(Colony11TubeAggnNetworkUnweighted, v = V(Colony11TubeAggnNetworkUnweighted), to = V(Colony11TubeAggnNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

#Harmonic centrality 
HC <- harmonic_centrality(Colony11TubeAggnNetworkUnweighted, mode=c("all") )

Colony11TubeAggnHC <- as.data.frame(cbind(HC))

Colony11TubeAggnHC <- rownames_to_column(Colony11TubeAggnHC, "ID") 

Colony11TubeAggnDist1 <- Colony11TubeAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony11TubeAggnDist1$ID <- interaction( "id", Colony11TubeAggnDist1$ID, sep = "_")

Colony11TubeAggnHCDist<-left_join(Colony11TubeAggnHC,Colony11TubeAggnDist1)%>%drop_na()

Colony11TubeAggnHCDistFinal <- as.data.frame(lapply(Colony11TubeAggnHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

##Value of efficiency of the real network
shortest_path_1<-distances(Colony11TubeAggnNetworkUnweighted, v = V(Colony11TubeAggnNetworkUnweighted), to = V(Colony11TubeAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony11TubeAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 11, Nest = "Tube", Trial = "Aggn")

##BASELINE

#Weighted
Colony11TubePreNetwork

#Unweighted
Colony11TubePreNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony11TubePreNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony11TubePreNetworkUnweighted)

#Network degree
d<-degree(Colony11TubePreNetworkUnweighted)

#Network characteristics
E(Colony11TubePreNetworkUnweighted)$width <- log(E(Colony11TubePreNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony11TubePreNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony11TubePreNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony11TubePreNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony11TubePreNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony11TubePreNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony11TubePreNetworkUnweighted,dim = 3, weights=NULL)
Colony11TubePreNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony11TubePreNetworkUnweighted)$width)
shortest_path_net<-distances(Colony11TubePreNetworkUnweighted, v = V(Colony11TubePreNetworkUnweighted), to = V(Colony11TubePreNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

HC <- harmonic_centrality(Colony11TubePreNetworkUnweighted, mode=c("all"))

Colony11TubePreHC <- as.data.frame(cbind(HC))

Colony11TubePreHC <- rownames_to_column(Colony11TubePreHC, "ID") 

Colony11TubePreDist1 <- Colony11TubePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony11TubePreDist1$ID <- interaction( "id", Colony11TubePreDist1$ID, sep = "_")

Colony11TubePreHCDist<-left_join(Colony11TubePreHC,Colony11TubePreDist1)%>%drop_na()

Colony11TubePreHCDistFinal <- as.data.frame(lapply(Colony11TubePreHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

Colony11TubeHCDistFinal <- full_join(Colony11TubeAggnHCDistFinal, Colony11TubePreHCDistFinal)

##Value of efficiency of the real network
shortest_path_1<-distances(Colony11TubePreNetworkUnweighted, v = V(Colony11TubePreNetworkUnweighted), to = V(Colony11TubePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony11TubePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 11, Nest = "Tube", Trial = "Pre")

#CIRCLE NEST
##AGGRESSION

#Weighted
Colony11CircleAggnNetwork

#Unweighted
Colony11CircleAggnNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony11CircleAggnNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony11CircleAggnNetworkUnweighted)

#Network degree
d<-degree(Colony11CircleAggnNetworkUnweighted)

#Network characteristics
E(Colony11CircleAggnNetworkUnweighted)$width <- log(E(Colony11CircleAggnNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony11CircleAggnNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony11CircleAggnNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony11CircleAggnNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony11CircleAggnNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony11CircleAggnNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony11CircleAggnNetworkUnweighted,dim = 3, weights=NULL)
Colony11CircleAggnNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony11CircleAggnNetworkUnweighted)$width)
shortest_path_net<-distances(Colony11CircleAggnNetworkUnweighted, v = V(Colony11CircleAggnNetworkUnweighted), to = V(Colony11CircleAggnNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

#Harmonic centrality 
HC <- harmonic_centrality(Colony11CircleAggnNetworkUnweighted, mode=c("all") )

Colony11CircleAggnHC <- as.data.frame(cbind(HC))

Colony11CircleAggnHC <- rownames_to_column(Colony11CircleAggnHC, "ID") 

Colony11CircleAggnDist1 <- Colony11CircleAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony11CircleAggnDist1$ID <- interaction( "id", Colony11CircleAggnDist1$ID, sep = "_")

Colony11CircleAggnHCDist<-left_join(Colony11CircleAggnHC,Colony11CircleAggnDist1)%>%drop_na()

Colony11CircleAggnHCDistFinal <- as.data.frame(lapply(Colony11CircleAggnHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

##Value of efficiency of the real network
shortest_path_1<-distances(Colony11CircleAggnNetworkUnweighted, v = V(Colony11CircleAggnNetworkUnweighted), to = V(Colony11CircleAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony11CircleAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 11, Nest = "Circle", Trial = "Aggn")

##BASELINE

#Weighted
Colony11CirclePreNetwork

#Unweighted
Colony11CirclePreNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony11CirclePreNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony11CirclePreNetworkUnweighted)

#Network degree
d<-degree(Colony11CirclePreNetworkUnweighted)

#Network characteristics
E(Colony11CirclePreNetworkUnweighted)$width <- log(E(Colony11CirclePreNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony11CirclePreNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony11CirclePreNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony11CirclePreNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony11CirclePreNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony11CirclePreNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony11CirclePreNetworkUnweighted,dim = 3, weights=NULL)
Colony11CirclePreNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony11CirclePreNetworkUnweighted)$width)
shortest_path_net<-distances(Colony11CirclePreNetworkUnweighted, v = V(Colony11CirclePreNetworkUnweighted), to = V(Colony11CirclePreNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

HC <- harmonic_centrality(Colony11CirclePreNetworkUnweighted, mode=c("all") )

Colony11CirclePreHC <- as.data.frame(cbind(HC))

Colony11CirclePreHC <- rownames_to_column(Colony11CirclePreHC, "ID") 

Colony11CirclePreDist1 <- Colony11CirclePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony11CirclePreDist1$ID <- interaction( "id", Colony11CirclePreDist1$ID, sep = "_")

Colony11CirclePreHCDist<-left_join(Colony11CirclePreHC,Colony11CirclePreDist1)%>%drop_na()

Colony11CirclePreHCDistFinal <- as.data.frame(lapply(Colony11CirclePreHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

Colony11CircleHCDistFinal <- full_join(Colony11CircleAggnHCDistFinal, Colony11CirclePreHCDistFinal)

##Value of efficiency of the real network
shortest_path_1<-distances(Colony11CirclePreNetworkUnweighted, v = V(Colony11CirclePreNetworkUnweighted), to = V(Colony11CirclePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony11CirclePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 11, Nest = "Circle", Trial = "Pre")

#FULL DATASET
#Harmonic centrality
Colony11HCDistFinal <- full_join(Colony11TubeHCDistFinal, Colony11CircleHCDistFinal)

#Network efficiency
Colony11NetEfficiency <- full_join(Colony11TubeAggnEfficiency, Colony11TubePreEfficiency) %>%
  full_join(Colony11CircleAggnEfficiency) %>%
  full_join(Colony11CirclePreEfficiency)

##COLONY 13
#TUBE NEST
##AGGRESSION

#Weighted
Colony13TubeAggnNetwork

#Unweighted
Colony13TubeAggnNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony13TubeAggnNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony13TubeAggnNetworkUnweighted)

#Network degree
d<-degree(Colony13TubeAggnNetworkUnweighted)

#Network characteristics
E(Colony13TubeAggnNetworkUnweighted)$width <- log(E(Colony13TubeAggnNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony13TubeAggnNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony13TubeAggnNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony13TubeAggnNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony13TubeAggnNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony13TubeAggnNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony13TubeAggnNetworkUnweighted,dim = 3, weights=NULL)
Colony13TubeAggnNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony13TubeAggnNetworkUnweighted)$width)
shortest_path_net<-distances(Colony13TubeAggnNetworkUnweighted, v = V(Colony13TubeAggnNetworkUnweighted), to = V(Colony13TubeAggnNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

#Harmonic centrality 
HC <- harmonic_centrality(Colony13TubeAggnNetworkUnweighted, mode=c("all") )

Colony13TubeAggnHC <- as.data.frame(cbind(HC))

Colony13TubeAggnHC <- rownames_to_column(Colony13TubeAggnHC, "ID") 

Colony13TubeAggnDist1 <- Colony13TubeAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony13TubeAggnDist1$ID <- interaction( "id", Colony13TubeAggnDist1$ID, sep = "_")

Colony13TubeAggnHCDist<-left_join(Colony13TubeAggnHC,Colony13TubeAggnDist1)%>%drop_na()

Colony13TubeAggnHCDistFinal <- as.data.frame(lapply(Colony13TubeAggnHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

##Value of efficiency of the real network
shortest_path_1<-distances(Colony13TubeAggnNetworkUnweighted, v = V(Colony13TubeAggnNetworkUnweighted), to = V(Colony13TubeAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony13TubeAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 13, Nest = "Tube", Trial = "Aggn")

##BASELINE

#Weighted
Colony13TubePreNetwork

#Unweighted
Colony13TubePreNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony13TubePreNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony13TubePreNetworkUnweighted)

#Network degree
d<-degree(Colony13TubePreNetworkUnweighted)

#Network characteristics
E(Colony13TubePreNetworkUnweighted)$width <- log(E(Colony13TubePreNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony13TubePreNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony13TubePreNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony13TubePreNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony13TubePreNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony13TubePreNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony13TubePreNetworkUnweighted,dim = 3, weights=NULL)
Colony13TubePreNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony13TubePreNetworkUnweighted)$width)
shortest_path_net<-distances(Colony13TubePreNetworkUnweighted, v = V(Colony13TubePreNetworkUnweighted), to = V(Colony13TubePreNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

HC <- harmonic_centrality(Colony13TubePreNetworkUnweighted, mode=c("all") )

Colony13TubePreHC <- as.data.frame(cbind(HC))

Colony13TubePreHC <- rownames_to_column(Colony13TubePreHC, "ID") 

Colony13TubePreDist1 <- Colony13TubePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony13TubePreDist1$ID <- interaction( "id", Colony13TubePreDist1$ID, sep = "_")

Colony13TubePreHCDist<-left_join(Colony13TubePreHC,Colony13TubePreDist1)%>%drop_na()

Colony13TubePreHCDistFinal <- as.data.frame(lapply(Colony13TubePreHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

Colony13TubeHCDistFinal <- full_join(Colony13TubeAggnHCDistFinal, Colony13TubePreHCDistFinal)

##Value of efficiency of the real network
shortest_path_1<-distances(Colony13TubePreNetworkUnweighted, v = V(Colony13TubePreNetworkUnweighted), to = V(Colony13TubePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony13TubePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 13, Nest = "Tube", Trial = "Pre")

#CIRCLE NEST
##AGGRESSION

#Weighted
Colony13CircleAggnNetwork

#Unweighted
Colony13CircleAggnNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony13CircleAggnNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony13CircleAggnNetworkUnweighted)

#Network degree
d<-degree(Colony13CircleAggnNetworkUnweighted)

#Network characteristics
E(Colony13CircleAggnNetworkUnweighted)$width <- log(E(Colony13CircleAggnNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony13CircleAggnNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony13CircleAggnNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony13CircleAggnNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony13CircleAggnNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony13CircleAggnNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony13CircleAggnNetworkUnweighted,dim = 3, weights=NULL)
Colony13CircleAggnNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony13CircleAggnNetworkUnweighted)$width)
shortest_path_net<-distances(Colony13CircleAggnNetworkUnweighted, v = V(Colony13CircleAggnNetworkUnweighted), to = V(Colony13CircleAggnNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

#Harmonic centrality 
HC <- harmonic_centrality(Colony13CircleAggnNetworkUnweighted, mode=c("all") )

Colony13CircleAggnHC <- as.data.frame(cbind(HC))

Colony13CircleAggnHC <- rownames_to_column(Colony13CircleAggnHC, "ID") 

Colony13CircleAggnDist1 <- Colony13CircleAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony13CircleAggnDist1$ID <- interaction( "id", Colony13CircleAggnDist1$ID, sep = "_")

Colony13CircleAggnHCDist<-left_join(Colony13CircleAggnHC,Colony13CircleAggnDist1)%>%drop_na()

Colony13CircleAggnHCDistFinal <- as.data.frame(lapply(Colony13CircleAggnHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

##Value of efficiency of the real network
shortest_path_1<-distances(Colony13CircleAggnNetworkUnweighted, v = V(Colony13CircleAggnNetworkUnweighted), to = V(Colony13CircleAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony13CircleAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 13, Nest = "Circle", Trial = "Aggn")

##BASELINE

#Weighted
Colony13CirclePreNetwork

#Unweighted
Colony13CirclePreNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony13CirclePreNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony13CirclePreNetworkUnweighted)

#Network degree
d<-degree(Colony13CirclePreNetworkUnweighted)

#Network characteristics
E(Colony13CirclePreNetworkUnweighted)$width <- log(E(Colony13CirclePreNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony13CirclePreNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony13CirclePreNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony13CirclePreNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony13CirclePreNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony13CirclePreNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony13CirclePreNetworkUnweighted,dim = 3, weights=NULL)
Colony13CirclePreNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony13CirclePreNetworkUnweighted)$width)
shortest_path_net<-distances(Colony13CirclePreNetworkUnweighted, v = V(Colony13CirclePreNetworkUnweighted), to = V(Colony13CirclePreNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

HC <- harmonic_centrality(Colony13CirclePreNetworkUnweighted, mode=c("all") )

Colony13CirclePreHC <- as.data.frame(cbind(HC))

Colony13CirclePreHC <- rownames_to_column(Colony13CirclePreHC, "ID") 

Colony13CirclePreDist1 <- Colony13CirclePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony13CirclePreDist1$ID <- interaction( "id", Colony13CirclePreDist1$ID, sep = "_")

Colony13CirclePreHCDist<-left_join(Colony13CirclePreHC,Colony13CirclePreDist1)%>%drop_na()

Colony13CirclePreHCDistFinal <- as.data.frame(lapply(Colony13CirclePreHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

Colony13CircleHCDistFinal <- full_join(Colony13CircleAggnHCDistFinal, Colony13CirclePreHCDistFinal)

##Value of efficiency of the real network
shortest_path_1<-distances(Colony13CirclePreNetworkUnweighted, v = V(Colony13CirclePreNetworkUnweighted), to = V(Colony13CirclePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony13CirclePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 13, Nest = "Circle", Trial = "Pre")

#FULL DATASET
#Harmonic centrality
Colony13HCDistFinal <- full_join(Colony13TubeHCDistFinal, Colony13CircleHCDistFinal)

#Network efficiency
Colony13NetEfficiency <- full_join(Colony13TubeAggnEfficiency, Colony13TubePreEfficiency) %>%
  full_join(Colony13CircleAggnEfficiency) %>%
  full_join(Colony13CirclePreEfficiency)

##COLONY 17
#TUBE NEST
##AGGRESSION

#Weighted
Colony17TubeAggnNetwork

#Unweighted
Colony17TubeAggnNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony17TubeAggnNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony17TubeAggnNetworkUnweighted)

#Network degree
d<-degree(Colony17TubeAggnNetworkUnweighted)

#Network characteristics
E(Colony17TubeAggnNetworkUnweighted)$width <- log(E(Colony17TubeAggnNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony17TubeAggnNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony17TubeAggnNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony17TubeAggnNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony17TubeAggnNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony17TubeAggnNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony17TubeAggnNetworkUnweighted,dim = 3, weights=NULL)
Colony17TubeAggnNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony17TubeAggnNetworkUnweighted)$width)
shortest_path_net<-distances(Colony17TubeAggnNetworkUnweighted, v = V(Colony17TubeAggnNetworkUnweighted), to = V(Colony17TubeAggnNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

#Harmonic centrality 
HC <- harmonic_centrality(Colony17TubeAggnNetworkUnweighted, mode=c("all") )

Colony17TubeAggnHC <- as.data.frame(cbind(HC))

Colony17TubeAggnHC <- rownames_to_column(Colony17TubeAggnHC, "ID") 

Colony17TubeAggnDist1 <- Colony17TubeAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony17TubeAggnDist1$ID <- interaction( "id", Colony17TubeAggnDist1$ID, sep = "_")

Colony17TubeAggnHCDist<-left_join(Colony17TubeAggnHC,Colony17TubeAggnDist1)%>%drop_na()

Colony17TubeAggnHCDistFinal <- as.data.frame(lapply(Colony17TubeAggnHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

##Value of efficiency of the real network
shortest_path_1<-distances(Colony17TubeAggnNetworkUnweighted, v = V(Colony17TubeAggnNetworkUnweighted), to = V(Colony17TubeAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony17TubeAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 17, Nest = "Tube", Trial = "Aggn")

##BASELINE

#Weighted
Colony17TubePreNetwork

#Unweighted
Colony17TubePreNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony17TubePreNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony17TubePreNetworkUnweighted)

#Network degree
d<-degree(Colony17TubePreNetworkUnweighted)

#Network characteristics
E(Colony17TubePreNetworkUnweighted)$width <- log(E(Colony17TubePreNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony17TubePreNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony17TubePreNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony17TubePreNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony17TubePreNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony17TubePreNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony17TubePreNetworkUnweighted,dim = 3, weights=NULL)
Colony17TubePreNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony17TubePreNetworkUnweighted)$width)
shortest_path_net<-distances(Colony17TubePreNetworkUnweighted, v = V(Colony17TubePreNetworkUnweighted), to = V(Colony17TubePreNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

HC <- harmonic_centrality(Colony17TubePreNetworkUnweighted, mode=c("all") )

Colony17TubePreHC <- as.data.frame(cbind(HC))

Colony17TubePreHC <- rownames_to_column(Colony17TubePreHC, "ID") 

Colony17TubePreDist1 <- Colony17TubePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony17TubePreDist1$ID <- interaction( "id", Colony17TubePreDist1$ID, sep = "_")

Colony17TubePreHCDist<-left_join(Colony17TubePreHC,Colony17TubePreDist1)%>%drop_na()

Colony17TubePreHCDistFinal <- as.data.frame(lapply(Colony17TubePreHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

Colony17TubeHCDistFinal <- full_join(Colony17TubeAggnHCDistFinal, Colony17TubePreHCDistFinal)

##Value of efficiency of the real network
shortest_path_1<-distances(Colony17TubePreNetworkUnweighted, v = V(Colony17TubePreNetworkUnweighted), to = V(Colony17TubePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony17TubePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 17, Nest = "Tube", Trial = "Pre")

#CIRCLE NEST
##AGGRESSION

#Weighted
Colony17CircleAggnNetwork

#Unweighted
Colony17CircleAggnNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony17CircleAggnNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony17CircleAggnNetworkUnweighted)

#Network degree
d<-degree(Colony17CircleAggnNetworkUnweighted)

#Network characteristics
E(Colony17CircleAggnNetworkUnweighted)$width <- log(E(Colony17CircleAggnNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony17CircleAggnNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony17CircleAggnNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony17CircleAggnNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony17CircleAggnNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony17CircleAggnNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony17CircleAggnNetworkUnweighted,dim = 3, weights=NULL)
Colony17CircleAggnNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony17CircleAggnNetworkUnweighted)$width)
shortest_path_net<-distances(Colony17CircleAggnNetworkUnweighted, v = V(Colony17CircleAggnNetworkUnweighted), to = V(Colony17CircleAggnNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

#Harmonic centrality 
HC <- harmonic_centrality(Colony17CircleAggnNetworkUnweighted, mode=c("all") )

Colony17CircleAggnHC <- as.data.frame(cbind(HC))

Colony17CircleAggnHC <- rownames_to_column(Colony17CircleAggnHC, "ID") 

Colony17CircleAggnDist1 <- Colony17CircleAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony17CircleAggnDist1$ID <- interaction( "id", Colony17CircleAggnDist1$ID, sep = "_")

Colony17CircleAggnHCDist<-left_join(Colony17CircleAggnHC,Colony17CircleAggnDist1)%>%drop_na()

Colony17CircleAggnHCDistFinal <- as.data.frame(lapply(Colony17CircleAggnHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

##Value of efficiency of the real network
shortest_path_1<-distances(Colony17CircleAggnNetworkUnweighted, v = V(Colony17CircleAggnNetworkUnweighted), to = V(Colony17CircleAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony17CircleAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 17, Nest = "Circle", Trial = "Aggn")

##BASELINE

#Weighted
Colony17CirclePreNetwork

#Unweighted
Colony17CirclePreNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony17CirclePreNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony17CirclePreNetworkUnweighted)

#Network degree
d<-degree(Colony17CirclePreNetworkUnweighted)

#Network characteristics
E(Colony17CirclePreNetworkUnweighted)$width <- log(E(Colony17CirclePreNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony17CirclePreNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony17CirclePreNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony17CirclePreNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony17CirclePreNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony17CirclePreNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony17CirclePreNetworkUnweighted,dim = 3, weights=NULL)
Colony17CirclePreNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony17CirclePreNetworkUnweighted)$width)
shortest_path_net<-distances(Colony17CirclePreNetworkUnweighted, v = V(Colony17CirclePreNetworkUnweighted), to = V(Colony17CirclePreNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

HC <- harmonic_centrality(Colony17CirclePreNetworkUnweighted, mode=c("all") )

Colony17CirclePreHC <- as.data.frame(cbind(HC))

Colony17CirclePreHC <- rownames_to_column(Colony17CirclePreHC, "ID") 

Colony17CirclePreDist1 <- Colony17CirclePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony17CirclePreDist1$ID <- interaction( "id", Colony17CirclePreDist1$ID, sep = "_")

Colony17CirclePreHCDist<-left_join(Colony17CirclePreHC,Colony17CirclePreDist1)%>%drop_na()

Colony17CirclePreHCDistFinal <- as.data.frame(lapply(Colony17CirclePreHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

Colony17CircleHCDistFinal <- full_join(Colony17CircleAggnHCDistFinal, Colony17CirclePreHCDistFinal)

##Value of efficiency of the real network
shortest_path_1<-distances(Colony17CirclePreNetworkUnweighted, v = V(Colony17CirclePreNetworkUnweighted), to = V(Colony17CirclePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony17CirclePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 17, Nest = "Circle", Trial = "Pre")

#FULL DATASET
#Harmonic centrality
Colony17HCDistFinal <- full_join(Colony17TubeHCDistFinal, Colony17CircleHCDistFinal)

#Network efficiency
Colony17NetEfficiency <- full_join(Colony17TubeAggnEfficiency, Colony17TubePreEfficiency) %>%
  full_join(Colony17CircleAggnEfficiency) %>%
  full_join(Colony17CirclePreEfficiency)

##COLONY 18
#TUBE NEST
##AGGRESSION

#Weighted
Colony18TubeAggnNetwork

#Unweighted
Colony18TubeAggnNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony18TubeAggnNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony18TubeAggnNetworkUnweighted)

#Network degree
d<-degree(Colony18TubeAggnNetworkUnweighted)

#Network characteristics
E(Colony18TubeAggnNetworkUnweighted)$width <- log(E(Colony18TubeAggnNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony18TubeAggnNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony18TubeAggnNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony18TubeAggnNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony18TubeAggnNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony18TubeAggnNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony18TubeAggnNetworkUnweighted,dim = 3, weights=NULL)
Colony18TubeAggnNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony18TubeAggnNetworkUnweighted)$width)
shortest_path_net<-distances(Colony18TubeAggnNetworkUnweighted, v = V(Colony18TubeAggnNetworkUnweighted), to = V(Colony18TubeAggnNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

#Harmonic centrality 
HC <- harmonic_centrality(Colony18TubeAggnNetworkUnweighted, mode=c("all") )

Colony18TubeAggnHC <- as.data.frame(cbind(HC))

Colony18TubeAggnHC <- rownames_to_column(Colony18TubeAggnHC, "ID") 

Colony18TubeAggnDist1 <- Colony18TubeAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony18TubeAggnDist1$ID <- interaction( "id", Colony18TubeAggnDist1$ID, sep = "_")

Colony18TubeAggnHCDist<-left_join(Colony18TubeAggnHC,Colony18TubeAggnDist1)%>%drop_na()

Colony18TubeAggnHCDistFinal <- as.data.frame(lapply(Colony18TubeAggnHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

##Value of efficiency of the real network
shortest_path_1<-distances(Colony18TubeAggnNetworkUnweighted, v = V(Colony18TubeAggnNetworkUnweighted), to = V(Colony18TubeAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony18TubeAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 18, Nest = "Tube", Trial = "Aggn")

##BASELINE

#Weighted
Colony18TubePreNetwork

#Unweighted
Colony18TubePreNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony18TubePreNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony18TubePreNetworkUnweighted)

#Network degree
d<-degree(Colony18TubePreNetworkUnweighted)

#Network characteristics
E(Colony18TubePreNetworkUnweighted)$width <- log(E(Colony18TubePreNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony18TubePreNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony18TubePreNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony18TubePreNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony18TubePreNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony18TubePreNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony18TubePreNetworkUnweighted,dim = 3, weights=NULL)
Colony18TubePreNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony18TubePreNetworkUnweighted)$width)
shortest_path_net<-distances(Colony18TubePreNetworkUnweighted, v = V(Colony18TubePreNetworkUnweighted), to = V(Colony18TubePreNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

HC <- harmonic_centrality(Colony18TubePreNetworkUnweighted, mode=c("all") )

Colony18TubePreHC <- as.data.frame(cbind(HC))

Colony18TubePreHC <- rownames_to_column(Colony18TubePreHC, "ID") 

Colony18TubePreDist1 <- Colony18TubePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony18TubePreDist1$ID <- interaction( "id", Colony18TubePreDist1$ID, sep = "_")

Colony18TubePreHCDist<-left_join(Colony18TubePreHC,Colony18TubePreDist1)%>%drop_na()

Colony18TubePreHCDistFinal <- as.data.frame(lapply(Colony18TubePreHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

Colony18TubeHCDistFinal <- full_join(Colony18TubeAggnHCDistFinal, Colony18TubePreHCDistFinal)

##Value of efficiency of the real network
shortest_path_1<-distances(Colony18TubePreNetworkUnweighted, v = V(Colony18TubePreNetworkUnweighted), to = V(Colony18TubePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony18TubePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 18, Nest = "Tube", Trial = "Pre")

#CIRCLE NEST
##AGGRESSION

#Weighted
Colony18CircleAggnNetwork

#Unweighted
Colony18CircleAggnNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony18CircleAggnNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony18CircleAggnNetworkUnweighted)

#Network degree
d<-degree(Colony18CircleAggnNetworkUnweighted)

#Network characteristics
E(Colony18CircleAggnNetworkUnweighted)$width <- log(E(Colony18CircleAggnNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony18CircleAggnNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony18CircleAggnNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony18CircleAggnNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony18CircleAggnNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony18CircleAggnNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony18CircleAggnNetworkUnweighted,dim = 3, weights=NULL)
Colony18CircleAggnNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony18CircleAggnNetworkUnweighted)$width)
shortest_path_net<-distances(Colony18CircleAggnNetworkUnweighted, v = V(Colony18CircleAggnNetworkUnweighted), to = V(Colony18CircleAggnNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

#Harmonic centrality 
HC <- harmonic_centrality(Colony18CircleAggnNetworkUnweighted, mode=c("all") )

Colony18CircleAggnHC <- as.data.frame(cbind(HC))

Colony18CircleAggnHC <- rownames_to_column(Colony18CircleAggnHC, "ID") 

Colony18CircleAggnDist1 <- Colony18CircleAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony18CircleAggnDist1$ID <- interaction( "id", Colony18CircleAggnDist1$ID, sep = "_")

Colony18CircleAggnHCDist<-left_join(Colony18CircleAggnHC,Colony18CircleAggnDist1)%>%drop_na()

Colony18CircleAggnHCDistFinal <- as.data.frame(lapply(Colony18CircleAggnHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

##Value of efficiency of the real network
shortest_path_1<-distances(Colony18CircleAggnNetworkUnweighted, v = V(Colony18CircleAggnNetworkUnweighted), to = V(Colony18CircleAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony18CircleAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 18, Nest = "Circle", Trial = "Aggn")

##BASELINE

#Weighted
Colony18CirclePreNetwork

#Unweighted
Colony18CirclePreNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony18CirclePreNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony18CirclePreNetworkUnweighted)

#Network degree
d<-degree(Colony18CirclePreNetworkUnweighted)

#Network characteristics
E(Colony18CirclePreNetworkUnweighted)$width <- log(E(Colony18CirclePreNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony18CirclePreNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony18CirclePreNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony18CirclePreNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony18CirclePreNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony18CirclePreNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony18CirclePreNetworkUnweighted,dim = 3, weights=NULL)
Colony18CirclePreNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony18CirclePreNetworkUnweighted)$width)
shortest_path_net<-distances(Colony18CirclePreNetworkUnweighted, v = V(Colony18CirclePreNetworkUnweighted), to = V(Colony18CirclePreNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

HC <- harmonic_centrality(Colony18CirclePreNetworkUnweighted, mode=c("all") )

Colony18CirclePreHC <- as.data.frame(cbind(HC))

Colony18CirclePreHC <- rownames_to_column(Colony18CirclePreHC, "ID") 

Colony18CirclePreDist1 <- Colony18CirclePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony18CirclePreDist1$ID <- interaction( "id", Colony18CirclePreDist1$ID, sep = "_")

Colony18CirclePreHCDist<-left_join(Colony18CirclePreHC,Colony18CirclePreDist1)%>%drop_na()

Colony18CirclePreHCDistFinal <- as.data.frame(lapply(Colony18CirclePreHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

Colony18CircleHCDistFinal <- full_join(Colony18CircleAggnHCDistFinal, Colony18CirclePreHCDistFinal)

##Value of efficiency of the real network
shortest_path_1<-distances(Colony18CirclePreNetworkUnweighted, v = V(Colony18CirclePreNetworkUnweighted), to = V(Colony18CirclePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony18CirclePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 18, Nest = "Circle", Trial = "Pre")

#FULL DATASET
#Harmonic centrality
Colony18HCDistFinal <- full_join(Colony18TubeHCDistFinal, Colony18CircleHCDistFinal)

#Network efficiency
Colony18NetEfficiency <- full_join(Colony18TubeAggnEfficiency, Colony18TubePreEfficiency) %>%
  full_join(Colony18CircleAggnEfficiency) %>%
  full_join(Colony18CirclePreEfficiency)

##COLONY 20
#TUBE NEST
##AGGRESSION

#Weighted
Colony20TubeAggnNetwork

#Unweighted
Colony20TubeAggnNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony20TubeAggnNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony20TubeAggnNetworkUnweighted)

#Network degree
d<-degree(Colony20TubeAggnNetworkUnweighted)

#Network characteristics
E(Colony20TubeAggnNetworkUnweighted)$width <- log(E(Colony20TubeAggnNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony20TubeAggnNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony20TubeAggnNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony20TubeAggnNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony20TubeAggnNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony20TubeAggnNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony20TubeAggnNetworkUnweighted,dim = 3, weights=NULL)
Colony20TubeAggnNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony20TubeAggnNetworkUnweighted)$width)
shortest_path_net<-distances(Colony20TubeAggnNetworkUnweighted, v = V(Colony20TubeAggnNetworkUnweighted), to = V(Colony20TubeAggnNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

#Harmonic centrality 
HC <- harmonic_centrality(Colony20TubeAggnNetworkUnweighted, mode=c("all") )

Colony20TubeAggnHC <- as.data.frame(cbind(HC))

Colony20TubeAggnHC <- rownames_to_column(Colony20TubeAggnHC, "ID") 

Colony20TubeAggnDist1 <- Colony20TubeAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony20TubeAggnDist1$ID <- interaction( "id", Colony20TubeAggnDist1$ID, sep = "_")

Colony20TubeAggnHCDist<-left_join(Colony20TubeAggnHC,Colony20TubeAggnDist1)%>%drop_na()

Colony20TubeAggnHCDistFinal <- as.data.frame(lapply(Colony20TubeAggnHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

##Value of efficiency of the real network
shortest_path_1<-distances(Colony20TubeAggnNetworkUnweighted, v = V(Colony20TubeAggnNetworkUnweighted), to = V(Colony20TubeAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony20TubeAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 20, Nest = "Tube", Trial = "Aggn")

##BASELINE

#Weighted
Colony20TubePreNetwork

#Unweighted
Colony20TubePreNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony20TubePreNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony20TubePreNetworkUnweighted)

#Network degree
d<-degree(Colony20TubePreNetworkUnweighted)

#Network characteristics
E(Colony20TubePreNetworkUnweighted)$width <- log(E(Colony20TubePreNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony20TubePreNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony20TubePreNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony20TubePreNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony20TubePreNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony20TubePreNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony20TubePreNetworkUnweighted,dim = 3, weights=NULL)
Colony20TubePreNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony20TubePreNetworkUnweighted)$width)
shortest_path_net<-distances(Colony20TubePreNetworkUnweighted, v = V(Colony20TubePreNetworkUnweighted), to = V(Colony20TubePreNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

HC <- harmonic_centrality(Colony20TubePreNetworkUnweighted, mode=c("all") )

Colony20TubePreHC <- as.data.frame(cbind(HC))

Colony20TubePreHC <- rownames_to_column(Colony20TubePreHC, "ID") 

Colony20TubePreDist1 <- Colony20TubePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony20TubePreDist1$ID <- interaction( "id", Colony20TubePreDist1$ID, sep = "_")

Colony20TubePreHCDist<-left_join(Colony20TubePreHC,Colony20TubePreDist1)%>%drop_na()

Colony20TubePreHCDistFinal <- as.data.frame(lapply(Colony20TubePreHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

Colony20TubeHCDistFinal <- full_join(Colony20TubeAggnHCDistFinal, Colony20TubePreHCDistFinal)

##Value of efficiency of the real network
shortest_path_1<-distances(Colony20TubePreNetworkUnweighted, v = V(Colony20TubePreNetworkUnweighted), to = V(Colony20TubePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony20TubePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 20, Nest = "Tube", Trial = "Pre")

#CIRCLE NEST
##AGGRESSION

#Weighted
Colony20CircleAggnNetwork

#Unweighted
Colony20CircleAggnNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony20CircleAggnNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony20CircleAggnNetworkUnweighted)

#Network degree
d<-degree(Colony20CircleAggnNetworkUnweighted)

#Network characteristics
E(Colony20CircleAggnNetworkUnweighted)$width <- log(E(Colony20CircleAggnNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony20CircleAggnNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony20CircleAggnNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony20CircleAggnNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony20CircleAggnNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony20CircleAggnNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony20CircleAggnNetworkUnweighted,dim = 3, weights=NULL)
Colony20CircleAggnNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony20CircleAggnNetworkUnweighted)$width)
shortest_path_net<-distances(Colony20CircleAggnNetworkUnweighted, v = V(Colony20CircleAggnNetworkUnweighted), to = V(Colony20CircleAggnNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

#Harmonic centrality 
HC <- harmonic_centrality(Colony20CircleAggnNetworkUnweighted, mode=c("all") )

Colony20CircleAggnHC <- as.data.frame(cbind(HC))

Colony20CircleAggnHC <- rownames_to_column(Colony20CircleAggnHC, "ID") 

Colony20CircleAggnDist1 <- Colony20CircleAggnDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony20CircleAggnDist1$ID <- interaction( "id", Colony20CircleAggnDist1$ID, sep = "_")

Colony20CircleAggnHCDist<-left_join(Colony20CircleAggnHC,Colony20CircleAggnDist1)%>%drop_na()

Colony20CircleAggnHCDistFinal <- as.data.frame(lapply(Colony20CircleAggnHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

##Value of efficiency of the real network
shortest_path_1<-distances(Colony20CircleAggnNetworkUnweighted, v = V(Colony20CircleAggnNetworkUnweighted), to = V(Colony20CircleAggnNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony20CircleAggnEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 20, Nest = "Circle", Trial = "Aggn")

##BASELINE

#Weighted
Colony20CirclePreNetwork

#Unweighted
Colony20CirclePreNetworkUnweighted

#Number of edges in the matrix
m<-gorder(Colony20CirclePreNetworkUnweighted)

#Number of nodes in the matrix
n<-gsize(Colony20CirclePreNetworkUnweighted)

#Network degree
d<-degree(Colony20CirclePreNetworkUnweighted)

#Network characteristics
E(Colony20CirclePreNetworkUnweighted)$width <- log(E(Colony20CirclePreNetworkUnweighted)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(Colony20CirclePreNetworkUnweighted)$arrow.size <- .2 #Change arrow size
E(Colony20CirclePreNetworkUnweighted)$edge.color <- "gray80" #Change arrow color
V(Colony20CirclePreNetworkUnweighted)$vertex.color<-'blue' #Change nodes colour
V(Colony20CirclePreNetworkUnweighted)$label=d #Name of the nodes is the same as the number of links
V(Colony20CirclePreNetworkUnweighted)$size=d
coords<-layout_with_fr(Colony20CirclePreNetworkUnweighted,dim = 3, weights=NULL)
Colony20CirclePreNetworkUnweighted$layout<-coords

#Diameter
dist_net<-1/(E(Colony20CirclePreNetworkUnweighted)$width)
shortest_path_net<-distances(Colony20CirclePreNetworkUnweighted, v = V(Colony20CirclePreNetworkUnweighted), to = V(Colony20CirclePreNetworkUnweighted), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

HC <- harmonic_centrality(Colony20CirclePreNetworkUnweighted, mode=c("all") )

Colony20CirclePreHC <- as.data.frame(cbind(HC))

Colony20CirclePreHC <- rownames_to_column(Colony20CirclePreHC, "ID") 

Colony20CirclePreDist1 <- Colony20CirclePreDist %>% ungroup() %>% filter(Seconds < 0.05) %>% mutate(ID = as.character(ID)) %>%select(Colony, Nest, Trial, ScaledDist, ID, Seconds)

Colony20CirclePreDist1$ID <- interaction( "id", Colony20CirclePreDist1$ID, sep = "_")

Colony20CirclePreHCDist<-left_join(Colony20CirclePreHC,Colony20CirclePreDist1)%>%drop_na()

Colony20CirclePreHCDistFinal <- as.data.frame(lapply(Colony20CirclePreHCDist, unlist)) %>% mutate(ScaledHC = HC/(n-1))

Colony20CircleHCDistFinal <- full_join(Colony20CircleAggnHCDistFinal, Colony20CirclePreHCDistFinal)

##Value of efficiency of the real network
shortest_path_1<-distances(Colony20CirclePreNetworkUnweighted, v = V(Colony20CirclePreNetworkUnweighted), to = V(Colony20CirclePreNetworkUnweighted), mode = "all",  algorithm = "dijkstra")

a<-1/shortest_path_1

diag(a)=NA

Efficiency<-((sum(a, na.rm = TRUE)*(1/(n*(n-1))))/((n*(n-1)))/2)

Colony20CirclePreEfficiency <- as.data.frame(Efficiency) %>% mutate(Colony = 20, Nest = "Circle", Trial = "Pre")

#FULL DATASET
#Harmonic centrality
Colony20HCDistFinal <- full_join(Colony20TubeHCDistFinal, Colony20CircleHCDistFinal)

#Network efficiency
Colony20NetEfficiency <- full_join(Colony20TubeAggnEfficiency, Colony20TubePreEfficiency) %>%
  full_join(Colony20CircleAggnEfficiency) %>%
  full_join(Colony20CirclePreEfficiency)

#FINAL COMBINED DATASETS  
#Harmonic centrality
FullHCDistFinal <- full_join(Colony5HCDistFinal, Colony6HCDistFinal) %>%
  full_join(Colony7HCDistFinal) %>%
  full_join(Colony8HCDistFinal) %>%
  full_join(Colony9HCDistFinal) %>%
  full_join(Colony11HCDistFinal) %>%
  full_join(Colony13HCDistFinal) %>%
  full_join(Colony17HCDistFinal) %>%
  full_join(Colony18HCDistFinal) %>%
  full_join(Colony20HCDistFinal) 

#Network efficiency
FullNetEfficiencyFinal <- full_join(Colony5NetEfficiency, Colony6NetEfficiency) %>%
  full_join(Colony7NetEfficiency) %>%
  full_join(Colony8NetEfficiency) %>%
  full_join(Colony9NetEfficiency) %>%
  full_join(Colony11NetEfficiency) %>%
  full_join(Colony13NetEfficiency) %>%
  full_join(Colony17NetEfficiency) %>%
  full_join(Colony18NetEfficiency) %>%
  full_join(Colony20NetEfficiency) 

#ANALYSIS
summary(lm(HC ~ ScaledDist*Nest*Trial + Colony, data = FullHCDistFinal))
FullHCDistFinal
#ANALYSIS
summary(lm(log(Efficiency) ~ Nest * Trial + Colony, data = FullNetEfficiencyFinal))


#PLOTS
#Function to create large points in a geom_point legend
large_points <- function(data, params, size) {
  # Multiply by some number, it doesn't matter what value, but larger numbers = large sized points in the legend
  data$size <- data$size * 2.5
  draw_key_point(data = data, params = params, size = size)
}

#Harmonic centrality
TubeAggnHC<-ggplot(data=FullHCDistFinal %>% filter(Nest == "Tube" & Trial == "Aggn"),
                    aes(x=ScaledDist, y=ScaledHC)) +
  ggtitle("Tube Aggn")+
  geom_point(key_glyph = large_points,size=2.5,alpha=0.75,color="red") +
  geom_smooth(method='lm',se=FALSE,size=1.5,color="black")+
  theme_pubclean()+
  theme(axis.ticks = element_blank(),
        axis.text.y = element_text(size= 18,color="white",family="Arial"),
        axis.text.x = element_text(size = 18, color = "black",family="Arial"),
        axis.title = element_blank(),
        plot.title=element_text(size=20,family="Arial", face = "bold"),
        legend.key=element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  ylim(0, 0.33) + 
  xlim(0, 1)
  
TubePreHC<-ggplot(data=FullHCDistFinal %>% filter(Nest == "Tube" & Trial == "Pre"),
                     aes(x=ScaledDist, y=ScaledHC)) +
    ggtitle("Tube Baseline")+
    geom_point(key_glyph = large_points,size=2.5,alpha=0.75,color="red") +
    geom_smooth(method='lm',se=FALSE,size=1.5,color="black")+
    theme_pubclean()+
    theme(axis.ticks = element_blank(),
          axis.text.y = element_text(size= 18,color="white",family="Arial"),
          axis.text.x = element_text(size = 18, color = "black",family="Arial"),
          axis.title = element_blank(),
          plot.title=element_text(size=20,family="Arial", face = "bold"),
          legend.key=element_blank()) +
    xlab(NULL) +
    ylab(NULL) +
  ylim(0, 0.33) + 
  xlim(0, 1)

CircleAggnHC<-ggplot(data=FullHCDistFinal %>% filter(Nest == "Circle" & Trial == "Aggn"),
                     aes(x=ScaledDist, y=ScaledHC)) +
    ggtitle("Circle Aggn")+
    geom_point(key_glyph = large_points,size=2.5,alpha=0.75,color="blue") +
    geom_smooth(method='lm',se=FALSE,size=1.5,color="black")+
    theme_pubclean()+
    theme(axis.ticks = element_blank(),
          axis.text.y = element_text(size= 18,color="black",family="Arial"),
          axis.text.x = element_text(size = 18, color = "black",family="Arial"),
          axis.title = element_blank(),
          plot.title=element_text(size=20,family="Arial", face = "bold"),
          legend.key=element_blank()) +
    xlab(NULL) +
    ylab(NULL) +
  ylim(0, 0.33) + 
  xlim(0, 1)
  
CirclePreHC<-ggplot(data=FullHCDistFinal %>% filter(Nest == "Circle" & Trial == "Pre"),
                    aes(x=ScaledDist, y=ScaledHC)) +
    ggtitle("Circle Baseline")+
    geom_point(key_glyph = large_points,size=2.5,alpha=0.75,color="blue") +
    geom_smooth(method='lm',se=FALSE,size=1.5,color="black")+
    theme_pubclean()+
    theme(axis.ticks = element_blank(),
          axis.text.y = element_text(size= 18,color="black",family="Arial"),
          axis.text.x = element_text(size = 18, color = "black",family="Arial"),
          axis.title = element_blank(),
          plot.title=element_text(size=20,family="Arial", face = "bold"),
          legend.key=element_blank()) +
    xlab(NULL) +
    ylab(NULL) +
  ylim(0, 0.33) + 
  xlim(0, 1)
  
TestHCDistPlot<-ggarrange(CirclePreHC, TubePreHC,
                          CircleAggnHC, TubeAggnHC,
                        labels = c("A", "B", "C", "D"),
                        font.label = list(size = 24,
                                          family="Arial"),
                        ncol = 2, nrow = 2)

annotate_figure(TestHCDistPlot,
                top = NULL,
                bottom = text_grob("Worker scaled dist entrance, frame 1", color = "black",
                                   size = 26, x = 0.53,family="Arial"),
                left = text_grob("Harmonic centrality", color = "black",
                                 size = 26, rot = 90,family="Arial"),
                right = NULL
)

pacman::p_load(MASS, scales)
#Network efficiency
NetworkEfficiencyPre<-ggplot(data=FullNetEfficiencyFinal%>%filter(Trial=="Pre"),aes(y=Efficiency,x=Nest,fill=Nest))+
  stat_boxplot_custom(qs=c(0, 0.25, 0.5, 0.75, 1.00), alpha=0.65) +
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Baseline")+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=18,family="Arial", color = "black"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size=18,family="Arial", color = "black"),
        legend.key=element_blank(),
        legend.position = "none",
        legend.justification = c(1,1),
        legend.text=element_text(size = 18,family="Arial"),
        legend.title=element_text(size=18,family="Arial"),
        plot.title=element_text(size=20,face="bold",family="Arial"))+
  scale_y_log10(breaks = c(10^-13, 10^-12, 10^-11, 10^-10, 10^-9, 10^-8, 10^-7, 10^-6, 10^-5, 10^-4, 10^-3),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^-13, 10^-3)) +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name="Nest",
                    values=c("blue", "red")) + 
  annotation_logticks(sides = "l")  
NetworkEfficiencyPre

NetworkEfficiencyAggn<-ggplot(data=FullNetEfficiencyFinal%>%filter(Trial=="Aggn"),aes(y=Efficiency,x=Nest,fill=Nest))+
  stat_boxplot_custom(qs=c(0, 0.25, 0.5, 0.75, 1.00), alpha=0.65) +
  xlab(NULL)+
  ylab(NULL)+
  ggtitle("Aggn")+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=18,family="Arial", color = "black"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size=18,family="Arial", color = "white"),
        legend.key=element_blank(),
        legend.position = "none",
        legend.justification = c(1,1),
        legend.text=element_text(size = 18,family="Arial"),
        legend.title=element_text(size=18,family="Arial"),
        plot.title=element_text(size=20,face="bold",family="Arial"))+
  scale_y_log10(breaks = c(10^-13, 10^-12, 10^-11, 10^-10, 10^-9, 10^-8, 10^-7, 10^-6, 10^-5, 10^-4, 10^-3),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^-13, 10^-3)) +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name="Nest",
                    values=c("blue", "red")) 
NetworkEfficiencyAggn


NetEfficiency<-ggarrange(NetworkEfficiencyPre, NetworkEfficiencyAggn,
                         labels = c("A","B"),
                         ncol = 2, nrow = 1,
                         font.label = list(size = 24,
                                           family="Arial"))

annotate_figure(NetEfficiency,
                top = NULL,
                bottom = text_grob("Nest shape", color = "black",
                                   size = 26, x = 0.53,family="Arial"),
                left = text_grob("Global network efficiency", color = "black",
                                 size = 26, rot = 90,family="Arial"),
                right = NULL
)


plot(TestHCDistComb$ScaledDist,TestHCDistComb$HarmCent)
abline(lm(TestHCDistComb$HarmCent~TestHCDistComb$ScaledDist))

s<-d[d==0] #The nodes with a number of links equal to 0 
o<-d[d<2] #Nodes with 0 or 1 link
length(s) #Number of nodes that have 0 link
length(o) #Number of nodes that have 0 or 1 link
(length(s)*100)/length(d) #Proportion of individuals in the nest that has 0 link
(length(o)*100)/length(d) #Proportion of individuals in the nest that has 1 link maximum

# Efficiency
##Value of efficiency of the real network
dist<-1/(trylinks$weight) 
shortest_path_1<-distances(Network, v = V(Network), to = V(Network), mode = "all", weights =dist, algorithm = "dijkstra")
a<-1/shortest_path_1
diag(a)=NA
real_value<-((sum(a ,na.rm = T))/(nrow(trynodes)*((nrow(trynodes)-1)))/2)

#Creation of the layouts for the network
plot(Network)
l <- layout_with_graphopt(Network, charge=0.5)
plot(Network, edge.color="darkred", edge.arrow.size=.4, main='Colony 5 Circle Baseline', layout=l, vertex.label=NA)
#vertex.color= d stands for the fact that the color of the node depends on the numers of links of it 

#Degree
deg_dist<-degree_distribution(Network, normalized=F) #To have the distribution of connection for the network
plot(deg_dist, type = 'l') #Plot of this connections distribution
hist(d)
deg <- degree(Network, mode="all") #This is the probability of each degree to appear in the network
deg.dist <- degree_distribution(Network, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange",xlab="Degree", ylab="Cumulative Frequency", type='l')

#Reciprocity
r<-reciprocity(Network)

#Net1 is the largest cluster of the network
cl <- components(Network) #Largest component of the graph
net1<-induced_subgraph(Network,which(cl$membership == which.max(cl$csize)))#give the cluster id of each vertex (membership), the size of 
# each cluster (csize) and the number of cluster (no)

coords<-layout_with_fr(net1,dim = 3)
net1$layout<-coords
plot(net1, vertex.shape='sphere',edge.color="darkred", vertex.size=2, main='Colony17_Tube')
(length(degree(net1))/length(degree(net)))*100 #Proportion fo individuals involved in the larger cluster
(sum(E(net1)$width)/sum(E(net)$width))*100 #Proportion of weight of the cluster 
dist_net1<-1/(E(net1)$width)
shortest_path_net1<-distances(net1, v = V(net1), to = V(net1), mode = "all", weights =dist_net1, algorithm = "dijkstra")
shortest_path_net1[is.infinite(shortest_path_net1)] = NA
b<-length(degree(net1))
b
b*b
mean_length_net1<-(sum(shortest_path_net1/2,na.rm=T))/(b*b)#Mean Length
mean_length_net1
net_diam<-max(shortest_path_net1,na.rm=T) #Diameter of the network

#Transitivity
c<-transitivity(Network, type = "global") #clustering for undirected
hist(c)
plot(c)

#net1<-cl$membership[22] #w is the id of the cluster of the individuals 22 (to be change with the intruder)
trylinks
#Creat a random network
trial<-erdos.renyi.game(n, m, type = 'gnm') #Followng Erdos-Renyi model
weight_random<-sample(trylinks$weight,nrow(trylinks)) #generate same weight of links but random order 
edge.attributes(trial)$weight<-weight_random #Define the weight of edges same as the true network

i=1 #Counter for the while loop 
mat<-matrix(nrow = 1000, ncol = 4)
degree_distrib_random<-matrix(nrow=n, ncol=1001)
degree_distrib_random[,1]<-c(1:n)
while (i<1001) {
  trial<-erdos.renyi.game(n, m, type = 'gnm') #Model of random networks
  weight_random<-sample(E(Network)$weight,m) #Attribution of the same weight in the real and random networks
  edge.attributes(trial)$weight<-weight_random 
  dist_random=1/weight_random #Distance is 1/ of the duration of the communication 
  #Definition of the shortest path distances through 
  shortest_path<-distances(trial, v = V(trial), to = V(trial), mode = "all", weights = dist_random, algorithm = "dijkstra")
  g<-1/shortest_path #We divided here 1 by the path to have no Inf values
  diag(g)=NA #We remoe the path of length 0 of each pair of similar nodes (1,1 or 2,2 for instance)
  mat[i,1]<-i #Include the counting in a column
  mat[i,2] <- (sum(g ,na.rm = T))/(nrow(trynodes)*((nrow(trynodes)-1)))/2 #We divided 1 first to have a value between 0 and 1
# we divided at the end by 2 because the matrix is a reciprocal one 
  shortest_path[!is.finite(shortest_path)] <- NA
  mat[i,3]<- (sum(1/g ,na.rm = T)/2)/(nrow(trynodes)*((nrow(trynodes)-1)))
  mat[i,4]<-1/(sum(1/g, na.rm=T)/2)
#Degree distribution:
  degree_distrib_random[,i+1]<-degree(trial) #Have a look on what to do with that 
  i=i+1
}

mat <- mat[!is.infinite(rowSums(mat)),] #Removing the rows that shows infinite values 
colnames(mat)<- c('occurence','average_efficiency', 'Mean_length', 'Centrality')
mat<-as.data.frame(mat) # We need the matrix to be a dataframe for the histogramm 
hist(mat$average_efficiency)
mat

#Defining the upper and lower boundaries of the 95% values 
error<-qnorm(0.975)*sd(mat$average_efficiency)/sqrt(nrow(mat))
left<- mean(mat$average_efficiency)-error
right<-mean(mat$average_efficiency)+error

#Having the 95% of the values range:
qts <- quantile(mat$average_efficiency,probs=c(.025,.975))
trylinks

##Value of efficiency of the real network
dist<-1/(E(Network)$weight) #Same as the random network above
shortest_path_1<-distances(Network, v = V(Network), to = V(Network), mode = "all", weights =dist, algorithm = "dijkstra")
a<-1/shortest_path_1
diag(a)=NA
Efficiency<-((sum(a ,na.rm = T)*(1/(n*(n-1))))/(n*((n-1)))/2)
Efficiency

#Plotting
ggplot(Network, aes(x=average_efficiency))+ #lot with the 95% CI
  geom_histogram(color="black", fill="white",bins = 50)+
  geom_vline(aes(xintercept=mean(average_efficiency)),col='darkgreen')+ #Line of the mean on the histogram
  geom_vline(aes(xintercept=mean(average_efficiency)-error),col='red',linetype='dashed')+ #Lower boundary 95% CI
  geom_vline(aes(xintercept=mean(average_efficiency)+error),col='red',linetype='dashed')+ #Upper boundary 95% CI
  geom_vline(aes(xintercept=qts[1]),col='blue',linetype='dashed')+ #5% quantile
  geom_vline(aes(xintercept=qts[2]),col='blue',linetype='dashed') +#95% quantile
  geom_vline(aes(xintercept=real_value), col='black', linetype='dotted')+ #Real value of the network
  ggtitle(label = 'Colony 6 Circle')+
  theme_pubclean()
p 


#### Few more lines of codes to try, maybe to use 
plot(trial,edge.width=weight_random) # Random network plotting
deg_dist<-degree_distribution(net, normalized=F) #To have the distribution of connection for the random net
plot(deg_dist, type = 'l') #Plot of this connections distribution
deg_dist<-degree_distribution(trial, normalized=F)
plot(deg_dist, type = 'l')
hist(d)
deg <- degree(net, mode="all") #This is the probability of each degree to appear in the network
deg.dist <- degree_distribution(net, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange",xlab="Degree", ylab="Cumulative Frequency", type='l')
trial_deg <- degree(trial, mode="all") 
trial.dist <- degree_distribution(trial, cumulative=T, mode="all")
plot( x=0:max(trial_deg), y=1-trial.dist, pch=19, cex=1.2, col="orange",xlab="Degree", ylab="Cumulative Frequency", type='l')
deg <- degree(net1, mode="all") 
deg.dist <- degree_distribution(net1, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange",xlab="Degree", ylab="Cumulative Frequency", type='l')
nodes<-
connected<- 

install.packages(c("igraph","graphlayouts","ggraph","wesanderson"))
library(graphlayouts)
library(ggraph)
library(wesanderson)
library(igraph)
pal <- wes_palette("Zissou1", 100, type = "continuous")


ggraph(Network,layout = "stress",bbox = 15)+
  geom_edge_link(aes(edge_width = E(Network)$width),arrow = arrow(length = unit(4, 'mm'),type = "closed"), 
                 end_cap = circle(3, 'mm'),edge_colour = "grey66")+
  geom_node_point(aes(size = ),shape=21,fill="tomato")+
  scale_edge_width_continuous(range = c(0.2,2))+
  theme_graph()+
  theme(legend.position = "bottom")

ggraph(Network,layout = "stress",bbox = 15)+
  geom_edge_fan(aes(alpha = stat(index))) +
  guides(edge_alpha = guide_edge_direction())
  geom_edge_link(aes(edge_width = E(Network)$width), arrow = arrow(length = unit(4, 'mm'),type = "closed"), 
                 end_cap = circle(3, 'mm'),edge_colour = "grey66")+
  geom_node_point(aes(size = 3),shape=21,fill="tomato")+
  scale_edge_width_continuous(range = c(0.2,2))+
  theme_graph()+
  theme(legend.position = "bottom")
Aggn

Aggn
NetworkTubeBaseIndir <- Network
TubeBaseIndir <- ggraph(NetworkTubeBaseIndir,layout = "stress",bbox = 9)+
  geom_edge_link0(aes(edge_width = E(NetworkTubeBaseIndir)$width),edge_colour = "grey66")+
  geom_node_point(aes(fill = d,size = d),shape=21)+
  scale_edge_width_continuous("Weight",range = c(0.2,2))+
  scale_size("Degree",breaks=c(0,5,10,15,20),labels=c(0,5,10,15,20),range=c(1,5))+
  scale_fill_gradientn("Degree",colours = pal,breaks=c(0,5,10,15,20),labels=c(0,5,10,15,20)) +
  ggtitle("Baseline indirect network") +
  theme_graph()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(0.25, 'cm')) +
  guides(edge_width = guide_legend(order = 1),
         fill = guide_legend(order = 2),
         size = guide_legend(order = 2))

NetworkTubeBaseDir <- Network
TubeBaseDir <- ggraph(Network,layout = "stress",bbox = 9)+
  geom_edge_link0(aes(edge_width = E(Network)$width),edge_colour = "grey66")+
  geom_node_point(aes(fill = d,size = d),shape=21)+
  scale_edge_width_continuous("Weight",range = c(0.2,2))+
  scale_size("Degree",breaks=c(0,5,10,15,20),labels=c(0,5,10,15,20),range=c(1,5))+
  scale_fill_gradientn("Degree",colours = pal,breaks=c(0,5,10,15,20),labels=c(0,5,10,15,20)) +
  ggtitle("Baseline direct network") +
  theme_graph()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(0.25, 'cm')) +
  guides(edge_width = guide_legend(order = 1),
         fill = guide_legend(order = 2),
         size = guide_legend(order = 2))

NetworkCircleBaseIndir <- Network
CircleBaseIndir <- ggraph(NetworkCircleBaseIndir,layout = "stress",bbox = 9)+
  geom_edge_link0(aes(edge_width = E(NetworkCircleBaseIndir)$width),edge_colour = "grey66")+
  geom_node_point(aes(fill = d,size = d),shape=21)+
  scale_edge_width_continuous("Weight",range = c(0.2,2))+
  scale_size("Degree",breaks=c(0,5,10,15,20),labels=c(0,5,10,15,20),range=c(1,5))+
  scale_fill_gradientn("Degree",colours = pal,breaks=c(0,5,10,15,20),labels=c(0,5,10,15,20)) +
  ggtitle("Circle baseline indirect network") +
  theme_graph()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(0.25, 'cm')) +
  guides(edge_width = guide_legend(order = 1),
         fill = guide_legend(order = 2),
         size = guide_legend(order = 2))

NetworkCircleBaseDir <- Network
CircleBaseDir <- ggraph(NetworkCircleBaseDir,layout = "stress",bbox = 9)+
  geom_edge_link0(aes(edge_width = E(NetworkCircleBaseDir)$width),edge_colour = "grey66")+
  geom_node_point(aes(fill = d,size = d),shape=21)+
  scale_edge_width_continuous("Weight",range = c(0.2,2))+
  scale_size("Degree",breaks=c(0,5,10,15,20),labels=c(0,5,10,15,20),range=c(1,5))+
  scale_fill_gradientn("Degree",colours = pal,breaks=c(0,5,10,15,20),labels=c(0,5,10,15,20)) +
  ggtitle("Circle baseline direct network") +
  theme_graph()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(0.25, 'cm')) +
  guides(edge_width = guide_legend(order = 1),
         fill = guide_legend(order = 2),
         size = guide_legend(order = 2))

NetworkTubeAggnIndir <- Network
TubeAggnIndir <- ggraph(NetworkTubeAggnIndir,layout = "stress",bbox = 9)+
  geom_edge_link0(aes(edge_width = E(NetworkTubeAggnIndir)$width),edge_colour = "grey66")+
  geom_node_point(aes(fill = d,size = d),shape=21)+
  scale_edge_width_continuous("Weight",range = c(0.2,2))+
  scale_size("Degree",breaks=c(0,5,10,15,20),labels=c(0,5,10,15,20),range=c(1,5))+
  scale_fill_gradientn("Degree",colours = pal,breaks=c(0,5,10,15,20),labels=c(0,5,10,15,20)) +
  ggtitle("Tube aggn assay indirect network") +
  theme_graph()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(0.25, 'cm')) +
  guides(edge_width = guide_legend(order = 1),
         fill = guide_legend(order = 2),
         size = guide_legend(order = 2))

TubeAggnDir <- ggraph(Network,layout = "stress",bbox = 9)+
  geom_edge_link0(aes(edge_width = E(Network)$width),edge_colour = "grey66")+
  geom_node_point(aes(fill = d,size = d),shape=21)+
  scale_edge_width_continuous("Weight",range = c(0.2,2))+
  scale_size("Degree",breaks=c(0,5,10,15,20),labels=c(0,5,10,15,20),range=c(1,5))+
  scale_fill_gradientn("Degree",colours = pal,breaks=c(0,5,10,15,20),labels=c(0,5,10,15,20)) +
  ggtitle("Tube aggn assay direct network") +
  theme_graph()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(0.25, 'cm')) +
  guides(edge_width = guide_legend(order = 1),
         fill = guide_legend(order = 2),
         size = guide_legend(order = 2))

CircleAggnIndir <- ggraph(Network,layout = "stress",bbox = 9)+
  geom_edge_link0(aes(edge_width = E(Network)$width),edge_colour = "grey66")+
  geom_node_point(aes(fill = d,size = d),shape=21)+
  scale_edge_width_continuous("Weight",range = c(0.2,2))+
  scale_size("Degree",breaks=c(0,5,10,15,20),labels=c(0,5,10,15,20),range=c(1,5))+
  scale_fill_gradientn("Degree",colours = pal,breaks=c(0,5,10,15,20),labels=c(0,5,10,15,20)) +
  ggtitle("Circle baseline indirect network") +
  theme_graph()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(0.25, 'cm')) +
  guides(edge_width = guide_legend(order = 1),
         fill = guide_legend(order = 2),
         size = guide_legend(order = 2))

CircleAggnDir <- ggraph(Network,layout = "stress",bbox = 9)+
  geom_edge_link0(aes(edge_width = E(Network)$width),edge_colour = "grey66")+
  geom_node_point(aes(fill = d,size = d),shape=21)+
  scale_edge_width_continuous("Weight",range = c(0.2,2))+
  scale_size("Degree",breaks=c(0,5,10,15,20),labels=c(0,5,10,15,20),range=c(1,5))+
  scale_fill_gradientn("Degree",colours = pal,breaks=c(0,5,10,15,20),labels=c(0,5,10,15,20)) +
  ggtitle("Circle baseline direct network") +
  theme_graph()+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 18, color = "black"),
        legend.title = element_text(size = 18, color = "black"),
        legend.key.size = unit(0.25, 'cm')) +
  guides(edge_width = guide_legend(order = 1),
         fill = guide_legend(order = 2),
         size = guide_legend(order = 2))

TubeAggn <- ggraph(Network,layout = "lgl")+
  geom_edge_fan(aes(alpha = stat(index))) +
  guides(edge_alpha = guide_edge_direction())+
  geom_edge_link(aes(edge_width = E(Network)$width),
                  edge_colour = "grey66")+
  geom_node_point(aes(fill = d,size = d),shape=21)+
  scale_edge_width_continuous("Weight",range = c(0.1,1))+
  scale_size("Degree",breaks=c(5,10,20,30,40,50,60,70,80,90,100,120,140,160),labels=c(5,10,20,30,40,50,60,70,80,90,100,120,140,160),range=c(1,7))+
  scale_fill_gradientn("Degree",colours = pal,breaks=c(5,10,20,30,40,50,60,70,80,90,100,120,140,160),labels=c(5,10,20,30,40,50,60,70,80,90,100,120,140,160)) +
  theme_graph()+
  theme(legend.position = "bottom")+
  guides(fill = guide_colourbar(frame.colour = "black",
                                ticks.colour = "black"))+
  guides(edge_width = guide_legend(order = 1),
         fill = guide_legend(order = 2),
         size = guide_legend(order = 2))
max(d)
Pre
ggraph(Network,"centrality",cent = graph.strength(Network))+
  geom_edge_link0(edge_colour = "grey66")+
  geom_node_point(aes(fill = min(d)),size=1,shape=21)+
  geom_node_point(aes(fill = c,size = c),shape=21)+
  scale_edge_width_continuous("Weight",range = c(0.2,2))+
  scale_size("Clustering",breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06),labels=c(0,0.01,0.02,0.03,0.04,0.05,0.06),range=c(2,10))+
  scale_fill_gradientn(colours = pal,"Clustering",breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06),labels=c(0,0.01,0.02,0.03,0.04,0.05,0.06)) +
  theme_graph()+
  ggtitle("Colony 5 Circle, Baseline Assay")+
  theme(legend.position = "bottom")+
  guides(fill = guide_colourbar(frame.colour = "black",
                                ticks.colour = "black"))+
  guides(edge_width = guide_legend(order = 1),
         fill = guide_legend(order = 2),
         size = guide_legend(order = 2))


ggraph(Network,layout = "stress",bbox = 15)+
  geom_edge_link(arrow = arrow(length = unit(4, 'mm')), 
                 end_cap = circle(3, 'mm'),edge_colour = "grey66")+
  geom_node_point(aes(fill = d,size = d),shape=21)+
  scale_edge_width_continuous(range = c(0.2,3))+
  scale_size(range=c(2,5),guide = FALSE)+
  scale_fill+
  theme_graph()+
  theme(legend.position = "bottom")

geom_edge_link(arrow = arrow(length = unit(4, 'mm')), 
               end_cap = circle(3, 'mm')) + 

normalise <- function (x, from = range(x), to = c(0, 1)) {
  x <- (x - from[1])/(from[2] - from[1])
  if (!identical(to, c(0, 1))) {
    x <- x * (to[2] - to[1]) + to[1]
  }
  x
}

bb <- layout_as_backbone(net,keep = 0.4,backbone = TRUE)
E(net)$col <- FALSE
E(net)$col[bb$backbone] <- TRUE

V(net)$degree <- normalise(V(net)$degree, to = c(3, 11))

ggraph(net,"stress",bbox = 15)+
  geom_edge_link0(edge_colour = "grey66",edge_width = E(net)$width)+
  geom_node_point(shape = 21,size = d)+
  geom_node_text(aes(label = name,size = d),
                 family = "serif",repel = TRUE)+
  scale_fill_manual(values=c("F" = "#EEB422","M" = "#424242","grey66"))+
  scale_size(range=c(2,5),guide = FALSE)+
  theme_graph()+
  theme(legend.position = "bottom")

eqarrowPlot <- function(graph, layout, edge.lty=rep(1, ecount(graph)),
                        edge.arrow.size=rep(1, ecount(graph)),
                        vertex.shape="circle",
                        edge.curved=autocurve.edges(graph), ...) {
  plot(graph, edge.lty=0, edge.arrow.size=0, layout=layout,
       vertex.shape="none")
  for (e in seq_len(ecount(graph))) {
    graph2 <- delete.edges(graph, E(graph)[(1:ecount(graph))[-e]])
    plot(graph2, edge.lty=edge.lty[e], edge.arrow.size=edge.arrow.size[e],
         edge.curved=edge.curved[e], layout=layout, vertex.shape="none",
         vertex.label=NA, add=TRUE, ...)
  }
  plot(graph, edge.lty=0, edge.arrow.size=0, layout=layout,
       vertex.shape=vertex.shape, add=TRUE, ...)
  invisible(NULL)
}
    