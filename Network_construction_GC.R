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
library(reshape2)
library(tidyverse)
library(igraph)
library(igraph)
library(ggraph)
library(graphlayouts)
library(matrixStats)
library(CINNA)
install.packages("matrixStats")
install.packages("ggraph")
install.packages("graphlayouts")
install.packages("CINNA")
###### 
#FIRST PART: Transformation
setwd("~/Desktop/Work Arizona/Datasets Alann Stage")
view(fakematrix)
fakematrix
fakematrix<-read.csv("Col7CircleRmatrix",sep=',',h=T) #Introduction of the dataset 
fakematrix2 <- fakematrix[,-1] #These 2 lines are for setting up the first column as row names
rownames(fakematrix) <- fakematrix[,1]
fakematrix[upper.tri(fakematrix)] <- 0 #If the matrix is a reciprocal one we will have 2 links per combination
fakematrix[upper.tri(R)] <- 0 #same here

#Number of edges in the matrix
m<-length(fakematrix[fakematrix>0])

fakematrix
###For the nodes dataset it's pretty easy because we just need to take all id as the dataset
###Aggression
trynodes<-data.frame(c(1:length(Col7CircleRmatrix))) #The nodes dataset is just all the ID's
###Pre
trynodes<-data.frame(c(1:length(Col7_5Rmatrix))) #The nodes dataset is just all the ID's
colnames(trynodes)<-'id' #We give the name of the column as id 
trynodes
###For the links dataset
trylinks<-melt(Col7_5Rmatrix,id=c("id"))
colnames(trylinks) <- c('from','to','weight') #Change column names
b = str_replace_all(trylinks$to, "X", "") # Remove the X in the second column 
trylinks$to=b #Include the new vector into the dataset
trylinks<-subset(trylinks, weight!=0)

#######
#SECOND PART: Creation of the network

##Introduction of our nodes and links
nodes <- trynodes
links <- trylinks

#Number of nodes in the matrix
n<-max(nodes)
n
links
##Quick look on the data
head(nodes)
head(links)
nrow(nodes)#; length(unique(nodes$id))
nrow(links)#; nrow(unique(links[,c("from", "to")]))

## Collapse multiple links of the same type between the same two nodes
# by summing their weights, using aggregate() by "from", "to", & "type":
links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),] #Put that in order for the colomn from 
colnames(links)[3] <- "weight" #Name the third column as weight
rownames(links) <- NULL

net <- graph.data.frame(links, nodes, directed=F) #I choose here to not directed the network
net1 <- graph.data.frame(links, nodes, directed=F) #I choose here to not directed the network

#Non-reciprocal 
Conn<-n/m #Connectedness (non-reciprocal)
Conn<-(2*n)/m #Connectedness (reciprocal)
d<-degree(net) #Allow us to detect the number of links per nodes/ degree

c<-transitivity(net, type = "localundirected") #clustering 
hist(c)
plot(c)

E(net)$width <- log(E(net)$weight) #To plot the widht of arrows according to their weight (of the relation)
E(net)$arrow.size <- .2 #Change arrow size
E(net)$edge.color <- "gray80" #Change arrow color
V(net)$vertex.color<-'blue' #Change nodes colour
V(net)$label=d #Name of the nodes is the same as the number of links
V(net)$size=d
coords<-layout_with_fr(net,dim = 3, weights=NULL)
net$layout<-coords
#Creation of the layouts for the network
plot(net)
l <- layout_with_graphopt(net, charge=0.5)
plot(net, edge.color="darkred", edge.arrow.size=.4, main='Colony7 Circle', layout=l, vertex.label=NA)
#vertex.color= d stands for the fact that the color of the node depends on the numers of links of it 

#Degree
deg_dist<-degree_distribution(net, normalized=F) #To have the distribution of connection for the network
plot(deg_dist, type = 'l') #Plot of this connections distribution
hist(d)
deg <- degree(net, mode="all") #This is the probability of each degree to appear in the network
deg.dist <- degree_distribution(net, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange",xlab="Degree", ylab="Cumulative Frequency", type='l')

#Reciprocity
r<-reciprocity(net)

#Diameter
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network
dist_net<-1/(E(net)$width)
shortest_path_net<-distances(net, v = V(net), to = V(net), mode = "all", weights =dist_net, algorithm = "dijkstra")
shortest_path_net[is.infinite(shortest_path_net)] = NA
net_diam<-max(shortest_path_net,na.rm=T) #Diameter of the network

#Harmonic centrality 
hc<-harmonic_centrality(net,mode=c("all"),weights = dist_net)
hist(hc)
plot(hc,type= 'l')

s<-d[d==0] #The nodes with a number of links equal to 0 
o<-d[d<2] #Nodes with 0 or 1 link
length(s) #Number of nodes that have 0 link
length(o) #Number of nodes that have 0 or 1 link
(length(s)*100)/length(d) #Proportion of individuals in the nest that has 0 link
(length(o)*100)/length(d) #Proportion of individuals in the nest that has 1 link maximum

# Efficiency
##Value of efficiency of the real network
dist<-1/(trylinks$weight) 
shortest_path_1<-distances(net, v = V(net), to = V(net), mode = "all", weights =dist, algorithm = "dijkstra")
a<-1/shortest_path_1
diag(a)=NA
real_value<-((sum(a ,na.rm = T))/(nrow(trynodes)*((nrow(trynodes)-1)))/2)


#Net1 is the largest cluster of the network
cl <- components(net) #Largest component of the graph
net1<-induced_subgraph(net,which(cl$membership == which.max(cl$csize)))#give the cluster id of each vertex (membership), the size of 
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

#net1<-cl$membership[22] #w is the id of the cluster of the individuals 22 (to be change with the intruder)

#Creat a random network
trial<-erdos.renyi.game(nrow(trynodes), nrow(trylinks), type = 'gnm') #Followng Erdos-Renyi model
weight_random<-sample(trylinks$weight,nrow(trylinks)) #generate same weight of links but random order 
edge.attributes(trial)$weight<-weight_random #Define the weight of edges same as the true network

i=1 #Counter for the while loop 
mat<-matrix(nrow = 1000, ncol = 4)
degree_distrib_random<-matrix(nrow=nrow(trynodes), ncol=1001)
degree_distrib_random[,1]<-c(1:nrow(trynodes))
while (i<1001) {
  trial<-erdos.renyi.game(nrow(trynodes), nrow(trylinks), type = 'gnm') #Model of random networks
  weight_random<-sample(trylinks$weight,nrow(trylinks)) #Attribution of the same weight in the real and random networks
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

#Defining the upper and lower boundaries of the 95% values 
error<-qnorm(0.975)*sd(mat$average_efficiency)/sqrt(nrow(mat))
left<- mean(mat$average_efficiency)-error
right<-mean(mat$average_efficiency)+error

#Having the 95% of the values range:
qts <- quantile(mat$average_efficiency,probs=c(.025,.975))

##Value of efficiency of the real network
dist<-1/(trylinks$weight) #Same as the random network above
shortest_path_1<-distances(net, v = V(net), to = V(net), mode = "all", weights =dist, algorithm = "dijkstra")
a<-1/shortest_path_1
diag(a)=NA
real_value<-((sum(a ,na.rm = T))/(nrow(trynodes)*((nrow(trynodes)-1)))/2)
real_value


#Plotting
ggplot(mat, aes(x=average_efficiency))+ #lot with the 95% CI
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


ggraph(net,layout = "stress",bbox = 15)+
  geom_edge_link(aes(edge_width = E(net)$width),arrow = arrow(length = unit(4, 'mm'),type = "closed"), 
                 end_cap = circle(3, 'mm'),edge_colour = "grey66")+
  geom_node_point(aes(size = ),shape=21,fill="tomato")+
  scale_edge_width_continuous(range = c(0.2,2))+
  theme_graph()+
  theme(legend.position = "bottom")

ggraph(net,layout = "stress",bbox = 15)+
  geom_edge_link(aes(edge_width = E(net)$width),arrow = arrow(length = unit(4, 'mm'),type = "closed"), 
                 end_cap = circle(3, 'mm'),edge_colour = "grey66")+
  geom_node_point(aes(size = 3),shape=21,fill="tomato")+
  scale_edge_width_continuous(range = c(0.2,2))+
  theme_graph()+
  theme(legend.position = "bottom")
Aggn
Aggn<-ggraph(net,layout = "stress",bbox = 9)+
  geom_edge_link0(aes(edge_width = E(net)$width),edge_colour = "grey66")+
  geom_node_point(aes(fill = d,size = d),shape=21)+
  scale_edge_width_continuous("Weight",range = c(0.2,2))+
  scale_size("Degree",breaks=c(0,5,10,15,20),labels=c(0,5,10,15,20),range=c(1,5))+
  scale_fill_gradientn("Degree",colours = pal,breaks=c(0,5,10,15,20),labels=c(0,5,10,15,20)) +
  theme_graph()+
  ggtitle("Colony 7 Circle, Aggression Assay")+
  theme(legend.position = "bottom")+
  guides(fill = guide_colourbar(frame.colour = "black",
                                ticks.colour = "black"))+
  guides(edge_width = guide_legend(order = 1),
         fill = guide_legend(order = 2),
         size = guide_legend(order = 2))
Pre
Pre<-ggraph(net1,layout = "stress",bbox = 9)+
  geom_edge_link0(aes(edge_width = E(net)$width),edge_colour = "grey66")+
  geom_node_point(aes(fill = d,size = d),shape=21)+
  scale_edge_width_continuous("Weight",range = c(0.2,2))+
  scale_size("Degree",breaks=c(0,5,10,15,20),labels=c(0,5,10,15,20),range=c(1,5))+
  scale_fill_gradientn("Degree",colours = pal,breaks=c(0,5,10,15,20),labels=c(0,5,10,15,20)) +
  theme_graph()+
  ggtitle("Colony 7 Circle, Baseline Assay")+
  theme(legend.position = "bottom")+
  guides(fill = guide_colourbar(frame.colour = "black",
                                ticks.colour = "black"))+
  guides(edge_width = guide_legend(order = 1),
         fill = guide_legend(order = 2),
         size = guide_legend(order = 2))

ggraph(net,"centrality",cent = graph.strength(net))+
  geom_edge_link0(edge_colour = "grey66")+
  geom_node_point(aes(fill = min(d)),size=1,shape=21)+
  geom_node_point(aes(fill = c,size = c),shape=21)+
  scale_edge_width_continuous("Weight",range = c(0.2,2))+
  scale_size("Clustering",breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06),labels=c(0,0.01,0.02,0.03,0.04,0.05,0.06),range=c(2,10))+
  scale_fill_gradientn(colours = pal,"Clustering",breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06),labels=c(0,0.01,0.02,0.03,0.04,0.05,0.06)) +
  theme_graph()+
  ggtitle("Colony 7 Circle, Aggression Assay")+
  theme(legend.position = "bottom")+
  guides(fill = guide_colourbar(frame.colour = "black",
                                ticks.colour = "black"))+
  guides(edge_width = guide_legend(order = 1),
         fill = guide_legend(order = 2),
         size = guide_legend(order = 2))


ggraph(net,layout = "stress",bbox = 15)+
  geom_edge_link(arrow = arrow(length = unit(4, 'mm')), 
                 end_cap = circle(3, 'mm'),edge_colour = "grey66")+
  geom_node_point(aes(fill = d,size = d),shape=21)+
  scale_edge_width_continuous(range = c(0.2,3))+
  scale_size(range=c(2,5),guide = FALSE)+
  scale_fil+
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
    