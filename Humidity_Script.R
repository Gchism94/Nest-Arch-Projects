## The below script was utilized to analyze and visualize data from the associated manuscript.
## The associated databases are "HumidityExperimentDatabase" and "HumidityPairedDatabase"

## Installing and loading packages used for graphs and analyses
install.packages("pacman") #Download package with function to load multiple packaged at once
pacman::p_load(tidyverse, #Loading required packages for code below. p_load() will download packages that aren't in system library
               scales,
               ggpubr)

## Constructing the final experimental database using the HumidityExperimentDatabase and 
## SupplementalHygrometerDatabase databases

## Reducing the full database to mean and sd.dev for relative humidity (%) and temperature (celcius)
SupplementalHygrometerDatabaseReduced<-SupplementalHygrometerDatabase%>%
  select(c("Colony","Trial","Temp","RH","Salt"))%>%
  group_by(Colony,Trial)%>%
  mutate(Humidity=mean(RH),Temperature=mean(Temp),Std.DevHum=sd(RH),Std.DevTemp=sd(Temp))%>%
  group_by(Salt)%>%
  mutate(FullHumidity=mean(RH),FullTemperature=mean(Temp),FullStd.DevHum=sd(RH),FullStd.DevTemp=sd(Temp))%>%
  select(-c("RH","Temp"))%>%
  distinct()

## Joining the mean and sd.dev for relative humidity (%) and temperature (celcius) to recorded nest features database 
HumidityExperimentDatabase<-left_join(HumidityExperimentDatabaseObs,SupplementalHygrometerDatabaseReduced)


## Binomial test to determine if colonies preferred substrate II
## Here we first distinguish a success as having a wall composed with greater than 50% of substrate II 
HumidityExperimentDatabase<-HumidityExperimentDatabase%>%
  mutate(TrialNumber=ifelse(Trial<=2,"First","Second"), 
         SubPrefer=ifelse(PropIIWall > 0.5, 1, 0))

## We then counted the number of successes
HumidityExperimentDatabase%>%
  filter(SubPrefer=="1")%>%
  summarise(n=n())
## Produces 26, which will be successes (x) for the binomial test 

## Here we have the total observations
HumidityExperimentDatabase%>%
  summarise(n=n())
## Produces 28, which will be the total observations (n) for the binomial test

## Binomial test with 28 successes across 30 observations
binom.test(26, 28, p = 0.5)

## Binomial test to determine if a side bias was observed
## Here we first distinguish a success as choosing substrate I on the right side 
HumidityExperimentDatabase<-HumidityExperimentDatabase%>%
  mutate(SideChoice=ifelse(SubstrateIISide == "L" & SubPrefer == "1", 1, 0))

## We then counted the number of successes
HumidityExperimentDatabase%>%
  filter(SideChoice=="1")%>%
  summarise(n=n())
## Produces 12, which will be successes (x) for the binomial test 

## Here we have the total observations
HumidityExperimentDatabase%>%
  summarise(n=n())
## Produces 28, which will be the total observations (n) for the binomial test

## Binomial test with 28 successes across 30 observations
binom.test(12, 28, p = 0.5)



## Graphs and analyses

## This below plots visualize the relationship between experimental relative humidity (RH %) 
## and quantified nest properties. 

## We used linear regressions to examine the relationship between 
## experimental relative humidity (RH %) and each quantified nest property. 

## Wall Length
## Plot
LengthPlot<-ggplot(HumidityExperimentDatabase, aes(x=Humidity, y=Length))+
  geom_point(size = 2)+
  theme_pubr()+ 
  theme(axis.text=element_text(size=14,family="Times"),
        axis.title = element_text(size = 18,family="Times"),
        plot.title=element_text(size=18,face="bold",family="Times")) + 
  xlab(NULL)+
  ylab(expression(paste('Wall length '(mm))))+
  ggtitle("Wall length")

##Linear regression 
summary(lm(Length~Humidity+Avg.Brood+Avg.Worker+Colony,data=HumidityExperimentDatabase))

## Wall Area
## Plot
AreaPlot<-ggplot(HumidityExperimentDatabase, aes(x=Humidity, y=Area))+
  geom_point(size = 2)+
  theme_pubr()+ 
  theme(axis.text=element_text(size=14,family="Times"),
        axis.title = element_text(size = 18,family="Times"),
        plot.title=element_text(size=18,face="bold",family="Times")) +  
  xlab(NULL)+
  ylab(expression(paste('Wall area '(mm^{2}))))+
  ggtitle("Wall area")

##Linear regression 
summary(lm(Area~Humidity+Avg.Brood+Avg.Worker+Colony,data=HumidityExperimentDatabase))

HumidPlots1<-ggarrange(LengthPlot, AreaPlot,
                       labels = c("A", "B"),
                       font.label = list(size = 18, family = "Times"),
                       ncol = 2, nrow = 1)

annotate_figure(HumidPlots1,
                top = NULL,
                bottom = text_grob("Relative humidity (%)", color = "black",
                                   size = 18, x = 0.5, family = "Times"),
                left = NULL,
                right = NULL
)

## Wall Density
## Plot
scaleFUN <- function(x) sprintf("%.4f", x)
DensityPlot<-ggplot(HumidityExperimentDatabase, aes(x=Humidity, y=Density))+
  geom_point(size = 2)+
  theme_pubr()+ 
  theme(axis.text=element_text(size=14,family="Times"),
        axis.title = element_text(size = 18,family="Times"),
        plot.title=element_text(size=18,face="bold",family="Times")) + 
  xlab(NULL)+
  ylab(expression(paste('Density '(g/mm^{3}))))+
  ggtitle("Wall density")+
  scale_y_continuous(labels=scaleFUN)

##Linear regression 
summary(lm(Density~Humidity+Avg.Brood+Avg.Worker+Colony,data=HumidityExperimentDatabase))

## Wall Substrate Composition
##Plot
CompnPlot<-ggplot(HumidityExperimentDatabase, aes(x=Humidity, y=PropIIWall))+
  geom_point(size = 2)+
  theme_pubr()+ 
  theme(axis.text=element_text(size=12,family="Times"),
        axis.title = element_text(size = 18,family="Times"),
        plot.title=element_text(size=18,face="bold",family="Times")) + 
  xlab(NULL)+
  ylab('Small substrate wall proportion')+
  ggtitle("Wall substrate composition")

##Linear regression 
summary(lm(PropIIWall~Humidity+Avg.Brood+Avg.Worker+Colony,data=HumidityExperimentDatabase))



## Internal Nest Area
## Plot
IntAreaPlot<-ggplot(HumidityExperimentDatabase, aes(x=Humidity, y=Nest.Area))+
  geom_point(size = 2)+
  theme_pubr()+ 
  theme(axis.text=element_text(size=12,family="Times"),
        axis.title = element_text(size = 18,family="Times"),
        plot.title=element_text(size=18,face="bold",family="Times")) + 
  xlab("Relative humidity (%)")+
  ylab(expression(paste('Nest area '(mm^{2}))))+
  ggtitle("Internal nest area")

##Linear regression 
summary(lm(Nest.Area~Humidity+Avg.Brood+Avg.Worker+Colony,data=HumidityExperimentDatabase))

HumidPlots2<-ggarrange(DensityPlot, CompnPlot,
                       labels = c("C", "D"),
                       font.label = list(size = 18, family = "Times"),
                       ncol = 2, nrow = 1)

annotate_figure(HumidPlots2,
                top = NULL,
                bottom = text_grob("Relative humidity (%)", color = "black",
                                   size = 18, x = 0.5, family = "Times"),
                left = NULL,
                right = NULL
)

ggarrange(IntAreaPlot,
                       labels = c("E"),
                       font.label = list(size = 18, family = "Times"),
                       ncol = , nrow = 1)

## We used wilcoxon signed rank tests to compare artificial and natural nest substrate porosities
## Plot
## Creating the base plot, significance brackets represent results from Dunn's pairwise comparisons below. 
ggplot(PorosityComparison, aes(x=reorder(SubCategory, Porosity, FUN = median), y=Porosity))+
  geom_boxplot(coef=200)+
  geom_bracket(
    xmin = c("Sub I", "Sub II","Sub I"), xmax = c("Sub II", "Natural","Natural"),
    y.position = c(55, 73, 75), label = c("***", "NS","***"),
    tip.length = 0.01,size=0.5,label.size=5,family="Times")+
  theme_pubr()+ 
  theme(axis.text=element_text(size=14,family="Times"),
        axis.title = element_text(size = 18,family="Times"),
        plot.title=element_text(size=18,face="bold",family="Times")) + 
  xlab("Substrate category")+
  ylab(expression(paste('Porosity %')))+
  ggtitle("Porosity comparison")

## Creating data subsets
## Substrate I
PorosityComparisonI<-PorosityComparison%>%
  filter(SubCategory=="Sub I")

## Substrate II
PorosityComparisonII<-PorosityComparison%>%
  filter(SubCategory=="Sub II")

## Natural
PorosityComparisonNat<-PorosityComparison%>%
  filter(SubCategory=="Natural")

## Wilcoxon signed rank tests 
## Substrate I v. Substrate II
wilcox.test(PorosityComparisonI$Porosity,PorosityComparisonII$Porosity)

## Substrate I v. Natural
wilcox.test(PorosityComparisonI$Porosity,PorosityComparisonNat$Porosity)

## Substrate II v. Natural
wilcox.test(PorosityComparisonII$Porosity,PorosityComparisonNat$Porosity)


## We used linear regressions to determine if worker and brood die-offs in each colony related
## to the relative humidity they were exposed to in the first trial. 
## Workers
summary(lm(WorkerLossPercent~Humidity,data=HumidityExperimentDatabaseLoss))

## Brood
summary(lm(BroodLossPercent~Humidity,data=HumidityExperimentDatabaseLoss))
