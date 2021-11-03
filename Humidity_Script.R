## The below script was utilized to analyze and visualize data from the associated manuscript.
## The associated databases are "HumidityExperimentDatabase" and "HumidityPairedDatabase"

## Installing and loading packages used for graphs and analyses
install.packages("pacman") #Download package with function to load multiple packaged at once
pacman::p_load(tidyverse, #Loading required packages for code below. p_load() will download packages that aren't in system library
               scales,
               ggpubr)
view(Complete_Data_final_Working)
## Constructing the final experimental database using the HumidityExperimentDatabase and 
## SupplementalHygrometerDatabase databases
Complete_Data_final_Trial1 <- Complete_Data_final_Working %>%
  filter(Trial == 1 & Colony != 6) %>%
  mutate(Density = ifelse(Volume !=0, CollWallWt / Volume, 0))

Complete_Data_final_Trial2 <- Complete_Data_final_Working %>%
  filter(Trial == 2 & Colony != 8) %>%
  mutate(Density = ifelse(Volume !=0, CollWallWt / Volume, 0))

## Reducing the full database to mean and sd.dev for relative humidity (%) and temperature (celcius)
SupplementalHygrometerDatabaseReduced<-SupplementalHygrometerDatabase%>%
  select(c("Colony","Trial","Temp","RH","Salt"))%>%
  group_by(Colony,Trial)%>%
  mutate(Humidity=mean(RH),Temperature=mean(Temp),Std.DevHum=sd(RH),Std.DevTemp=sd(Temp))%>%
  group_by(Salt)%>%
  mutate(FullHumidity=mean(RH),FullTemperature=mean(Temp),FullStd.DevHum=sd(RH),FullStd.DevTemp=sd(Temp))%>%
  select(-c("RH","Temp"))%>%
  distinct()

Complete_Data_final_Trial1 %>% 
  group_by(Salt) %>%
  mutate(MeanHumidity = mean(Humidity), MeanStDevHum = mean(St.DevHum)) %>%
  select(c(Salt, MeanHumidity, MeanStDevHum)) %>%
  distinct()

Complete_Data_final_Trial2 %>% 
  group_by(Salt) %>%
  mutate(MeanHumidity = mean(Humidity), MeanStDevHum = mean(St.DevHum)) %>%
  select(c(Salt, MeanHumidity, MeanStDevHum)) %>%
  distinct()

## Binomial test to determine if colonies preferred substrate II
## Here we first distinguish a success as having a wall composed with greater than 50% of substrate II 
Complete_Data_final_Trial1%>% 
  group_by(Colony) %>%
  mutate(SubPrefer=ifelse(PropIIWall > 0.5, 1, 0)) %>%
  select(c(SubPrefer)) %>%
  ungroup() %>%
  filter(SubPrefer=="1")%>%
  summarise(n=n())
## Produces 18 successes and 1 failure, which will be successes (x) for the binomial test 

## Binomial test with 18 successes across 19 observations
binom.test(18, 19, p = 0.5)

Complete_Data_final_Trial2%>% 
  group_by(Colony) %>%
  mutate(SubPrefer=ifelse(PropIIWall > 0.5, 1, 0)) %>%
  select(c(SubPrefer)) %>%
  ungroup() %>%
  filter(SubPrefer=="1")%>%
  summarise(n=n())
## Produces 14 successes and 2 failures, which will be successes (x) for the binomial test

## Binomial test with 28 successes across 30 observations
binom.test(14, 16, p = 0.5)

## Binomial test to determine if a side bias was observed
## Here we first distinguish a success as choosing primarily substrate II with substrate I on the left side 
Complete_Data_final_Trial1%>%
  mutate(SideChoice=ifelse(SubstrateISide == "L" & PropIIWall > 0.5, 1, 0)) %>%
  select(c(SideChoice)) %>%
  ungroup() %>%
  filter(SideChoice=="1")%>%
  summarise(n=n())
## Produces 9 successes and 10 failures, which will be successes (x) for the binomial test

## Binomial test with 9 successes across 19 observations
binom.test(9, 19, p = 0.5)

Complete_Data_final_Trial2%>%
  mutate(SideChoice=ifelse(SubstrateISide == "L" & PropIIWall > 0.5, 1, 0)) %>%
  select(c(SideChoice)) %>%
  ungroup() %>%
  filter(SideChoice=="1")%>%
  summarise(n=n())
## Produces 7 successes and 9 failures, which will be successes (x) for the binomial test

## Binomial test with 7 successes across 16 observations
binom.test(7, 16, p = 0.5)

## Binomial test with 28 successes across 30 observations
binom.test(12, 28, p = 0.5)

## Median amount of substrate I and substrate II used to build walls, and the median ratio 
#Trial 1
Complete_Data_final_Trial1 %>%
  mutate(MedSubI = median(StartWtI - UsedWtI), MedSubII = median(StartWtII - UsedWtII), MedRatio = median(PropIIWall)) %>%
  select(c(MedSubI, MedSubII, MedRatio)) %>%
  distinct()

#Trial 2
Complete_Data_final_Trial2 %>%
  mutate(MedSubI = median(StartWtI - UsedWtI), MedSubII = median(StartWtII - UsedWtII), MedRatio = median(PropIIWall)) %>%
  select(c(MedSubI, MedSubII, MedRatio)) %>%
  distinct()

## Graphs and analyses

## This below plots visualize the relationship between experimental relative humidity (RH %) 
## and quantified nest properties. 

ColonyPalette1 <- c("1" = "coral3", "2" = "chartreuse2", "3" = "cadetblue", "4" = "burlywood4", "5" = "grey40", 
                   "7" = "blue4", "8" = "lightslateblue", "9" = "purple2", "10" = "blue", "11" = "darkseagreen4",
                   "12" = "turquoise2", "13" = "darkgreen", "14" = "firebrick4", "15" = "magenta3", "17" = "paleturquoise3",
                   "18" = "seagreen3", "20" = "grey12", "21" = "rosybrown3", "22" = "yellow3")

ColonyPalette2 <- c("1" = "coral3", "2" = "chartreuse2", "3" = "cadetblue", "4" = "burlywood4", "5" = "brown4", 
                    "7" = "blue4", "11" = "darkseagreen4", "12" = "turquoise2", "13" = "darkgreen", 
                    "14" = "firebrick4", "15" = "magenta3", "17" = "paleturquoise3",
                    "18" = "seagreen3", "20" = "grey12", "21" = "rosybrown3", "22" = "yellow3")

## We used linear regressions to examine the relationship between 
## experimental relative humidity (RH %) and each quantified nest property. 

#Power analysis for a linear regression 
#We used the true sample size of our trials and determined the power of our linear regressions, where 0.1, 0.3, and 0.5 
# represent a "small", "medium", and "large" effect size 
#Trial 1
pwr.f2.test(u = 2, v = 16, f2 = 0.02, sig.level = 0.05)
pwr.f2.test(u = 2, v = 16, f2 = 0.15, sig.level = 0.05)
pwr.f2.test(u = 2, v = 16, f2 = 0.35, sig.level = 0.05)

#Trial 2
pwr.f2.test(u = 2, v = 14, f2 = 0.02, sig.level = 0.05)
pwr.f2.test(u = 2, v = 14, f2 = 0.15, sig.level = 0.05)
pwr.f2.test(u = 2, v = 14, f2 = 0.35, sig.level = 0.05)

#Needed sample size
pwr.f2.test(u = 2, f2 = 0.02, sig.level = 0.05, power = 0.8)
pwr.f2.test(u = 2, f2 = 0.15, sig.level = 0.05, power = 0.8)
pwr.f2.test(u = 2, f2 = 0.35, sig.level = 0.05, power = 0.8)

## Wall Weight
## Plot
WeightPlot1 <- ggplot(Complete_Data_final_Trial1, aes(x=Humidity, y=CollWallWt))+
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Wall weight") +
  xlab("Relative humidity (%)") +
  ylab(expression(paste('Wall weight (g)')))+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) 

WeightPlot2 <- ggplot(Complete_Data_final_Trial2, aes(x = Humidity, y = CollWallWt))+
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Wall weight") +
  xlab("Relative humidity (%)") +
  ylab(expression(paste('Wall weight (g)')))+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) 

##Linear regression 
#Trial 1
#Linear fit
summary(lm(CollWallWt ~ Humidity, data = Complete_Data_final_Trial1))

#Quadratic fit
summary(lm(CollWallWt ~ I(Humidity^2) + Humidity, data = Complete_Data_final_Trial1))

#Trial 2
#Linear fit
summary(lm(CollWallWt ~ Humidity, data = Complete_Data_final_Trial2))

#Quadratic fit
summary(lm(CollWallWt ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial2))$coefficients

## Wall Length
## Plot
LengthPlot1<-ggplot(Complete_Data_final_Trial1, aes(x=Humidity, y=Length)) +
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Wall length") +
  xlab("Relative humidity (%)") +
  ylab("Wall length (mm)")+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.justification = c(1, 1),
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) + 
  ylim(0, 400)

LengthPlot2<-ggplot(Complete_Data_final_Trial2, aes(x=Humidity, y=Length)) +
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Wall length") +
  xlab("Relative humidity (%)") +
  ylab("Wall length (mm)")+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.justification = c(1, 1),
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) + 
  ylim(0, 400)

##Linear regression 
#Trial 1
#Linear fit
summary(lm(Length ~ Humidity, data = Complete_Data_final_Trial1))

#Quadratic fit
summary(lm(Length ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial1))$coefficients

#Trial 2
#Linear fit
summary(lm(Length ~ Humidity, data = Complete_Data_final_Trial2))

#Quadratic fit
summary(lm(Length ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial2))$coefficients

## Wall Area
## Plot
AreaPlot1<-ggplot(Complete_Data_final_Trial1, aes(x=Humidity, y=Area)) +
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Wall area") +
  xlab("Relative humidity (%)") +
  ylab(expression(paste('Wall area ('*mm^2*')')))+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) + 
  ylim(0, 8000)

AreaPlot2<-ggplot(Complete_Data_final_Trial2, aes(x=Humidity, y=Area)) +
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Wall area") +
  xlab("Relative humidity (%)") +
  ylab(expression(paste('Wall area ('*mm^2*')')))+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) + 
  ylim(0, 8000)

##Linear regression 
#Trial 1
#Linear fit
summary(lm(Area ~ Humidity, data = Complete_Data_final_Trial1))

#Quadratic fit
summary(lm(Area ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial1))$coefficients

#Trial 2
#Linear fit
summary(lm(Area ~ Humidity, data = Complete_Data_final_Trial2))

#Quadratic fit
summary(lm(Area ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial2))$coefficients

## Wall Density
## Plot
scaleFUN <- function(x) sprintf("%.4f", x)
DensityPlot1<-ggplot(Complete_Data_final_Trial1, aes(x=Humidity, y=Density))+
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Wall density") +
  xlab("Relative humidity (%)") +
  ylab(expression(paste('Wall density ('*g/mm^{3}*')')))+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.785, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) + 
  scale_y_continuous(labels=scaleFUN)

DensityPlot2<-ggplot(Complete_Data_final_Trial2, aes(x=Humidity, y=Density))+
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Wall density") +
  xlab("Relative humidity (%)") +
  ylab(expression(paste('Wall density ('*g/mm^{3}*')')))+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.785, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) + 
  scale_y_continuous(labels=scaleFUN)

#Trial 1
#Linear fit
summary(lm(Density ~ Humidity, data = Complete_Data_final_Trial1))

#Quadratic fit
summary(lm(Density ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial1))
$coefficients

#Trial 2
#Linear fit
summary(lm(Density ~ Humidity, data = Complete_Data_final_Trial2))

#Quadratic fit
summary(lm(Density ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial2))
$coefficients

## Wall Substrate Composition
##Plot
CompnPlot1<-ggplot(Complete_Data_final_Trial1, aes(x=Humidity, y=PropIIWall))+
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Wall composition") +
  xlab("Relative humidity (%)") +
  ylab("Substrate II propn")+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.795, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) +
  ylim(0.2, 1)

CompnPlot2<-ggplot(Complete_Data_final_Trial2, aes(x=Humidity, y=PropIIWall))+
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Wall composition") +
  xlab("Relative humidity (%)") +
  ylab("Substrate II propn")+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.795, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) +
  ylim(0.2, 1)



#Trial 1
#Linear fit
summary(lm(PropIIWall ~ Humidity, data = Complete_Data_final_Trial1))

#Quadratic fit
summary(lm(PropIIWall ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial1))
$coefficients

#Trial 2
#Linear fit
summary(lm(PropIIWall ~ Humidity, data = Complete_Data_final_Trial2))

#Quadratic fit
summary(lm(PropIIWall ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial2))
$coefficients

## Internal Nest Area
## Plot
IntAreaPlot1<-ggplot(Complete_Data_final_Trial1, aes(x=Humidity, y=Nest.Area))+
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Internal nest area") +
  xlab("Relative humidity (%)") +
  ylab(expression(paste('Nest area ('*mm^2*')')))+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.720, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) + 
  ylim(0, 8000)

IntAreaPlot2<-ggplot(Complete_Data_final_Trial2, aes(x=Humidity, y=Nest.Area))+
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Internal nest area") +
  xlab("Relative humidity (%)") +
  ylab(expression(paste('Nest area ('*mm^2*')')))+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.720, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) + 
  ylim(0, 8000)

#Trial 1
#Linear fit
summary(lm(Nest.Area ~ Humidity, data = Complete_Data_final_Trial1))

#Quadratic fit
summary(lm(Nest.Area ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial1))

#Trial 2
#Linear fit
summary(lm(Nest.Area ~ Humidity, data = Complete_Data_final_Trial2))

#Quadratic fit
summary(lm(Nest.Area ~ poly(Humidity, 2, raw = TRUE), data = Complete_Data_final_Trial2))

HumidPlots1<-ggarrange(WeightPlot1, LengthPlot1, 
                       AreaPlot1, DensityPlot1, 
                       CompnPlot1, IntAreaPlot1,
                       labels = c("(a)", "(b)",
                                  "(c)", "(d)",
                                  "(e)", "(f)"),
                       font.label = list(size = 26, family = "Arial", face = "plain"),
                       label.x = 0.9,
                       ncol = 2, nrow = 3, 
                       common.legend = TRUE,
                       legend = "top") 

annotate_figure(HumidPlots1,
                top = NULL,
                bottom = text_grob("Relative humidity (%)", color = "black",
                                   size = 32, x = 0.5, y = 1, family = "Arial"),
                left = NULL,
                right = NULL
)



HumidPlots2<-ggarrange(WeightPlot2, LengthPlot2, 
                       AreaPlot2, DensityPlot2, 
                       CompnPlot2, IntAreaPlot2,
                       labels = c("(a)", "(b)",
                                  "(c)", "(d)",
                                  "(e)", "(f)"),
                       font.label = list(size = 26, family = "Arial", face = "plain"),
                       label.x = 0.9,
                       ncol = 2, nrow = 3, 
                       common.legend = TRUE,
                       legend = "top") 

annotate_figure(HumidPlots2,
                top = NULL,
                bottom = text_grob("Relative humidity (%)", color = "black",
                                   size = 32, x = 0.5, y = 1, family = "Arial"),
                left = NULL,
                right = NULL
)

#Worker and brood number
#Creating the datasets for analysis

#Median & range
#Trial 1
#Workers
median(Complete_Data_final_Trial1$Number.Ant)
range(Complete_Data_final_Trial1$Number.Ant)

#Brood
median(Complete_Data_final_Trial1$Number.Brood)
range(Complete_Data_final_Trial1$Number.Brood)

#Queens (same across trials)
median(Complete_Data_final_Trial1$Number.Queens)
range(Complete_Data_final_Trial1$Number.Queens)

#Workers
median(Complete_Data_final_Trial2$Number.Ant)
range(Complete_Data_final_Trial2$Number.Ant)

#Brood
median(Complete_Data_final_Trial2$Number.Brood)
range(Complete_Data_final_Trial2$Number.Brood)

#Trial 1
#Worker count
Complete_Data_final_Trial1WorkerCount <- Complete_Data_final_Trial1 %>%
  select(-c(Number.Brood)) %>%
  rename(Number.Colony = Number.Ant) %>%
  mutate(ColonyMember = "Workers")

#Brood count
Complete_Data_final_Trial1BroodCount <- Complete_Data_final_Trial1 %>%
  select(-c(Number.Ant))%>%
  rename(Number.Colony = Number.Brood) %>%
  mutate(ColonyMember = "Brood")

#Combined dataset - for plots
Complete_Data_final_Trial1ColonyCount <- full_join(Complete_Data_final_Trial1WorkerCount, Complete_Data_final_Trial1BroodCount)

#Trial 2
#Worker count
Complete_Data_final_Trial2WorkerCount <- Complete_Data_final_Trial2 %>%
  select(-c(Number.Brood)) %>%
  rename(Number.Colony = Number.Ant) %>%
  mutate(ColonyMember = "Workers")

#Brood count
Complete_Data_final_Trial2BroodCount <- Complete_Data_final_Trial2 %>%
  select(-c(Number.Ant))%>%
  rename(Number.Colony = Number.Brood) %>%
  mutate(ColonyMember = "Brood")

#Combined dataset - for plots
Complete_Data_final_Trial2ColonyCount <- full_join(Complete_Data_final_Trial2WorkerCount, Complete_Data_final_Trial2BroodCount)

#Power analysis for a correlation test
#We used the true sample size of our trials and determined the power of our correlation tests, where 0.1, 0.3, and 0.5 
# represent a "small", "medium", and "large" effect size 

#Trial 1
pwr.r.test(n = 19, r = 0.1, sig.level = 0.05, alternative = "two.sided")
pwr.r.test(n = 19, r = 0.3, sig.level = 0.05, alternative = "two.sided")
pwr.r.test(n = 19, r = 0.5, sig.level = 0.05, alternative = "two.sided")

#Trial 2
pwr.r.test(n = 17, r = 0.1, sig.level = 0.05, alternative = "two.sided")
pwr.r.test(n = 17, r = 0.3, sig.level = 0.05, alternative = "two.sided")
pwr.r.test(n = 17, r = 0.5, sig.level = 0.05, alternative = "two.sided")

#Needed sample size
pwr.r.test(r = 0.1, sig.level = 0.05, power = 0.8, alternative = "two.sided")
pwr.r.test(r = 0.3, sig.level = 0.05, power = 0.8, alternative = "two.sided")
pwr.r.test(r = 0.5, sig.level = 0.05, power = 0.8, alternative = "two.sided")

#Needed effect size
pwr.r.test(r = 0.6, sig.level = 0.05, power = 0.8, alternative = "two.sided")

## Wall Weight
## Plot

WeightPlotColony1 <- ggplot(Complete_Data_final_Trial1ColonyCount, aes(x=Number.Colony, y=CollWallWt, group = ColonyMember))+
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  ggtitle("Wall weight") +
  xlab(NULL) +
  ylab(expression(paste('Wall weight (g)')))+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) +
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) +
  scale_x_continuous(breaks = seq(75, 225, by = 75))
WeightPlotColony2
WeightPlotColony2 <- ggplot(Complete_Data_final_Trial2ColonyCount, aes(x=Number.Colony, y=CollWallWt, group = ColonyMember, linetype = ColonyMember))+
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  geom_smooth(color = "black", se = FALSE, method = "lm", size = 2.5) +
  ggtitle("Wall weight") +
  xlab(NULL) +
  ylab(expression(paste('Wall weight (g)')))+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.key.width = unit(2.5, "line")) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial"),
         linetype = guide_legend(title = "Colony member", family = "Arial")) + 
  facet_wrap(~ColonyMember) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) 

##Spearman's rho 
#Trial 1
#Number of workers
cor.test(Complete_Data_final_Trial1$Number.Ant, Complete_Data_final_Trial1$CollWallWt, method = 'spearman', alternative = "two.sided")

#Number of brood
cor.test(Complete_Data_final_Trial1$Number.Brood, Complete_Data_final_Trial1$CollWallWt, method = 'spearman', alternative = "two.sided")

#Trial 2
#Number of workers
cor.test(Complete_Data_final_Trial2$Number.Ant, Complete_Data_final_Trial2$CollWallWt, method = 'spearman', alternative = "two.sided")

#Number of brood
cor.test(Complete_Data_final_Trial2$Number.Brood, Complete_Data_final_Trial2$CollWallWt, method = 'spearman', alternative = "two.sided")

## Wall Length
## Plot
LengthPlotColony1 <- ggplot(Complete_Data_final_Trial1ColonyCount, aes(x=Number.Colony, y=Length, group = ColonyMember))+
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  geom_smooth(data = Complete_Data_final_Trial1ColonyCount %>% filter(ColonyMember == "Brood"), 
              aes(x=Number.Colony, y=Length), 
              color = "black", se = FALSE, method = "lm", size = 2.5) +
  ggtitle("Wall length") +
  xlab(NULL) +
  ylab(expression(paste('Wall length (mm)')))+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  facet_wrap(~ColonyMember) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) +
  scale_x_continuous(breaks = seq(75, 225, by = 75))

LengthPlotColony2 <- ggplot(Complete_Data_final_Trial2ColonyCount, aes(x=Number.Colony, y=Length, group = ColonyMember)) +
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  ggtitle("Wall length") +
  xlab(NULL) +
  ylab(expression(paste('Wall length (mm)')))+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) 
view(Complete_Data_final_Trial1)
##Linear regression 
#Trial 1
#Number of workers
cor.test(Complete_Data_final_Trial1$Number.Ant, Complete_Data_final_Trial1$Length, method = 'spearman', alternative = "two.sided")

#Number of brood
cor.test(Complete_Data_final_Trial1$Number.Brood, Complete_Data_final_Trial1$Length, method = 'spearman', alternative = "two.sided")

#Trial 2
#Number of workers
cor.test(Complete_Data_final_Trial2$Number.Ant, Complete_Data_final_Trial2$Length, method = 'spearman', alternative = "two.sided")

#Number of brood
cor.test(Complete_Data_final_Trial2$Number.Brood, Complete_Data_final_Trial2$Length, method = 'spearman', alternative = "two.sided")

## Wall Area
## Plot
AreaPlotColony1 <- ggplot(Complete_Data_final_Trial1ColonyCount, aes(x=Number.Colony, y=Area, group = ColonyMember))+
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  geom_smooth(data = Complete_Data_final_Trial1ColonyCount %>% filter(ColonyMember == "Brood"), 
              aes(x=Number.Colony, y=Area), 
              color = "black", se = FALSE, method = "lm", size = 2.5) +
  ggtitle("Wall area") +
  xlab(NULL) +
  ylab(expression(paste('Wall area ('*mm^2*')')))+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  facet_wrap(~ColonyMember) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) +
  scale_x_continuous(breaks = seq(75, 225, by = 75))

AreaPlotColony2 <- ggplot(Complete_Data_final_Trial2ColonyCount, aes(x=Number.Colony, y=Area, group = ColonyMember))+
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  ggtitle("Wall area") +
  xlab(NULL) +
  ylab(expression(paste('Wall area ('*mm^2*')')))+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.850, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) 
max(Complete_Data_final_Trial1ColonyCount$Number.Colony)

##Linear regression 
#Trial 1
#Number of workers
cor.test(Complete_Data_final_Trial1$Number.Ant, Complete_Data_final_Trial1$Area, method = 'spearman', alternative = "two.sided")

#Number of brood
cor.test(Complete_Data_final_Trial1$Number.Brood, Complete_Data_final_Trial1$Area, method = 'spearman', alternative = "two.sided")

#Trial 2
#Number of workers
cor.test(Complete_Data_final_Trial2$Number.Ant, Complete_Data_final_Trial2$Area, method = 'spearman', alternative = "two.sided")

#Number of brood
cor.test(Complete_Data_final_Trial2$Number.Brood, Complete_Data_final_Trial2$Area, method = 'spearman', alternative = "two.sided")

## Wall Density
## Plot
scaleFUN <- function(x) sprintf("%.4f", x)
DensityPlotColony1 <- ggplot(Complete_Data_final_Trial1ColonyCount, aes(x=Number.Colony, y=Density, group = ColonyMember))+
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  ggtitle("Wall density") +
  xlab(NULL) +
  ylab(expression(paste('Wall density ('*g/mm^{3}*')')))+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.825, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) +
  scale_x_continuous(breaks = seq(75, 225, by = 75))

DensityPlotColony2 <- ggplot(Complete_Data_final_Trial2ColonyCount, aes(x=Number.Colony, y=Density, group = ColonyMember))+
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  ggtitle("Wall density") +
  xlab(NULL) +
  ylab(expression(paste('Wall density ('*g/mm^{3}*')')))+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.825, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) 

##Linear regression 
#Trial 1
#Number of workers
cor.test(Complete_Data_final_Trial1$Number.Ant, Complete_Data_final_Trial1$Density, method = 'spearman', alternative = "two.sided")

#Number of brood
cor.test(Complete_Data_final_Trial1$Number.Brood, Complete_Data_final_Trial1$Density, method = 'spearman', alternative = "two.sided")

#Trial 2
#Number of workers
cor.test(Complete_Data_final_Trial2$Number.Ant, Complete_Data_final_Trial2$Density, method = 'spearman', alternative = "two.sided")

#Number of brood
cor.test(Complete_Data_final_Trial2$Number.Brood, Complete_Data_final_Trial2$Density, method = 'spearman', alternative = "two.sided")

## Wall Substrate Composition
##Plot
CompnPlotColony1 <- ggplot(Complete_Data_final_Trial1ColonyCount, aes(x=Number.Colony, y=PropIIWall, group = ColonyMember))+
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  ggtitle("Wall composition") +
  xlab(NULL) +
  ylab("Substrate II propn")+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.825, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) +
  scale_x_continuous(breaks = seq(75, 225, by = 75))

CompnPlotColony2 <- ggplot(Complete_Data_final_Trial2ColonyCount, aes(x=Number.Colony, y=PropIIWall, group = ColonyMember))+
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  ggtitle("Wall composition") +
  xlab(NULL) +
  ylab("Substrate II propn")+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.825, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) 
(Complete_Data_final_Trial1ColonyCount$Colony)
##Linear regression 
#Trial 1
#Number of workers
cor.test(Complete_Data_final_Trial1$Number.Ant, Complete_Data_final_Trial1$PropIIWall, method = 'spearman', alternative = "two.sided")

#Number of brood
cor.test(Complete_Data_final_Trial1$Number.Brood, Complete_Data_final_Trial1$PropIIWall, method = 'spearman', alternative = "two.sided")

#Trial 2
#Number of workers
cor.test(Complete_Data_final_Trial2$Number.Ant, Complete_Data_final_Trial2$PropIIWall, method = 'spearman', alternative = "two.sided")

#Number of brood
cor.test(Complete_Data_final_Trial2$Number.Brood, Complete_Data_final_Trial2$PropIIWall, method = 'spearman', alternative = "two.sided")

## Internal Nest Area
## Plot
IntAreaColony1 <- ggplot(Complete_Data_final_Trial1ColonyCount, aes(x=Number.Colony, y=Nest.Area, group = ColonyMember))+
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  ggtitle("Internal nest area") +
  xlab(NULL) +
  ylab(expression(paste('Nest area ('*mm^2*')'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.775, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) + 
  ylim(0, 8000) +
  scale_x_continuous(breaks = seq(50, 225, by = 75))

IntAreaColony2 <- ggplot(Complete_Data_final_Trial2ColonyCount, aes(x=Number.Colony, y=Nest.Area, group = ColonyMember))+
  geom_point(size = 6, aes(color = as.factor(Colony), shape = ColonyMember)) +
  ggtitle("Internal nest area") +
  xlab(NULL) +
  ylab(expression(paste('Nest area ('*mm^2*')'))) +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 26, family = "Arial", color = "black", hjust = 0.775, vjust = 0),
        axis.text = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 26, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 26, family = "Arial", color = "white"),
        legend.text = element_text(size = 26, family = "Arial", color = "black"),
        legend.title = element_text(size = 26, family = "Arial", color = "black"),
        legend.position = "right",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial"), 
         shape = guide_legend(title = "Colony member", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette2)) + 
  ylim(0, 8000)

##Linear regression 
#Trial 1
#Number of workers
cor.test(Complete_Data_final_Trial1$Number.Ant, Complete_Data_final_Trial1$Nest.Area, method = 'spearman', alternative = "two.sided")

#Number of brood
cor.test(Complete_Data_final_Trial1$Number.Brood, Complete_Data_final_Trial1$Nest.Area, method = 'spearman', alternative = "two.sided")

#Trial 2
#Number of workers
cor.test(Complete_Data_final_Trial2$Number.Ant, Complete_Data_final_Trial2$Nest.Area, method = 'spearman', alternative = "two.sided")

#Number of brood
cor.test(Complete_Data_final_Trial2$Number.Brood, Complete_Data_final_Trial2$Nest.Area, method = 'spearman', alternative = "two.sided")

HumidPlotsColony1<-ggarrange(WeightPlotColony1, LengthPlotColony1, 
                       AreaPlotColony1, DensityPlotColony1, 
                       CompnPlotColony1, IntAreaColony1,
                       labels = c("(a)", "(b)",
                                  "(c)", "(d)",
                                  "(e)", "(f)"),
                       font.label = list(size = 26, family = "Arial", face = "plain"),
                       label.x = 0.9,
                       ncol = 2, nrow = 3, 
                       common.legend = TRUE,
                       legend = "top") 

annotate_figure(HumidPlotsColony1,
                top = NULL,
                bottom = text_grob("Number of workers / brood", color = "black",
                                   size = 32, x = 0.5, y = 0.5, family = "Arial"),
                left = NULL,
                right = NULL
)


HumidPlotsColony2<-ggarrange(WeightPlotColony2, LengthPlotColony2, 
                             AreaPlotColony2, DensityPlotColony2, 
                             CompnPlotColony2, IntAreaColony2,
                             labels = c("(a)", "(b)",
                                        "(c)", "(d)",
                                        "(e)", "(f)"),
                             font.label = list(size = 26, family = "Arial", face = "plain"),
                             label.x = 0.9,
                             ncol = 2, nrow = 3, 
                             common.legend = TRUE,
                             legend = "top") 

annotate_figure(HumidPlotsColony2,
                top = NULL,
                bottom = text_grob("Number of workers / brood", color = "black",
                                   size = 32, x = 0.5, y = 0.5, family = "Arial"),
                left = NULL,
                right = NULL
)


#Humidity & colony death
WorkerDeathPlot<-ggplot(HumidMortality, aes(x=Humidity, y=WorkerDeath))+
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Worker death") +
  xlab(NULL) +
  ylab("Worker death (%)")+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 22, family = "Arial", color = "black", hjust = 0.825, vjust = 0),
        axis.text = element_text(size = 22, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 22, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 22, family = "Arial", color = "black"),
        legend.text = element_text(size = 22, family = "Arial", color = "black"),
        legend.title = element_text(size = 22, family = "Arial", color = "black"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) 

BroodDeathPlot<-ggplot(HumidMortality, aes(x=Humidity, y=BroodDeath))+
  geom_point(size = 6, aes(color = as.factor(Colony))) +
  ggtitle("Brood death") +
  xlab(NULL) +
  ylab("Brood death (%)")+
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 22, family = "Arial", color = "black", hjust = 0.825, vjust = 0),
        axis.text = element_text(size = 22, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 22, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 22, family = "Arial", color = "black"),
        legend.text = element_text(size = 22, family = "Arial", color = "black"),
        legend.title = element_text(size = 22, family = "Arial", color = "black"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.key=element_blank()) + 
  guides(color = guide_legend(title = "Colony ID", family = "Arial")) + 
  scale_color_manual(values = as.vector(ColonyPalette1)) 

summary(glm(WorkerDeath ~ Humidity, family = "binomial", data = HumidMortality))

summary(glm(BroodDeath ~ Humidity, family = "binomial", data = HumidMortality)) 

ColonyDeathHumid <- ggarrange(WorkerDeathPlot, BroodDeathPlot,
                       labels = c("(a)", "(b)"),
                       font.label = list(size = 22, family = "Arial", face = "plain"),
                       label.x = 0.9,
                       label.y = 1.005,
                       ncol = 2, nrow = 1, 
                       common.legend = TRUE,
                       legend = "top") 

annotate_figure(ColonyDeathHumid,
                top = NULL,
                bottom = text_grob("Relative humidity (%)", color = "black",
                                   size = 26, x = 0.5, y = 0.5, family = "Arial"),
                left = NULL,
                right = NULL
)

## We used wilcoxon signed rank tests to compare artificial and natural nest substrate porosities
## Plot
## Creating the base plot, significance brackets represent results from Dunn's pairwise comparisons below. 
ggplot(PorosityComparison, aes(x = reorder(SubCategory, PorosityII, FUN = median), y = PorosityII))+
  geom_boxplot(coef = 200, lwd = 0.55) +
  geom_bracket(
    xmin = c("Sub I", "Sub I", "Sub I", "Sub II", "Built", "Built"), xmax = c("Sub II", "Natural", "Built", "Natural", "Sub II", "Natural"),
    y.position = c(58, 62, 54, 43, 46.5, 51), label = c("***", "***", "***", "NS", "NS", "NS"),
    tip.length = 0.01, size = 0.65,label.size = 6,family="Arial")+
  ggtitle("Porosity comparison") +
  xlab("Substrate category") +
  ylab("Porosity (%)") +
  theme_pubclean() +
  theme(axis.ticks = element_blank(),
        plot.title = element_text(size = 22, family = "Arial", color = "black", hjust = -0.1, vjust = 0),
        axis.text = element_text(size = 22, family = "Arial", color = "black"),
        axis.title.y = element_text(size = 22, family = "Arial", color = "black"),
        axis.title.x = element_text(size = 22, family = "Arial", color = "black")) 

PorosityComparison<-PorosityComparison%>%mutate(PorosityII=PorosityII*100)
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

## Built wall substrate
PorosityComparisonBuilt<-PorosityComparison%>%
  filter(SubCategory=="Built")

ggqqplot(PorosityComparisonI$Porosity)

ggqqplot(PorosityComparisonII$Porosity)

ggqqplot(PorosityComparisonNat$Porosity)

ggqqplot(PorosityComparisonBuilt$Porosity)

var.test(PorosityComparisonI$Porosity,PorosityComparisonII$Porosity, alternative = "two.sided")

var.test(PorosityComparisonI$Porosity,PorosityComparisonNat$Porosity, alternative = "two.sided")

var.test(PorosityComparisonII$Porosity,PorosityComparisonNat$Porosity, alternative = "two.sided")

shiehpow(n = 10, m = 10, p = 0.1, alpha = 0.5, dist = "norm", sides = "two.sided")

shiehpow(n = 10, m = 10, p = 0.3, alpha = 0.5, dist = "norm", sides = "two.sided")

shiehpow(n = 10, m = 10, p = 0.5, alpha = 0.5, dist = "norm", sides = "two.sided")

## Wilcoxon signed rank tests 
## Substrate I v. Substrate II
wilcox.test(PorosityComparisonI$PorosityII,PorosityComparisonII$PorosityII, paired = FALSE, alternative = "two.sided")

## Substrate I v. Natural
wilcox.test(PorosityComparisonI$PorosityII,PorosityComparisonNat$PorosityII, paired = FALSE, alternative = "two.sided")

## Substrate II v. Natural
wilcox.test(PorosityComparisonII$PorosityII,PorosityComparisonNat$PorosityII, paired = FALSE, alternative = "two.sided")

## Substrate I v. Built
wilcox.test(PorosityComparisonI$PorosityII,PorosityComparisonBuilt$PorosityII, paired = FALSE, alternative = "two.sided")

#Substrate II v. Built 
wilcox.test(PorosityComparisonII$PorosityII,PorosityComparisonBuilt$PorosityII, paired = FALSE, alternative = "two.sided")

#Natural v. Built
wilcox.test(PorosityComparisonNat$PorosityII,PorosityComparisonBuilt$PorosityII, paired = FALSE, alternative = "two.sided")
