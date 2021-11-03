#AGGRESSION
BinCoordAssignMove <- read_excel("/Volumes/ChismHardDriveMAC_1/ChismDatasets/Reference_Coords/AggnStudyRefCoords_Master.xlsx", 
                                 sheet = "AllBins")
CoordBinnedMoveAggn<-function(data_table){
  BinCoordAssignMoveTube<-BinCoordAssignMove%>%
    filter(Colony=="5"& Nest=="Tube"&Trial=="Aggn")
  BinCoordAssignMoveCircle<-BinCoordAssignMove%>%
    filter(Colony=="5"& Nest=="Circle"&Trial=="Aggn") 
  Colony5BinnedMoveTubeAggn <<-data_table%>%
    filter(Colony=="5"& Nest=="Tube")%>%
    mutate(Bin =
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1,
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2,
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3,
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4,
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5,
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6,
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7,
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8,0
                                                                     )))))))))%>%
    select(c("Colony", "Nest", "Trial", "Seconds","AntLength.sec","Bin"))
   Colony5BinnedMoveCircleAggn <-data_table%>%
    filter(Colony=="5"& Nest=="Circle")%>%
    mutate(Bin =
             if_else(Y <= BinCoordAssignMoveCircle$Y[1],1,
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2],2,
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3],3,
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4],4,
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5],5,
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6],6,
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7],7,
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7],8,0
                                                                     )))))))))%>%
     select(c("Colony", "Nest", "Trial", "Seconds","AntLength.sec","Bin"))
  Colony5AggnBinned<<-full_join(Colony5BinnedMoveTubeAggn,Colony5BinnedMoveCircleAggn)
}

CoordBinnedMoveAggn(Colony5Aggn)

Colony5AggnBinned<-Colony5AggnBinned%>%
  select(-c(X,Y))
Colony5AggnBinned%>%
  filter(Bin=="0")
  group_by(Bin)%>%
  summarise(n=n())

Colony6AggnBinned<-Colony6AggnBinned%>%
  select(-c(X,Y))

  filter(Bin=="0")%>%
  group_by(Bin)%>%
  summarise(n=n())

Colony7AggnBinned<-Colony7AggnBinned%>%
  select(-c(X,Y))

  filter(Bin=="0")%>%
  group_by(Bin)%>%
  summarise(n=n())

Colony8AggnBinned<-Colony8AggnBinned%>%
  select(-c(X,Y))

  filter(Bin=="0")%>%
  group_by(Bin)%>%
  summarise(n=n())

Colony9AggnBinned<-Colony9AggnBinned%>%
  select(-c(X,Y))

  filter(Bin=="0")%>%
  group_by(Bin)%>%
  summarise(n=n())
  
left_join(Colony11AggnBinned,Colony11TubeAggn)
  
Colony11AggnBinned<-Colony11AggnBinned%>%
  filter(Bin=="0")
  group_by(Bin)%>%
  summarise(n=n())

Colony13AggnBinned<-Colony13AggnBinned%>%
  select(-c(X,Y))

  filter(Bin=="0")%>%
  group_by(Bin)%>%
  summarise(n=n())

Colony17AggnBinned<-Colony17AggnBinned%>%
  select(-c(X,Y))

  filter(Bin=="0")%>%
  group_by(Bin)%>%
  summarise(n=n())

Colony18AggnBinned<-Colony18AggnBinned%>%
  select(-c(X,Y))

  filter(Bin=="0")%>%
  group_by(Bin)%>%
  summarise(n=n())

Colony20AggnBinned<-Colony20AggnBinned%>%
  select(-c(X,Y))

  filter(Bin=="0")%>%
  group_by(Bin)%>%
  summarise(n=n())


#Baseline
CoordBinnedMovePre<-function(data_table){
  BinCoordAssignMoveTube<-BinCoordAssignMove%>%
    filter(Colony=="6"& Nest=="Tube" & Trial=="Pre")
  BinCoordAssignMoveCircle<-BinCoordAssignMove%>%
    filter(Colony=="6"& Nest=="Circle" & Trial=="Pre") 
  Colony6BinnedMoveTubePre <-data_table%>%
    filter(Colony=="6"& Nest=="Tube")%>%
    mutate(Bin =
             if_else(X >= BinCoordAssignMoveTube$X[2] & 
                       X <= BinCoordAssignMoveTube$X[1] &
                       Y >= BinCoordAssignMoveTube$Y[2], 1,
                     if_else(X >= BinCoordAssignMoveTube$X[3] & 
                               X <= BinCoordAssignMoveTube$X[2] &
                               Y <= BinCoordAssignMoveTube$Y[3] &
                               Y >= BinCoordAssignMoveTube$Y[2], 2,
                             if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                       X <= BinCoordAssignMoveTube$X[3] &
                                       Y <= BinCoordAssignMoveTube$Y[3] &
                                       Y >= BinCoordAssignMoveTube$Y[4], 3,
                                     if_else(X >= BinCoordAssignMoveTube$X[4] & 
                                               X <= BinCoordAssignMoveTube$X[5] &
                                               Y <= BinCoordAssignMoveTube$Y[4] &
                                               Y >= BinCoordAssignMoveTube$Y[5], 4,
                                             if_else(X >= BinCoordAssignMoveTube$X[5] & 
                                                       X <= BinCoordAssignMoveTube$X[6] &
                                                       Y <= BinCoordAssignMoveTube$Y[4] &
                                                       Y >= BinCoordAssignMoveTube$Y[5], 5,
                                                     if_else(X >= BinCoordAssignMoveTube$X[6] & 
                                                               X <= BinCoordAssignMoveTube$X[7] &
                                                               Y <= BinCoordAssignMoveTube$Y[7] &
                                                               Y >= BinCoordAssignMoveTube$Y[6], 6,
                                                             if_else(X >= BinCoordAssignMoveTube$X[8] & 
                                                                       X <= BinCoordAssignMoveTube$X[7] &
                                                                       Y >= BinCoordAssignMoveTube$Y[7], 7,
                                                                     if_else(X >= BinCoordAssignMoveTube$X[9] & 
                                                                               X <= BinCoordAssignMoveTube$X[8] &
                                                                               Y >= BinCoordAssignMoveTube$Y[9], 8,0
                                                                     )))))))))
  Colony6BinnedMoveCirclePre <-data_table%>%
    filter(Colony=="6"& Nest=="Circle")%>%
    mutate(Bin =
             if_else(Y <= BinCoordAssignMoveCircle$Y[1],1,
                     if_else(Y >= BinCoordAssignMoveCircle$Y[1] & 
                               Y <= BinCoordAssignMoveCircle$Y[2],2,
                             if_else(Y >= BinCoordAssignMoveCircle$Y[2] & 
                                       Y <= BinCoordAssignMoveCircle$Y[3],3,
                                     if_else(Y >= BinCoordAssignMoveCircle$Y[3] & 
                                               Y <= BinCoordAssignMoveCircle$Y[4],4,
                                             if_else(Y >= BinCoordAssignMoveCircle$Y[4] & 
                                                       Y <= BinCoordAssignMoveCircle$Y[5],5,
                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[5] & 
                                                               Y <= BinCoordAssignMoveCircle$Y[6],6,
                                                             if_else(Y >= BinCoordAssignMoveCircle$Y[6] & 
                                                                       Y <= BinCoordAssignMoveCircle$Y[7],7,
                                                                     if_else(Y >= BinCoordAssignMoveCircle$Y[7],8,0
                                                                     )))))))))
  
  Colony6PreBinned<<-full_join(Colony6BinnedMoveTubePre,Colony6BinnedMoveCirclePre)
}

CoordBinnedMovePre(Colony6Pre)

Colony5PreBinned<-Colony5PreBinned%>%
  filter(Bin=="0")
  group_by(Bin)%>%
  summarise(n=n())
  Colony6PreBinned
  Colony6PreBinned
  
write.csv(Colony6PreBinned,"Colony6PreBinnedZero.csv",row.names = FALSE)
Colony6PreBinnedCheck<-Colony6PreBinned%>%
  filter(Bin=="0")
  group_by(Bin)%>%
  summarise(n=n())
  Colony6PreBinnedCheck
Colony7PreBinned<-Colony7PreBinned%>%
  select(-c(X,Y))

  filter(Bin=="0")%>%
  group_by(Bin)%>%
  summarise(n=n())

Colony8PreBinned<-Colony8PreBinned%>%
  select(-c(X,Y))

  filter(Bin=="0")%>%
  group_by(Bin)%>%
  summarise(n=n())

Colony9PreBinned<-Colony9PreBinned%>%
  select(-c(X,Y))

  filter(Bin=="0")%>%
  group_by(Bin)%>%
  summarise(n=n())

Colony11PreBinned<-Colony11PreBinned%>%
  select(-c(X,Y))

  filter(Bin=="0")%>%
  group_by(Bin)%>%
  summarise(n=n())

Colony13PreBinned<-Colony13PreBinned%>%
  select(-c(X,Y))

  filter(Bin=="0")%>%
  group_by(Bin)%>%
  summarise(n=n())

Colony17PreBinned<-Colony17PreBinned%>%
  select(-c(X,Y))

  filter(Bin=="0")%>%
  group_by(Bin)%>%
  summarise(n=n())

Colony18PreBinned<-Colony18PreBinned%>%
  select(-c(X,Y))

  filter(Bin=="0")%>%
  group_by(Bin)%>%
  summarise(n=n())

Colony20PreBinned<-Colony20PreBinned%>%
  select(-c(X,Y))

  filter(Bin=="0")%>%
  group_by(Bin)%>%
  summarise(n=n())

#Density Transformation
AggnAssayDensity<-Colony5AggnBinned%>%
  full_join(Colony6AggnBinned)%>%
  full_join(Colony7AggnBinned)%>%
  full_join(Colony8AggnBinned)%>%
  full_join(Colony9AggnBinned)%>%
  full_join(Colony13AggnBinned)%>%
  filter(Bin != 0) %>%
  group_by(Colony,Nest,Seconds)%>%
  mutate(Count=n())%>%
  group_by(Colony,Nest,Seconds,Bin)%>%
  mutate(BinCount=n(),PropWorkers=BinCount/Count,AvgSpeed=mean(AntLength.sec))%>%
  ungroup()%>%
  select(c(Colony,Nest,Trial,Seconds,Bin,PropWorkers,AvgSpeed))%>%
  distinct()

AggnAssayDensityNull<-AggnAssayDensity%>%
  select(Colony,Nest,Trial,Seconds)%>%
  distinct()

AggnAssayDensityBinsNull<-left_join(AggnAssayDensityNull,BinsNullFullMove)

AggnAssayDensityFull<-full_join(AggnAssayDensityBinsNull,AggnAssayDensity)%>%
  group_by(Colony,Nest,Seconds)%>%
  mutate(PropWorkers=ifelse(is.na(PropWorkers),0,PropWorkers),
         AvgSpeed=ifelse(is.na(AvgSpeed),0,AvgSpeed))%>%
  select(c(Colony,Nest,Trial,Seconds,Bin,PropWorkers,AvgSpeed))%>%
  distinct()%>%
  drop_na()

AggnAssayDensity1<-Colony11AggnBinned%>%
  full_join(Colony17AggnBinned)%>%
  full_join(Colony18AggnBinned)%>%
  full_join(Colony20AggnBinned)%>%
  group_by(Colony,Nest,Seconds)%>%
  mutate(Count=n())%>%
  group_by(Colony,Nest,Seconds,Bin)%>%
  mutate(BinCount=n(),PropWorkers=BinCount/Count,AvgSpeed=mean(AntLength.sec))%>%
  ungroup()%>%
  select(c(Colony,Nest,Trial,Seconds,Bin,PropWorkers,AvgSpeed))%>%
  distinct()

AggnAssayDensityNull1<-AggnAssayDensity1%>%
  select(Colony,Nest,Trial,Seconds)%>%
  distinct()

AggnAssayDensityBinsNull1<-full_join(AggnAssayDensityNull1,BinsNullFullMove)

AggnAssayDensityFull1<-full_join(AggnAssayDensityBinsNull1,AggnAssayDensity1)%>%
  group_by(Colony,Nest,Seconds)%>%
  mutate(PropWorkers=ifelse(is.na(PropWorkers),0,PropWorkers),
         AvgSpeed=ifelse(is.na(AvgSpeed),0,AvgSpeed))%>%
  select(c(Colony,Nest,Trial,Seconds,Bin,PropWorkers,AvgSpeed))%>%
  distinct()%>%
  drop_na()
PreAssayDensity
PreAssayDensity<-Colony5PreBinned%>%
  full_join(Colony6PreBinned)%>%
  full_join(Colony7PreBinned)%>%
  full_join(Colony8PreBinned)%>%
  full_join(Colony9PreBinned)%>%
  full_join(Colony13PreBinned)%>%
  group_by(Colony,Nest,Seconds)%>%
  mutate(Count=n())%>%
  group_by(Colony,Nest,Seconds,Bin)%>%
  mutate(BinCount=n(),PropWorkers=BinCount/Count,AvgSpeed=mean(AntLength.sec))%>%
  ungroup()%>%
  select(c(Colony,Nest,Trial,Seconds,Bin,PropWorkers,AvgSpeed))%>%
  distinct()

PreAssayDensityNull<-PreAssayDensity%>%
  select(Colony,Nest,Trial,Seconds)%>%
  distinct()

PreAssayDensityBinsNull<-full_join(PreAssayDensityNull,BinsNullFull)

PreAssayDensityFull<-full_join(PreAssayDensityBinsNull,PreAssayDensity)%>%
  group_by(Colony,Nest,Seconds)%>%
  mutate(PropWorkers=ifelse(is.na(PropWorkers),0,PropWorkers),
         AvgSpeed=ifelse(is.na(AvgSpeed),0,AvgSpeed))%>%
  select(c(Colony,Nest,Trial,Seconds,Bin,PropWorkers,AvgSpeed))%>%
  distinct()%>%
  drop_na()

#TAGS need to be the difference in the avg speeds, not IQR  
#Also SFZ & Occur zones v. avg movement speeds

PreAssayDensity1<-Colony11PreBinned%>%
  full_join(Colony17PreBinned)%>%
  full_join(Colony18PreBinned)%>%
  full_join(Colony20PreBinned)%>%
  group_by(Colony,Nest,Seconds)%>%
  mutate(Count=n())%>%
  group_by(Colony,Nest,Seconds,Bin)%>%
  mutate(BinCount=n(),PropWorkers=BinCount/Count,AvgSpeed=mean(AntLength.sec))%>%
  ungroup()%>%
  select(c(Colony,Nest,Trial,Seconds,Bin,PropWorkers,AvgSpeed))%>%
  distinct()

PreAssayDensityNull1<-PreAssayDensity1%>%
  select(Colony,Nest,Trial,Seconds)%>%
  distinct()

PreAssayDensityBinsNull1<-full_join(PreAssayDensityNull1,BinsNullFull)

PreAssayDensityFull1<-full_join(PreAssayDensityBinsNull1,PreAssayDensity1)%>%
  group_by(Colony,Nest,Seconds)%>%
  mutate(PropWorkers=ifelse(is.na(PropWorkers),0,PropWorkers),
         AvgSpeed=ifelse(is.na(AvgSpeed),0,AvgSpeed))%>%
  select(c(Colony,Nest,Trial,Seconds,Bin,PropWorkers,AvgSpeed))%>%
  distinct()%>%
  drop_na()

FullAssayDensity<-AggnAssayDensity%>%
  full_join(AggnAssayDensity1)%>%
  full_join(PreAssayDensity)%>%
  full_join(PreAssayDensity1)

FullAssayDensity %>% 
  filter(Trial == "Pre") %>%
  mutate(MeanProp = mean(PropWorkers)) %>%
  ungroup() %>%
  select(c(MeanProp)) %>%
  distinct()
  mutate(Count = n()) %>%
  ungroup() %>%
  mutate(AvgCount = mean(Count), StdDev = sd(Count)) %>%
  select(c(AvgCount), StdDev) %>%
  distinct()



summary(lmer(AvgSpeed~PropWorkers*Nest*Trial+(Bin^2)*Bin+Seconds+(1|Colony),FullAssayDensity))
summary(lmer(AvgSpeed~PropWorkers*Nest + (Bin^2)*Bin+Seconds+(1|Colony),FullAssayDensity%>%filter(Trial=="Aggn")))
summary(lmer(AvgSpeed~PropWorkers*Nest + (Bin^2)*Bin+Seconds+(1|Colony),FullAssayDensity%>%filter(Trial=="Pre")))
r.squaredGLMM(lmer(AvgSpeed~PropWorkers*Nest*Trial+(Bin^2)*Bin+Seconds+(1|Colony),FullAssayDensity))

CircleAggn.Density<-ggplot(data=FullAssayDensity%>%filter(Trial=="Aggn" & Nest == "Circle")%>%drop_na(),
                           aes(x=PropWorkers,y=AvgSpeed)) +
  ggtitle("Circle Aggn")+
  geom_point(key_glyph = large_points,size=2.5,alpha=0.15,color="blue", shape = 16) +
  geom_smooth(method='lm',se=FALSE,size=1.5,color="black")+
  theme_pubclean()+
  theme(axis.ticks = element_blank(),
        axis.text.y = element_text(size= 18,color="black",family="Arial"),
        axis.text.x = element_text(size = 18, color = "black",family="Arial"),
        axis.title = element_blank(),
        plot.title=element_text(size=20,family="Arial", face = "bold"),
        legend.key=element_blank()) +
  xlab(NULL)+
  ylab(NULL)+
  xlim(0, 1) +
  ylim(0, 0.1)

TubeAggn.Density<-ggplot(data=FullAssayDensity%>%filter(Trial=="Aggn" & Nest == "Tube")%>%drop_na(),
                         aes(x=PropWorkers,y=AvgSpeed)) +
  ggtitle("Tube Aggn")+
  geom_point(key_glyph = large_points,size=2.5,alpha=0.15,color="red", shape = 17) +
  geom_smooth(method='lm',se=FALSE,size=1.5,color="black",linetype="dashed")+
  theme_pubclean()+
  theme(axis.ticks = element_blank(),
        axis.text.y = element_text(size= 16,color="white",family="Arial"),
        axis.text.x = element_text(size = 16, color = "black",family="Arial"),
        axis.title = element_blank(),
        plot.title=element_text(size=20,family="Arial", face = "bold"),
        legend.key=element_blank())+
  xlab(NULL)+
  ylab(NULL)+
  guides(shape = guide_legend(override.aes = list(alpha = 0.75))) +
  xlim(0, 1) +
  ylim(0, 0.1) 


CirclePre.Density<-ggplot(data=FullAssayDensity%>%filter(Trial=="Pre" & Nest == "Circle")%>%drop_na(),aes(x=PropWorkers,y=AvgSpeed)) +
  ggtitle("Circle Baseline")+
  geom_point(key_glyph = large_points,size=2.5,alpha=0.15,color="blue", shape = 16) +
  geom_smooth(method='lm',se=FALSE,size=1.5,color="black")+
  theme_pubclean()+
  theme(axis.text.y = element_text(size= 18,color="black",family="Arial"),
        axis.text.x = element_text(size = 18, color = "black",family="Arial"),
        axis.title = element_blank(),
        plot.title=element_text(size=20,family="Arial", face = "bold"),
        legend.key=element_blank()) +
  xlab(NULL)+
  ylab(NULL)+
  xlim(0, 1) +
  ylim(0, 0.1) 

TubePre.Density<-ggplot(data=FullAssayDensity%>%filter(Trial=="Pre" & Nest == "Tube")%>%drop_na(),
                        aes(x=PropWorkers,y=AvgSpeed)) +
  ggtitle("Tube Baseline")+
  geom_point(key_glyph = large_points,size=2.5,alpha=0.15,color="red", shape = 17) +
  geom_smooth(method='lm',se=FALSE,size=1.5,color="black",linetype="dashed")+
  theme_pubclean()+
  theme(axis.ticks = element_blank(),
        axis.text.y = element_text(size= 18,color="white",family="Arial"),
        axis.text.x = element_text(size = 18, color = "black",family="Arial"),
        axis.title = element_blank(),
        plot.title=element_text(size=20,family="Arial", face = "bold"),
        legend.key=element_blank())+
  xlab(NULL)+
  ylab(NULL)+
  guides(shape = guide_legend(override.aes = list(alpha = 0.75)))+
  xlim(0, 1) +
  ylim(0, 0.1) 

SpeedDensity<-ggarrange(CirclePre.Density, TubePre.Density, 
                        CircleAggn.Density, TubeAggn.Density,
                     labels = c("A", "B","C","D"),
                     font.label = list(size = 24,
                                       family="Arial"),
                     ncol = 2, nrow = 2)

annotate_figure(SpeedDensity,
                top = NULL,
                bottom = text_grob("Worker density through the nest", color = "black",
                                   size = 26, x = 0.53,family="Arial"),
                left = text_grob("Average ant-lengths / sec", color = "black",
                                 size = 26, rot = 90,family="Arial"),
                right = NULL
)


Pre.Density<-ggplot(data=FullAssayDensity%>%filter(Trial=="Pre")%>%drop_na(),
                    aes(x=PropWorkers,y=AvgSpeed,color=fct_rev(Nest),linetype=fct_rev(Nest),
                                                                                    shape = fct_rev(Nest))) +
  ggtitle("Baseline")+
  geom_point(key_glyph = large_points,size=2,alpha=0.33) +
  geom_smooth(method='lm',se=FALSE,size=1.25,color="black")+
  theme_pubclean()+
  theme(axis.line = element_line(color="black", size = 0.3), 
        axis.ticks = element_blank(),
        axis.text.y = element_text(size= 14,color="white"),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.title = element_blank(),
        legend.justification = c(1, 1),
        legend.text=element_text(size = 16),
        legend.title=element_text(size=18),
        legend.key=element_blank())+
  labs(color="Nest",linetype="Nest", shape = "Nest")+
  xlab(NULL)+
  ylab(NULL)+
  scale_color_manual(breaks = c("Tube", "Circle"), 
                     name="Nest",
                     values=c("red", "blue"),
                     labels=c("Tube","Circle"))+
  guides(shape = guide_legend(override.aes = list(alpha = 0.75)))+
  ylim(0,0.1)

Density.Fig<-ggarrange(Pre.Density, Aggn.Density,
          labels = c("A", "B"),
          ncol = 2, nrow = 1,
          common.legend = TRUE)

annotate_figure(Density.Fig,
                top = NULL,
                bottom = text_grob("Worker density through the nest", color = "black",
                                   size = 18, x = 0.53),
                left = text_grob("Average ant-lengths/sec", color = "black",
                                 size = 18, rot = 90),
                right = NULL
)

large_points <- function(data, params, size) {
  # Multiply by some number
  data$size <- data$size * 2
  draw_key_point(data = data, params = params, size = size)
}

#Density & Speed Heatmaps (not using this)

ggplot(data=AggnAssayTest%>%filter(Colony=="6",Nest=="Circle",Seconds<10), aes(X, Y, fill= AntLength.sec)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="blue") +
  theme_ipsum()

ggplot(data=AggnAssayTest%>%filter(Colony=="6",Nest=="Circle",Seconds<10)) +
  stat_density_2d(aes(x=X, y=Y, color=AntLength.sec, fill=AntLength.sec), 
                  geom="polygon", bins=4, alpha=0.05)
  scale_color_manual(values=c("Workers"="red","Brood"="blue","Standard"="purple2","Random"="orange1")) +
  scale_fill_manual(values=c("Workers"="red","Brood"="blue","Standard"="purple2","Random"="orange1")) +
  theme_set+
  xlab("X (cm)")+
  ylab("Y (cm)")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        legend.text=element_text(size = 12),
        legend.title = element_blank(),
        plot.title=element_text(size=18,face="bold"))+
  ggtitle("Colony 17 distributions")+
  facet_grid(Nest~CoordType2)+
  theme(strip.text.x = element_text(
    size = 12, color = "black"),
    strip.text.y = element_text(
      size = 12, color = "black"))+
  xlim(0,7.5)+
  ylim(-0.25,5)