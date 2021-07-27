# Title: Checking Stats - Foster et al. Current Zoology (2017)
# Author: Greg Chism, Email: gchism@email.arizona.edu
# This code may be reproduced without crediting the author and under any condition.

# Here I replicate the statistical tests used in the aforementioned manuscript to the best of my ability

# Packages used
install.packages ("afex")
install.packages ("lme4")
library (afex)
library (lme4)
library (car)
library (tidyverse)
library (ggpubr)

#Manuscript describes using a GLMM according to the formula
# Snail survival ~ snall shell width (mm) + Experimental density + 
# average height snails climbed (cm) +
# random effects of sea star ID and replicate (trial) ID

# Using R version 3.6.2 I utilized the following statistical analysis 
# analyzing the original dataset given to me by lead author Will Foster:

summary(full.mod)
full.mod<-glmer ( #GLMM saved to object
  Survived_1.0 ~ #binary response variable
    AverageHeight_cm + Snail_Width_mm + Density + #fixed effects
    (1|SeaStar_ID) + (1|Trial_ID), #random effects
  family=
    binomial( #testing against a binomial distribution
      link = 
        "logit" #represents the logarithm of the odds (p/1-p) of the response variable
      ),
  data=Foster_etal_CurrentZoo2017 #dataset given to me
  )
r.squaredGLMM(full.mod)
# A type 2 anova is used here as there are more than two main effects being tested
# Note, no test for interactions is produced in the manuscripts due to 
# a large difference in the size of each main effect (e.g. number of sea stars) 

Anova (full.mod, #Type 2 anova examining the influence of one main effect after another and so on.
      type = 2, 
      test.statistic = "Chisq"
      )

# Results are very different than prescribed in the manuscript, only significant fixed effect is snail shell width

# Next, per the methods, a Liklihood-ratio test was conducted on the effects
# I utilized the following statistical test in attempt to duplicate this result

model.LR<-
  mixed ( #utilized a mixed effect model
    Survived_1.0 ~ #binary response variable
      AverageHeight_cm + Snail_Width_mm + Density + #fixed effects
      (1|SeaStar_ID) + (1|Trial_ID), #random effects
    data = Foster_etal_CurrentZoo2017, #dataset given to me
    family = binomial, #testing against a binomial distribution, specifies a GLMM
    method = "LRT") #Liklihood-ratio test

# I again utilized a type 2 anova to test more than 2 fixed effects

anova ( # Defaults to type 3 anova producing slightly less powerful results than type 2 
        # since there are no significant interactions
  model.LR # GLMM produced above
)

full.mod<-glm(Survived_1.0 ~ AverageHeight_cm  Snail_Width_mm + Density, data=Foster_etal_CurrentZoo2017,
    family= binomial)
# Results are very different than prescribed in the manuscript, only significant fixed effect is snail shell width

  
# Pearson's Correlations
# Snail width v. peak height climbed
cor.test( ~ Snail_Width_mm + Peak_Height_cm,
            data=Foster_etal_CurrentZoo2017,
            method = "pearson",
            conf.level = 0.95)

# Snail width v. average height climbed
cor.test( ~ Snail_Width_mm + AverageHeight_cm,
          data=Foster_etal_CurrentZoo2017,
          method = "pearson",
          conf.level = 0.95)

# Snail width v. average time out of the water
cor.test( ~ Snail_Width_mm + Average_Time_Out,
          data=Foster_etal_CurrentZoo2017,
          method = "pearson",
          conf.level = 0.95)

# Peak height climbed v. average height climbed
cor.test( ~ Peak_Height_cm + AverageHeight_cm,
          data=Foster_etal_CurrentZoo2017,
          method = "pearson",
          conf.level = 0.95)

# Peak height climbed v. average time out of the water
cor.test( ~ Peak_Height_cm + Average_Time_Out,
          data=Foster_etal_CurrentZoo2017,
          method = "pearson",
          conf.level = 0.95)

# Peak height climbed v. average time out of the water
cor.test( ~ AverageHeight_cm + Average_Time_Out,
          data=Foster_etal_CurrentZoo2017,
          method = "pearson",
          conf.level = 0.95)

# Figure 1: Assessing the relationship between snail shell width and snail survival
# -- Figure in manuscript represents data cropped below 15mm, cutting the left tail. 
ggplot(Foster_etal_CurrentZoo2017,aes(Snail_Width_mm,as.factor(Survived_1.0)))+
  geom_boxplot()

# Figure 2: Assessing the relationship between average height snails climbed and snail survival
# -- Figure in manuscript represents data cropped after 3cm, cutting the right tail.
ggplot(Foster_etal_CurrentZoo2017,aes(AverageHeight_cm,as.factor(Survived_1.0)))+
  geom_boxplot()

# New Fig 1 planned
ggplot(Foster_etal_CurrentZoo2017, aes(x=Snail_Width_mm, y=Survived_1.0)) + 
  geom_jitter(height = 0) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=F,color="black")+
  theme_pubr()+
  labs_pubr()+
  labs(y="Survival",x="Snail shell width (mm)")
  
