##############################BS Experiment Summer 2019#######################################

###Set Working Directory

####all packages used here####
library(plyr)
library(ggplot2) 
library(ggpubr) 
library(lme4)
library(multcomp)
library(car)
library(AICcmodavg)
library(lmtest)
library(dplyr) 
library(tidyr)
library(cowplot)


#### * LOADING DATASET * ####
completeDataset <- read.csv("completeDataset.csv", header = T)

  #checking dataset
dim(completeDataset)
head(completeDataset)
tail(completeDataset)
summary(completeDataset)
attach(completeDataset) #to access any variables of dataset

  #excluding males
summary(comments)
femData <- completeDataset[-which(comments == 'male'),]
dim(femData)
head(femData)
tail(femData)
summary(femData)
attach(femData)
summary(comments)

  #get only bees from 1st brood 
summary(Brood)
data.BS <- femData[which(Brood == '1stBrood'),]
dim(data.BS)
head(data.BS)
tail(data.BS)
summary(data.BS)
attach(data.BS)
summary(Brood)

#order dataset by treatment group and QueenID
data.BS <- data.BS[with(data.BS, order(Treatment, QueenID)),]
dim(data.BS)
head(data.BS)
tail(data.BS)
summary(data.BS)

#Body Size complete####
summary(Avg.mm)

  #excluding NA values 
bodysize <- subset(data.BS, !is.na(Avg.mm))
dim(bodysize)
head(bodysize)
tail(bodysize)
attach(bodysize)
summary(Avg.mm)

mu <- ddply(bodysize, "Treatment", summarise, grp.mean=mean(Avg.mm))
head(mu)

#Development time complete####
summary(Time.development.days)

  #excluding missing values
devtime <- subset(data.BS, !is.na(Time.development.days))
dim(devtime)
head(devtime)
tail(devtime)
summary(devtime)
attach(devtime)
summary(Time.development.days)

mi <- ddply(devtime, "Treatment", summarise, grp.mean=mean(Time.development.days))
head(mi)

#Dataset with samples with Body Size and Development time information####

  #excluding NA values 
bodytime <- subset(data.BS, !is.na(Avg.mm) & !is.na(Time.development.days))
head(bodytime)
dim(bodytime)
summary(bodytime)
attach(bodytime)

#Sucrose####
summary(Sucrose.summary)

sucrose <- bodytime[-which(Sucrose.summary == ""),]
dim(sucrose)
head(sucrose)
tail(sucrose)
summary(sucrose)

suc.con <- subset(sucrose, !is.na(Sucrose.conc))

#Learning graphics####
summary(LearningTraining.summary)
summary(LearningTest.summary)

  #Learning Training 
learningTraining <- bodytime[-which(LearningTraining.summary == ""),]
dim(learningTraining)
head(learningTraining)
tail(learningTraining)
summary(learningTraining)

  #Learning Test
learningTest <- bodytime[-which(LearningTest.summary == ""),]
dim(learningTest)
head(learningTest)
tail(learningTest)
summary(learningTest)


#Survival####
summary(Survival.hours)

survival <- subset(bodytime, !is.na(Survival.hours))
survival <- survival[-which(Survival.hours >= 60),]
head(survival)
dim(survival)
summary(survival$Survival.hours)

cdata <- ddply(survival, c("Treatment"), summarise,
               N    = sum(Survival.hours),
               mean = mean(Survival.hours, na.rm=TRUE),
               sd   = sd(Survival.hours, na.rm=TRUE),
               se   = sd / sqrt(N))
cdata


#### * STATS * ####

#Body size stats####

  #testing whether a random sample of data comes from a normal distribution
shapiro.test(bodytime$Avg.mm)
ggdensity(bodytime$Avg.mm)
hist(bodytime$Avg.mm)

          #obs.: data doesn't come from a normal distribution: Gamma
BSnull <- glmer(Avg.mm ~1 + (1 | ColonyID_queen), data=bodytime, family=Gamma(link = "inverse"))
BS1 <- glmer(Avg.mm ~ Treatment + (1 | ColonyID_queen), data=bodytime, family=Gamma(link = "inverse"))
BS2 <- glmer(Avg.mm ~ Treatment + Time.development.days + (1 | ColonyID_queen), data=bodytime, family=Gamma(link = "inverse"))
BS3 <- glmer(Avg.mm ~ Treatment * Time.development.days + (1 | ColonyID_queen), data=bodytime, family=Gamma(link = "inverse"))
BS4 <- glmer(Avg.mm ~ Time.development.days + (1 | ColonyID_queen), data=bodytime, family=Gamma(link = "inverse"))

AIC(BSnull, BS1, BS2, BS3, BS4) # choose the best fit model (lower AIC value)

#for best fit model
summary(BS2)
anova(BSnull, BS2)
posthoc <- glht(BS2, linfct = mcp(Treatment = "Tukey"))
summary(posthoc)

  #testing differences in BS between treatments

#body size vs rearing history 
wilcox.test(Avg.mm~Treatment, data=bodytime)


#Development time stats####

  #testing whether a random sample of data comes from a normal distribution
shapiro.test(bodytime$Time.development.days)
ggdensity(bodytime$Time.development.days)
hist(bodytime$Time.development.days)

          #obs.: data doesn't come from a normal distribution: Gamma

DTnull <- glmer(Time.development.days ~1 + (1 | ColonyID_queen), data=bodytime, family=Gamma(link = "inverse"))
DT1 <- glmer(Time.development.days ~ Treatment + (1 | ColonyID_queen), data=bodytime, family=Gamma(link = "inverse"))

AIC(DTnull, DT1) # choose the best fit model (lower AIC value)

#for best fit model
summary(DT1)
anova(DTnull, DT1)
posthoc <- glht(DT1, linfct = mcp(Treatment = "Tukey"))
summary(posthoc)

  #testing differences in Development Time between treatments
#developmental time vs rearing history 
wilcox.test(Time.development.days~Treatment, data=bodytime)


#Correlation BS vs Development time####
cor.test(Avg.mm, Time.development.days, data=bodytime, method="spearman")


#Survival####

compare_means(Survival.hours ~ Treatment,  data = survival, method = "anova")


  #testing whether a random sample of data comes from a normal distribution
shapiro.test(survival$Survival.hours)
ggdensity(survival$Survival.hours)
hist(survival$Survival.hours)

          #obs.: data doesn't come from a normal distribution: Poisson 
Snull <- glmer(Survival.hours ~1 + (1 | ColonyID_queen), data=survival, family=poisson(link="log"))
S1 <- glmer(Survival.hours ~ Treatment + (1 | ColonyID_queen), data=survival, family=poisson(link="log"))
S2 <- glmer(Survival.hours ~ Avg.mm + (1 | ColonyID_queen), data=survival, family=poisson(link="log"))
S3 <- glmer(Survival.hours ~ Time.development.days + (1 | ColonyID_queen), data=survival, family=poisson(link="log"))
S4 <- glmer(Survival.hours ~ Treatment + Avg.mm + Time.development.days + (1 | ColonyID_queen), data=survival, family=poisson(link="log"))
S5 <- glmer(Survival.hours ~ Treatment + Avg.mm * Time.development.days + (1 | ColonyID_queen), data=survival, family=poisson(link="log"))
S6 <- glmer(Survival.hours ~ Treatment + Avg.mm + (1 | ColonyID_queen), data=survival, family=poisson(link="log"))
S7 <- glmer(Survival.hours ~ Treatment + Time.development.days + (1 | ColonyID_queen), data=survival, family=poisson(link="log"))
S8 <- glmer(Survival.hours ~ Avg.mm + Time.development.days + (1 | ColonyID_queen), data=survival, family=poisson(link="log"))
S9 <- glmer(Survival.hours ~ Avg.mm * Time.development.days + (1 | ColonyID_queen), data=survival, family=poisson(link="log"))
S10 <- glmer(Survival.hours ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days + (1 | ColonyID_queen), data=survival, family=poisson(link="log"))
S11 <- glmer(Survival.hours ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=survival, family=poisson(link="log"))
S12 <- glmer(Survival.hours ~ Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=survival, family=poisson(link="log"))
S13 <- glmer(Survival.hours ~ Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=survival, family=poisson(link="log"))
S14 <- glmer(Survival.hours ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=survival, family=poisson(link="log"))
S15 <- glmer(Survival.hours ~ Treatment + Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=survival, family=poisson(link="log"))
S16 <- glmer(Survival.hours ~ Treatment + Avg.mm + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=survival, family=poisson(link="log"))
S17 <- glmer(Survival.hours ~ Treatment + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=survival, family=poisson(link="log"))
S18 <- glmer(Survival.hours ~ Avg.mm + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=survival, family=poisson(link="log"))
S19 <- glmer(Survival.hours ~ Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=survival, family=poisson(link="log"))

AIC(Snull, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18, S19) # choose the best fit model (lower AIC value)

#for best fit model
summary(S6)
anova(Snull, S6)
posthoc <- glht(S6, linfct = mcp(Treatment = "Tukey"))
summary(posthoc)


#Learning stats####


          #obs.: data is binomial (yes - 1 or no - 0): binomal

#LearningTraining
LTRnull <- glmer(LearningTraining ~1 + (1 | ColonyID_queen), data=learningTraining, family=binomial(link = "logit"))
LTR1 <- glmer(LearningTraining ~ Treatment + (1 | ColonyID_queen), data=learningTraining, family=binomial(link = "logit"))
LTR2 <- glmer(LearningTraining ~ Avg.mm + (1 | ColonyID_queen), data=learningTraining, family=binomial(link = "logit"))
LTR3 <- glmer(LearningTraining ~ Time.development.days + (1 | ColonyID_queen), data=learningTraining, family=binomial(link = "logit"))
LTR4 <- glmer(LearningTraining ~ Treatment + Avg.mm + Time.development.days + (1 | ColonyID_queen), data=learningTraining, family=binomial(link = "logit"))
LTR5 <- glmer(LearningTraining ~ Treatment + Avg.mm * Time.development.days + (1 | ColonyID_queen), data=learningTraining, family=binomial(link = "logit"))
LTR6 <- glmer(LearningTraining ~ Treatment + Avg.mm + (1 | ColonyID_queen), data=learningTraining, family=binomial(link = "logit"))
LTR7 <- glmer(LearningTraining ~ Treatment + Time.development.days + (1 | ColonyID_queen), data=learningTraining, family=binomial(link = "logit"))
LTR8 <- glmer(LearningTraining ~ Avg.mm + Time.development.days + (1 | ColonyID_queen), data=learningTraining, family=binomial(link = "logit"))
LTR9 <- glmer(LearningTraining ~ Avg.mm * Time.development.days + (1 | ColonyID_queen), data=learningTraining, family=binomial(link = "logit"))
LTR10 <- glmer(LearningTraining ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days + (1 | ColonyID_queen), data=learningTraining, family=binomial(link = "logit"))
LTR11 <- glmer(LearningTraining ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=learningTraining, family=binomial(link = "logit"))
LTR12 <- glmer(LearningTraining ~ Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=learningTraining, family=binomial(link = "logit"))
LTR13 <- glmer(LearningTraining ~ Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=learningTraining, family=binomial(link = "logit"))
LTR14 <- glmer(LearningTraining ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=learningTraining, family=binomial(link = "logit"))
LTR15 <- glmer(LearningTraining ~ Treatment + Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=learningTraining, family=binomial(link = "logit"))
LTR16 <- glmer(LearningTraining ~ Treatment + Avg.mm + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=learningTraining, family=binomial(link = "logit"))
LTR17 <- glmer(LearningTraining ~ Treatment + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=learningTraining, family=binomial(link = "logit"))
LTR18 <- glmer(LearningTraining ~ Avg.mm + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=learningTraining, family=binomial(link = "logit"))
LTR19 <- glmer(LearningTraining ~ Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=learningTraining, family=binomial(link = "logit"))


AIC(LTRnull, LTR1, LTR2, LTR3, LTR4, LTR5, LTR6, LTR7, LTR8, LTR9, LTR10, LTR11, LTR12, LTR13, LTR14, LTR15, LTR16, LTR17, LTR18, LTR19) # choose the best fit model (lower AIC value)

#for best fit model
summary(LTR2)
anova(LTRnull, LTR2)
posthoc <- glht(LTR2, linfct = mcp(Treatment = "Tukey"))
summary(posthoc)


#LearningTesting
LTnull <- glmer(LearningTest ~1 + (1 | ColonyID_queen) , data=learningTest, family=binomial(link = "logit"))
LT1 <- glmer(LearningTest ~ Treatment + (1 | ColonyID_queen) , data=learningTest, family=binomial(link = "logit"))
LT2 <- glmer(LearningTest ~ Avg.mm + (1 | ColonyID_queen) , data=learningTest, family=binomial(link = "logit"))
LT3 <- glmer(LearningTest ~ Time.development.days + (1 | ColonyID_queen) , data=learningTest, family=binomial(link = "logit"))
LT4 <- glmer(LearningTest ~ Treatment + Avg.mm + Time.development.days + (1 | ColonyID_queen), data=learningTest, family=binomial(link = "logit"))
LT5 <- glmer(LearningTest ~ Treatment + Avg.mm * Time.development.days + (1 | ColonyID_queen), data=learningTest, family=binomial(link = "logit"))
LT6 <- glmer(LearningTest ~ Treatment + Avg.mm + (1 | ColonyID_queen), data=learningTest, family=binomial(link = "logit"))
LT7 <- glmer(LearningTest ~ Treatment + Time.development.days + (1 | ColonyID_queen), data=learningTest, family=binomial(link = "logit"))
LT8 <- glmer(LearningTest ~ Avg.mm + Time.development.days + (1 | ColonyID_queen), data=learningTest, family=binomial(link = "logit"))
LT9 <- glmer(LearningTest ~ Avg.mm * Time.development.days + (1 | ColonyID_queen), data=learningTest, family=binomial(link = "logit"))
LT10 <- glmer(LearningTest ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days + (1 | ColonyID_queen), data=learningTest, family=binomial(link = "logit"))
LT11 <- glmer(LearningTest ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=learningTest, family=binomial(link = "logit"))
LT12 <- glmer(LearningTest ~ Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) , data=learningTest, family=binomial(link = "logit"))
LT13 <- glmer(LearningTest ~ Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) , data=learningTest, family=binomial(link = "logit"))
LT14 <- glmer(LearningTest ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) , data=learningTest, family=binomial(link = "logit"))
LT15 <- glmer(LearningTest ~ Treatment + Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) , data=learningTest, family=binomial(link = "logit"))
LT16 <- glmer(LearningTest ~ Treatment + Avg.mm + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) , data=learningTest, family=binomial(link = "logit"))
LT17 <- glmer(LearningTest ~ Treatment + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) , data=learningTest, family=binomial(link = "logit"))
LT18 <- glmer(LearningTest ~ Avg.mm + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) , data=learningTest, family=binomial(link = "logit"))
LT19 <- glmer(LearningTest ~ Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) , data=learningTest, family=binomial(link = "logit"))

AIC(LTnull, LT1, LT2, LT3, LT4, LT5, LT6, LT7, LT8, LT9, LT10, LT11, LT12, LT13, LT14, LT15, LT16, LT17, LT18, LT19) # choose the best fit model (lower AIC value)

#Sucrose####

          #obs.: we have data is binomial (yes - 1 or no - 0): binomal

SUnull <- glmer(Sucrose.response ~1 + (1 | ColonyID_queen), data=sucrose, family=binomial(link = "logit"))
SU1 <- glmer(Sucrose.response ~ Treatment + (1 | ColonyID_queen), data=sucrose, family=binomial(link = "logit"))
SU2 <- glmer(Sucrose.response ~ Avg.mm + (1 | ColonyID_queen), data=sucrose, family=binomial(link = "logit"))
SU3 <- glmer(Sucrose.response ~ Time.development.days + (1 | ColonyID_queen), data=sucrose, family=binomial(link = "logit"))
SU4 <- glmer(Sucrose.response ~ Treatment + Avg.mm + Time.development.days + (1 | ColonyID_queen), data=sucrose, family=binomial(link = "logit"))
SU5 <- glmer(Sucrose.response ~ Treatment + Avg.mm * Time.development.days + (1 | ColonyID_queen), data=sucrose, family=binomial(link = "logit"))
SU6 <- glmer(Sucrose.response ~ Treatment + Avg.mm + (1 | ColonyID_queen), data=sucrose, family=binomial(link = "logit"))
SU7 <- glmer(Sucrose.response ~ Treatment + Time.development.days + (1 | ColonyID_queen), data=sucrose, family=binomial(link = "logit"))
SU8 <- glmer(Sucrose.response ~ Avg.mm + Time.development.days + (1 | ColonyID_queen), data=sucrose, family=binomial(link = "logit"))
SU9 <- glmer(Sucrose.response ~ Avg.mm * Time.development.days + (1 | ColonyID_queen), data=sucrose, family=binomial(link = "logit"))
SU10 <- glmer(Sucrose.response ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days + (1 | ColonyID_queen), data=sucrose, family=binomial(link = "logit"))
SU11 <- glmer(Sucrose.response ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=sucrose, family=binomial(link = "logit"))
SU12 <- glmer(Sucrose.response ~ Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=sucrose, family=binomial(link = "logit"))
SU13 <- glmer(Sucrose.response ~ Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=sucrose, family=binomial(link = "logit"))
SU14 <- glmer(Sucrose.response ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=sucrose, family=binomial(link = "logit"))
SU15 <- glmer(Sucrose.response ~ Treatment + Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=sucrose, family=binomial(link = "logit"))
SU16 <- glmer(Sucrose.response ~ Treatment + Avg.mm + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=sucrose, family=binomial(link = "logit"))
SU17 <- glmer(Sucrose.response ~ Treatment + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=sucrose, family=binomial(link = "logit"))
SU18 <- glmer(Sucrose.response ~ Avg.mm + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=sucrose, family=binomial(link = "logit"))
SU19 <- glmer(Sucrose.response ~ Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=sucrose, family=binomial(link = "logit"))

AIC(SUnull, SU1, SU2, SU3, SU4, SU5, SU6, SU7, SU8, SU9, SU10, SU11, SU12, SU13, SU14, SU15, SU16, SU17, SU18, SU19) # choose the best fit model (lower AIC value)

#for best fit model
summary(SU9)
anova(SUnull, SU9)
posthoc <- glht(SU9, linfct = mcp(Treatment = "Tukey"))
summary(posthoc)


#Sucrose.concentration

#testing whether a random sample of data comes from a normal distribution
shapiro.test(suc.con$Sucrose.conc)
ggdensity(suc.con$Sucrose.conc)
hist(suc.con$Sucrose.conc)

          #obs.: data doesn't come from a normal distribution:  poisson

suc.con$Sucrose.conc <- suc.con$Sucrose.conc * 100 #The Poisson model is about counts. Count values are positive natural numbers including zero (0, 1, 2, 3, ...)

SCnull <- glmer(Sucrose.conc ~1 + (1 | ColonyID_queen), data=suc.con, family=poisson(link="log"))
SC1 <- glmer(Sucrose.conc ~ Treatment + (1 | ColonyID_queen), data=suc.con, family=poisson(link="log"))
SC2 <- glmer(Sucrose.conc ~ Avg.mm + (1 | ColonyID_queen), data=suc.con, family=poisson(link="log"))
SC3 <- glmer(Sucrose.conc ~ Time.development.days + (1 | ColonyID_queen), data=suc.con, family=poisson(link="log"))
SC4 <- glmer(Sucrose.conc ~ Treatment + Avg.mm + Time.development.days + (1 | ColonyID_queen), data=suc.con, family=poisson(link="log"))
SC5 <- glmer(Sucrose.conc ~ Treatment + Avg.mm * Time.development.days + (1 | ColonyID_queen), data=suc.con, family=poisson(link="log"))
SC6 <- glmer(Sucrose.conc ~ Treatment + Avg.mm + (1 | ColonyID_queen), data=suc.con, family=poisson(link="log"))
SC7 <- glmer(Sucrose.conc ~ Treatment + Time.development.days + (1 | ColonyID_queen), data=suc.con, family=poisson(link="log"))
SC8 <- glmer(Sucrose.conc ~ Avg.mm + Time.development.days + (1 | ColonyID_queen), data=suc.con, family=poisson(link="log"))
SC9 <- glmer(Sucrose.conc ~ Avg.mm * Time.development.days + (1 | ColonyID_queen), data=suc.con, family=poisson(link="log"))
SC10 <- glmer(Sucrose.conc ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days + (1 | ColonyID_queen), data=suc.con, family=poisson(link="log"))
SC11 <- glmer(Sucrose.conc ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=suc.con, family=poisson(link="log"))
SC12 <- glmer(Sucrose.conc ~ Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=suc.con, family=poisson(link="log"))
SC13 <- glmer(Sucrose.conc ~ Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=suc.con, family=poisson(link="log"))
SC14 <- glmer(Sucrose.conc ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=suc.con, family=poisson(link="log"))
SC15 <- glmer(Sucrose.conc ~ Treatment + Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=suc.con, family=poisson(link="log"))
SC16 <- glmer(Sucrose.conc ~ Treatment + Avg.mm + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=suc.con, family=poisson(link="log"))
SC17 <- glmer(Sucrose.conc ~ Treatment + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=suc.con, family=poisson(link="log"))
SC18 <- glmer(Sucrose.conc ~ Avg.mm + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=suc.con, family=poisson(link="log"))
SC19 <- glmer(Sucrose.conc ~ Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen), data=suc.con, family=poisson(link="log"))

AIC(SCnull, SC1, SC2, SC3, SC4, SC5, SC6, SC7, SC8, SC9, SC10, SC11, SC12, SC13, SC14, SC15, SC16, SC17, SC18, SC19) # choose the best fit model (lower AIC value)

#for best fit model
summary(SC1)
anova(SCnull, SC1)
posthoc <- glht(SC1, linfct = mcp(Treatment = "Tukey"))
summary(posthoc)

#### * GRAPHICS * ####

#Body Size####
h<-ggplot(bodytime, aes(x=Avg.mm, fill=Treatment, color=Treatment)) +
  geom_histogram(position="identity", alpha = 0.5, binwidth = 0.1)
h <- h + geom_vline(data=mu, aes(xintercept=grp.mean, color=Treatment),
                    linetype="dashed", size=0.7) # Add mean lines
h <- h + xlab("Maginal cell length (mm)")+ylab("Frequency")
h <- h + theme(legend.position = "right") + theme(legend.title = element_blank())
h <- h + theme_classic()
h <- h + scale_fill_brewer(name = "Rearing History", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen Reared", "Worker Reared")) + scale_color_brewer(name = "Rearing History", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen Reared", "Worker Reared"),palette = "Set1")
h

#Development time####
t<-ggplot(devtime, aes(x=Time.development.days, fill=Treatment, color=Treatment)) +
  geom_histogram(position="identity", alpha = 0.5, binwidth = 1)
t <- t + geom_vline(data=mi, aes(xintercept=grp.mean, color=Treatment),
                    linetype="dashed", size=.7) # Add mean lines
t <- t + labs(x = "Developmental time (Days)", y = "Frequency")
t <- t + theme(legend.title = element_blank()) + theme(legend.position = "right")
t <- t + theme_classic()
t <- t + scale_fill_brewer(name = "Rearing History", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen Reared", "Worker Reared")) + scale_color_brewer(name = "Rearing History", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen Reared", "Worker Reared"),palette = "Set1")
t

#Correlation Developmental Time X Body Size####

c<- ggscatter(bodytime, x = "Time.development.days", y = "Avg.mm",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Development time (Days)", ylab = "Maginal celll length (mm)")
c

c1 <- ggplot(bodytime, aes(x = Time.development.days, y = Avg.mm))
c1 <- c1 + geom_point(aes(color = Treatment)) +
  geom_smooth(aes(color = Treatment, fill = Treatment), method = lm) +
  scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
c1 <- c1 + labs(x = "Developmental time (Days)", y = "Maginal cell length (mm)")
c1 <- c1 + theme(legend.title = element_blank()) + theme(legend.position = "right")
c1 <- c1 + theme_classic()
c1 <- c1 + scale_fill_brewer(name = "Rearing History", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen Reared", "Worker Reared")) + scale_color_brewer(name = "Rearing History", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen Reared", "Worker Reared"),palette = "Set1")
c1

#Sucrose####

s<- ggplot(sucrose, aes(x=Sucrose.summary, fill=Treatment, color=Treatment)) +
  geom_bar(data=sucrose, stat="count", position="dodge", alpha = 0.5)
s <- s + labs(x = "Assay Response (Sucrose)", y = "Frequency")
s <- s + theme(legend.title = element_blank()) + theme(legend.position = "right")
s <- s + theme_classic()
s <- s + scale_fill_brewer(name = "Rearing History", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen Reared", "Worker Reared")) + scale_color_brewer(name = "Rearing History", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen Reared", "Worker Reared"),palette = "Set1")
s

s1 <- ggplot(sucrose,aes(x = Treatment,fill = Sucrose.summary)) + 
  geom_bar(position = "fill")+ylab("Proportion Responded")+scale_fill_discrete(name="Responded to Sucrose")
s1 <- s1 + labs(x = "Treatment", title = "Sucrose response", fill = "Sucrose response")
s1 <- s1 + theme(legend.title = element_blank()) + theme(legend.position = "right")
s1 <- s1 + theme_classic()
s1 <- s1 + scale_fill_brewer(palette = "Dark2") + scale_color_brewer(palette = "Dark2")
s1

s2 <- ggplot(sucrose, aes(x=Sucrose.conc, fill=Treatment, color=Treatment)) +
  geom_bar(data=sucrose, stat="count", position="dodge", alpha = 0.5)
s2 <- s2 + labs(x = "Sucrose Response (Concentration)", y = "Count", title = "Sucrose response (Concentration) versus Group reared")
s2 <- s2 + theme(legend.title = element_blank()) + theme(legend.position = "right")
s2 <- s2 + theme_classic()
s2 <- s2 + scale_fill_brewer(name = "Rearing History", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen Reared", "Worker Reared")) + scale_color_brewer(name = "Rearing History", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen Reared", "Worker Reared"),palette = "Set1")
s2

s3 <- ggplot(sucrose, aes(x=Sucrose.conc, fill=Treatment, color=Treatment)) +
  geom_bar(position = "fill", alpha = 0.5)+ylab("Proportion Responded")+scale_fill_discrete(name="Responded to Sucrose")
s3 <- s3 + labs(x = "Sucrose Response (Concentration)", title = "Sucrose response (Concentration) versus Group reared")
s3 <- s3 + theme(legend.title = element_blank()) + theme(legend.position = "right")
s3 <- s3 + theme_classic()
s3 <- s3 + scale_fill_brewer(name = "Rearing History", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen Reared", "Worker Reared")) + scale_color_brewer(name = "Rearing History", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen Reared", "Worker Reared"),palette = "Set1")
s3

s4 <-ggplot(sucrose, aes(x=Sucrose.conc, fill=Treatment, color=Treatment)) +
  geom_histogram(position="identity", alpha = 0.5, binwidth = 0.05)
s4 <- s4 + labs(x = "Sucrose Response (Concentration)", title = "Sucrose response (Concentration) versus Group reared")
s4 <- s4 + theme(legend.title = element_blank()) + theme(legend.position = "right")
s4 <- s4 + theme_classic()
s4 <- s4 + scale_fill_brewer(name = "Rearing History", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen Reared", "Worker Reared")) + scale_color_brewer(name = "Rearing History", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen Reared", "Worker Reared"),palette = "Set1")
s4

s5 <- ggplot(sucrose, aes(x=Sucrose.conc, y=Avg.mm, fill=Treatment, color=Treatment)) + 
  geom_point(alpha = 0.6, position = "jitter")
s5 <- s5 + labs(x = "Sucrose Response (Concentration)", y = "Maginal cell length (mm)", title = "Sucrose response versus Body size")
s5 <- s5 + theme(legend.title = element_blank()) + theme(legend.position = "right")
s5 <- s5 + theme_classic()
s5 <- s5 + scale_fill_brewer(name = "Rearing History", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen Reared", "Worker Reared")) + scale_color_brewer(name = "Rearing History", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen Reared", "Worker Reared"),palette = "Set1")
s5

s6 <- ggplot(sucrose, aes(x=Sucrose.conc, y=Avg.mm, fill=Treatment, color=Treatment))
s6 <- s6 + geom_point() +
geom_smooth(aes(color = Treatment, fill = Treatment), method = lm) +
  scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
s6 <- s6 + labs(x = "Sucrose Response (Concentration)", y = "Maginal cell length (mm)", title = "Sucrose response versus Body size")
s6 <- s6 + theme(legend.title = element_blank()) + theme(legend.position = "right")
s6 <- s6 + theme_classic()
s6 <- s6 + scale_fill_brewer(name = "Rearing History", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen Reared", "Worker Reared")) + scale_color_brewer(name = "Rearing History", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen Reared", "Worker Reared"),palette = "Set1")
s6

s7 <- ggscatter(sucrose, x = "Sucrose.conc", y = "Avg.mm",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Sucrose Response (Concentration)", ylab = "Maginal celll length (mm)")
s7

#Learning####

l <- ggplot(learningTraining,aes(x = Treatment,fill = LearningTraining.summary)) + 
  geom_bar(position = "fill")+ylab("Proportion Trained")
l <- l + labs(x = "Treatment", title = "Learning training", fill = "Learning training")
l <- l + theme(legend.title = element_blank()) + theme(legend.position = "right")
l <- l + theme_classic()
l <- l + scale_fill_brewer(palette = "Dark2") + scale_color_brewer(palette = "Dark2")
l

l1 <- ggplot(learningTest,aes(x = Treatment,fill = LearningTest.summary)) + 
  geom_bar(position = "fill")+ylab("Proportion Successful")
l1 <- l1 + labs(x = "Treatment", title = "Learning successful", fill = "Learning test")
l1 <- l1 + theme(legend.title = element_blank()) + theme(legend.position = "right")
l1 <- l1 + theme_classic()
l1 <- l1 + scale_fill_brewer(palette = "Dark2") + scale_color_brewer(palette = "Dark2")
l1

#Survival####
su <- ggplot(cdata, aes(x=Treatment, y=mean, fill = Treatment, color = Treatment)) + 
  geom_bar(position=position_dodge(), stat="identity", alpha = 0.5) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
su <- su + labs(x = "Treatment", title = "Survival")
su <- su + theme(legend.title = element_blank()) + theme(legend.position = "right")
su <- su + theme_classic()
su <- su + scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
su


su1 <- ggplot(survival, aes(x=Survival.hours, y=Avg.mm, fill=Treatment, color=Treatment))
su1 <- su1 + geom_point() +
  geom_smooth(aes(color = Treatment, fill = Treatment), method = lm) +
  scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
su1 <- su1 + labs(x = "Survival (hours)", y = "Maginal cell length (mm)", title = "Survival")
su1 <- su1 + theme(legend.title = element_blank()) + theme(legend.position = "right")
su1 <- su1 + theme_classic()
su1 <- su1 + scale_fill_brewer(name = "Rearing History", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen Reared", "Worker Reared")) + scale_color_brewer(name = "Rearing History", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen Reared", "Worker Reared"),palette = "Set1")
su1

su2 <- ggscatter(survival, x = "Survival.hours", y = "Avg.mm",
                add = "reg.line", conf.int = TRUE,
                cor.coef = TRUE, cor.method = "spearman",
                xlab = "Survival (hours)", ylab = "Maginal celll length (mm)")
su2


#Figures to export 
fig1 <- ggarrange(
  h,t,c, ncol = 2, nrow =  2,
  labels = c("A", "B", "C"),
  common.legend = TRUE, legend = "bottom"
)
fig1

figSucrose <- ggarrange(s, s1, s2, s3, s4, s5, s6, s7, ncol = 3, nrow =  3)
figSucrose

figLearning <- ggarrange(l, l1, ncol = 2, nrow =  1)
figLearning

figSurvival <- ggarrange(su, su1, su2, ncol = 2, nrow =  2)
figSurvival

