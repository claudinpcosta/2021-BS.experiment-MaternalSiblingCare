##############################BS Experiment Summer 2019#######################################

###Set Working Directory

#### * ALL PACKAGES USED HERE * ####
library(plyr)
library(ggplot2) 
library(gridExtra)
library(ggpubr) 
library(lme4)
library(multcomp)
library(car)
library(AICcmodavg)
library(lmtest)
library(dplyr) 
library(tidyr)
library(cowplot)
library(outliers)
library(MuMIn)


#### * LOADING DATASET * ####
completeDataset <- read.csv("Data/completeDataset.csv", header = T)

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

#Body Size complete (with samples from colonies without DevTime information)####
summary(Avg.mm)

  #excluding NA values 
bodysize <- subset(data.BS, !is.na(Avg.mm))
dim(bodysize)
head(bodysize)
tail(bodysize)
attach(bodysize)
summary(Avg.mm)

mu <- ddply(bodysize, "Treatment", summarise,
            No.bees=length(Avg.mm),
            grp.mean=mean(Avg.mm),
            var=var(Avg.mm),
            sd   = sd(Avg.mm, na.rm=TRUE),
            se   = sd / sqrt(sum(Avg.mm)),
            min=min(Avg.mm),
            max=max(Avg.mm))
mu

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


#Dataset with samples with Body Size and Development time information####

  #excluding NA values 
bsdt <- subset(data.BS, !is.na(Avg.mm) & !is.na(Time.development.days))
head(bsdt)
dim(bsdt)
summary(bsdt)
attach(bsdt)

  #searching Dev.Time outliers
qnt <- quantile(Time.development.days, probs=c(.25, .75), na.rm = T)
caps <- quantile(Time.development.days, probs=c(.05, .95), na.rm = T)
HI <- 1.5 * IQR(Time.development.days, na.rm = T)
Time.development.days[Time.development.days < (qnt[1] - HI)] <- caps[1]
Time.development.days[Time.development.days > (qnt[2] + HI)] <- caps[2]
scores(Time.development.days)  # z-scores => (x-mean)/sd
out.DevTime <- scores(Time.development.days, type = "z", prob = 0.95)
length(out.DevTime)
bsdt$out.DevTime <- out.DevTime
dim(bsdt)

  #keep outliers
bodytime <- subset(data.BS, !is.na(Avg.mm) & !is.na(Time.development.days))
head(bodytime)
dim(bodytime)
summary(bodytime)
attach(bodytime)

dt <- ddply(bodytime, "Treatment", summarise, 
            No.bees=length(Time.development.days),
            grp.mean=mean(Time.development.days),
            var=var(Time.development.days),
            sd   = sd(Time.development.days, na.rm=TRUE),
            se   = sd / sqrt(sum(Time.development.days)),
            min=min(Time.development.days),
            max=max(Time.development.days))
dt

bs <- ddply(bodytime, "Treatment", summarise,
            No.bees=length(Avg.mm),
            grp.mean=mean(Avg.mm),
            var=var(Avg.mm),
            sd   = sd(Avg.mm, na.rm=TRUE),
            se   = sd / sqrt(sum(Avg.mm)),
            min=min(Avg.mm),
            max=max(Avg.mm))
bs


#Sucrose####
summary(Sucrose.summary)

sucrose <- bodytime[-which(Sucrose.summary == ""),]
dim(sucrose)
head(sucrose)
tail(sucrose)
summary(sucrose$Sucrose.summary)

sucrose %>%
  count(Treatment, Sucrose.summary, sort = TRUE)

suc.con <- subset(sucrose, !is.na(Sucrose.conc))
suc.con %>%
  count(Treatment, Sucrose.conc.summary, sort = TRUE)
dim(suc.con)
head(suc.con)

suc.con.data <- read.csv("Data/suc.concentration.csv", row.names = 1)
head(suc.con.data)

#Learning####
summary(LearningTraining.summary)
summary(LearningTest.summary)


  #Learning Training 
learningTraining <- bodytime[-which(LearningTraining.summary == ""),]
dim(learningTraining)
head(learningTraining)
tail(learningTraining)
summary(learningTraining$LearningTraining.summary)
learningTraining %>%
  count(Treatment, LearningTraining.summary, sort = TRUE)


  #Learning Test
learningTest <- bodytime[-which(LearningTest.summary == ""),]
dim(learningTest)
head(learningTest)
tail(learningTest)
summary(learningTest$LearningTest.summary)
learningTest %>%
  count(Avg.mm, LearningTest.summary, sort = TRUE)


#Survival####
summary(bodytime$Survival.hours)

survival <- subset(bodytime, !is.na(Survival.hours))
survival <- survival[-which(survival$Survival.hours > 60),]
head(survival)
dim(survival)
summary(survival$Survival.hours)

surv <- ddply(survival, c("Treatment"), summarise,
            No.bees=length(Survival.hours),
            grp.mean=mean(Survival.hours),
            var=var(Survival.hours),
            sd   = sd(Survival.hours, na.rm=TRUE),
            se   = sd / sqrt(sum(Survival.hours)),
            min=min(Survival.hours),
            max=max(Survival.hours))
surv




#Nest####

nestDevTime <- ddply(bodytime, c("QueenID", "Treatment"), summarise,
                     No.bees=length(Time.development.days),
                     mean = mean(Time.development.days, na.rm=TRUE),
                     var=var(Time.development.days),
                     sd   = sd(Time.development.days, na.rm=TRUE),
                     se   = sd / sqrt(sum(Time.development.days)),
                     min=min(Time.development.days),
                     max=max(Time.development.days))
nestDevTime <- nestDevTime[with(nestDevTime, order(Treatment, QueenID)),]
nestDevTime

nestBodySize <- ddply(bodytime, c("QueenID", "Treatment"), summarise,
                     No.bees=length(Avg.mm),
                     mean = mean(Avg.mm, na.rm=TRUE),
                     var=var(Avg.mm),
                     sd   = sd(Avg.mm, na.rm=TRUE),
                     se   = sd / sqrt(sum(Avg.mm)),
                     min=min(Avg.mm),
                     max=max(Avg.mm))
nestBodySize <- nestBodySize[with(nestBodySize, order(Treatment, QueenID)),]
nestBodySize


nestSurvival <- ddply(survival, c("QueenID", "Treatment"), summarise,
                      No.bees=length(Survival.hours),
                      mean = mean(Survival.hours, na.rm=TRUE),
                      var=var(Survival.hours),
                      sd   = sd(Survival.hours, na.rm=TRUE),
                      se   = sd / sqrt(sum(Survival.hours)),
                      min=min(Survival.hours),
                      max=max(Survival.hours))
nestSurvival <- nestSurvival[with(nestSurvival, order(Treatment, QueenID)),]
nestSurvival

  #excluding nests with < 3 bees
nestDT <- nestDevTime[which(nestDevTime$No.bees >= 3),]
nestDT

nestBS <- nestBodySize[which(nestBodySize$No.bees >= 3),]
nestBS

#Feeding rates####

feeding.rate <- read.csv("Data/feedingRates.csv")
feeding.rate

fd <- ddply(feeding.rate, c("Treatment"), summarise,
                     mean3d = mean(Feeding.3d, na.rm=TRUE),
                     sd3d   = sd(Feeding.3d, na.rm=TRUE),
                     se3d   = sd3d / sqrt(sum(Feeding.3d)),
                     min3d =min(Feeding.3d),
                     max3d =max(Feeding.3d),
            mean5d = mean(Feeding.5d, na.rm=TRUE),
            sd5d   = sd(Feeding.5d, na.rm=TRUE),
            se5d   = sd5d / sqrt(sum(Feeding.5d)),
            min5d =min(Feeding.5d),
            max5d =max(Feeding.5d))
fd


feeding.data <- read.csv("Data/Feeding.csv")
feeding.data



#### * STATS * ####

#Body size stats####

  #testing whether a random sample of data comes from a normal distribution
shapiro.test(bodytime$Avg.mm)
ggdensity(bodytime$Avg.mm)
hist(bodytime$Avg.mm)

          #obs.: data doesn't come from a normal distribution: Gamma
BSnull <- glmer(Avg.mm ~1 + (1 | ColonyID_queen) + (1 | QueenID), data=bodytime, family = Gamma(link = "identity"))
BS1 <- glmer(Avg.mm ~ Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=bodytime, family= Gamma(link = "identity"))
BS2 <- glmer(Avg.mm ~ Treatment + Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=bodytime, family= Gamma(link = "identity"))
BS3 <- glmer(Avg.mm ~ Treatment * Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=bodytime, family= Gamma(link = "identity"))
BS4 <- glmer(Avg.mm ~ Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=bodytime, family= Gamma(link = "identity"))

model.sel(BSnull, BS1, BS2, BS3, BS4) # choose the best fit model

#for best fit model
summary(BS1)
Anova(BS1)
lrtest(BSnull, BS1)
summary(glht(BS1, linfct=mcp(Treatment="Tukey")))

#   #testing differences in BS between treatments
# 
# #body size vs Care-giver identity 
# wilcox.test(Avg.mm~Treatment, data=bodytime)
# 
# #body size vs Care-giver identity (using mean)
# wilcox.test(mean~Treatment, data=nestBodySize)
# 
# #test of homogeneity of variances
#   #treatment
# leveneTest(Avg.mm~Treatment, data=bodytime)
# 
#   #nest
# leveBSvsNest <- bodytime[-which(QueenID == "MT035" | QueenID == "MT039" | QueenID == "MT042" ),]
# leveneTest(leveBSvsNest$Avg.mm~leveBSvsNest$QueenID)


#Development time stats####

  #testing whether a random sample of data comes from a normal distribution
shapiro.test(bodytime$Time.development.days)
ggdensity(bodytime$Time.development.days)
hist(bodytime$Time.development.days)

          #obs.: data doesn't come from a normal distribution: Gamma

DTnull <- glmer(Time.development.days ~1 + (1 | ColonyID_queen) + (1 | QueenID), data=bodytime, family=Gamma(link = "identity"))
DT1 <- glmer(Time.development.days ~ Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=bodytime, family=Gamma(link = "identity"))

model.sel(DTnull, DT1) # choose the best fit model


  #testing differences in Development Time between treatments

# #developmental time vs Care-giver identity 
# wilcox.test(bodytime$Time.development.days~bodytime$Treatment)
# 
# #developmental time vs Care-giver identity
# wilcox.test(mean~Treatment, data=nestDevTime)
# 
# #test of homogeneity of variances
#   #treatment
# leveneTest(bodytime$Time.development.day~bodytime$Treatment)
# 
#   #nest
# leveDTvsNest <- bodytime[-which(QueenID == "MT035" | QueenID == "MT039" | QueenID == "MT042" ),]
# leveneTest(leveDTvsNest$Time.development.day~leveDTvsNest$QueenID)


#Correlation BS vs Development time####
cor.test(bodytime$Avg.mm, bodytime$Time.development.days, data=bodytime, method="spearman")


#Correlation Survival vs BS####
cor.test(survival$Survival.hours, survival$Avg.mm, method="spearman")

#test of homogeneity of variances
leveneTest(survival$Survival.hours ~ survival$Treatment)

#nest
leveSvsNest <- survival[-which(QueenID == "MT035" | QueenID == "MT039" | QueenID == "MT019" | QueenID == "MT001" | QueenID == "MT025"),]
leveneTest(leveSvsNest$Avg.mm~leveSvsNest$QueenID)


#Survival####

# #testing differences in Survival between treatments
# wilcox.test(survival$Survival.hours ~ survival$Treatment)

  #testing whether a random sample of data comes from a normal distribution
shapiro.test(survival$Survival.hours)
ggdensity(survival$Survival.hours)
hist(survival$Survival.hours)

          #obs.: data doesn't come from a normal distribution: Poisson 
Snull <- glmer(Survival.hours ~1 + (1 | ColonyID_queen) + (1 | QueenID), data=survival, family=Gamma(link="log"))
S1 <- glmer(Survival.hours ~ Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=survival, family=Gamma(link="log"))
S2 <- glmer(Survival.hours ~ Avg.mm + (1 | ColonyID_queen) + (1 | QueenID), data=survival, family=Gamma(link="log"))
S3 <- glmer(Survival.hours ~ Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=survival, Gamma(link="log"))
S4 <- glmer(Survival.hours ~ Treatment + Avg.mm + Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=survival, family=Gamma(link="log"))
S5 <- glmer(Survival.hours ~ Treatment + Avg.mm * Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=survival, family=Gamma(link="log"))
S6 <- glmer(Survival.hours ~ Treatment + Avg.mm + (1 | ColonyID_queen) + (1 | QueenID), data=survival, family=Gamma(link="log"))
S7 <- glmer(Survival.hours ~ Treatment + Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=survival, family=Gamma(link="log"))
S8 <- glmer(Survival.hours ~ Avg.mm + Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=survival, family=Gamma(link="log"))
S9 <- glmer(Survival.hours ~ Avg.mm * Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=survival, family=Gamma(link="log"))
S10 <- glmer(Survival.hours ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=survival, family=Gamma(link="log"))
S11 <- glmer(Survival.hours ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=survival, family=Gamma(link="log"))
S12 <- glmer(Survival.hours ~ Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=survival, family=Gamma(link="log"))
S13 <- glmer(Survival.hours ~ Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=survival, family=Gamma(link="log"))
S14 <- glmer(Survival.hours ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=survival, family=Gamma(link="log"))
S15 <- glmer(Survival.hours ~ Treatment + Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=survival, family=Gamma(link="log"))
S16 <- glmer(Survival.hours ~ Treatment + Avg.mm + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=survival, family=Gamma(link="log"))
S17 <- glmer(Survival.hours ~ Treatment + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=survival, family=Gamma(link="log"))
S18 <- glmer(Survival.hours ~ Avg.mm + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=survival, family=Gamma(link="log"))
S19 <- glmer(Survival.hours ~ Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=survival, family=Gamma(link="log"))

model.sel(Snull, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15, S16, S17, S18, S19) # choose the best fit model

#for best fit model
summary(S6)
Anova(S6)
lrtest(Snull,S6)
summary(glht(S6, linfct=mcp(Treatment="Tukey")))


#Learning stats####


          #obs.: data is binomial (yes - 1 or no - 0): binomal

#LearningTraining
LTRnull <- glmer(LearningTraining ~1 + (1 | ColonyID_queen) + (1 | QueenID), data=learningTraining, family=binomial(link = "logit"))
LTR1 <- glmer(LearningTraining ~ Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=learningTraining, family=binomial(link = "logit"))
LTR2 <- glmer(LearningTraining ~ Avg.mm + (1 | ColonyID_queen) + (1 | QueenID), data=learningTraining, family=binomial(link = "logit"))
LTR3 <- glmer(LearningTraining ~ Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=learningTraining, family=binomial(link = "logit"))
LTR4 <- glmer(LearningTraining ~ Treatment + Avg.mm + Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=learningTraining, family=binomial(link = "logit"))
LTR5 <- glmer(LearningTraining ~ Treatment + Avg.mm * Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=learningTraining, family=binomial(link = "logit"))
LTR6 <- glmer(LearningTraining ~ Treatment + Avg.mm + (1 | ColonyID_queen) + (1 | QueenID), data=learningTraining, family=binomial(link = "logit"))
LTR7 <- glmer(LearningTraining ~ Treatment + Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=learningTraining, family=binomial(link = "logit"))
LTR8 <- glmer(LearningTraining ~ Avg.mm + Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=learningTraining, family=binomial(link = "logit"))
LTR9 <- glmer(LearningTraining ~ Avg.mm * Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=learningTraining, family=binomial(link = "logit"))
LTR10 <- glmer(LearningTraining ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=learningTraining, family=binomial(link = "logit"))
LTR11 <- glmer(LearningTraining ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=learningTraining, family=binomial(link = "logit"))
LTR12 <- glmer(LearningTraining ~ Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=learningTraining, family=binomial(link = "logit"))
LTR13 <- glmer(LearningTraining ~ Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=learningTraining, family=binomial(link = "logit"))
LTR14 <- glmer(LearningTraining ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=learningTraining, family=binomial(link = "logit"))
LTR15 <- glmer(LearningTraining ~ Treatment + Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=learningTraining, family=binomial(link = "logit"))
LTR16 <- glmer(LearningTraining ~ Treatment + Avg.mm + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=learningTraining, family=binomial(link = "logit"))
LTR17 <- glmer(LearningTraining ~ Treatment + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=learningTraining, family=binomial(link = "logit"))
LTR18 <- glmer(LearningTraining ~ Avg.mm + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=learningTraining, family=binomial(link = "logit"))
LTR19 <- glmer(LearningTraining ~ Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=learningTraining, family=binomial(link = "logit"))

model.sel(LTRnull, LTR1, LTR2, LTR3, LTR4, LTR5, LTR6, LTR7, LTR8, LTR9, LTR10, LTR11, LTR12, LTR13, LTR14, LTR15, LTR16, LTR17, LTR18, LTR19) # choose the best fit model

#for best fit model
summary(LTR2)
Anova(LTR2)
lrtest(LTRnull, LTR2)



#LearningTesting
LTnull <- glmer(LearningTest ~1 + (1 | ColonyID_queen) + (1 | QueenID) , data=learningTest, family=binomial(link = "logit"))
LT1 <- glmer(LearningTest ~ Treatment + (1 | ColonyID_queen) + (1 | QueenID) , data=learningTest, family=binomial(link = "logit"))
LT2 <- glmer(LearningTest ~ Avg.mm + (1 | ColonyID_queen) + (1 | QueenID) , data=learningTest, family=binomial(link = "logit"))
LT3 <- glmer(LearningTest ~ Time.development.days + (1 | ColonyID_queen) + (1 | QueenID) , data=learningTest, family=binomial(link = "logit"))
LT4 <- glmer(LearningTest ~ Treatment + Avg.mm + Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=learningTest, family=binomial(link = "logit"))
LT5 <- glmer(LearningTest ~ Treatment + Avg.mm * Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=learningTest, family=binomial(link = "logit"))
LT6 <- glmer(LearningTest ~ Treatment + Avg.mm + (1 | ColonyID_queen) + (1 | QueenID), data=learningTest, family=binomial(link = "logit"))
LT7 <- glmer(LearningTest ~ Treatment + Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=learningTest, family=binomial(link = "logit"))
LT8 <- glmer(LearningTest ~ Avg.mm + Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=learningTest, family=binomial(link = "logit"))
LT9 <- glmer(LearningTest ~ Avg.mm * Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=learningTest, family=binomial(link = "logit"))
LT10 <- glmer(LearningTest ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=learningTest, family=binomial(link = "logit"))
LT11 <- glmer(LearningTest ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=learningTest, family=binomial(link = "logit"))
LT12 <- glmer(LearningTest ~ Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID) , data=learningTest, family=binomial(link = "logit"))
LT13 <- glmer(LearningTest ~ Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID) , data=learningTest, family=binomial(link = "logit"))
LT14 <- glmer(LearningTest ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID) , data=learningTest, family=binomial(link = "logit"))
LT15 <- glmer(LearningTest ~ Treatment + Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID) , data=learningTest, family=binomial(link = "logit"))
LT16 <- glmer(LearningTest ~ Treatment + Avg.mm + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID) , data=learningTest, family=binomial(link = "logit"))
LT17 <- glmer(LearningTest ~ Treatment + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID) , data=learningTest, family=binomial(link = "logit"))
LT18 <- glmer(LearningTest ~ Avg.mm + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID) , data=learningTest, family=binomial(link = "logit"))
LT19 <- glmer(LearningTest ~ Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID) , data=learningTest, family=binomial(link = "logit"))

model.sel(LTnull, LT1, LT2, LT3, LT4, LT5, LT6, LT7, LT8, LT9, LT10, LT11, LT12, LT13, LT14, LT15, LT16, LT17, LT18, LT19) # choose the best fit model


#Sucrose####

          #obs.: we have data is binomial (yes - 1 or no - 0): binomal

SUnull <- glmer(Sucrose.response ~1 + (1 | ColonyID_queen) + (1 | QueenID), data=sucrose, family=binomial(link = "logit"))
SU1 <- glmer(Sucrose.response ~ Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=sucrose, family=binomial(link = "logit"))
SU2 <- glmer(Sucrose.response ~ Avg.mm + (1 | ColonyID_queen) + (1 | QueenID), data=sucrose, family=binomial(link = "logit"))
SU3 <- glmer(Sucrose.response ~ Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=sucrose, family=binomial(link = "logit"))
SU4 <- glmer(Sucrose.response ~ Treatment + Avg.mm + Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=sucrose, family=binomial(link = "logit"))
SU5 <- glmer(Sucrose.response ~ Treatment + Avg.mm * Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=sucrose, family=binomial(link = "logit"))
SU6 <- glmer(Sucrose.response ~ Treatment + Avg.mm + (1 | ColonyID_queen) + (1 | QueenID), data=sucrose, family=binomial(link = "logit"))
SU7 <- glmer(Sucrose.response ~ Treatment + Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=sucrose, family=binomial(link = "logit"))
SU8 <- glmer(Sucrose.response ~ Avg.mm + Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=sucrose, family=binomial(link = "logit"))
SU9 <- glmer(Sucrose.response ~ Avg.mm * Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=sucrose, family=binomial(link = "logit"))
SU10 <- glmer(Sucrose.response ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=sucrose, family=binomial(link = "logit"))
SU11 <- glmer(Sucrose.response ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=sucrose, family=binomial(link = "logit"))
SU12 <- glmer(Sucrose.response ~ Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=sucrose, family=binomial(link = "logit"))
SU13 <- glmer(Sucrose.response ~ Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=sucrose, family=binomial(link = "logit"))
SU14 <- glmer(Sucrose.response ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=sucrose, family=binomial(link = "logit"))
SU15 <- glmer(Sucrose.response ~ Treatment + Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=sucrose, family=binomial(link = "logit"))
SU16 <- glmer(Sucrose.response ~ Treatment + Avg.mm + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=sucrose, family=binomial(link = "logit"))
SU17 <- glmer(Sucrose.response ~ Treatment + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=sucrose, family=binomial(link = "logit"))
SU18 <- glmer(Sucrose.response ~ Avg.mm + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=sucrose, family=binomial(link = "logit"))
SU19 <- glmer(Sucrose.response ~ Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=sucrose, family=binomial(link = "logit"))

model.sel(SUnull, SU1, SU2, SU3, SU4, SU5, SU6, SU7, SU8, SU9, SU10, SU11, SU12, SU13, SU14, SU15, SU16, SU17, SU18, SU19) # choose the best fit model

#for best fit model
summary(SU3)
Anova(SU3)
lrtest(SUnull, SU3)


#Sucrose.concentration
 chisq.test(suc.con.data)

#testing whether a random sample of data comes from a normal distribution
shapiro.test(suc.con$Sucrose.conc)
ggdensity(suc.con$Sucrose.conc)
hist(suc.con$Sucrose.conc)

          #obs.: data doesn't come from a normal distribution:  poisson

suc.con$Sucrose.conc <- suc.con$Sucrose.conc * 100 #The Poisson model is about counts. Count values are positive natural numbers including zero (0, 1, 2, 3, ...)

SCnull <- glmer(Sucrose.conc ~1 + (1 | ColonyID_queen) + (1 | QueenID), data=suc.con, family=Gamma(link="inverse"))
SC1 <- glmer(Sucrose.conc ~ Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=suc.con, family=Gamma(link="inverse"))
SC2 <- glmer(Sucrose.conc ~ Avg.mm + (1 | ColonyID_queen) + (1 | QueenID), data=suc.con, family=Gamma(link="inverse"))
SC3 <- glmer(Sucrose.conc ~ Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=suc.con, family=Gamma(link="inverse"))
SC4 <- glmer(Sucrose.conc ~ Treatment + Avg.mm + Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=suc.con, family=Gamma(link="inverse"))
SC5 <- glmer(Sucrose.conc ~ Treatment + Avg.mm * Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=suc.con, family=Gamma(link="inverse"))
SC6 <- glmer(Sucrose.conc ~ Treatment + Avg.mm + (1 | ColonyID_queen) + (1 | QueenID), data=suc.con, family=Gamma(link="inverse"))
SC7 <- glmer(Sucrose.conc ~ Treatment + Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=suc.con, family=Gamma(link="inverse"))
SC8 <- glmer(Sucrose.conc ~ Avg.mm + Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=suc.con, family=Gamma(link="log"))
SC9 <- glmer(Sucrose.conc ~ Avg.mm * Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=suc.con, family=Gamma(link="log"))
SC10 <- glmer(Sucrose.conc ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days + (1 | ColonyID_queen) + (1 | QueenID), data=suc.con, family=Gamma(link="log"))
SC11 <- glmer(Sucrose.conc ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=suc.con, family=Gamma(link="log"))
SC12 <- glmer(Sucrose.conc ~ Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=suc.con, family=Gamma(link="log"))
SC13 <- glmer(Sucrose.conc ~ Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=suc.con, family=Gamma(link="log"))
SC14 <- glmer(Sucrose.conc ~ Treatment + Avg.mm + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=suc.con, family=Gamma(link="log"))
SC15 <- glmer(Sucrose.conc ~ Treatment + Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=suc.con, family=Gamma(link="log"))
SC16 <- glmer(Sucrose.conc ~ Treatment + Avg.mm + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=suc.con, family=Gamma(link="log"))
SC17 <- glmer(Sucrose.conc ~ Treatment + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=suc.con, family=Gamma(link="log"))
SC18 <- glmer(Sucrose.conc ~ Avg.mm + Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=suc.con, family=Gamma(link="log"))
SC19 <- glmer(Sucrose.conc ~ Avg.mm * Time.development.days + Avg.mm * Time.development.days * Treatment + (1 | ColonyID_queen) + (1 | QueenID), data=suc.con, family=Gamma(link="log"))

model.sel(SCnull, SC1, SC2, SC3, SC4, SC5, SC6, SC7, SC8, SC9, SC10, SC11, SC12, SC13, SC14, SC15, SC16, SC17, SC18, SC19) # choose the best fit model


#Feeding####
shapiro.test(feeding.rate$Feeding.3d)
shapiro.test(feeding.rate$Feeding.5d)

res.3d <- aov(Feeding.3d~Treatment, data=feeding.rate)
summary(res.3d)
cor.test(feeding.rate$Feeding.3d, feeding.rate$N.larvae, method="spearman")


res.5d <- aov(Feeding.5d~Treatment, data=feeding.rate)
summary(res.5d)
cor.test(feeding.rate$Feeding.5d, feeding.rate$N.larvae, method="spearman")



#### * GRAPHICS * ####

#Body Size####
h<-ggplot(bodysize, aes(x=Avg.mm, fill=Treatment, color=Treatment)) +
  geom_histogram(position="identity", alpha = 0.5, binwidth = 0.1)
h <- h + geom_vline(data=mu, aes(xintercept=grp.mean, color=Treatment),
                    linetype="dashed", size=0.7) # Add mean lines
h <- h + xlab("Marginal cell length (mm)")+ylab("No. bees")
h <- h + theme(legend.position = "right") + theme(legend.title = element_blank())
h <- h + theme_classic()
h <- h + scale_fill_brewer(name = "Care-giver identity", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared")) + scale_color_brewer(name = "Care-giver identity", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared"),palette = "Set1")
h <- h + theme(text = element_text(size = 12)) 
h

h1 <-ggplot(bodytime, aes(x=Avg.mm, fill=Treatment, color=Treatment)) +
  geom_histogram(position="identity", alpha = 0.5, binwidth = 0.1)
h1 <- h1 + geom_vline(data=bs, aes(xintercept=grp.mean, color=Treatment),
                    linetype="dashed", size=0.7) # Add mean lines
h1 <- h1 + xlab("Marginal cell length (mm)")+ylab("No. bees")
h1 <- h1 + theme(legend.position = "right") + theme(legend.title = element_blank())
h1 <- h1 + theme_classic()
h1 <- h1 + scale_fill_brewer(name = "Care-giver identity", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared")) + scale_color_brewer(name = "Care-giver identity", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared"),palette = "Set1")
h1 <- h1 + theme(text = element_text(size = 12)) 
h1

#Development time####
t<-ggplot(bodytime, aes(x=Time.development.days, fill=Treatment, color=Treatment)) +
  geom_histogram(position="identity", alpha = 0.5, binwidth = 1)
t <- t + geom_vline(data=dt, aes(xintercept=grp.mean, color=Treatment),
                    linetype="dashed", size=.7) # Add mean lines
t <- t + labs(x = "Development time (days)", y = "No. bees")
t <- t + theme(legend.title = element_blank()) + theme(legend.position = "right")
t <- t + theme_classic()
t <- t + scale_fill_brewer(name = "Care-giver identity", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared")) + scale_color_brewer(name = "Care-giver identity", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared"),palette = "Set1")
t <- t + theme(text = element_text(size = 12)) 
t

#Nests####
ne <- ggplot(nestDevTime, aes(x=reorder(QueenID, -mean), y=mean, fill = Treatment, color = Treatment)) + 
  geom_bar(position=position_dodge(), stat="identity", alpha = 0.5) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
ne <- ne + labs(x = "Nests", y = "Development Duration (days)")
ne <- ne + theme(legend.title = element_blank()) + theme(legend.position = "right")
ne <- ne + theme_classic()
ne <- ne + scale_fill_brewer(name = "Care-giver identity", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared")) + scale_color_brewer(name = "Care-giver identity", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared"),palette = "Set1")
ne


ne2 <-ggplot(nestDevTime, aes(x=Treatment, y=mean, fill = Treatment, color = Treatment)) +
  geom_boxplot(alpha = 0.5)
ne2 <- ne2 + labs(x = "Treatment", y = "Development time (days)")
ne2 <- ne2 + theme(legend.title = element_blank()) + theme(legend.position = "right")
ne2 <- ne2 + theme_classic()
ne2 <- ne2 + scale_fill_brewer(name = "Care-giver identity", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared")) + scale_color_brewer(name = "Care-giver identity", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared"),palette = "Set1")
ne2 <- ne2 + scale_x_discrete(labels=c("Queen.reared" = "Queen-Reared", "Worker.reared" = "Worker-Reared"))
ne2


ne3 <- ggplot(nestBodySize, aes(x=reorder(QueenID, -mean), y=mean, fill = Treatment, color = Treatment)) + 
  geom_bar(position=position_dodge(), stat="identity", alpha = 0.5) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
ne3 <- ne3 + labs(x = "Nests", y = "Marginal cell length (mm)")
ne3 <- ne3 + theme(legend.title = element_blank()) + theme(legend.position = "right")
ne3 <- ne3 + theme_classic()
ne3 <- ne3 + scale_fill_brewer(name = "Care-giver identity", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared")) + scale_color_brewer(name = "Care-giver identity", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared"),palette = "Set1")
ne3


#Correlation Developmental Time X Body Size####

c<- ggscatter(bodytime, x = "Time.development.days", y = "Avg.mm",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Development time (Days)", ylab = "Marginal celll length (mm)")
c

c1 <- ggplot(bodytime, aes(y = Avg.mm, x = Time.development.days))
c1 <- c1 + geom_smooth(method=lm, se=FALSE, color="black")
c1 <- c1 + geom_jitter(aes(color = Treatment)) + 
  geom_smooth(aes(color = Treatment, fill = Treatment), linetype="dashed", method = lm) +
  scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
c1 <- c1 + labs(y = "Marginal cell length (mm)", x = "Development time (days)")
c1 <- c1 + theme(legend.title = element_blank()) + theme(legend.position = "right")
c1 <- c1 + theme_classic()
c1 <- c1 + scale_fill_brewer(name = "Care-giver identity", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared")) + scale_color_brewer(name = "Care-giver identity", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared"),palette = "Set1")
c1 <- c1 + theme(text = element_text(size = 12)) 
c1


#Feeding####

f <- ggplot(feeding.data, aes(x=day, y=mean, group=Treatment, color=Treatment, fill = Treatment)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, alpha = 0.5, size = 1) +
  geom_line(alpha = 0.5, size = 1) + geom_point(alpha = 2)
f <- f + xlab("Larval age (days)")+ylab("No. Feeding events")
f <- f + theme(legend.position = "right") + theme(legend.title = element_blank())
f <- f + theme_classic()
f <- f + scale_fill_brewer(name = "Care-giver identity", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared")) + scale_color_brewer(name = "Care-giver identity", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared"),palette = "Set1")
f <- f + theme(text = element_text(size = 12))
f


f1 <- ggplot(feeding.rate, aes(y = N.larvae, x = Feeding.3d))
f1 <- f1 + geom_smooth(method=lm, se=FALSE, color="black", linetype="dashed")
f1 <- f1 + geom_jitter(aes(color = Treatment))
f1 <- f1 + labs(y = "No. Bees from First Brood", x = "No. Feeding events")
f1 <- f1 + theme(legend.position = "bottom")
f1 <- f1 + theme_classic()
f1 <- f1 + scale_fill_brewer(name = "Care-giver identity", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared")) + scale_color_brewer(name = "Care-giver identity", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared"),palette = "Set1")
f1 <- f1 + theme(text = element_text(size = 12)) 
f1


                      #figure1 to export#### 
                      fig1 <- ggarrange(
                        h1,t,c1,f, ncol = 2, nrow =  2,
                        labels = c("A", "B", "C", "D"),
                        common.legend = TRUE, legend = "bottom",
                        widths = c(3, 3)
                      )
                      fig1

#Sucrose####

s<- ggplot(sucrose, aes(x=Sucrose.summary, fill=Treatment, color=Treatment)) +
  geom_bar(data=sucrose, stat="count", position="dodge", alpha = 0.5)
s <- s + labs(x = "Sucrose Response", y = "No. bees")
s <- s + theme(legend.title = element_blank()) + theme(legend.position = "right")
s <- s + theme_classic()
s <- s + scale_fill_brewer(name = "Care-giver identity", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared")) + scale_color_brewer(name = "Care-giver identity", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared"),palette = "Set1")
s

s1 <- ggplot(sucrose,aes(x = Treatment,fill = Sucrose.summary)) + 
  geom_bar(position = "fill")+ylab("Proportion Responded")+scale_fill_discrete(name="Responded to Sucrose")
s1 <- s1 + labs(x = "Care-giver identity", fill = "Assay response")
s1 <- s1 + theme(legend.title = element_blank())
s1 <- s1 + theme_classic()
s1 <- s1 + scale_color_grey()+scale_fill_grey()
s1 <- s1 + scale_x_discrete(labels=c("Queen.reared" = "Queen-Reared", "Worker.reared" = "Worker-Reared"))
s1 <- s1 + theme(legend.position = "bottom")
s1 <- s1 + theme(text = element_text(size = 12))
s1

s2 <- ggplot(sucrose, aes(x=Sucrose.conc, fill=Treatment, color=Treatment)) +
  geom_bar(data=sucrose, stat="count", position="dodge", alpha = 0.5)
s2 <- s2 + labs(x = "Sucrose Response (Concentration)", y = "No. bees", title = "Sucrose response (Concentration) versus Group reared")
s2 <- s2 + theme(legend.title = element_blank()) + theme(legend.position = "right")
s2 <- s2 + theme_classic()
s2 <- s2 + scale_fill_brewer(name = "Care-giver identity", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared")) + scale_color_brewer(name = "Care-giver identity", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared"),palette = "Set1")
s2

s3 <- ggplot(sucrose, aes(x=Sucrose.conc, fill=Treatment, color=Treatment)) +
  geom_bar(position = "fill", alpha = 0.5)+ylab("Proportion Responded")+scale_fill_discrete(name="Responded to Sucrose")
s3 <- s3 + labs(x = "Sucrose Response (Concentration)", title = "Sucrose response (Concentration) versus Group reared")
s3 <- s3 + theme(legend.title = element_blank()) + theme(legend.position = "right")
s3 <- s3 + theme_classic()
s3 <- s3 + scale_fill_brewer(name = "Care-giver identity", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared")) + scale_color_brewer(name = "Care-giver identity", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared"),palette = "Set1")
s3

s4 <-ggplot(sucrose, aes(x=Sucrose.conc, fill=Treatment, color=Treatment)) +
  geom_histogram(position="identity", alpha = 0.5, binwidth = 0.05)
s4 <- s4 + labs(x = "Sucrose concentration", y = "No. bees responded")
s4 <- s4 + theme(legend.title = element_blank()) 
s4 <- s4 + theme_classic()
s4 <- s4 + scale_fill_brewer(name = "Care-giver identity", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared")) + scale_color_brewer(name = "Care-giver identity", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared"),palette = "Set1")
s4 <- s4 + theme(legend.position = "bottom")
s4 <- s4 + theme(text = element_text(size = 12))
s4

s5 <- ggplot(sucrose, aes(x=Sucrose.conc, y=Avg.mm, fill=Treatment, color=Treatment)) + 
  geom_point(alpha = 0.6, position = "jitter")
s5 <- s5 + labs(x = "Sucrose Response (Concentration)", y = "Marginal cell length (mm)", title = "Sucrose response versus Body size")
s5 <- s5 + theme(legend.title = element_blank()) + theme(legend.position = "right")
s5 <- s5 + theme_classic()
s5 <- s5 + scale_fill_brewer(name = "Care-giver identity", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared")) + scale_color_brewer(name = "Care-giver identity", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared"),palette = "Set1")
s5

s6 <- ggplot(sucrose, aes(x=Sucrose.conc, y=Avg.mm, fill=Treatment, color=Treatment))
s6 <- s6 + geom_point() +
geom_smooth(aes(color = Treatment, fill = Treatment), method = lm) +
  scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
s6 <- s6 + labs(x = "Sucrose Response (Concentration)", y = "Marginal cell length (mm)", title = "Sucrose response versus Body size")
s6 <- s6 + theme(legend.title = element_blank()) + theme(legend.position = "right")
s6 <- s6 + theme_classic()
s6 <- s6 + scale_fill_brewer(name = "Care-giver identity", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared")) + scale_color_brewer(name = "Care-giver identity", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared"),palette = "Set1")
s6

s7 <- ggscatter(sucrose, x = "Sucrose.conc", y = "Avg.mm",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Sucrose Response (Concentration)", ylab = "Marginal cell length (mm)")
s7

#Learning####

l <- ggplot(learningTraining,aes(x = Treatment,fill = LearningTraining.summary)) + 
  geom_bar(position = "fill")+ylab("Proportion Trained")
l <- l + labs(x = "Care-giver identity", fill = "Assay response")
l <- l + theme(legend.title = element_blank())
l <- l + theme_classic()
l <- l + scale_fill_brewer(palette = "Dark2") + scale_color_brewer(palette = "Dark2")
l <- l + scale_color_grey()+scale_fill_grey()
l <- l + scale_x_discrete(labels=c("Queen.reared" = "Queen-Reared", "Worker.reared" = "Worker-Reared"))
l <- l + theme(legend.position = "top")
l <- l + theme(text = element_text(size = 12))
l

l1 <- ggplot(learningTest,aes(x = Treatment,fill = LearningTest.summary)) + 
  geom_bar(position = "fill")+ylab("Proportion Successful")
l1 <- l1 + labs(x = "Care-giver identity", fill = "Assay response")
l1 <- l1 + theme(legend.title = element_blank()) + theme(legend.position = "right")
l1 <- l1 + theme_classic()
l1 <- l1 + scale_color_grey()+scale_fill_grey()
l1 <- l1 + scale_x_discrete(labels=c("Queen.reared" = "Queen-Reared", "Worker.reared" = "Worker-Reared"))
l1 <- l1 + theme(legend.position = "top")
l1 <- l1 + theme(text = element_text(size = 12))
l1

#Survival####
su <- ggplot(surv, aes(x=Treatment, y=grp.mean, fill = Treatment, color = Treatment)) + 
  geom_bar(position=position_dodge(), stat="identity", alpha = 0.5) +
  geom_errorbar(aes(ymin=grp.mean-se, ymax=grp.mean+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
su <- su + labs(x = "Care-giver identity", y = "Hours")
su <- su + theme(legend.title = element_blank()) + theme(legend.position = "right")
su <- su + theme_classic()
su <- su + scale_fill_brewer(name = "Care-giver identity", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared")) + scale_color_brewer(name = "Care-giver identity", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared"),palette = "Set1")
su <- su + scale_x_discrete(labels=c("Queen.reared" = "Queen-Reared", "Worker.reared" = "Worker-Reared"))
su


su1 <- ggplot(survival, aes(x=Avg.mm, y=Survival.hours))
su1 <- su1 + geom_smooth(method=lm, se=FALSE, color="black" )
su1 <- su1 +  geom_jitter(aes(color = Treatment)) +
  geom_smooth(aes(color = Treatment, fill = Treatment), linetype="dashed", method = lm) +
  scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
su1 <- su1 + labs(x = "Marginal cell length (mm)", y = "Survival (hours)")
su1 <- su1 + theme(legend.title = element_blank())
su1 <- su1 + theme_classic()
su1 <- su1 + scale_fill_brewer(name = "Care-giver identity", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared")) + scale_color_brewer(name = "Care-giver identity", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared"),palette = "Set1")
su1 <- su1 + theme(legend.position = "top")
su1 <- su1 + theme(text = element_text(size = 12))
su1


su2 <- ggscatter(survival, x = "Survival.hours", y = "Avg.mm",
                add = "reg.line", conf.int = TRUE,
                cor.coef = TRUE, cor.method = "spearman",
                xlab = "Survival (hours)", ylab = "Marginal celll length (mm)")
su2


su3 <-ggplot(survival, aes(x=Survival.hours, fill=Treatment, color=Treatment)) +
  geom_histogram(position="identity", alpha = 0.5, binwidth = 3)
su3 <- su3 + geom_vline(data=surv, aes(xintercept=grp.mean, color=Treatment),
                    linetype="dashed", size=0.7) 
su3 <- su3 + labs(x = "Survival (hours)", y = "No. bees")
su3 <- su3 + theme(legend.title = element_blank()) 
su3 <- su3 + theme_classic()
su3 <- su3 + scale_fill_brewer(name = "Care-giver identity", palette = "Set1",breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared")) + scale_color_brewer(name = "Care-giver identity", breaks=c("Queen.reared", "Worker.reared"),labels=c("Queen-Reared", "Worker-Reared"),palette = "Set1")
su3 <- su3 + theme(legend.position = "top")
su3 <- su3 + theme(text = element_text(size = 12))
su3

               
                #figure3 to export####
                l <- l + theme(legend.position="none")
                l1 <- l1 + theme(legend.position="none")
                su3 <- su3 + theme(legend.position="none")
                su1 <- su1 + theme(legend.position="none")
                
               
                fig3 <- ggarrange(
                  su3, su1,
                  widths = c(3, 3), heights = c(2.5, 0.2),
                  common.legend = TRUE, legend = "bottom",
                  labels = c("A", "B"))
                fig3
                
                #figure SI Learning to export####
                figSI <- ggarrange(
                  l,l1,
                  widths = c(3, 3), heights = c(2.5, 0.2),
                  common.legend = TRUE, legend = "bottom",
                  labels = c("A", "B"))
                figSI
                
