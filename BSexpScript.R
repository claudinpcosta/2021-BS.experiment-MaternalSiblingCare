##############################BS Experiment Summer 2019#######################################

###Set Working Directory

####all packages used here####
library(plyr)
library(ggplot2) #Makes pretty graphs
library(ggpubr) #Makes graphs
library(lme4) #Runs linear mixed effect models
library(multcomp) #Posthoc analyses
library(car)
library(AICcmodavg)
library(lmtest)

####loading dataset####
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

####Body Size analysism####
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

#histograma Body Size (by wing measurement) versus Treatment (queen vs worker reared)
h<-ggplot(bodysize, aes(x=Avg.mm, fill=Treatment, color=Treatment)) +
  geom_histogram(position="identity", alpha = 0.5, binwidth = 0.1)
h <- h + geom_vline(data=mu, aes(xintercept=grp.mean, color=Treatment),
                    linetype="dashed") # Add mean lines
h <- h + labs(x = "Maginal cell length (mm)", y = "Count", title = "Body size")
h <- h + theme(legend.title = element_blank()) + theme(legend.position = "right")
h <- h + theme_classic()
h <- h + scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
h

####Development time analysis####
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

#histograma Developmental time versus Treatment (queen vs worker reared)
t<-ggplot(devtime, aes(x=Time.development.days, fill=Treatment, color=Treatment)) +
  geom_histogram(position="identity", alpha = 0.5, binwidth = 1)
t <- t + geom_vline(data=mi, aes(xintercept=grp.mean, color=Treatment),
                    linetype="dashed") # Add mean lines
t <- t + labs(x = "Developmental time (Days)", y = "Count", title = "Developmental time")
t <- t + theme(legend.title = element_blank()) + theme(legend.position = "right")
t <- t + theme_classic()
t <- t + scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
t

####Correlation Developmental Time X Body Size####

#excluding NA values 
bodytime <- subset(data.BS, !is.na(Avg.mm) & !is.na(Time.development.days))
head(bodytime)
dim(bodytime)
summary(bodytime)
attach(bodytime)

#correlation ggpubr
ggscatter(bodytime, x = "Time.development.days", y = "Avg.mm",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Development time (Days)", ylab = "Maginal celll length (mm)")


c <- ggplot(bodytime, aes(x = Time.development.days, y = Avg.mm))
c <- c + geom_point(aes(color = Treatment)) +
  geom_smooth(aes(color = Treatment, fill = Treatment), method = lm) +
  scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
c <- c + labs(x = "Developmental time (Days)", y = "Maginal cell length (mm)", title = "Developmental time versus Body size")
c <- c + theme(legend.title = element_blank()) + theme(legend.position = "right")
c <- c + theme_classic()
c <- c + scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
c

#Figures
figureA <- ggarrange(h,t,c,
                     ncol = 2, nrow = 2 )
figureA

####Sucrose graphics####
summary(Sucrose.summary)

#Sucrose data 
sucrose <- bodytime[-which(Sucrose.summary == ""),]
dim(sucrose)
head(sucrose)
tail(sucrose)
summary(sucrose)

#different options of graphics for sucrose assay 
s<- ggplot(sucrose, aes(x=Sucrose.summary, fill=Treatment, color=Treatment)) +
  geom_bar(data=sucrose, stat="count", position="dodge", alpha = 0.5)
s <- s + labs(x = "Assay Response (Sucrose)", y = "Count", title = "Sucrose response versus Group reared")
s <- s + theme(legend.title = element_blank()) + theme(legend.position = "right")
s <- s + theme_classic()
s <- s + scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
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
s2 <- s2 + scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
s2

s3 <- ggplot(sucrose, aes(x=Sucrose.conc, fill=Treatment, color=Treatment)) +
  geom_bar(position = "fill", alpha = 0.5)+ylab("Proportion Responded")+scale_fill_discrete(name="Responded to Sucrose")
s3 <- s3 + labs(x = "Sucrose Response (Concentration)", title = "Sucrose response (Concentration) versus Group reared")
s3 <- s3 + theme(legend.title = element_blank()) + theme(legend.position = "right")
s3 <- s3 + theme_classic()
s3 <- s3 + scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
s3

s4 <-ggplot(sucrose, aes(x=Sucrose.conc, fill=Treatment, color=Treatment)) +
  geom_histogram(position="identity", alpha = 0.5, binwidth = 0.05)
s4 <- s4 + labs(x = "Sucrose Response (Concentration)", title = "Sucrose response (Concentration) versus Group reared")
s4 <- s4 + theme(legend.title = element_blank()) + theme(legend.position = "right")
s4 <- s4 + theme_classic()
s4 <- s4 + scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
s4

s5 <- ggplot(sucrose, aes(x=Sucrose.conc, y=Avg.mm, fill=Treatment, color=Treatment)) + 
  geom_point(alpha = 0.6, position = "jitter")
s5 <- s5 + labs(x = "Sucrose Response (Concentration)", y = "Maginal cell length (mm)", title = "Sucrose response versus Body size")
s5 <- s5 + theme(legend.title = element_blank()) + theme(legend.position = "right")
s5 <- s5 + theme_classic()
s5 <- s5 + scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
s5

####Learning graphics####
summary(LearningTraining.summary)
summary(LearningTest.summary)

#Learning data 
learningTraining <- bodytime[-which(LearningTraining.summary == ""),]
dim(learningTraining)
head(learningTraining)
tail(learningTraining)
summary(learningTraining)

learningTest <- bodytime[-which(LearningTest.summary == ""),]
dim(learningTest)
head(learningTest)
tail(learningTest)
summary(learningTest)

#different options of graphics for learning assay 

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


####Survival graphics####
summary(Survival.hours)

#Survival data 
survival <- subset(bodytime, !is.na(Survival.hours))
head(survival)
dim(survival)
summary(survival$Survival.hours)

cdata <- ddply(survival, c("Treatment"), summarise,
               N    = sum(Survival.hours),
               mean = mean(Survival.hours, na.rm=TRUE),
               sd   = sd(Survival.hours, na.rm=TRUE),
               se   = sd / sqrt(N))
cdata

#barplot to survival
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

####Stats####

####Body size stats####

#testing whether a random sample of data comes from a normal distribution
shapiro.test(bodytime$Avg.mm)
ggdensity(bodytime$Avg.mm)
hist(bodytime$Avg.mm)

#obs.: data doesn't come from a normal distribution: Gamma

BSglobal<-glm(Avg.mm~Treatment+Time.development.days+ColonyID_queen, data=bodytime, family=Gamma(link = "inverse")) ## Global Model 
BS1<-glm(Avg.mm~Time.development.days+ColonyID_queen, data=bodytime, family=Gamma(link = "inverse")) #testing Treatment (queen.reared vs worker.reared)
BS2<-glm(Avg.mm~Treatment+ColonyID_queen, data=bodytime, family=Gamma(link = "inverse")) #testing developmental time
BS3<-glm(Avg.mm~Treatment+Time.development.days, data=bodytime, family=Gamma(link = "inverse")) #testing colony source 

lrtest(BSglobal, BS1) 
lrtest(BSglobal, BS2) 
lrtest(BSglobal, BS3) 

#testing differences in variance between two treatments

#body size vs rearing history 
wilcox.test(Avg.mm~Treatment, data=bodytime)

#developmental time vs rearing history 
wilcox.test(Time.development.days~Treatment, data=bodytime)

####Perfomance stats####

####Survival stats####
summary(survival$Survival.hours)

#testing whether a random sample of data comes from a normal distribution
shapiro.test(survival$Survival.hours)
ggdensity(survival$Survival.hours)
hist(survival$Survival.hours)

#obs.: data doesn't come from a normal distribution: Poisson 

Sglobal<-glm(Survival.hours~Avg.mm+Treatment, data=survival, family=poisson(link="log")) #Global Model 
S1<-glm(Survival.hours~Avg.mm, data=survival, family=poisson(link="log")) #testing Treatment (queen.reared vs worker.reared)
S2<-glm(Survival.hours~Treatment, data=survival, family=poisson(link="log")) #testing body size
S3<-glm(Survival.hours~Avg.mm*Treatment, data=survival, family=poisson(link="log")) #interaction 

lrtest(Sglobal, S1) 
lrtest(Sglobal, S2) 
lrtest(Sglobal, S3) 

####Learning stats####

#obs.: data is binomial (yes - 1 or no - 0): binomal

#LearningTraining
TrainGlobal<-glm(LearningTraining~Treatment+Avg.mm+Time.development.days, data = learningTraining, family = binomial(link = "logit")) #Global Model
Train1<-glm(LearningTraining~Avg.mm, data = learningTraining, family = binomial(link = "logit")) #testing Treatment (queen.reared vs worker.reared) & developmental time
Train2<-glm(LearningTraining~Treatment, data = learningTraining, family = binomial(link = "logit")) #testing body size & developmental time
Train3<-glm(LearningTraining~Treatment+Avg.mm, data = learningTraining, family = binomial(link = "logit")) # #testing developmental time
Train4<-glm(LearningTraining~Time.development.days+Avg.mm, data = learningTraining, family = binomial(link = "logit")) ##testing treatment
Train5<-glm(LearningTraining~Time.development.days+Treatment, data = learningTraining, family = binomial(link = "logit")) ##testing body size

lrtest(TrainGlobal, Train1)
lrtest(TrainGlobal, Train2)
lrtest(TrainGlobal, Train3)
lrtest(TrainGlobal, Train4)
lrtest(TrainGlobal, Train5)

#LearningTesting
TestGlobal<-glm(LearningTest~Treatment+Avg.mm+Time.development.days, data = learningTest, family = binomial(link = "logit")) #Global Model
Test1<-glm(LearningTest~Avg.mm, data = learningTest, family = binomial(link = "logit")) #testing Treatment (queen.reared vs worker.reared) & developmental time
Test2<-glm(LearningTest~Treatment, data = learningTest, family = binomial(link = "logit")) #testing body size & developmental time
Test3<-glm(LearningTest~Treatment+Avg.mm, data = learningTest, family = binomial(link = "logit")) #testing developmental time
Test4<-glm(LearningTest~Avg.mm+Time.development.days, data = learningTest, family = binomial(link = "logit")) #testing treatment
Test5<-glm(LearningTest~Treatment+Time.development.days, data = learningTest, family = binomial(link = "logit")) #testing body size

lrtest(TestGlobal, Test1)
lrtest(TestGlobal, Test2)
lrtest(TestGlobal, Test3)
lrtest(TestGlobal, Test4)
lrtest(TestGlobal, Test5)

####Learning stats####

#Sucrose.response
#obs.: we have data is binomial (yes - 1 or no - 0): binomal

SRGlobal<-glm(Sucrose.response~Treatment+Avg.mm+Time.development.days, data = sucrose, family = binomial(link = "logit")) #Global Model
SR1<-glm(Sucrose.response~Avg.mm, data = sucrose, family = binomial(link = "logit")) #testing Treatment (queen.reared vs worker.reared) & developmental time
SR2<-glm(Sucrose.response~Treatment, data = sucrose, family = binomial(link = "logit")) #testing body size & developmental time
SR3<-glm(Sucrose.response~Treatment+Avg.mm, data = sucrose, family = binomial(link = "logit")) # #testing developmental time
SR4<-glm(Sucrose.response~Time.development.days+Avg.mm, data = sucrose, family = binomial(link = "logit")) ##testing treatment
SR5<-glm(Sucrose.response~Time.development.days+Treatment, data = sucrose, family = binomial(link = "logit")) ##testing body size

lrtest(SRGlobal, SR1)
lrtest(SRGlobal, SR2)
lrtest(SRGlobal, SR3)
lrtest(SRGlobal, SR4)
lrtest(SRGlobal, SR5)

#Sucrose.concentration
suc.con <- subset(sucrose, !is.na(Sucrose.conc))

#testing whether a random sample of data comes from a normal distribution
shapiro.test(suc.con$Sucrose.conc)
ggdensity(suc.con$Sucrose.conc)
hist(suc.con$Sucrose.conc)

#obs.: data doesn't come from a normal distribution:  poisson

suc.con$Sucrose.conc <- suc.con$Sucrose.conc * 100 #The Poisson model is about counts. Count values are positive natural numbers including zero (0, 1, 2, 3, ...)

SCGlobal<-glm(Sucrose.conc~Treatment+Avg.mm+Time.development.days, data = suc.con, family = poisson(link = "log")) #Global Model
SC1<-glm(Sucrose.conc~Avg.mm, data = suc.con, family = poisson(link = "log")) #testing Treatment (queen.reared vs worker.reared) & developmental time
SC2<-glm(Sucrose.conc~Treatment, data = suc.con, family = poisson(link = "log")) #testing body size & developmental time
SC3<-glm(Sucrose.conc~Treatment+Avg.mm, data = suc.con, family = poisson(link = "log")) # #testing developmental time
SC4<-glm(Sucrose.conc~Time.development.days+Avg.mm, data = suc.con, family = poisson(link = "log")) ##testing treatment
SC5<-glm(Sucrose.conc~Time.development.days+Treatment, data = suc.con, family = poisson(link = "log")) ##testing body size

lrtest(SCGlobal, SC1)
lrtest(SCGlobal, SC2)
lrtest(SCGlobal, SC3)
lrtest(SCGlobal, SC4)
lrtest(SCGlobal, SC5)


