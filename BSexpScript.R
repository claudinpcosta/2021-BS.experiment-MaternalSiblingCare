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

  #test t Body Size (by wing measurement) versus Treatment (queen vs worker reared)
testBSxTreat <- t.test(Avg.mm ~ Treatment, bodysize)
testBSxTreat

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

  #test t Body Size versus group.reared
testDTxTreat <- t.test(Time.development.days ~ Treatment, devtime)
testDTxTreat

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

  #test t survival versus group.reared
testSxGroup <- t.test(Survival.hours ~ Treatment, survival)
testSxGroup

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

  ####Body size####
BS1<-glm(Avg.mm~Treatment+Time.development.days+ColonyID_queen, data=bodytime, family=gaussian(link="identity")) ## Global Model 
BS2<-glm(Avg.mm~Time.development.days+ColonyID_queen, data=bodytime, family=gaussian(link="identity"))
BS3<-glm(Avg.mm~Treatment+ColonyID_queen, data=BS1$model, family=gaussian(link="identity")) 
BS4<-glm(BS~Treatment+Time.development, data=alldata, family=gaussian(link="identity"))

lrtest(BS1, BS2) ##Treatment p=0.01
lrtest(BS1, BS3) ##Development time p=0.07
lrtest(BS1, BS4) ##Queen source colony not significant p=0.4




########################################By Kaleigh###############################################
#BS Experiment Summer 2019
#Set Working Directory  
## Import Data 
alldata<-read.csv("BS.S19.csv", sep=',', header=TRUE)
head(alldata)
survival<-subset(alldata, Survival.hours >=5, select=c(BeeID, MicrocolonyID, Treatment, BS, Time.development, Survival.hours))
attach(alldata)
detach(alldata)

learning<-read.csv("learning.csv", header=TRUE)
sucrose<-read.csv("sucrose.csv", header=TRUE)
attach(learning)
#Packages 
library(ggplot2) #Makes pretty graphs
library(lme4) #Runs linear mixed effect models
library(multcomp) #Posthoc analyses
library(car)
library(AICcmodavg)
library(lmtest)

##BODY SIZE STATS 


BS1<-glm(BS~Treatment+Time.development+ColonyID_queen, data=alldata, family=gaussian(link="identity")) ## Global Model 
BS2<-glm(BS~Time.development+ColonyID_queen, data=alldata, family=gaussian(link="identity"))
BS3<-glm(BS~Treatment+ColonyID_queen, data=BS1$model, family=gaussian(link="identity")) 
BS4<-glm(BS~Treatment+Time.development, data=alldata, family=gaussian(link="identity"))

lrtest(BS1, BS2) ##Treatment p=0.01
lrtest(BS1, BS3) ##Development time p=0.07
lrtest(BS1, BS4) ##Queen source colony not significant p=0.4

##PLOT for body size
cdat<-ddply(alldata, "Treatment", summarise, BS.mean=mean(BS, na.rm=TRUE))

graph<-ggplot(alldata, aes(x=BS, fill=Treatment)) +
  geom_histogram(binwidth=.1, alpha=.5, position="identity") +
  geom_vline(data=cdat, aes(xintercept=BS.mean,  colour=Treatment),
             linetype="dashed", size=1)+xlab("Body Size (mm)")+ylab("Frequency")+scale_fill_brewer(palette="Set1")

graph+theme(axis.title=element_text(size=14, face="bold"))+scale_fill_discrete(name="Rearing History",
                                                                               breaks=c("queen.reared", "worker.reared"),
                                                                               labels=c("Queen Reared", "Worker Reared"))


pdat<-ddply(alldata, "Treatment", summarise, Time.development.mean=mean(Time.development, na.rm=TRUE))

graph<-ggplot(alldata, aes(x=Time.development, fill=Treatment)) +
  geom_histogram(binwidth=1, alpha=.5, position="identity") +
  geom_vline(data=pdat, aes(xintercept=Time.development.mean,  colour=Treatment),
             linetype="dashed", size=1)+xlab("Development Time (days)")+ylab("Frequency")+scale_fill_brewer(palette="Set1")

graph+theme(axis.title=element_text(size=14, face="bold"))+scale_fill_discrete(name="Rearing History",
                                                                               breaks=c("queen.reared", "worker.reared"),
                                                                               labels=c("Queen Reared", "Worker Reared"))

##Levene's test to test for differences in variance between two treatments

leveneTest(BS~Treatment, data=alldata)

leveneTest(Time.development~Treatment, data=alldata)


##Development time plot
regression.plot<-ggplot(alldata, aes(x=Time.development, y=BS, color=Treatment)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm, fullrange=TRUE)

regression.plot+xlab("Development Time (days)")+ylab("Body size (mm)")



## PERFORMANCE STATS 

#1) Survival

hist(alldata$Survival.hours) ##Poisson distribution

SP1<-glm(alldata$Survival.hours~alldata$BS+alldata$ColonyID_queen+alldata$Treatment, family=poisson(link="log")) 
SP2<-glm(alldata$Survival.hours~alldata$BS+alldata$Treatment,family=poisson(link="log"))
SP3<-glm(alldata$Survival.hours~alldata$BS, family=poisson(link="log"))
SP4<-glm(alldata$Survival.hours~alldata$Treatment,  data=SP2$model, family=poisson(link="log"))

lrtest(SP2, SP3)
lrtest(SP2, SP4) 



survival.plot<-ggplot(alldata, aes(x=BS, y=Survival.hours, fill= Treatment, color=Treatment)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm, fullrange=TRUE)+
  labs(x = "Maginal cell length (mm)", y = "Survival Time (hours)", title = "Survival versus Body size") 
r <- survival.plot + theme(legend.title = element_blank()) + theme(legend.position = "right")
r <- r + theme_classic()
r <- r + scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
r


cdata <- ddply(alldata, c("Treatment"), summarise,
               N    = sum(!is.na(Survival.hours)),
               mean = mean(Survival.hours, na.rm=TRUE),
               sd   = sd(Survival.hours, na.rm=TRUE),
               se   = sd / sqrt(N)
)



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

##Learning

tmodel<-glm(alldata$LearningTraining~alldata$Treatment+alldata$BS, binomial)
tmodel2<-glm(alldata$LearningTraining~alldata$Treatment, data=tmodel$model, binomial)

lrtest(tmodel, tmodel2) ##Body size influences training regardless of treatment


Lmodel<-glm(LearningTest~Treatment+BS+Time.development, binomial)
Lmodel2<-glm(LearningTest~Treatment+BS, binomial)
Lmodel3<-glm(LearningTest~Treatment+Time.development, binomial)
Lmodel4<-glm(LearningTest~BS+Time.development, binomial)
Lmodel5<-glm(LearningTest~BS, binomial)

ggplot(learning,aes(x = Treatment,fill = LearningTest)) + 
  geom_bar(position = "fill")+ylab("Proportion Trained")+scale_fill_discrete(name="Trained")+scale_fill_brewer(palette="RdYlGn")

ggplot(learning, aes(x=Treatment, fill=LearningTest))+geom_bar(position = "fill")+ylab("Proportion Successful")+scale_fill_discrete(name="Chose Correct Color")


ggplot(alldata, aes(x=LearningTraining, y=BS, color=Treatment))+geom_point(shape=1)+geom_smooth(method=lm)+ylab("Body Size")+xlab("Successfully Trained")


##Sucrose Responsivness

attach(sucrose)
Smodel1<-glm(Response~Treatment+BS+Time.development, binomial)

ggplot(sucrose,aes(x = Treatment,fill = Response)) + 
  geom_bar(position = "fill")+ylab("Proportion Responded")+scale_fill_discrete(name="Responded to Sucrose")

hist(Sucrose.conc)
Smodel2<-glm(Sucrose.conc~Treatment+BS+Time.development, family=poisson(link="log"))
Smodel3<-glm(Sucrose.conc~Treatment, family=poisson(link="log"))


sdat<-ddply(sucrose, "Treatment", summarise, sucrose.mean=mean(Sucrose.conc, na.rm=TRUE))

ggplot(sucrose, aes(x=Sucrose.conc, fill=Treatment)) +
  geom_histogram(binwidth=.05, alpha=.5, position="identity") +
  geom_vline(data=sdat, aes(xintercept=sucrose.mean,  colour=Treatment),
             linetype="dashed", size=1)+xlab("Sucrose Concentration")+ylab("Frequency")+scale_fill_brewer(palette="Set1")



