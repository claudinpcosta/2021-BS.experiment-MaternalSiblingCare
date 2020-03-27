##############################BS Experiment Summer 2019#######################################

#Set Working Directory (in my case, dropbox)

#Body Size Histrogram 
bodysize <- read.csv("bodysize.csv", header = T)
head(bodysize)
dim(bodysize)

#test t Body Size versus group.reared
testBSxGroup <- t.test(Wings ~ Group, bodysize)
testBSxGroup

library(plyr)
mu <- ddply(bodysize, "Group", summarise, grp.mean=mean(Wings))
head(mu)

# queen.reared <- subset(bodysize, Group == "queen.reared")
# summary(queen.reared)
# QReared <- data.frame(queen.reared)
# head(QReared)
# 
# worker.reared <- subset(bodysize, Group == "worker.reared")
# summary(worker.reared)
# WReared <- data.frame(worker.reared)
# head(WReared)

library(ggplot2)

h<-ggplot(bodysize, aes(x=Wings, fill=Group, color=Group)) +
  geom_histogram(position="identity", alpha = 0.5, binwidth = 0.1)
h <- h + geom_vline(data=mu, aes(xintercept=grp.mean, color=Group),
                    linetype="dashed") # Add mean lines
h <- h + labs(x = "Maginal cell length (mm)", y = "Count", title = "Body size")
h <- h + theme(legend.title = element_blank()) + theme(legend.position = "right")
h <- h + theme_classic()
h <- h + scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
h


# d<-ggplot(bodysize, aes(x=Wings, color=Group)) +
#   geom_histogram(aes(y=..density.., fill=Group), position="identity", alpha = 0.3, binwidth = 0.1) +
#   geom_density(size = 1) 
# d <- d 
# d <- d + labs(x = "Maginal celll length (mm)", y = "Count", title = "Body size")
# d <- d + theme(legend.title = element_blank()) + theme(legend.position = "right")
# d <- d + theme_classic() + scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
# d


#Development Time Histrogram 
devtime <- read.csv("developmentdays.csv", header = T)
head(devtime)
dim(devtime)

#test t Body Size versus group.reared
testDTxGroup <- t.test(Age ~ Group, devtime)
testDTxGroup

mi <- ddply(devtime, "Group", summarise, grp.mean=mean(Age))
head(mi)

t<-ggplot(devtime, aes(x=Age, fill=Group, color=Group)) +
  geom_histogram(position="identity", alpha = 0.5, binwidth = 1)
t <- t + geom_vline(data=mi, aes(xintercept=grp.mean, color=Group),
                    linetype="dashed") # Add mean lines
t <- t + labs(x = "Development time (Days)", y = "Count", title = "Development time")
t <- t + theme(legend.title = element_blank()) + theme(legend.position = "right")
t <- t + theme_classic()
t <- t + scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
t


#Correlation Time X Body Size 
bodytime <- read.csv("correlation.csv", header = T)
head(bodytime)
dim(bodytime)

#correlation ggpubr
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

library("ggpubr")

ggscatter(bodytime, x = "Age", y = "Wings",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Development time (Days)", ylab = "Maginal celll length (mm)")


c <- ggscatter(bodytime, x = "Wings", y = "Age", color = "Group",
               palette = "Set1",     
               add = "reg.line", conf.int = TRUE,
               cor.method = "spearman",
               xlab = "Maginal cell length (mm)", ylab = "Development time (Days)", title = "Body size versus Development time")
c <- c + theme(legend.title = element_blank()) + theme(legend.position = "right")
c

# c <- ggplot(bodytime, aes(x = Days, y = Wings))
# c <- c + geom_point(aes(color = Group)) +
#   geom_smooth(aes(color = Group, fill = Group), method = lm) +
#   scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
# c <- c + labs(x = "Development time (Days)", y = "Count")
# c <- c + theme(legend.title = element_blank()) + theme(legend.position = "right")
# c <- c + theme_classic()
# c <- c + scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
# c

#################################################################################################
figure_body <- ggarrange(h,t,c,
                         ncol = 2, nrow = 2 )
figure_body


#################################################################################################


#Sucrose Histogram 
sucrose <- read.csv("sucrose.csv", header = T)
head(sucrose)
dim(sucrose)
summary(sucrose)

s<- ggplot(sucrose, aes(x=Response, fill=Group, color=Group)) +
  geom_bar(data=sucrose, stat="count", position="dodge", alpha = 0.5)
s <- s + labs(x = "Assay Response (Sucrose)", y = "Count", title = "Sucrose response versus Group reared")
s <- s + theme(legend.title = element_blank()) + theme(legend.position = "right")
s <- s + theme_classic()
s <- s + scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
s


s1<- ggplot(sucrose, aes(x=Conc, fill=Group, color=Group)) +
  geom_bar(data=sucrose, stat="count", position="dodge", alpha = 0.5)
s1 <- s1 + labs(x = "Sucrose Response (Concentration)", y = "Count", title = "Sucrose response (Concentration) versus Group reared")
s1 <- s1 + theme(legend.title = element_blank()) + theme(legend.position = "right")
s1 <- s1 + theme_classic()
s1 <- s1 + scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
s1


s2 <- ggplot(sucrose, aes(x=Conc, y=Wings, fill=Group, color=Group)) + 
  geom_count(alpha = 0.6)
s2 <- s2 + labs(x = "Sucrose Response (Concentration)", y = "Maginal celll length (mm)", title = "Sucrose response versus Body size")
s2 <- s2 + theme(legend.title = element_blank()) + theme(legend.position = "right")
s2 <- s2 + theme_classic()
s2 <- s2 + scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
s2


s3 <- ggplot(sucrose, aes(x=Conc, y=Wings)) + 
  geom_count()
s3 <- s3 + labs(x = "Sucrose Response (Concentration)", y = "Maginal celll length (mm)", title = "Sucrose response versus Body size")
s3 <- s3 + theme(legend.title = element_blank()) + theme(legend.position = "right")
s3 <- s3 + theme_classic()
s3 <- s3 + scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
s3


s4 <- ggplot(sucrose, aes(x=Conc, y=Wings, fill=Group, color=Group)) + 
  geom_point(alpha = 0.6, position = "jitter")
s4 <- s4 + labs(x = "Sucrose Response (Concentration)", y = "Maginal cell length (mm)", title = "Sucrose response versus Body size")
s4 <- s4 + theme(legend.title = element_blank()) + theme(legend.position = "right")
s4 <- s4 + theme_classic()
s4 <- s4 + scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
s4



s5 <- ggplot(sucrose,aes(x = Group,fill = Response)) + 
  geom_bar(position = "fill")+ylab("Proportion Responded")+scale_fill_discrete(name="Responded to Sucrose")
s5 <- s5 + labs(x = "Treatment", title = "Sucrose response")
s5 <- s5 + theme(legend.title = element_blank()) + theme(legend.position = "right")
s5 <- s5 + theme_classic()
s5 <- s5 + scale_fill_brewer(palette = "Dark2") + scale_color_brewer(palette = "Dark2")
s5



#concentration

sucroseCon <- read.csv("sucroseCon.csv", header = T)
head(sucroseCon)
dim(sucroseCon)
summary(sucroseCon)


s6 <-ggplot(sucroseCon, aes(x=Sucrose.conc, fill=Treatment)) +
  geom_histogram(position="identity", alpha = 0.5, binwidth = 0.05)
s6 <- s6 + labs(x = "Sucrose concentration", y = "Count", title = "Sucrose response versus Sucrose concentration")
s6 <- s6 + theme(legend.title = element_blank()) + theme(legend.position = "right")
s6 <- s6 + theme_classic()
s6 <- s6 + scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")
s6



#################################################################################################

figure_sucrose <- ggarrange(s, s1, s2, s3, s4,
                            ncol = 2, nrow = 3 )
figure_sucrose

#################################################################################################

#Learning
learning <- read.csv("Learning.csv", header = T)
head(learning)
dim(learning)
summary(learning)

l <- ggplot(learning,aes(x = Group,fill = LearningTraining)) + 
  geom_bar(position = "fill")+ylab("Proportion Trained")
l <- l + labs(x = "Treatment", title = "Learning training")
l <- l + theme(legend.title = element_blank()) + theme(legend.position = "right")
l <- l + theme_classic()
l <- l + scale_fill_brewer(palette = "Dark2") + scale_color_brewer(palette = "Dark2")
l


l1 <- ggplot(learning,aes(x = Group,fill = LearningTest)) + 
  geom_bar(position = "fill")+ylab("Proportion Successful")
l1 <- l1 + labs(x = "Treatment", title = "Learning successful")
l1 <- l1 + theme(legend.title = element_blank()) + theme(legend.position = "right")
l1 <- l1 + theme_classic()
l1 <- l1 + scale_fill_brewer(palette = "Dark2") + scale_color_brewer(palette = "Dark2")
l1

learningNu <- read.csv("LearningNumber.csv", header = T)
head(learningNu)
dim(learningNu)
summary(learningNu)

c1 <- ggplot(learningNu, aes(x=LearningTraining, y=Wings, color = Group, fill = Group))+
  geom_point(aes(color = Group))+
  geom_smooth(aes(color = Group, fill = Group), method = lm)+
  labs(x = "Maginal cell length (mm)", y = "Maginal cell length (mm)", title = "Learning successful versus Body size") +
  scale_fill_brewer(palette = "Set1") + scale_color_brewer(palette = "Set1")+
  theme_classic()+
  theme(legend.title = element_blank()) + theme(legend.position = "right")
c1



#################################################################################################

#Survival
survival <- read.csv("survival.csv", header = T)
head(survival)
dim(survival)
summary(survival)

#test t survival versus group.reared
testSxGroup <- t.test(Survival ~ Group, survival)
testSxGroup





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



