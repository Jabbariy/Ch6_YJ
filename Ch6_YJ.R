library(aod)
library (readxl)
library(ggplot2)
library(multcomp)
library(lme4)
library(nlme)
library(emmeans)
library(lmtest)
library (car)

setwd("")
data<- read_excel("Ch6_YJ.xlsx")

data$GPSDepandanceScore<- as.integer(data$GPSDepandanceScore)
data$SAQImager<- as.integer(data$SAQImager)
data$SAQNavigation<-as.integer(data$SAQNavigation)
data$SAQMentalmanipulation<-as.integer(data$SAQMentalmanipulation)
data$ID <- factor(data$ID)
data$GPSRelianceScore<- as.integer(data$GPSRelianceScore)
data$SBSODScore<-as.integer(data$SBSODScore)
data$DASSDEPRESSION<-as.integer(data$DASSDEPRESSION)
data$DASSANXIETY<-as.integer(data$DASSANXIETY)
data$DASSSTRESS<-as.integer(data$DASSSTRESS)
data$AdexiScore<-as.integer(data$AdexiScore)
data$GPSScore<-as.integer(data$GPSScore)
data$AccDiff<-as.integer(data$AccDiff)
data$AccSame<-as.integer(data$AccSame)
data$DriveFrequency<-as.integer(data$DriveFrequency)
data$rt_test_same<-as.integer(data$rt_test_same)
data$rt_test_diff<-as.integer(data$rt_test_diff)

attach(data)

#SBSOD
fit.lm2 <- lm(SBSODScore~poly(SAQMentalmanipulation,2)+poly(SAQNavigation,2)+poly(SAQImager,2))
summary(fit.lm2) 
fit.lm2 <- lm(SBSODScore~poly(DASSANXIETY,2)+poly(DASSDEPRESSION,2)+poly(DASSSTRESS,2))
summary(fit.lm2) 
fit.lm2 <- lm(SBSODScore~poly(AdexiScore,2))
summary(fit.lm2)

#GPS SCORE
fit.lm2 <- lm(GPSScore~poly(SAQMentalmanipulation,2)+poly(SAQNavigation,2)+poly(SAQImager,2))
summary(fit.lm2) 
fit.lm2 <- lm(GPSScore~poly(DASSANXIETY,2)+poly(DASSDEPRESSION,2)+poly(DASSSTRESS,2))
summary(fit.lm2) 
plot(fitted(fit.lm2))
abline(fit.lm2)
avPlots(fit.lm2)

fit.lm2 <- lm(GPSScore~poly(AdexiScore,2))
summary(fit.lm2)
fit.lm2 <- lm(GPSScore~poly(SBSODScore,2))
summary(fit.lm2)

#Accuracy
fit.lm2 <- lm(AccSame~poly(SAQMentalmanipulation,2)+poly(SAQNavigation,2)+poly(SAQImager,2)+DriveFrequency)
summary(fit.lm2)
fit.lm2 <- lm(AccDiff~poly(SAQMentalmanipulation,2)+poly(SAQNavigation,2)+poly(SAQImager,2))
summary(fit.lm2)

fit.lm2 <- lm(AccSame~poly(DASSANXIETY,2)+poly(DASSDEPRESSION,2)+poly(DASSSTRESS,2)+ DriveFrequency)
summary(fit.lm2) 
fit.lm2 <- lm(AccDiff~poly(DASSANXIETY,2)+poly(DASSDEPRESSION,2)+poly(DASSSTRESS,2))
summary(fit.lm2) 

fit.lm2 <- lm(AccSame~poly(AdexiScore,2))
summary(fit.lm2)
avPlots(fit.lm2)
fit.lm2 <- lm(AccDiff~poly(SBSODScore,2))
summary(fit.lm2)

fit.lm2 <- lm(AccSame~poly(GPSScore,2))
summary(fit.lm2)

#RT

fit.lm2 <- lm(rt_test_same~poly(SAQMentalmanipulation,2)+poly(SAQNavigation,2)+poly(SAQImager,2))
summary(fit.lm2)
fit.lm2 <- lm(rt_test_diff~poly(SAQMentalmanipulation,2)+poly(SAQNavigation,2)+poly(SAQImager,2))
summary(fit.lm2)

fit.lm2 <- lm(rt_test_same~poly(DASSANXIETY,2)+poly(DASSDEPRESSION,2)+poly(DASSSTRESS,2))
summary(fit.lm2) 
fit.lm2 <- lm(rt_test_diff~poly(DASSANXIETY,2)+poly(DASSDEPRESSION,2)+poly(DASSSTRESS,2))
summary(fit.lm2) 

fit.lm2 <- lm(rt_test_diff~poly(AdexiScore,2))
summary(fit.lm2)
avPlots(fit.lm2)
fit.lm2 <- lm(rt_test_diff~poly(SBSODScore,2))
summary(fit.lm2)

fit.lm2 <- lm(rt_test_diff~poly(GPSScore,2))
summary(fit.lm2)
##################

fit.lm <- lm(GPSDepandanceScore~SAQMentalmanipulation+SAQNavigation+SAQImager)
summary(fit.lm)

plot(AdexiScore,GPSScore)
abline(lm(GPSScore~AdexiScore), col = 'red')
lines(lowess(AdexiScore, GPSScore), col = 3, lwd = 3)

plot(DASSDEPRESSION,GPSScore)
abline(lm(GPSScore~DASSDEPRESSION), col = 'red')
lines(lowess(DASSDEPRESSION, GPSScore), col = 3, lwd = 3)

plot(DASSANXIETY,GPSScore)
abline(lm(GPSScore~DASSANXIETY), col = 'red')
lines(lowess(DASSANXIETY, GPSScore), col = 3, lwd = 3)

plot(DASSANXIETY,GPSScore)
abline(lm(GPSScore~DASSANXIETY), col = 'red')
lines(lowess(DASSANXIETY, GPSScore), col = 3, lwd = 3)

fit.lm2 <- lm(GPSDepandanceScore~poly(SAQMentalmanipulation,2)+poly(SAQNavigation,2)+poly(SAQImager,2))
summary(fit.lm2)              
fit.interaction <- lm(GPSDepandanceScore~SAQMentalmanipulation+SAQNavigation+SAQImager+SAQImager*SAQNavigation)
summary(fit.interaction)

########Seperate Analysis of Dependnace and Reliance

#GPSReliance
fit.lm2 <- lm(GPSRelianceScore~poly(SAQMentalmanipulation,2)+poly(SAQNavigation,2)+poly(SAQImager,2))
summary(fit.lm2) 
fit.lm2 <- lm(GPSRelianceScore~poly(SBSODScore,2))
summary(fit.lm2) 
fit.lm2 <- lm(GPSRelianceScore~poly(DASSANXIETY,2)+poly(DASSDEPRESSION,2)+poly(DASSSTRESS,2))
summary(fit.lm2) 
fit.lm2 <- lm(GPSRelianceScore~poly(AdexiScore,2))
summary(fit.lm2)

#GPSDepandanceScore
fit.lm2 <- lm(GPSDepandanceScore~poly(SAQMentalmanipulation,2)+poly(SAQNavigation,2)+poly(SAQImager,2))
summary(fit.lm2) 
fit.lm2 <- lm(GPSDepandanceScore~poly(SBSODScore,2))
summary(fit.lm2)
fit.lm2 <- lm(GPSDepandanceScore~poly(DASSANXIETY,2)+poly(DASSDEPRESSION,2)+poly(DASSSTRESS,2))
summary(fit.lm2) 
fit.lm2 <- lm(GPSDepandanceScore~poly(AdexiScore,2))
summary(fit.lm2)
