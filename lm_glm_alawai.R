#linear and categorical models reported in the Ala Wai study

# Set working directory
setwd("C:/Users/chris/OneDrive/Documents/UH/Ch2Fluorometer/R")

# Load needed libraries 
library(MuMIn) # for dredge() in model selection
library(tidyverse)

# Clear workspace
rm(list=ls()) 
#load DsquaredR in case we want to compare our model with GLM
source('Dsquared.r') # Estimates explained deviance.  See Dsquared.R.
#myData <- read.csv("~/UH/Ch2Fluorometer/turbidity_strict_v2.csv")
myData <- read.csv("~/UH/Ch2Fluorometer/turbidity_strict_add2026.csv")
str(myData) # Examine data file structure

resp = "logentero" # Defining our response name
hist(myData[,resp]) #visualize response variable histogram, check for approximately normal distribution



#linear models
myFit<-lm(logentero~logtr+logob+logturb+expsal+temp+logchl,data=myData)
options(na.action = "na.fail") # change na.action (required by dredge)
dd<-dredge(myFit) # Check all possible model combinations using dredge in MuMIn
BestMods<-subset(dd, delta <2)
BestMods #see which models are best fit, delta AICc <2 
(myFitBestInt<-(eval(attr(dd, "model.calls")$'29'))) 
summary(myFitBestInt) #view specific model number results, change number above to view others
par(mfrow=c(2,2)) 
plot(myFitBestInt)#model summary plots
dd #view all models

#categorical models

library(brglm2)
options(na.action = "na.fail")
myFit= glm(entero130~logturb+logtr+logob+expsal+logchl+temp, data=myData, family=binomial(link="logit"), method=brglmFit)
dd<-dredge(myFit)
BestMods<-subset(dd, delta <2)
BestMods
(myFitBestInt<-(eval(attr(dd, "model.calls")$'49')))
summary(myFitBestInt)
par(mfrow=c(2,2))
plot(myFitBestInt)        