library(tidyverse)
# Install and load the vegan package if you haven't already
#install.packages("vegan")
library(vegan)
#install.packages("ggplot2")
#library(ggplot2)
#install.packages("ggcorrplot")
library(ggcorrplot)
library(ggplot2)
library(glue)
library(caret)

# Clear workspace
rm(list=ls()) 
#mydata <- read.csv("~/UH/Ch2Fluorometer/ob_entero_field.csv")
#mydata <- read.csv("~/UH/Ch2Fluorometer/ob_tr_field.csv")
#mydata <-read.csv("~/UH/Ch2Fluorometer/ob_entero_lab.csv")
#mydata <- read.csv("~/UH/Ch2Fluorometer/turbidity_strict_v2.csv")
#mydata <- read.csv("~/UH/Ch2Fluorometer/turbidity_strict_v2_sucralose_nounderscores.csv")
myData <- read.csv("~/UH/Ch2Fluorometer/turbidity_strict_add2026.csv")

#TRY CROSS VALIDATION METHODS from https://www.modernstatisticswithr.com/mlchapter.html
#train_control <- trainControl(method = "repeatedcv", number=10, repeats=100, savePredictions=TRUE, classProbs=TRUE)
train_control <- trainControl(method = "LOOCV", summaryFunction = twoClassSummary, savePredictions=TRUE, classProbs=TRUE)


#myData$enterobi <- factor(myData$enterobi)
myData$enterobi <- factor(myData$enterobi)
#myData$enterobowls <- factor(myData$enterobowls)

#mydata$type <-factor(mydata$type)
model <- train(enterobi ~ logob + logturb + expsal, data = myData, method = "glm", family = "binomial", trControl = train_control)
print(model)
library(MLeval)
plots <- evalm(model) #I think this is the opposite Pos/Negative of caret package, !!!!
#this shows positive "under" and negative "over" but caret has it as "over" "under"
plots$ROC

#95% condfence interval for AUC
plots$optres[[1]][13,]
# Calibration curve:
plots$cc
#the red should follow the grey, it's not that good

#need two predictors - maybe can do with tryptophan (not really working)
#visualizing decision boundaries
contour_data <- expand.grid(
  brightener = seq(min(mydata$logbr), max(mydata$logbr), length = 5),
  tryptophan = seq(min(mydata$logtr), max(mydata$logtr), length = 5))

predictions <- data.frame(contour_data,
                          type = as.numeric(predict(model, contour_data)))




#old code
m1<-lm(br_field~br_lab,data=SampleData)
summary(m1)

m2<-lm(log(entero_mpn)~log(br_lab),data=LabData)
summary(m2)
plot(m2)

m3<-lm(log(entero_mpn)~log(br_field),data=SampleData)
summary(m3)
plot(m3)