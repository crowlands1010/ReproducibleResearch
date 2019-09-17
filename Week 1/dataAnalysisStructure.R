## Install kernlab pack
install.packages("kernlab")

library(kernlab)
data(spam)
str(spam[, 1:5])

##Perform sampling
set.seed(3435)
trainIndicator=rbinom(4601,size=1,prob=0.5)
table(trainIndicator)

##Summaries - Exploratory Analysis
trainSpam = spam[trainIndicator==1, ]
testSpam = spam[trainIndicator==0, ]

names(trainSpam)
head(trainSpam)

table(trainSpam$type)

##Plots - Exploratory Analysis
plot(trainSpam$capitalAve ~ trainSpam$type,xlab="Type",ylab="capitalAve")
plot(log10(trainSpam$capitalAve+1)~trainSpam$type,xlab="Type",ylab="capitalAve")
plot(log10(trainSpam[, 1:4]+1)) ##relationships between predictors

hCluster = hclust(dist(t(trainSpam[, 1:57]))) ##clustering
plot(hCluster)##cluster dendomgram

hClusterUpdated = hclust(dist(t(log10(trainSpam[, 1:55]+1)))) ##clustering
plot(hClusterUpdated) ##cluster dendogram

##Statistcal prediction/modeling
trainSpam$numType = as.numeric(trainSpam$type)-1
costFunction = function(x,y) sum(x != (y>0.5)) 
cvError = rep(NA,55)
library(boot)
for(i in 1:55){
  lmFormula = reformulate(names(trainSpam)[i],response="numType")
  glmFit = glm(lmFormula,family="binomial",data=trainSpam)
  cvError[i] = cv.glm(trainSpam,glmFit,costFunction,2)$delta[2]
}


names(trainSpam)[which.min(cvError)] ##Which predictor has minimum cross-validated error?

##Get a measure of uncretainty
##Use the best model from the group
predictionModel = glm(numType ~ charDollar,family="binomial",data=trainSpam)

##Get predictions on the test set
predictionTest = predict(predictionModel,testSpam)
predictedSpam = rep("nonspam",dim(testSpam)[1])

##Classify as 'spam' for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam"

##Classification table
table(predictedSpam,testSpam$type)
##predictedSpam nonspam spam
##nonspam    1346  458
##spam         61  449

##Error rate
(61+458)/(1346+458+61+449)
##[1] 0.2242869