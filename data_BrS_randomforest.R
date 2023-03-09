#install.packages('randomForest')
library(randomForest)
#install.packages('caTools')
require(caTools)
library(dplyr)
#install.packages('caret')
library(caret)
#install.packages('e1071', dependencies=TRUE)

data<-read.csv("new_data_3.txt", sep = "\t", header = T)
names(data)
data$BrS[data$BrS == "No"] <- 0
data$BrS[data$BrS == "Yes"] <- 1
summary(data)
sapply(data, class)
data <- transform(data, BrS=as.factor(BrS))
data=na.omit(data)


sapply(data, class)
summary(data)
data=data[,-1]

sample = sample.split(data$BrS, SplitRatio = .75)

train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)

#Create a Random forest model
rf <- randomForest(y=train[,1], x=train[,-1], ntreeTry=500, mtry=3, importance=T, keep.forest=T)

rf
plot(rf, main = "Random Forest")

importance(rf)
varImpPlot(rf,bg = "skyblue", cex=1, main = "Feature Importance")

#Make predictions on test data

pred = predict(rf, newdata=test)
pred

confusionMatrix(table(pred, test$BrS))



##########################    TOP5

data<-read.csv("new_data_3.txt", sep = "\t", header = T)
data=data[,c(2,52,56,57,92,99)]

names(data)
data$BrS[data$BrS == "No"] <- 0
data$BrS[data$BrS == "Yes"] <- 1
summary(data)
sapply(data, class)
data <- transform(data, BrS=as.factor(BrS))
data=na.omit(data)
sapply(data, class)
summary(data)
sample = sample.split(data$BrS, SplitRatio = .85)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)
#Create a Random forest model
rf <- randomForest(y=train[,1], x=train[,-1], ntreeTry=1000, mtry=5, importance=T, keep.forest=T)
rf
plot(rf, main = "Random Forest")
importance(rf)
varImpPlot(rf, bg = "skyblue", cex=par("cex"), main = "")
#Make predictions on test data
pred = predict(rf, newdata=test)
pred
confusionMatrix(table(pred, test$BrS))

############################


data<-read.csv("new_data_3z.txt", sep = "\t", header = T)
names(data)
data$BrS[data$BrS == "No"] <- 0
data$BrS[data$BrS == "Yes"] <- 1
summary(data)
sapply(data, class)
data <- transform(data, BrS=as.factor(BrS))
data=na.omit(data)
dim(data)

sapply(data, class)
summary(data)
data=data[,c(2:180)]

sample = sample.split(data$BrS, SplitRatio = .75)

train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)

#Create a Random forest model
rf <- randomForest(y=train[,1], x=train[,-1], ntreeTry=1000, mtry=5, importance=T, keep.forest=T)

rf
plot(rf, main = "Random Forest")

importance(rf)
varImpPlot(rf,bg = "skyblue", cex=1, main = "Feature Importance")

#Make predictions on test data

pred = predict(rf, newdata=test)
pred

confusionMatrix(table(pred, test$BrS))



####################################TOP7

data<-read.csv("new_data_3z.txt", sep = "\t", header = T)
head(data)
data=data[,c(2,49,53,54,63,85,89,96)]
names(data)
data$BrS[data$BrS == "No"] <- 0
data$BrS[data$BrS == "Yes"] <- 1
summary(data)
sapply(data, class)
data <- transform(data, BrS=as.factor(BrS))
data=na.omit(data)
sapply(data, class)
summary(data)
sample = sample.split(data$BrS, SplitRatio = .85)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)
#Create a Random forest model
rf <- randomForest(y=train[,1], x=train[,-1], ntreeTry=1000, mtry=5, importance=T, keep.forest=T)
rf
plot(rf, main = "Random Forest")
importance(rf)
varImpPlot(rf,bg = "skyblue", cex=1, main = "")
#Make predictions on test data
pred = predict(rf, newdata=test)
pred
confusionMatrix(table(pred, test$BrS))




