rm(list = ls())
setwd("C:/Users/lopam/Desktop/coursera/practical machine learning")

#Installation of required packages
install.packages("caret")
install.packages("knitr")
install.packages("ggplot2")
install.packages("lattice")
install.packages("corrplot")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("tree")
install.packages("randomForest")
library(caret)
library(ggplot2)
library(lattice)
library(knitr)
library(corrplot)
library(rpart)
library(rpart.plot)
library(rattle)
library(tree)
library(randomForest)
##Loading of Dataset:
trainurl = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testurl= "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
trainData <- read.csv(url(trainurl),header = TRUE)
testData <- read.csv(url(testurl),header = TRUE)

##Data Cleaning
#Removing non zero variables (NA) with the mean value
NZV <- nearZeroVar(trainData, saveMetrics = TRUE)
NZV1 <- nearZeroVar(testData, saveMetrics = TRUE)
trainData <- trainData[ ,NZV$nzv==FALSE]
testData <- testData[ ,NZV1$nzv==FALSE]

AllNA <-sapply(trainData,function(x) mean(is.na(x)))
AllNA1 <- sapply(testData, function(x)mean(is.na(x)))
trainData <- trainData[,AllNA == FALSE]
testData <-  testData[ ,AllNA1 == FALSE]

#Removing first 5 features from the training  and testing dataset and Now number of variables are reduced to 54.
trainData <- trainData[,-(1:5)]
testData <- testData[,-(1:5)]
dim(trainData)
dim(testData)

##Partition of Dataset:
inTrain <- createDataPartition(trainData$classe,p = 0.7,list=FALSE)
training <- trainData[inTrain,]
testing <- trainData[-inTrain,]
head(training)
names(training)
dim(training)
#Correlation Analysis
corMatrix <- cor(training[,-54])
corrplot(corMatrix, order = "FPC", method = "color", type = "lower", tl.cex =0.8, tl.col =rgb(0,0,0))
###Prediction Model :
##1. Generalised Boosting Model(GBM)
set.seed(12345)
controlGBM <-trainControl(method = "cv", number = 5)
ModelFitGBM <- train(classe~., data = training, method = "gbm", trControl= controlGBM, verbose = FALSE)
print(ModelFitGBM$finalModel)
#Prediction on Testset
predictionsGBM <- predict(ModelFitGBM, newdata = testing)
confusionMatrix(predictionsGBM, testing$classe)
plot(ModelFitGBM,ylim= c(0.7,1))
##2.Random Forest Model(Using 5-fold cross validation for the algorithm)
set.seed(12345)
controlRF <- trainControl(method = "cv", number = 5)
ModelFitRF <- train(classe~., data = training, method ="rf",trControl = controlRF, verbose = FALSE)
print(ModelFitRF$finalModel)
#Prediction of the model on the Validation dataset 
predictionsRF <- predict(ModelFitRF, newdata = testing)
confusionMatrix(predictionsRF, testing$classe)
plot(ModelFitRF)
##3. Decision Tree Model
set.seed(12345)
ModelFitDT <- train(classe~.,data = training, method = "rpart")
print(ModelFitDT$finalModel)
#Prediction on Testset
predictionDT <- predict(ModelFitDT, newdata = testing)
confusionMatrix(predictionDT, testing$classe)
fancyRpartPlot(ModelFitDT$finalModel)
##APPLYING THE SELECTED MODEL TO THE TEST DATA
#The accuracy for the 3 models are:
#1. GENERALL BOOSTING MODEL(GBM) : 0.9856
#2. RANDOM FOREST MODEL(RF):0.9985
#3. DECISION TREES (DT):0.53
##Best Model : Random forest Model
#The estimated accuracy of the Model
Accuracy <- postResample(predictionsRF,testing$classe)
Accuracy
#The estimated out of sample error of the model
OoSerror <- 1 - as.numeric(confusionMatrix(testing$classe, predictionsRF)$overall[1])
OoSerror

#In this case, Random Forest model will be applied to predict 20 quiz results on testing dataset.
predictionTest <- predict(ModelFitRF, newdata = testData)
predictionTest

##Appendix : Figures
#1. Correlation Matrix Visualisation:
corMatrix <- cor(training[,-54])
corrplot(corMatrix, order = "FPC", method = "color", type = "lower", tl.cex =0.8, tl.col =rgb(0,0,0))

#2. Random Forest Model(tree model) using tree package
rftree <- tree(classe~.,data = training)
rftree$frame
rftree1 <- rpart(classe~., data = training, method = "class")
prp(rftree1)



