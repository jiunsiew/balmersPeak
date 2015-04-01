

# get data ----------------------------------------------------------------
library(RODBC)
conn <- odbcConnect('EVORA')

trainData <- sqlFetch(conn, 'restaurant_revenue_prediction.training_data')

trainPercent = 0.8

# look at relatioship with P ----------------------------------------------
library(tidyr)
plotDf <- gather(trainData[, 5:ncol(trainData)], variable, value, -Type, -revenue)

library(ggplot2)
theme_set(theme_bw())

ggplot(plotDf, aes(x = value, y = revenue)) + 
  geom_point(aes(colour = Type)) +
  facet_wrap(~variable)

library(caret)
#set.seed(107)
inTrain <- createDataPartition(y = trainData$revenue, p = trainPercent, list = FALSE)



training <- trainData[inTrain, !(names(trainData) %in% "City")]
testing <- trainData[-inTrain, !(names(trainData) %in% "City")]

rm(list = c("trainData","plotDf","ctrl"))

#library(mlbench)

#dt <- data(BostonHousing)

ctrl <- trainControl(method = "cv")
library(pROC)
rfFit <- train(revenue ~ P1:P37, data = training, method = "rf",tuneLength = 10, trControl = ctrl, preProc = c("center","scale"))

#plot(rfFit)

rfRevenue <- predict(rfFit, newdata = testing)
plot((rfRevenue - testing$revenue)/max(testing$revenue))



# use the model on given larger test set.

TestingData <- sqlFetch(conn,'restaurant_revenue_prediction.test_data')
TestingData <- TestingData[,!(names(TestingData) %in% "City")]

rfTestRevenue <- predict(rfFit, newdata = TestingData)

resultData <- data.frame(TestingData[,1:1], rfTestRevenue)

names(resultData)[1] <- paste("Id")
names(resultData)[2] <- paste("Prediction")

date <- Sys.Date()

fileName <- paste("submission_",date,"_1",".csv", sep = "")



write.csv(resultData,file=fileName,row.names = FALSE)

#xtab <- table(rfRevenue, testing$revenue)

#confusionMatrix(data = plsRevenue, testing$revenue)




