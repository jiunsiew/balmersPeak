

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
lmFit <- train(revenue ~ Type:P37, data = training, method = "lm",tuneLength = 10, trControl = ctrl, preProc = c("center","scale"))

#plot(lmFit)

lmRevenue <- predict(lmFit, newdata = testing)
plot((lmRevenue - testing$revenue)/max(testing$revenue))

xtab <- table(lmRevenue, testing$revenue)

confusionMatrix(data = plsRevenue, testing$revenue)
