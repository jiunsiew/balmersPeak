

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

ctrl <- trainControl(method = "repeatedcv", repeats = 3)
library(pROC)
plsFit <- train(revenue ~ ., data = training, method = "pls",tuneLength = 15, trControl = ctrl, preProc = c("center","scale"))

plot(plsFit)

plsRevenue <- predict(plsFit, newdata = testing)
plot((plsRevenue - testing$revenue)/max(testing$revenue))

xtab <- table(plsRevenue, testing$revenue)

#confusionMatrix(data = plsRevenue, testing$revenue)
