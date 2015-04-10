

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


library(rpart)
dTree <- rpart(revenue ~ .,  data = trainData[ 1:floor(trainPercent*nrow(trainData)) ,6:42])


#dTree <- rpart(revenue ~ P1 + P2 + P3 + P4 + P5 + P6 + P7 + P8 + P9 + P10 + 
#                 P11 + P12 + P13 + P14 + P15 + P16 + P17 + P18 + P19 + P20 +
#                 P21 + P22 + P23 + P24 + P25 + P26 + P27 + P28 + P29 + P30 +
#                 P31 + P32 + P33 + P34 + P35 + P36 + P37
#               ,  data = data[1:100,])



library(rpart.plot)
rpart.plot(dTree)

TestingData <- sqlFetch(conn,'restaurant_revenue_prediction.test_data')

TestingData <- trainData[(floor(trainPercent*nrow(trainData))+1):nrow(trainData), ]



library(rpart.utils)
prediction <- predict(dTree,TestingData[,6:42])


resultData <- data.frame(TestingData, prediction)

names(resultData)[1] <- paste("Id")
names(resultData)[2] <- paste("Prediction")

write.csv(resultData,file="submission_20150327_1.csv",row.names = FALSE)

library(caret)
inTrain <- createDataPartition(y = trainData$revenue, p = 0.75, list = FALSE)




