# developing structure to model the data

# 
source('./base.R')


conn <- getConn()
trainData <- sqlFetch(conn, 'restaurant_revenue_prediction.training_data')



# visuals -----------------------------------------------------------------
plotDf <- gather(trainData[, 5:ncol(trainData)], variable, value, -Type, -revenue)


ggplot(plotDf, aes(x = value, y = revenue)) + 
  geom_point(aes(colour = Type)) +
  facet_wrap(~variable)



# model -------------------------------------------------------------------
# split test/train set
library(caret)
set.seed(2048)
trainPercent = 0.8
inTrain <- createDataPartition(y = trainData$revenue, p = trainPercent, list = FALSE)

training <- trainData[inTrain, ] #, !(names(trainData) %in% "City")]
testing <- trainData[-inTrain, ] #, !(names(trainData) %in% "City")]


pVarNames <- paste0('P', 1:37)
varNames <- c('revenue', 'City Group', 'Type', pVarNames)


# look at a simple decision tree
library(rpart)
dTree <- rpart(log(revenue) ~ .,  data = training[, varNames])
library(rpart.plot)
rpart.plot(dTree)

#' Variables are P8, P28, P26, P14, P6, P19 and P29 --> without log(revenue) transformation
#' Variables are P28, P6, P25, P1, P2, P11 and P20 --> with log(revenue) transformation



# use decision tree but convert the P-variables to categoricals
# duplicate the P columns as factors
tmpPVars <- as.data.frame(lapply(trainData[, pVarNames], as.factor))
names(tmpPVars) <- paste0('Pcat', 1:37)

trainDataComb <- cbind(trainData, tmpPVars)

trainComb <- trainDataComb[inTrain, ]
testComb <- trainDataComb[-inTrain, ]

varNamesComb <- c(varNames, paste0('Pcat', 1:37))
dTreeLog <- rpart(log(revenue) ~ ., data = trainComb[, varNamesComb])
dtreeLogPred <- predict(dTreeLog, newdata = testComb)
rmse(exp(dtreeLogPred), testComb$revenue)


dTree <- rpart(revenue ~ ., data = trainComb[, varNamesComb])
dtreePred <- predict(dTree, newdata = testComb)
rmse(dtreePred, testComb$revenue)


library(randomForest)
rfFit <- randomForest(revenue ~ ., data = trainComb[, varNamesComb])


# use caret
# library(caret)
# ctrl <- trainControl(method = "cv")
# rfFit <- train(revenue ~ ., data = train[, varNamesComb], 
#                method = "rf", 
#                tuneLength = 10, trControl = ctrl, 
#                preProc = c("center","scale"))





# submission --------------------------------------------------------------
TestingData <- sqlFetch(conn,'restaurant_revenue_prediction.test_data')

# duplicate categoricals
# 
# rfTestRevenue <- predict(rfFit, newdata = TestingData)
# 
# resultData <- data.frame(TestingData[,1:1], rfTestRevenue)
# 
# names(resultData)[1] <- paste("Id")
# names(resultData)[2] <- paste("Prediction")
# 
# date <- Sys.Date()
# 
# fileName <- paste("submission_",date,"_1",".csv", sep = "")
# 
# 
# 
# write.csv(resultData,file=fileName,row.names = FALSE)
# 
