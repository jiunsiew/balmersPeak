

# get data ----------------------------------------------------------------
library(RODBC)
conn <- odbcConnect('EVORA')

data <- sqlFetch(conn, 'restaurant_revenue_prediction.training_data')


# look at relatioship with P ----------------------------------------------
library(tidyr)
plotDf <- gather(data[, 5:ncol(data)], variable, value, -Type, -revenue)

library(ggplot2)
theme_set(theme_bw())

ggplot(plotDf, aes(x = value, y = revenue)) + 
  geom_point(aes(colour = Type)) +
  facet_wrap(~variable)


library(rpart)
dTree <- rpart(revenue ~ .,  data = data[ ,4:43])


library(rpart)
dTree <- rpart(revenue ~ P1 + P2 + P3 + P4 + P5 + P6 + P7 + P8 + P9 + P10 + 
                 P11 + P12 + P13 + P14 + P15 + P16 + P17 + P18 + P19 + P20 +
                 P21 + P22 + P23 + P24 + P25 + P26 + P27 + P28 + P29 + P30 +
                 P31 + P32 + P33 + P34 + P35 + P36 + P37
               ,  data = data)
              


library(rpart.plot)
rpart.plot(dTree)

TestingData <- sqlFetch(conn,'restaurant_revenue_prediction.test_data')



library(predict)
result <- predict(dTree,TestingData)


resultData <- data.frame(TestingData[ ,1:1], result)

names(resultData)[1] <- paste("Id")
names(resultData)[2] <- paste("Prediction")

write.csv(resultData,file="submission_20150327_1.csv",row.names = FALSE)









