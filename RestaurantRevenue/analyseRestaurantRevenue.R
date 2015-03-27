

# get data ----------------------------------------------------------------
library(RODBC)
conn <- odbcConnect('kaggle')

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

rpart.plot(dTree)
