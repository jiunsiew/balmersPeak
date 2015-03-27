

# get data ----------------------------------------------------------------
library(RODBC)
conn <- odbcConnect('EVORA')

data <- sqlFetch(conn, 'restaurant_revenue_prediction.training_data')


# look at relatioship with P ----------------------------------------------
library(tidyr)
plotDf <- gather(data[, 6:ncol(data)], variable, value, -revenue)

library(ggplot2)
ggplot(plotDf, aes(x = value, y = revenue)) + g