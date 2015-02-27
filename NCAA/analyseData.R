## 

# get data
library(RODBC)
conn <- odbcConnect("EVORA")
# query <- "select * from kaggle.dbo.vw_regular_season_team_stats"
query <- "select * from kaggle.dbo.vw_regular_season_detailed_results_doubled"
data <- sqlQuery(conn, query)
data$id <- seq(1,nrow(data))

# munge the data
library(tidyr)
library(plyr)
library(dplyr)

# 