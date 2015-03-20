library(RODBC)
library(dplyr)

season <- 2013
numOfClusters = 9

conn <- odbcConnect('EVORA')
query <- paste("select * from vw_regular_season_team_stats  where season = \'",season,"\' ", sep = "")
# query <- sqlFetch(conn, sqtable = )
data <- sqlQuery(conn, query)

query2 <- paste('Select * FROM vw_regular_season_compact_results_doubled where season = \'',season,'\' ', sep = "")
teamDetails <- data.frame(sqlQuery(conn, query2))

TD <- merge( x = merge( x = teamDetails, y = data, by.x = 'team_A', by.y = 'team_id'), y = data, by.x = 'team_B', by.y = 'team_id')

TD$teamBWin <- TD$team_B_score > TD$team_A_score

TD <- TD[ TD$team_A < TD$team_B, ]

# tidy up data for modelling
modelData <- TD[, c('team_B', 'team_A', 'season', 
                    'team_A_location', 'team_B_location',
                    'avg_score.x', 'score_stddev.x', "field_goal_conversion.x",
                    "three_pointer_conversion.x", "free_throw_conversion.x", 
                    "avg_assists.x", "avg_blocks.x", "avg_steals.x", "avg_turnovers.x",
                    "avg_personal_fouls.x", "RPI.x", "ranking.x",
                    'avg_score.y', 'score_stddev.y', "field_goal_conversion.y",
                    "three_pointer_conversion.y", "free_throw_conversion.y", 
                    "avg_assists.y", "avg_blocks.y", "avg_steals.y", "avg_turnovers.y",
                    "avg_personal_fouls.y", "RPI.y", "ranking.y","teamBWin")]

names(modelData)[6]<-paste('team_A_avg_score')
names(modelData)[7]<-paste('team_A_score_stddev')
names(modelData)[8]<-paste('team_A_field_goal_conversion')
names(modelData)[9]<-paste('team_A_three_pointer_conversion')
names(modelData)[10]<-paste('team_A_free_throw_conversion')
names(modelData)[11]<-paste('team_A_avg_assists')
names(modelData)[12]<-paste('team_A_avg_blocks')
names(modelData)[13]<-paste('team_A_avg_steals')
names(modelData)[14]<-paste('team_A_avg_turnovers')
names(modelData)[15]<-paste('team_A_avg_personal_fouls')
names(modelData)[16]<-paste('team_A_RPI')
names(modelData)[17]<-paste('team_A_ranking')

names(modelData)[18]<-paste('team_B_avg_score')
names(modelData)[19]<-paste('team_B_score_stddev')
names(modelData)[20]<-paste('team_B_field_goal_conversion')
names(modelData)[21]<-paste('team_B_three_pointer_conversion')
names(modelData)[22]<-paste('team_B_free_throw_conversion')
names(modelData)[23]<-paste('team_B_avg_assists')
names(modelData)[24]<-paste('team_B_avg_blocks')
names(modelData)[25]<-paste('team_B_avg_steals')
names(modelData)[26]<-paste('team_B_avg_turnovers')
names(modelData)[27]<-paste('team_B_avg_personal_fouls')
names(modelData)[28]<-paste('team_B_RPI')
names(modelData)[29]<-paste('team_B_ranking')

# Get test data from playoff results

query3 <- paste('Select * FROM vw_tourney_compact_results_doubled where season = \'',season,'\' ', sep = "")
testData <- data.frame(sqlQuery(conn, query3))

testData <- testData[ testData$team_A < testData$team_B, ]
testData$teamBWin <- testData$team_B_score > testData$team_A_score

TD <- merge( x = merge( x = testData, y = data, by.x = 'team_A', by.y = 'team_id'), y = data, by.x = 'team_B', by.y = 'team_id')

testData <- TD[, c('team_B', 'team_A', 'season', 
                    'team_A_location', 'team_B_location',
                    'avg_score.x', 'score_stddev.x', "field_goal_conversion.x",
                    "three_pointer_conversion.x", "free_throw_conversion.x", 
                    "avg_assists.x", "avg_blocks.x", "avg_steals.x", "avg_turnovers.x",
                    "avg_personal_fouls.x", "RPI.x", "ranking.x",
                    'avg_score.y', 'score_stddev.y', "field_goal_conversion.y",
                    "three_pointer_conversion.y", "free_throw_conversion.y", 
                    "avg_assists.y", "avg_blocks.y", "avg_steals.y", "avg_turnovers.y",
                    "avg_personal_fouls.y", "RPI.y", "ranking.y","teamBWin")]

names(testData)[6]<-paste('team_A_avg_score')
names(testData)[7]<-paste('team_A_score_stddev')
names(testData)[8]<-paste('team_A_field_goal_conversion')
names(testData)[9]<-paste('team_A_three_pointer_conversion')
names(testData)[10]<-paste('team_A_free_throw_conversion')
names(testData)[11]<-paste('team_A_avg_assists')
names(testData)[12]<-paste('team_A_avg_blocks')
names(testData)[13]<-paste('team_A_avg_steals')
names(testData)[14]<-paste('team_A_avg_turnovers')
names(testData)[15]<-paste('team_A_avg_personal_fouls')
names(testData)[16]<-paste('team_A_RPI')
names(testData)[17]<-paste('team_A_ranking')

names(testData)[18]<-paste('team_B_avg_score')
names(testData)[19]<-paste('team_B_score_stddev')
names(testData)[20]<-paste('team_B_field_goal_conversion')
names(testData)[21]<-paste('team_B_three_pointer_conversion')
names(testData)[22]<-paste('team_B_free_throw_conversion')
names(testData)[23]<-paste('team_B_avg_assists')
names(testData)[24]<-paste('team_B_avg_blocks')
names(testData)[25]<-paste('team_B_avg_steals')
names(testData)[26]<-paste('team_B_avg_turnovers')
names(testData)[27]<-paste('team_B_avg_personal_fouls')
names(testData)[28]<-paste('team_B_RPI')
names(testData)[29]<-paste('team_B_ranking')

# Modelling.

x <- glm(teamBWin ~ team_A_avg_score + team_A_field_goal_conversion + team_A_RPI + team_B_avg_score + team_B_field_goal_conversion + team_B_RPI , data = modelData, family = binomial)

# evaluate model
source('~/balmersPeak/NCAA/kaggleNcaaUtitliyFunctions.R')
logLossFn(as.numeric(modelData$teamBWin), x$fitted.values)

# apply prediction
y <- predict(x, newdata = testData, type="response")


# test prediction via logloss
modelData$probs <- x$fittedValues

logLossFn(as.numeric(testData$teamBWin), y)





















