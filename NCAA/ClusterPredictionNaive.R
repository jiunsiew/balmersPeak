library(RODBC)
library(dplyr)

season <- 2013
numOfClusters = 9

conn <- odbcConnect('EVORA')
query <- paste("select * from vw_regular_season_team_stats  where season = \'",season,"\' ", sep = "")
# query <- sqlFetch(conn, sqtable = )
data <- sqlQuery(conn, query)

clust <- kmeans(data.frame (data$avg_score, data$opponent_avg_score, data$field_goal_conversion,data$three_pointer_conversion, data$RPI), numOfClusters)$cluster

plot( data.frame (data$avg_score, data$opponent_avg_score), pch = clust)
help(kmeans)

result <- data.frame(data, clust)

query2 <- paste('Select * FROM vw_regular_season_compact_results_doubled where season = \'',season,'\' ', sep = "")
teamDetails <- data.frame(sqlQuery(conn, query2))

teamExtendedDetails <- merge( x = merge( x = teamDetails, y = result, by.x = "team_A", by.y = "team_id", all.x = TRUE), y = result, by.x = "team_B", by.y = "team_id", all.x = TRUE)


clusterWinsLosses <- teamExtendedDetails %>% group_by(clust.x, clust.y) %>% summarise(nWinsA = sum(team_A_score > team_B_score), nLossesA = sum(team_A_score < team_B_score))

clusterProbWins <- teamExtendedDetails %>% group_by(clust.x, clust.y) %>% summarise(probAWins = sum(team_A_score > team_B_score) / (sum(team_A_score > team_B_score)+sum(team_A_score < team_B_score)  ), probBWins = sum(team_A_score < team_B_score) / (sum(team_A_score < team_B_score) + sum(team_A_score > team_B_score)))

query3 <- paste('SELECT DISTINCT CAST(r1.team_id AS INT) AS team_1, r2.team_id AS team_2
            FROM
              (
            		SELECT DISTINCT CAST(winner_team_id AS INT) AS team_id, season
            		FROM dbo.vw_regular_season_compact_results
            		UNION ALL
            		SELECT DISTINCT CAST(loser_team_id AS INT), season
            		FROM dbo.vw_regular_season_compact_results
            	) r1
            JOIN 
            	(
            		SELECT DISTINCT CAST(winner_team_id AS INT) AS team_id, season
            		FROM dbo.vw_regular_season_compact_results
            		UNION ALL
            		SELECT DISTINCT CAST(loser_team_id AS INT), season
            		FROM dbo.vw_regular_season_compact_results
            	) r2
            ON r1.team_id < r2.team_id AND r1.season = r2.season
            WHERE r1.season = \'',season,'\'
            ORDER BY r1.team_id, r2.team_id',sep = "")

teams = data.frame(sqlQuery(conn, query3))


z1 <- merge( x = merge( x = teams, y = result, by.x = "team_1", by.y = "team_id"), y = result, by.x = "team_2", by.y = "team_id")

z2 <- data.frame ( z1$team_1, z1$team_2, z1$clust.x, z1$clust.y)

z <- merge( x = z2, y = clusterProbWins, by.x = c("z1.clust.x", "z1.clust.y"), by.y = c("clust.x", "clust.y"))

z3 <- data.frame( z$z1.team_1, z$z1.team_2, z$probAWins)

zfinal <- z3[order(z3$z.z1.team_1, z3$z.z1.team_2),]

names(zfinal)[1]<-paste("team_1")
names(zfinal)[2]<-paste("team_2")
names(zfinal)[3]<-paste("pred")

#zfinal <- data.frame( paste(season,zfinal$team_1, zfinal$team_2, sep="_"), zfinal$pred )

#names(zfinal)[1]<-paste("id")
#names(zfinal)[2]<-paste("pred")

#write.csv(zfinal, file="prediction.csv", row.names = FALSE)

query4 <- paste('Select * FROM vw_regular_season_compact_results_doubled where season = \'',season+1,'\' ', sep = "")
teamDetails2 <- data.frame(sqlQuery(conn, query4))

Y <- data.frame( teamDetails2$team_A, teamDetails2$team_B, teamDetails2$team_A_score > teamDetails2$team_B_score, teamDetails2$team_A < teamDetails2$team_B)

names(Y)[1]<-paste("team_1")
names(Y)[2]<-paste("team_2")
names(Y)[3]<-paste("team_1_win")
names(Y)[4]<-paste("team_1<team_2")

Y1 <- merge( x = Y, y = zfinal, by.x = c("team_1","team_2"), by.y = c("team_1","team_2"))

Y1$pred[Y1$pred == 0] <- 0.5
Y1$pred[Y1$pred == 1] <- 0.5

source('~/balmersPeak/NCAA/kaggleNcaaUtitliyFunctions.R')
logLossFn(Y1$team_1_win,Y1$pred)

zfinal <- data.frame( paste(season,zfinal$team_1, zfinal$team_2, sep="_"), zfinal$pred )

names(zfinal)[1]<-paste("id")
names(zfinal)[2]<-paste("pred")

write.csv(zfinal, file="prediction.csv", row.names = FALSE)