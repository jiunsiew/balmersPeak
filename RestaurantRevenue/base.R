# Utitlity functions and inits

# call this file before everything else


# load common libs
library(RODBC)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())  # grey backgrounds suck
library(caret)


set.seed(7357)



# getConn -----------------------------------------------------------------
#' gets the odbc connection because some of us use evora for ibit_rimor
#' setup a ODBC connection on your PC called Evora unless you're jsiew
getConn <- function(){
  library(RODBC)
  
  if (Sys.getenv('USERNAME') == 'jsiew'){
    conn <- odbcConnect('kaggle')
  } else{
    conn <- odbcConnect('EVORA')
  }
}

conn <- getConn()
# combine data ------------------------------------------------------------
#' combine train and test data into one data frame in case of categorical vars
#' combines into one data frame and tags the data
combineData <- function(conn){
  trainData <- sqlFetch(conn, 'restaurant_revenue_prediction.training_data')
  trainData$dataType <- 'TRAIN'
  submitTestData <- sqlFetch(conn,'restaurant_revenue_prediction.test_data')
  submitTestData$revenue <- NA
  submitTestData$dataType <- 'SUBMIT'
  allData <- rbind(trainData, submitTestData)
  
  # now add the categorical variables
  pVarNames <- paste0('P', 1:37)
  tmpPVars <- as.data.frame(lapply(allData[, pVarNames], as.factor))
  names(tmpPVars) <- paste0('P', 1:37, '.cat')
  
  allData <- cbind(allData, tmpPVars)
  
  return(allData)
}

data <- combineData(conn)

#bootstrap ----------------------------------------------------------------
# takes 'size' random numbers between 0 and 'max' and changes the entries of 'data's dataType column to 'TEST'. Useful for cross validation.
# Or just useful for trying out different sets if training and test data.
bootStrap <- function(data, size, max){
  selection <- floor(runif(n = size, min = 0, max = max))
  data$dataType[selection] <- 'TEST'
  return(data)
}

dta <- bootStrap(data, 20, 137)

# RMSE --------------------------------------------------------------------
#' deprecate -- use caret method instead
rmse <- function(prediction, actual){
  rmse <- sqrt(mean(prediction - actual)^2)
  return(rmse)
}


