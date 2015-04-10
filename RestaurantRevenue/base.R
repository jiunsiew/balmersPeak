# Utitlity functions and inits

# call this file before everything else


# load common libs
library(RODBC)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())  # grey backgrounds suck





# getConn -----------------------------------------------------------------
#' gets the odbc connection because some of us use evora for ibit_rimor
getConn <- function(){
  library(RODBC)
  
  if (Sys.getenv('USERNAME') == 'jsiew'){
    conn <- odbcConnect('kaggle')
  } else{
    conn <- odbcConnect('EVORA')
  }
  
}
  



rmse <- function(prediction, actual){
  rmse <- sqrt(mean(prediction - actual)^2)
  return(rmse)
}