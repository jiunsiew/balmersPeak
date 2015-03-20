## Utitliy functions for Kaggle comp


# Log Loss function for evaluation ----------------------------------------
#' see http://www.kaggle.com/c/march-machine-learning-mania-2015/details/evaluation
#' y is vector with n samples where 1 = win, 0 = loss
#' yHat is the predicted probability of team 1 beating team 2

logLossFn <- function(y, yHat){
  
  # some checks
  if (length(y) != length(yHat)){
    error('Input vectors are not the same length')
  }
  
  n <- length(y)
  logLoss <- -(1/n)*sum(y*log(yHat) + (1-y)*log(1-yHat))
  return(logLoss)
}