SVM <- function(train, test){

  library(e1071)
  library(lubridate)
  library(RODBC)
  
train_cols<-train[,c(3:42)]
train_cols_catg<-train[,c(6:42)]

labels<-as.matrix(train[,43])

testdata<-test[,3:42]
testdata_catg<-test[,4:40]
names(testdata_catg) <- paste0("Pcat", 1:37)
testdata_catg <- data.frame(lapply(testdata_catg,as.factor))


testdata_comb <- cbind(test,testdata_catg)

train_cols <- data.frame(lapply(train_cols,as.numeric))

train_cols_catg <- data.frame(lapply(train_cols_catg,as.factor))
names(train_cols_catg) <- paste0("Pcat", 1:37)

train_cols_comb <- cbind(train,train_cols_catg)


testdata<-data.frame(lapply(testdata,as.numeric))

fit<- svm(x=as.matrix(train_cols),y=labels,cost=10,scale=TRUE,type="eps-regression")

predictions<-predict(fit,newdata=testdata)
return(predictions)
}


train <- data[data$dataType == 'TRAIN',]

test <- data[data$dataType == 'TEST',]

#aftermath.

prediction <- SVM(train, test)

actual <- test$revenue


rootmeansquareerror <- rmse(prediction, actual)
