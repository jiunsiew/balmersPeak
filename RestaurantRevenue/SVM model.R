library(e1071)
library(lubridate)
library(RODBC)



conn <- odbcConnect('EVORA')

train_data <- sqlFetch(conn, 'restaurant_revenue_prediction.training_data')

test_data <- sqlFetch(conn,'restaurant_revenue_prediction.test_data')


data <- combineData(conn)

train_cols<-train_data[,c(3:42)]
train_cols_catg<-train_data[,c(6:42)]

labels<-as.matrix(train_data[,43])

testdata<-test_data[,3:42]
testdata_catg<-testdata[,4:40]
names(testdata_catg) <- paste0("Pcat", 1:37)
testdata_catg <- data.frame(lapply(testdata_catg,as.factor))


testdata_comb <- cbind(test_data,testdata_catg)

train_cols <- data.frame(lapply(train_cols,as.numeric))

train_cols_catg <- data.frame(lapply(train_cols_catg,as.factor))
names(train_cols_catg) <- paste0("Pcat", 1:37)

train_cols_comb <- cbind(train_data,train_cols_catg)


testdata<-data.frame(lapply(testdata,as.numeric))

fit<- svm(x=as.matrix(train_cols),y=labels,cost=10,scale=TRUE,type="eps-regression")

predictions<-as.data.frame(predict(fit,newdata=testdata))

submit<-as.data.frame(cbind(test_data[,1],predictions))
colnames(submit)<-c("Id","Prediction")
)
write.csv(submit,"submission_20150410.csv",row.names=FALSE,quote=FALSE)
