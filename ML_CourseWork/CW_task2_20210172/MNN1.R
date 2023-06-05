#lord the data set 
install.packages("readxl")
library(readxl)
setwd("D:/2nd Year/ML_CourseWork")
UOW_data <- read_excel("uow_consumption.xlsx")


install.packages("forecast")
install.packages("neuralnet")
install.packages("MLmetrics")
install.packages("Metrics")

library(forecast)
library(neuralnet)
library(MLmetrics)
library(Metrics)

#data pre-processing
names(UOW_data)[2]<-'time_twenty'
names(UOW_data)[3]<-'time_nineteen'
names(UOW_data)[4]<-'time_eighteen'

day<- factor(UOW_data$date)
day<- as.numeric(day)
day

UOW_data_frame <- data.frame(day,UOW_data$'time_twenty',UOW_data$'time_nineteen',UOW_data$'time_eighteen')
UOW_data_frame

#normalization
function_normalization <-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

UOW_data_normaliz <-as.data.frame(lapply(UOW_data_frame,function(x){
  return((x-min(x))/(max(x)-min(x)))
}))
names(UOW_data_normaliz)[2]<-'time_twenty'
names(UOW_data_normaliz)[3]<-'time_nineteen'
names(UOW_data_normaliz)[4]<-'time_eighteen'

#dividing data into training and testing data set
set.seed(123)
UOW_data_training_normaliz <- UOW_data_normaliz[1:380,]
UOW_data_testing_normaliz <- UOW_data_normaliz[381:470,]


#-----AR Approach-------

#generate NN in AR
UOW_data_NNAR <- neuralnet(time_twenty~day+time_twenty,hidden=c(3,2),data=UOW_data_training_normaliz,linear.output=TRUE,threshold=0.01)
boxplot(UOW_data_NNAR)

#model performance evaluation
UOW_data_modelPerformence <-predict(UOW_data_NNAR,UOW_data_testing_normaliz)
UOW_data_modelPerformence

#get trained data set without normalize
UOW_data_train <- UOW_data[1:380,"time_twenty"]
UOW_data_test <- UOW_data[381:470,"time_twenty"]

#find min and max values for train data set
train_min <-min(UOW_data_train)
train_max <-max(UOW_data_train)

#un-normalizing the data
UnNormalized <- function(x,min,max){
  return((max-min)*x+min)
}

UOW_data_predicted_unNormalized <- UnNormalized(UOW_data_modelPerformence,train_min,train_max)
UOW_data_predicted_unNormalized

#for testing performance with RMSE
rmse(exp(UOW_data_predicted_unNormalized),UOW_data_test$time_twenty)

#for testing performance with MSE
mse(exp(UOW_data_predicted_unNormalized),UOW_data_test$time_twenty)

#for testing performance with MAPE
mape(exp(UOW_data_predicted_unNormalized),UOW_data_test$time_twenty)

#for testing performance with sMAPE
smape(exp(UOW_data_predicted_unNormalized),UOW_data_test$time_twenty)

#correlation between predict and actual vales
cor(UOW_data_predicted_unNormalized,UOW_data_test$time_twenty)



#generate 20 hour plot
par(mfrow=c(1,1))

plot (UOW_data_test$time_twenty,UOW_data_predicted_unNormalized,col='blue', main='Un-Normalized Preddiction Graph AR',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend("bottomright",legend="NN",pch=18,col="blue",bty="n")

UOW_data_final_result <-cbind(UOW_data_test,UOW_data_predicted_unNormalized)
UOW_data_final_result

plot(UOW_data_test$time_twenty,ylab = "Preddicted vs Expected AR", type = "l",col="red")
par(new=TRUE)

plot(UOW_data_predicted_unNormalized,ylab = "",yaxt="n",type="l",col="blue",main="Preddicted value vs Expected vale AR")
legend("topleft",c("Expected","Preddicted"),fill=c("red","blue"))


#calculate accuracy
UOW_data_predicted = UOW_data_modelPerformence*abs(diff(range(UOW_data_testing_normaliz$time_twenty)))+min(UOW_data_testing_normaliz$time_twenty)
UOW_data_actual =UOW_data_testing_normaliz$time_twenty*abs(diff(range(UOW_data_testing_normaliz$time_twenty)))+min(UOW_data_testing_normaliz$time_twenty)
preddict_actual_comparison = data.frame(UOW_data_predicted,UOW_data_actual) 

Data_deviation = ((UOW_data_actual-UOW_data_predicted)/UOW_data_actual)
Data_deviation
is.na(Data_deviation) <- sapply(Data_deviation,is.infinite)
Data_deviation

Data_deviation_omit <- na.omit(Data_deviation)
Data_deviation_omit

comparison = data.frame(UOW_data_predicted,UOW_data_actual,Data_deviation)
Accuracy_level = 1- abs(mean(Data_deviation_omit))
Accuracy_level


#-----NARX Approach-------

#generate NN in NARX
library(neuralnet)
UOW_data_narx <- neuralnet(time_twenty~day+time_nineteen+time_eighteen+time_twenty,hidden=c(3,2),data=UOW_data_training_normaliz,linear.output=TRUE,threshold=0.01)
boxplot(UOW_data_narx)

#model performance evaluation
UOW_data_narx_performance <-predict(UOW_data_narx,UOW_data_testing_normaliz)
UOW_data_narx_performance

UOW_data_predicted_narx_unNormalized <- UnNormalized(UOW_data_narx_performance,train_min,train_max)
UOW_data_predicted_narx_unNormalized


#for testing performance with RMSE
rmse(exp(UOW_data_predicted_narx_unNormalized),UOW_data_test$time_twenty)

#for testing performance with MSE
mse(exp(UOW_data_predicted_narx_unNormalized),UOW_data_test$time_twenty)

#for testing performance with MAPE
mape(exp(UOW_data_predicted_narx_unNormalized),UOW_data_test$time_twenty)

#for testing performance with sMAPE
smape(exp(UOW_data_predicted_narx_unNormalized),UOW_data_test$time_twenty)

#correlation between predict and actual vales
cor(UOW_data_predicted_narx_unNormalized,UOW_data_test$time_twenty)



#generate 20 hour plot in NARX

par(mfrow=c(1,1))

plot (UOW_data_test$time_twenty,UOW_data_predicted_narx_unNormalized,col='blue', main='Un-Normalized Preddiction Graph AR',pch=18,cex=0.7)
abline(0,1,lwd=2)
#legend("bottomright",legend="NN",pch=18,col="blue",bty="n")

UOW_data_final_result_narx <-cbind(UOW_data_test,UOW_data_predicted_narx_unNormalized)
UOW_data_final_result_narx

plot(UOW_data_test$time_twenty,ylab = "",yaxt="n",type="l",col="blue",main="Preddicted value vs Expected vale NARX")
legend("topleft",c("Expected","Preddicted"),fill=c("red","blue"))

#plot(UOW_data_predicted_narx_unNormalized,ylab = "",yaxt="n",type="l",col="blue",main="Preddicted value vs Expected vale NARX")
#legend("topleft",c("Expected","Preddicted"),fill=c("red","blue"))


#calculate accuracy
UOW_data_predicted_narx = UOW_data_narx_performance*abs(diff(range(UOW_data_testing_normaliz$time_twenty)))+min(UOW_data_testing_normaliz$time_twenty)
UOW_data_actual_narrx =UOW_data_testing_normaliz$time_twenty*abs(diff(range(UOW_data_testing_normaliz$time_twenty)))+min(UOW_data_testing_normaliz$time_twenty)
preddict_actual_comparison_narx = data.frame(UOW_data_predicted_narx,UOW_data_actual_narrx) 

Data_deviation_narx = ((UOW_data_actual_narrx-UOW_data_predicted_narx)/UOW_data_actual_narrx)
Data_deviation_narx
is.na(Data_deviation_narx) <- sapply(Data_deviation_narx,is.infinite)
Data_deviation_narx

Data_deviation_omit_narx <- na.omit(Data_deviation_narx)
Data_deviation_omit_narx

comparison_narx = data.frame(UOW_data_predicted_narx,UOW_data_actual_narrx,Data_deviation_narx)
Accuracy_narx = 1- abs(mean(Data_deviation_omit_narx))
Accuracy_narx













































