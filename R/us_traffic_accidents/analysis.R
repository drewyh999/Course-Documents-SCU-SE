library(lubridate)
library(corrplot)
library(ggplot2)
library(maps)
library(data.table)
library(openintro)
library(scales)
library(RColorBrewer)
library(stringi)
library(imputeTS)
library(tidyr)
library(zoo)
library(gvlma)
library(car)
library(randomForest)
library(keras)
library(dplyr)
library(naniar)

workspace <- "~/Downloads/R/us_traffic_accidents"
setwd(workspace)

#Read in the data file 
accident.data.ori <- read.csv("./US_Accidents_Dec19.csv",stringsAsFactors = FALSE) 
accident.data <- accident.data.ori
View(accident.data)

#Convert the values to proper types
accident.data$Start_Time <- parse_date_time(accident.data$Start_Time,"ymd HMS")
accident.data$End_Time <- parse_date_time(accident.data$Start_Time,"ymd HMS")
accident.data$Weather_Timestamp <- parse_date_time(accident.data$Weather_Timestamp,"ymd HMS")
accident.data$Amenity <- as.logical(accident.data$Amenity)
accident.data$Crossing <- as.logical(accident.data$Crossing)
accident.data$Junction <- as.logical(accident.data$Junction)
accident.data$Traffic_Signal <- as.logical(accident.data$Traffic_Signal)
ori.names <- colnames(accident.data)

#Choose part of the columns
demandedrow <- c(ori.names[4],ori.names[5],ori.names[7],ori.names[8],ori.names[11],ori.names[23],
                 ori.names[24],ori.names[25],ori.names[26],ori.names[27],ori.names[28],ori.names[30],ori.names[31],ori.names[32],ori.names[35],ori.names[37],ori.names[44])
accident.data.clean <- subset(accident.data,select = demandedrow)
accident.data.clean <- subset(accident.data.clean, Start_Time > as.Date("2017-9-1"))

#Visualize missing values
tiff("missing.tiff",height = 600*5 ,width = 600*5,res = 500,compression = "lzw")
vis_miss(accident.data.clean,warn_large_data = FALSE)
dev.off()
#Fill NAs
accident.data.clean$Wind_Chill.F. <- na.fill(accident.data.clean$Wind_Chill.F.,fill = "extend")
accident.data.clean$Wind_Speed.mph. <- na.fill(accident.data.clean$Wind_Speed.mph.,fill = "extend")
accident.data.clean$Precipitation.in. <- na.fill(accident.data.clean$Precipitation.in.,fill = "extend")
accident.data.clean$Temperature.F. <- na.fill(accident.data.clean$Temperature.F.,fill = "extend")
accident.data.clean$Visibility.mi. <- na.fill(accident.data.clean$Visibility.mi.,fill = "extend")
accident.data.clean$Humidity... <- na.fill(accident.data.clean$Humidity...,fill = "extend")
accident.data.clean$Pressure.in. <- na.fill(accident.data.clean$Pressure.in.,fill = "extend")


accident.data.clean.back <- accident.data.clean
#accident.data.clean <- accident.data.clean[,-14]
#accident.data.clean <- accident.data.clean[,-6]
#accident.data.clean <- accident.data.clean[,-2]
#Function to map the severity to delay time
accident.data.clean$Delay <- 0
severity2time <- function(data){
  class <- data["Severity"]
  if(class == 1){
    #The class 1 
    out <- floor(runif(1,0,150))
  }
  else if(class == 2){
    out <- floor(runif(1,150,480))
  }
  else if(class == 3){
   out <- floor(runif(1,480,1080))
  }
  else if(class == 4){
    out <- floor(runif(1,1080,1800)) 
  }
  return(out)
}

condition2level <- function(data){
  class <- data["Weather_Condition"]
  if(class == "Clear"){
    #The class 1 
    out <- 1
  }
  else if(class == "Fair"){
    out <- 2
  }
  else if(class == "Mostly Cloudy"){
    out <- 3
  }
  else if(class == "Overcast"){
    out <- 4
  }
  else{
    out <- 5
  }
  return(out)
}

accident.data.clean$Delay <- apply(accident.data.clean,1,FUN = severity2time)
#accident.data.clean$Weather_Condition <- apply(accident.data.clean,1,FUN = condition2level)
accident.data.clean.back <- accident.data.clean
#Discard Weather type column
accident.data.clean <- accident.data.clean[,-14]

#Get the correlation matrix
cor.mtx <- cor(accident.data.clean[,7:17],method = "spearman")
View(cor.mtx)
rownames(cor.mtx) <- c("Temperature(F)","Wind Chill(F)","Humidity(%)","Pressure(in)","Visibility(miles)","Wind Speed(mph)","Precipitation(in)","Crossing","Junction","Traffic Signal","Delay")
colnames(cor.mtx) <- rownames(cor.mtx)
corrplot(cor.mtx, type="lower", tl.col="black", tl.srt=30, method="color", tl.cex=0.8, diag=TRUE, tl.pos="ld")

weathercol <- accident.data.clean$Weather_Condition

#Get the trainning set and validation set
dt.v <- accident.data.clean
train_index <- sample(1:nrow(dt.v), 0.8 * nrow(dt.v))
test_index <- setdiff(1:nrow(dt.v), train_index)

accident.data.train.lm <- dt.v[train_index,]
accident.data.test.lm <- dt.v[test_index,]
#The data is incomplete before 2017-9-1
lm_model_1 <- lm(Delay ~ Temperature.F.+ Wind_Chill.F.:Wind_Speed.mph. +Pressure.in.+ Precipitation.in. + Humidity...+ Visibility.mi.+Crossing+Junction+Traffic_Signal
               ,data = accident.data.train.lm)
lm_model_2 <- lm(Delay ~ Temperature.F.+ Wind_Chill.F.:Wind_Speed.mph. +Pressure.in.+ Precipitation.in. + Humidity...+Crossing+Junction+Traffic_Signal
                 ,data = accident.data.train.lm)

summary(gvlma(lm_model_2))
confint(lm_model_2)
outlierTest(lm_model_2)
accident.data.clean <- accident.data.clean[-1482314,]
vif(lm_model)
#Make prediction
lm.pred.1 <- predict(lm_model_1,accident.data.test.lm)
lm.pred.2 <- predict(lm_model_2,accident.data.test.lm)

#Make the Chi-square test
df.pred <- data.frame(lm.pred.1,lm.pred.2)

#Cut the size to 50000 for the chisqure test size limit
df.pred <- df.pred[1:5000,]
chisq.test(df.pred$lm.pred.1,df.pred$lm.pred.2)

#Prediction model for time we use the weather timestamp as the start point of prediction 
accident.data.clean$TimeGap <- 0
accident.data.clean$TimeGap <- abs(accident.data.clean$Start_Time - accident.data.clean$Weather_Timestamp)
accident.data.clean$TimeGap <- as.numeric(accident.data.clean$TimeGap)
accident.data.clean$TimeGap <- na.fill(accident.data.clean$TimeGap,fill = "extend")

#We study accident with severity 3-4 only
df.svt <- subset(accident.data.clean, Severity > 2)
#Get the training set through random selection
train_index <- sample(1:nrow(df.svt), 0.8 * nrow(df.svt))
test_index <- setdiff(1:nrow(df.svt), train_index)

accident.data.train <- df.svt[train_index,]
accident.data.test <- df.svt[test_index,]

cor(accident.data.clean$Temperature.F.,accident.data.clean$TimeGap)

accident.train <- subset(accident.data.train, TimeGap <= 1700)
accident.train <- accident.train[1:50000,]
accident.test <- accident.data.test[1:5000,]

diff2bin <- function(data){
  gap <- as.numeric(data["TimeGap"])
  if(gap <= 300){
    bin <-  1
  }
  else if((gap > 300) && (gap <= 600)){
    bin <- 2
  }
  else {
    bin <- 3
  }
  return(bin)
}
accident.train$bin <- apply(accident.train,1,FUN = diff2bin)
accident.test$bin <- apply(accident.test,1,FUN = diff2bin)
accident.train.back <- accident.train
accident.test.back <- accident.test
accident.test <- accident.test[,-2]
accident.test <- accident.test[,-5]
table(accident.train$bin)
accident.train$bin <- as.factor(accident.train$bin)
accident.test$bin <- as.factor(accident.test$bin)
time_model <- randomForest(bin ~ Temperature.F.+ Wind_Chill.F.+Wind_Speed.mph. +Pressure.in.+ Precipitation.in. + Humidity...+ Visibility.mi.+Crossing+Junction+Traffic_Signal
                    ,data = accident.train)
summary(time_model)
print(time_model)
plot(time_model)

tm.pred <- predict(time_model,accident.test,type = "prob")
perf <- table(accident.test$bin,tm.pred,dnn = c("Actual","Predicted"))
perf 
multiclass.roc(accident.test$bin,tm.pred)

importance(time_model)

