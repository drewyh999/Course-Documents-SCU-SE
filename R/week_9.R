library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(rpart)
library(rpart.plot)
setwd("~/Downloads/R/sichuan_pm_demo")

#Read in the training data
ori.data <- read.table("train_data_demo.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
ori.data$weather_type <- ori.data$pm2_5 >= 75
#Classify the originate data into good and bad weather
for(i in (1:nrow(ori.data))){
  if(ori.data[i,"pm2_5"] >= 75){
    ori.data[i,"weather_type"] <- "bad"
  }
  else{
    ori.data[i,"weather_type"] <- "good"
  }
}


#Convert the mdate field to date objects
ori.data$mdate <- parse_date_time(ori.data$mdate,"ymd")

#Convert the fields with chr data type into factor type to get random forest do classifications
ori.data$source <- as.factor(ori.data$source)
ori.data$weather_type <- as.factor(ori.data$weather_type)

#Get the train data using subset funtion
train.date.start <- parse_date_time("2015-12-01","ymd")
train.data.end <- parse_date_time("2015-12-20","ymd")

train.data <- subset(ori.data,mdate >= train.date.start & mdate <= train.data.end)

#Get the test data 
test.date.start <- parse_date_time("2015-12-21","ymd")
test.data <- subset(ori.data,mdate >= test.date.start)

#Build up the model
set.seed(1234)
p_model <- randomForest(weather_type ~  evp       +   gst       +    pre        +   prs       +    rhu    +     
                          ssd  + tem  + win       +      pblh      +      ndvi       +     elv        +   
                          pop_den  + aod +     lu10       +     lu20       +     lu30        +  
                          lu40    +  lu50      +    lu60       +   lu80   +     doy ,data = train.data,na_action = na.roughfix,importance = TRUE)
plot(p_model)
print(p_model)

l_model <- glm(weather_type ~  evp       +   gst       +    pre        +   prs       +    rhu    +     
                          ssd  + tem  + win       +      pblh      +      ndvi       +     elv        +   
                          pop_den  + aod +     lu10       +     lu20       +     lu30        +  
                          lu40    +  lu50      +    lu60       +   lu80   +     doy ,data = train.data,family = binomial(),na.action = na.roughfix)    
summary(l_model)

#Make the prediction
pre_1 <- predict(p_model,test.data)
perf_1 <- table(test.data$weather_type,pre_1,dnn = c("Actual","Predicted"))
perf_1
pre_2 <- predict(l_model,test.data,type = "response")
pre_2 <- factor(pre_2 > 0.5, levels = c(FALSE,TRUE),labels = c("bad","good"))
perf_2 <- table(test.data$weather_type,pre_2,dnn = c("Actual","Predicted"))
perf_2
#Performance evaluations 
performance <- function(table, n=2){
  if(!all(dim(table) == c(2,2))){
    stop("Must be a 2 x 2 table")
  }
  tn = table[1,1]
  fp = table[1,2]
  fn = table[2,1]
  tp = table[2,2]
  sensitivity = tp/(tp+fn)
  specificity = tn/(tn+fp)
  ppp = tp/(tp+fp)
  npp = tn/(tn+fn)
  hitrate = (tp+tn)/(tp+tn+fp+fn)
  result <- paste("Sensitivity = ", round(sensitivity, n) ,
                  "\nSpecificity = ", round(specificity, n),
                  "\nPositive Predictive Value = ", round(ppp, n),
                  "\nNegative Predictive Value = ", round(npp, n),
                  "\nAccuracy = ", round(hitrate, n), "\n", sep="")
  cat(result)
}
performance(perf_1)
performance(perf_2)



