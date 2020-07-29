library(lubridate)
air_data <- read.table("~/Downloads/chengdu_air.txt",header=TRUE,row.names=NULL,sep = ',')

j_data <- subset(air_data,jczname == "jinquanlianghe")

j_data$year = year(j_data$time_point)
j_data$month = month(j_data$time_point)
j_data$day = day(j_data$time_point)

eight_m_data = subset(j_data,month=="8")
print(paste("The mean of o3 in august is ",mean(eight_m_data$o3,na.rm = TRUE)))
print(paste("The mean of pm2.5 in august is ",mean(eight_m_data$pm2_5,na.rm = TRUE)))
print(paste("The quantile of o3 in august is "))
quantile(eight_m_data$o3,na.rm = TRUE)
print("The quantile of pm2.5 in august is")
quantile(eight_m_data$pm2_5,na.rm = TRUE)
