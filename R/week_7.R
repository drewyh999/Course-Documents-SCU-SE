library(lubridate)
library(plyr) 
library(tidyverse)
library(lattice)
air_data <- read.table("~/Downloads/chengdu_air.txt",header=TRUE,row.names=NULL,sep = ',',stringsAsFactors = FALSE)
dates <- unique(date(air_data$time_point))
cal_o3mad8_year <- function(station_name){
  station_frame <- air_data[which(air_data$jczname == station_name),]
  station_frame$hours = hour(station_frame$time_point)
  #Go through all the unique day in the time_point column
  
  #Make sure every vector returned is 365 long
  all_dates <- seq(as.Date("2018-1-1"),as.Date("2018-12-31"),by="days")
  #View(station_frame[which(date(station_frame$time_point) == all_dates[5]),])
  result_list <- c()
  for(i in (1:length(all_dates))){
    max_value <- 0.0
    part <- station_frame[which(date(station_frame$time_point) == all_dates[i]),]
    for(j in 0:16){
      temp_value = mean(part[which((part$hours) >= j 
                                   & (part$hours) < j + 8 ),'o3'],na.rm=TRUE)
      if(is.na(temp_value)){next}
      if(temp_value > max_value){ max_value = temp_value }
    }
    station_frame[which(date(station_frame$time_point) == all_dates[i]),'max_o3'] <- max_value
    
    result_list <- c(result_list,max_value)
    
  }
  
  return(result_list)
}
plot_frame <- data.frame(all_dates)
stations <- unique(air_data$jczname)
for(i in (1:length(stations))){
  print(stations[i])
  plot_frame <- data.frame(plot_frame,cal_o3mad8_year(stations[i]))
  colnames(plot_frame)[i + 1] <- stations[i]
}
#Using tidyverse to reduce the data frame into key value pairs
df <- plot_frame %>%
  select(all_dates, sanwayao, shili.lidian,junbengjie,dadanxilu,shahepu,lingyanshi,jinquanlianghe,longquanyiououzhengfu) %>%
  gather(key = "stations", value = "o3", -all_dates)
ggplot(df, aes(x = all_dates, y = o3)) + geom_line(aes(color = stations)) + labs(x="month",y="o3") + scale_x_date(date_breaks = "1 month") + theme(axis.text.x = element_text(angle = 90))
#Using lattice to plot multiple graphs
xyplot(o3 ~ all_dates | stations
       ,data = df
       ,xlab = "month"
       ,main = "O3 8 Hours Sliding Max Values for Each Station"
       ,scales = list(relation = "free", x = list(format = "%b",
                                                  at = seq(as.Date("2018-1-1"), as.Date("2018-12-31"),
                                                           by = "1 month")))
       ,as.table = TRUE)



