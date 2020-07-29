library(lubridate)
air_data <- read.table("~/Downloads/chengdu_air.txt",header=TRUE,row.names=NULL,sep = ',')

cal_o3mad8 <- function(station_name,day_value){
  day_frame <- air_data[which(date(air_data$time_point) == date(day_value)
                          & air_data$jczname == station_name),]
  day_frame$time_point <- hour(day_frame$time_point)
  #View(day_frame)
  max_value <- 0
  for(j in 0:16){
    part <- day_frame[which((day_frame$time_point) >= j & (day_frame$time_point) < j + 8),]
    #print(paste('Length of window is',length(part)))
    temp_value = mean(day_frame[which((day_frame$time_point) >= j & (day_frame$time_point) < j + 8),'o3'],na.rm=TRUE)
    #print(paste('window',j,'to',j + 8,'mean is',temp_value))
    if(is.na(temp_value)){next}
    if(temp_value > max_value){ max_value = temp_value }
  }
  #print(paste('The highest o3 average value over the window of 8 hours in',station_name,'on',day_value,'is',max_value))
  return(max_value)
}
cal_o3mad8('sanwayao','2018-1-5')
#cal_o3mad8('lingyanshi','2018-9-8')
#cal_o3mad8('junbengjie','2018-12-14')