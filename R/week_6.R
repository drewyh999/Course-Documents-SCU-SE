library(lubridate)
air_data <- read.table("~/Downloads/chengdu_air.txt",header=TRUE,row.names=NULL,sep = ',',stringsAsFactors = FALSE)

cal_o3mad8_year <- function(station_name){
  station_frame <- air_data[which(air_data$jczname == station_name),]
  station_frame$hours = hour(station_frame$time_point)
  #Go through all the unique day in the time_point column
  all_dates <- unique(date(station_frame$time_point))
  #View(station_frame[which(date(station_frame$time_point) == all_dates[5]),])
  result_list <- c()
  for(i in (1:length(all_dates))){
    max_value <- 0.0
    part <- station_frame[which(date(station_frame$time_point) == all_dates[i]),]
    for(j in 0:16){
      #print(paste('Length of window is',length(part)))
      temp_value = mean(part[which((part$hours) >= j 
                                    & (part$hours) < j + 8 ),'o3'],na.rm=TRUE)
      #print(paste('window',j,'to',j + 8,'mean is',temp_value))
      if(is.na(temp_value)){next}
      if(temp_value > max_value){ max_value = temp_value }
    }
    station_frame[which(date(station_frame$time_point) == all_dates[i]),'max_o3'] <- max_value
    #Test code based on preceding function
    # test_value <- cal_o3mad8(station_name,parse_date_time(all_dates[i],c("ymd")))
    # if(max_value != test_value){
    #   print(paste(all_dates[i],"single:",test_value,"multi:",max_value))
    #   #View(part)
    #   for(j in 0:16){
    #     print(part[which((station_frame$hours) >= j & (station_frame$hours) < j + 8 ),'o3'])
    #     temp_value = mean(part[which((station_frame$hours) >= j & (station_frame$hours) < j + 8 ),'o3'],na.rm=TRUE)
    #     if(is.na(temp_value)){next}
    #     print(paste('window',j,'to',j + 8,'mean is',temp_value))
    #   }
    # }
    #print(paste(all_dates[i],max_value))
    result_list <- c(result_list,max_value)
    #print(result_list)
  }
  #print(paste('The highest o3 average value over the window of 8 hours in',station_name,'are',station_frame$max_o3))
  #Indicate all tests passed 
  #print("Complete")
  #print(result_list)
  #print(mean(result_list,na.rm=TRUE))
  plot(all_dates,result_list)
}
cal_o3mad8_year('sanwayao')
#cal_o3mad8_year('lingyanshi')
#cal_o3mad8_year('junbengjie')