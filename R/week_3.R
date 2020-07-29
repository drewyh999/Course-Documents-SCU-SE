
#Q1: Read the table 
#if parameter row.names=NULL is not set then ERROR: duplicate name in row.names)
#if parameter sep=',' is not set then incorrect columns would be read
air_data <- read.table("~/Downloads/chengdu_air.txt",header=TRUE,row.names=NULL,sep = ',')

#Q2: Find the unique obervatory stations in jczname
uni_stations <- unique(air_data[,'jczname'])
paste("number of unique stations is ", length(uni_stations))

#Q3: Find the mean, standard deviation, max and min for o3，pm2_5，pm10，so2，no2，co
#Set the parameter na.rm = TRUE so that NA value would not be considered

for( col in names(air_data)[3:8]){
  mean_value <- mean(air_data[,col],na.rm = TRUE)
  print(paste("mean of ",col," column is ", mean_value))
  max_value <- max(air_data[,col],na.rm = TRUE)
  print(paste("max of ",col," column is ", max_value))
  min_value <- min(air_data[,col],na.rm = TRUE)
  print(paste("min of ",col," column is ", min_value))
  sd_value <- sd(air_data[,col],na.rm = TRUE)
  print(paste("standard deviation of ",col," column is ", sd_value))
}



