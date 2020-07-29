
rm(list=ls())

air.data <- read.table("C:/Work/R_Class/chengdu_air.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)
air.data.bak <- air.data

library(lubridate)

unique(air.data$jczname)

air.data$date <- date(air.data$time_point)

# Plan A; 返回结果的长度可能小于365
date.list <- unique(air.data$date)

# Plan B; more robust than plan A； 返回结果是365
date.list <- seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by="days")

date.list
length(date.list)

# 测试数据中某一天的数据完全没有
air.data <- subset(air.data, date != "2018-01-01")
# date.list <- unique(air.data.1$date)


date.list
length(date.list)


cal_o3mda8 <- function(jczname.query, date.query) {
	# jczname.query <- "sanwayao"
	# date.query <- "2018-01-01"
		
	air.data.sub <- subset(air.data, jczname==jczname.query & date==date.query)
	air.data.sub$hour <- hour(air.data.sub$time_point)
	
	o3.v <- rep(NA, 24)
	
	for(i.hour in 1:length(air.data.sub$o3)) {
		a <- air.data.sub$hour[i.hour] + 1
		b <- air.data.sub$o3[i.hour]
		o3.v[a] <- b
	}
	
	# o3.v
	
	o3.mda8.v <- rep(NA, 17)
	# o3.mda8.v <- rep(NULL, 17)
	# o3.mda8.v
	
	for(i.hour in 1:17) {
		# i.hour <- 1
		
		o3.v.sub <- o3.v[i.hour:(i.hour+7)]
		# o3.v.sub
		# o3.v.sub[1] <- 1
		
		# Plan A: 基于NA的个数进行判断
		# na.len <- length(which(is.na(o3.v.sub)))
		# if(na.len == 8) {
		# 	o3.mda8.v[i.hour] <- NA
		# } else {
		# 	o3.mda8.v[i.hour] <- mean(o3.v.sub, na.rm=TRUE)
		# }
		
		# o3.v.sub[1] <- 1
		# na.v <- is.na(o3.v.sub)
		# na.v
		
		# 是否都是NA
		# all(na.v)
		
		# 是否至少有一个不是NA
		# any(na.v)
		
		# Plan B: 基于是否全是NA
		# TRUE==TRUE # TRUE
		# TRUE
		# FALSE==TRUE # FALSE
		# FALSE
		
		if(all(is.na(o3.v.sub))) {
			o3.mda8.v[i.hour] <- NA
		} else {
			o3.mda8.v[i.hour] <- mean(o3.v.sub, na.rm=TRUE)
		}
		
		# 如果直接运行下面这一步，可能返回NaN
		# o3.mda8.v[i.hour] <- mean(o3.v.sub, na.rm=TRUE) 
	}
	
	# o3.mda8.v
	# o3.mda8.v[1] <- 1
	# o3.mda8.v[1] <- NA
	
	if(all(is.na(o3.mda8.v))) {
		o3.mda8 <- NA
	} else {
		o3.mda8 <- max(o3.mda8.v, na.rm=TRUE)
	}
	
	# o3.mda8
	
	# 如果直接运行下面这一步，会报INF的警告
	# o3.mda8 <- max(o3.mda8.v, na.rm=TRUE)
	
	# 如果不加na.rm=TRUE，会返回过多的NaN
	# o3.mda8 <- max(o3.mda8.v)
	# o3.mda8
	
	return(o3.mda8)
}

jczname <- "sanwayao"
# jczname <- "lingyanshi"
mda8.v <- rep(NA, length(date.list))

for(i.date in 1:length(date.list)) {
	# i.date <- 203
	
	# date.list[i.date]
	# cal_o3mda8(jczname, date.list[i.date])

	mda8.v[i.date] <- cal_o3mda8(jczname, date.list[i.date])
}

mda8.v[1:10]
length(mda8.v)
mean(mda8.v, na.rm=TRUE)
length(which(is.na(mda8.v)))

plot(date.list, mda8.v)
plot(date.list, mda8.v, type="l")
plot(date.list, mda8.v, type="l", col="blue")
plot(date.list, mda8.v, type="l", col="blue", lwd=2)
plot(date.list, mda8.v, type="l", col="blue", lwd=2, lty=2)
plot(date.list, mda8.v, type="o", col="blue", lwd=2)


