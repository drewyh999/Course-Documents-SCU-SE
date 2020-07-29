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
#Set the workspace 
workspace <- "~/Downloads/R/us_traffic_accidents"
setwd(workspace)

#Read in the data file 
accident.data.ori <- read.csv("./US_Accidents_Dec19.csv") 
accident.data <- accident.data.ori
accident.data$Start_Time <- parse_date_time(accident.data$Start_Time,"ymd HMS")
accident.data$End_Time <- parse_date_time(accident.data$End_Time,"ymd HMS")


#Data visualization

#Visualizing the percentage of the TMC Type
TMC_df <- data.frame(table(accident.data$TMC))
names(TMC_df)[1] <- "TMC_code"
names(TMC_df)[2] <- "Frequency"

#Cut the TMC "Accidents"

TMC_df <- subset(TMC_df,TMC_code != 201)
other.freq.count <- 0
for(i in  (1 : nrow(TMC_df))){
  if(TMC_df[i,"Frequency"] < 11163){
    other.freq.count <- other.freq.count + TMC_df[i,"Frequency"]
  }
}
other.freq.count
TMC_df <- subset(TMC_df, Frequency >= 11163)
TMC_df <- rbind(TMC_df,data.frame(TMC_code = "others",Frequency = other.freq.count))
TMC_df
TMC_colors <- c("#888888", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00")
TMC_pie  <- ggplot(TMC_df, aes(x="", y=Frequency, fill=TMC_code)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_manual(values = TMC_colors,limits = c("241","others","245","244","229","222","203"),labels = c("accident(s). Right lane blocked","others","accident(s). Two lanes blocked","accident(s). Hard shoulder blocked","accident(s). Slow traffic","accident(s). Queuing traffic","multi-vehicle accident (involving Q vehicles)")) + theme_void() + labs(x = NULL,y = NULL, fill = NULL, title = "TMC Code Summary") + geom_text(aes(label = paste0(round((Frequency/sum(TMC_df$Frequency))*100), "%")), position = position_stack(vjust = 0.5),color = "black")
TMC_pie <- TMC_pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))
TMC_pie 


#Visualizing the Accidents on map state-wide
map.df <- data.table(subset(accident.data,select = c("Start_Lng","Start_Lat","Zipcode","County","City","State")))
map.df.back <- map.df
map.df <- subset(map.df, select = c("State"))
state.accident.count <- data.table(table(map.df))
names(state.accident.count)[1] <- "State"
names(state.accident.count)[2] <- "Number_of_Accidents"


#Get the map data and convert the state names 
us_map <- map_data("state")
us_map$region <- state2abbr(us_map$region)
us_map <- data.table(us_map)
names(us_map)[4] <- "order"
names(us_map)[5] <- "State"
names(us_map)[6] <- "County"
#setkey(us_map,State,County)
head(us_map)

#Merge the count and the map_data
map.data.plot <- merge(us_map,state.accident.count,by = "State")
head(map.data.plot)

#Get the plot
map.plot <- ggplot(map.data.plot, aes(long, lat)) +
  geom_polygon(aes(group = group,fill = Number_of_Accidents)) +
  coord_map() 
map.plot <- map.plot + labs(fill = "Number of Accidents",title = "Summery of Number of Accidents On Map",axis = "")
map.plot <- map.plot + theme_classic() +theme(plot.title = element_text(hjust = 0.5, color = "#666666"),axis.text = element_blank(),
                             axis.ticks = element_blank(),axis.line = element_blank(),axis.title = element_blank())
#Convert Scientific numbers to readable numbers
map.plot <- map.plot + scale_fill_distiller(palette = "Reds",labels = comma,direction = 1)
map.plot

#Visualize the CA state
ca.df <- data.table(subset(accident.data,State == "CA",select = c("Start_Lng","Start_Lat","Zipcode","County","City","State")))
head(ca.df)
#Convert the format of the county column so that they could be merged with map data
ca.df$County <- as.character(cs.df$County)

#Get the accident count
ca.accident.count <- data.table(table(ca.df$County))
names(ca.accident.count)[1] <- "County"
names(ca.accident.count)[2] <- "Number_of_Accidents"


#Get the map data and convert the state names 
ca_map <- map_data("county",region = "california")
ca_map$region <- state2abbr(ca_map$region)
ca_map <- data.table(ca_map)
#names(ca_map)[4] <- "order"
names(ca_map)[5] <- "State"
names(ca_map)[6] <- "County"
#setkey(us_map,State,County)
head(ca_map)
unique(ca_map$County)
#Capitalize every first character of each word so that they could be merged
ca_map$County <- stri_trans_totitle(ca_map$County)                
unique(ca_map$County)

#Merge the data for CA state
ca.data.plot <- merge(ca_map,ca.accident.count,by = "County")
head(ca.data.plot)

#Get the plot
ca.plot <- ggplot(ca.data.plot, aes(long, lat)) +
  geom_polygon(aes(group = group,fill = Number_of_Accidents)) +
  coord_map() 
ca.plot <- ca.plot + labs(fill = "Number of Accidents",title = "Summery of California Accidents On Map",axis = "")
ca.plot <- ca.plot + theme_classic() +theme(plot.title = element_text(hjust = 0.5, color = "#666666"),axis.text = element_blank(),
                                              axis.ticks = element_blank(),axis.line = element_blank(),axis.title = element_blank())
#Convert Scientific numbers to readable numbers
ca.plot <- ca.plot + scale_fill_distiller(palette = "Reds",labels = comma,direction = 1)
ca.plot


#Plot the accidents based on zipcode
zip.df <- data.table(subset(accident.data,select = c("Start_Lng","Start_Lat","Zipcode","County")))
names(zip.df)[1] <- "Lng"
names(zip.df)[2] <- "Lat"
table(zip.df$Zipcode)  

#Filled Line plot for top 3 states with highest accident counts
line.top.three.dt <- data.table(subset(accident.data,select = c("State","Start_Time")))
line.top.three.dt$Start_Time <- year(line.top.three.dt$Start_Time)
line.top.three.dt <- data.table(table(line.top.three.dt))
head(line.top.three.dt)
names(line.top.three.dt)[3] <- "Number_of_Accidents"

#Sort the data and get the top three 
state.accident.count <- state.accident.count[with(state.accident.count,order(-Number_of_Accidents)),]
top_three_state <- state.accident.count[1:3,]
line.top.three.dt <- subset(line.top.three.dt,State == top_three_state[1,State] | State == top_three_state[2,State] | State == top_three_state[3,State])
#Exclude years with no records 
line.top.three.dt <- line.top.three.dt[4:15,]
names(line.top.three.dt)[2] <- "year"
area.year.plot <- ggplot(line.top.three.dt,aes(x = year,y = Number_of_Accidents))  +  geom_area(aes(group = State,fill = State)) 
              + scale_fill_brewer(direction = -1,labels = c("California", "Florida"  ,  "Texas")) + scale_y_continuous(labels = comma)
area.year.plot <- area.year.plot + theme(plot.title = element_text(hjust = 0.5, color = "#666666"),axis.ticks = element_blank()) 
                  + labs(title = "Tendency of top 3 accidents states over 2016-2019")
area.year.plot

#Get the data by year
us.accident.dt <- data.table(subset(accident.data,select = "Start_Time"))
us.accident.dt$Start_Time <- year(us.accident.dt$Start_Time)
names(us.accident.dt)[1] <- "year"

#Get the count of the accidents 
us.tendency.count <- data.table(table(us.accident.dt))
us.tendency.count <- us.tendency.count[2:5,]
names(us.tendency.count)[1] <- "year"
names(us.tendency.count)[2] <- "Number_of_Accidents"

us.tendency.plot <- ggplot(us.tendency.count,aes(x = year,y = Number_of_Accidents,fill = year)) + geom_bar(stat="identity",width = 0.7) + geom_text(aes(label=Number_of_Accidents), vjust=1.6, color="white", size=5) + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","#f69ca6"))
us.tendency.plot <- us.tendency.plot + theme_minimal() + labs(title = "Number of Accidents in US over 2016-2019") + theme(plot.title = element_text(hjust = 0.5, color = "#666666"),axis.title = element_text(color = "#666666"))
us.tendency.plot

#Visiualize the number of accidents within different visiblity

#Get the data
visibility.dt <- data.table(subset(accident.data,Severity > 3,select = "Visibility.mi."))
visibility.count <- data.table(table(visibility.dt))
names(visibility.count)[1] <- "Visibility"
names(visibility.count)[2] <- "Number_of_Accidents"
visibility.count
visibility.count <- data.table(subset(visibility.count,Number_of_Accidents > 2000 & Number_of_Accidents < 230000))
visibility.count$Visibility <- as.numeric(visibility.count$Visibility)

visibility.count <- visibility.count[order(Visibility),]
#plot 
vis.count.plot <- ggplot(visibility.count,aes(x = Visibility,y = Number_of_Accidents)) + geom_point() + geom_smooth(color = "#522492",fill = "#e6d1bf") + theme_minimal() + labs(title = "Rough relationship between Visibilty and Number of Accidents") + xlab( "Visibility(miles)") + theme(plot.title = element_text(hjust = 0.5, color = "#666666"),axis.title = element_text(color = "#666666"))
vis.count.plot
visibility.count

#Correlation plot
cor.dt <- subset(accident.data,select = -c(ID,Source,End_Lat,End_Lng,Description,Number))
cor.dt <- subset(cor.dt,select = c(TMC,Severity,Start_Lat,Start_Lng,Distance.mi.,Temperature.F.,Wind_Chill.F.,Humidity...,Pressure.in.,Visibility.mi.,Wind_Speed.mph.,Precipitation.in.))
cor.dt <- na.locf(cor.dt)
head(cor.dt)
cor.rst <- cor(cor.dt, method="spearman")
View(cor.rst)
rownames(cor.rst) <- c("Temperature(F)","Wind Chill(F)","Humidity(%)","Pressure(in)","Visibility(miles)","Wind Speed(mph)","Precipitation(in)","Crossing","Junction","Traffic Signal","Delay")
colnames(cor.rst) <- rownames(cor.rst)
corrplot(cor.rst, type="lower", tl.col="black", tl.srt=30, method="color", tl.cex=0.8, diag=TRUE, tl.pos="ld")

#Percentage of accidents over time of the day
time.dt <- subset(accident.data,select = "Start_Time")
time.dt$Start_Time <- hour(time.dt$Start_Time)
head(time.dt)

time.count <- data.table(table(time.dt))
names(time.count)[1] <- "hour"
names(time.count)[2] <- "count"
time.count$hour <- as.numeric(time.count$hour)
time.count$count <- time.count$count / (sum(time.count$count))
time.count

time.plot.day <- ggplot(time.count,aes(x = hour,y = count * 100,group = 1)) + geom_point(color = "#761dbe",shape = 17,size = 3) 
                + geom_line(size = 1,color = "#ece0da") + scale_y_continuous(labels = comma) + ylab("Percentage")
time.plot.day <- time.plot.day + theme_minimal() + geom_hline(yintercept = 4.65,linetype = "dashed",color = "steelblue") 
                + labs(title = "Accident percentage over time of a day")
                + theme(plot.title = element_text(hjust = 0.5,color = "#666666"),axis.title = element_text(color = "#666666"))
time.plot.day




