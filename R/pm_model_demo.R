# Develop models to predict the spatiotemporal distributions of PM2.5 in the Sichuan Basin
library(lubridate)
library(corrplot)
library(randomForest)
library(raster)
library(RColorBrewer)
library(GGally)
library(colorRamps)
library(ggplot2)

# Important: change this path if your folder is located somewhere else!!!
# workspace <- "D:/SyncData/sichuan_pm_demo"
workspace <- "~/Downloads/R/sichuan_pm_demo" 
setwd(workspace)

# load data
train.data <- read.table("train_data_demo.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
head(train.data)
View(train.data)
summary(train.data)

pred.data <- read.table("pred_data_demo.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)

train.data$site_id <- paste0(train.data$lat, '_', train.data$lon)
pred.data$mdate <- as.Date(pred.data$mdate)

# data exploration #############################################################
y.name <- "pm2_5"
x.names <- c("aod", "evp", "pre", "prs", "rhu", "ssd", "tem", "win", "pblh", "ndvi", "elv", "pop_den", "lu10", "lu20", "lu30", "lu40", "lu60", "lu80")
var.names <- c(x.names, y.name)
train.data.need <- train.data[, var.names]

tiff("ggpairs_plot.tiff", width=800*5, height=600*5, res=100*5, compression="lzw")
ggpairs(train.data.need[, c("pm2_5", "aod", "tem", "pop_den", "elv")])
dev.off()

cor.rst <- cor(train.data.need, method="spearman")lm
View(cor.rst)
rownames(cor.rst) <- c("Aeros", "EVP", "PRE", "PRS", "RHU", "SSD", "TEM", "WIN", "PBLH", "NDVI", "ELV", "POP", "LU10", "LU20", "LU30", "LU40", "LU60", "LU80", "PM2_5")
rownames(cor.rst)
colnames(cor.rst) <- rownames(cor.rst)

# obtained from the corrplot R code
# tiff("corr_plot.tiff", width=800*5, height=600*5, res=100*5, compression="lzw")
tiff("corr_plot.tiff", width=800*5, height=600*5, res=100*5, compression="lzw")
corrplot(cor.rst, type="lower", tl.col="black", tl.srt=45, method="color", tl.cex=0.8, diag=TRUE, tl.pos="ld")
dev.off()
# dev.new()

# train a general linear regression model ######################################
plot(train.data$aod, train.data$pm2_5)
lines(loess.smooth(train.data$aod, train.data$pm2_5), col="blue", lwd=2, lty=2)
dev.off()

n.train.data <- nrow(train.data)
selector <- rep(1:10, length=n.train.data)
selector <- selector[order(runif(n.train.data, 1, 100))]
selector
table(selector)

train.data.train <- train.data[selector!=1, ]
train.data.test <- train.data[selector==1, ]

model.lm1 <- lm(pm2_5 ~ aod, data=train.data.test)

abline(model.lm1, col="red", lwd=2)
summary(model.lm1)

model.lm1.pred <- predict(model.lm1, train.data.test)
# model.lm1.pred <- predict(model.lm1, train.data)

plot(train.data.test$pm2_5, model.lm1.pred)
cor(train.data.test$pm2_5, model.lm1.pred)
abline(0, 1, lwd=2, col="blue")


plot(pred.data$elv, pred.data$tem)
df <- data.frame(x=pred.data$elv, y=pred.data$tem)
p_daily <- ggplot(df) + geom_hex(aes(x, y), bins=500)
print(p_daily)


model.lm2 <- lm(pm2_5 ~ aod + evp + pre + prs + rhu + ssd + tem + win + pblh + ndvi + elv + pop_den + lu10 + lu20 + lu30 + lu40 + lu60 + lu80, data=train.data.train)

summary(model.lm2)

model.lm2.pred <- predict(model.lm2, train.data.test)

plot(train.data.test$pm2_5, model.lm2.pred)
abline(0, 1, lwd=2, col="blue")
cor(train.data.test$pm2_5, model.lm2.pred)

library(randomForest)
model.rf <- randomForest(pm2_5 ~ aod + evp + pre + prs + rhu + ssd + tem + win + pblh + ndvi 
	+ elv + pop_den + lu10 + lu20 + lu30 + lu40 + lu60 + lu80, data=train.data.train, ntree=500)
model.rf
plot(model.rf)

set.seed(100)

model.rf.pred <- predict(model.rf, train.data.test)
plot(train.data.test$pm2_5, model.rf.pred)
abline(0, 1, lwd=2, col="blue")
cor(train.data.test$pm2_5, model.rf.pred)

# make prediction ##############################################################
model.rf <- randomForest(pm2_5 ~ aod + evp + pre + prs + rhu + ssd + tem + win + pblh + ndvi 
	+ elv + pop_den + lu10 + lu20 + lu30 + lu40 + lu60 + lu80, data=train.data, ntree=500)

pred.rst <- predict(model.rf, pred.data)
pred.df <- data.frame(row3k=pred.data$row3k, col3k=pred.data$col3k, mdate=pred.data$mdate, pm2_5=pred.rst)
pred.df <- pred.df[order(pred.df$row3k, pred.df$col3k),]

# draw maps ####################################################################
library(lubridate)

# value_cells: row3k, col3k, value
gen_raster <- function(X) {
	library(raster)
	r <- raster(paste0(workspace, "/grid_3k.tif"))
	e <- extent(r)
	r.res <- res(r)
	row.min <- ceiling((90-e@ymax)/r.res[2])
	col.min <- ceiling((180+e@xmin)/r.res[1])
	# plot(r)
	n.rows <- nrow(r)
	n.cols <- ncol(r)
	r.values <- rep(NA, n.rows*n.cols)
	# cell_values <- y_res
	i.loc <- (X[,1]-row.min)*n.cols + (X[,2]-col.min)
	r.values[i.loc] <- X[,3]
	
	values(r) <- r.values
	return(r)
}

pred.df.mean <- aggregate(pm2_5 ~ row3k + col3k, data=pred.df, FUN=mean)
# value_raster <- gen_raster(pred.df[pred.df$mdate==as.Date("2015-12-15"), c("row3k", "col3k", "pm2_5")])
value_raster <- gen_raster(pred.df.mean[, c("row3k", "col3k", "pm2_5")])

cols <- colorRampPalette(c("blue", "grey80", "red"))(1000)
cols <- matlab.like(10000)
cols <- rainbow(10000)

spplot(value_raster, col.regions=cols, maxpixels=500000)
