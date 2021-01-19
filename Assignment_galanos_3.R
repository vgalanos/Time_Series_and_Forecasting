############################################################################################################################
## EXERCISE 3 #############################################################################################################


# clear Global Environment
rm(list = ls())
cat("\014")

# import libraries
#library(dplyr)
#library(zoo)
#library(nortest)
#library(urca)
#library(corrplot)

# set project path
getwd()
setwd("C:\\Users\\Owner\\Desktop\\Msc Data Science\\Time Series & Forecasting methods\\Assignment-2020-2021\\TimeSeries_Assignment1\\codes")
project_path <- "C:\\Users\\Owner\\Desktop\\Msc Data Science\\Time Series & Forecasting methods\\Assignment-2020-2021\\TimeSeries_Assignment1\\codes"
images <- paste0(project_path,"\\images\\")

data_path <- "data_assignment.txt"
col_names <- c('y1'	,'y2'	,'y3','y4'	,'y5'	,'y6'	,'y7'	
               ,'y8'	,'y9'	,'x1','x2'	,'x3'	,'x4'	,'x5'	,'x6'	,'x7'	,'x8'	,'x9'	,'x10'	,'x11'	,'x12'	,'x13'	,'x14'	,'x15')
ts_data <- read.table(data_path, col.names = col_names)
head(ts_data,1)

y8 <- ts_data$y8
y5 <- ts_data$y5
x1 <- ts_data$x1
x2 <- ts_data$x2
x3 <- ts_data$x3
x4 <- ts_data$x4
x5 <- ts_data$x5
x6 <- ts_data$x6
x7 <- ts_data$x7
x8 <- ts_data$x8
x9 <- ts_data$x9
x10 <- ts_data$x10
x11 <- ts_data$x11
x12 <- ts_data$x12
x13 <- ts_data$x13
x14 <- ts_data$x14
x15 <- ts_data$x15


##$$$$$$$$$$$$$$$$$$$$$$$
## Y5 ED $$$$$$$$$$$$$$$$
##$$$$$$$$$$$$$$$$$$$$$$$
# best regression model:
y5res_fitbest <- lm(y5 ~  x1 + x2 + x4 + x5 + x6 + x7 + x8 + x11,data = ts_data)
summary(y5res_fitbest)


# Diagnostic plots for regression
#pdf(file = paste0(images,'y5res_histQQ.pdf'), width = 10, height = 10)
par(mfrow=c(2,1))
hist(y5res_fitbest$residuals, col='deepskyblue4',main='Histogram of y5 regression residuals', xlab='res',breaks=20) # histogram
lines(density(y5res_fitbest$residuals)) # smooth it - ?density for details
qqnorm(y5res_fitbest$residuals,main="Normal QQplot of y5 regression residuals") # normal Q-Q plot
qqline(y5res_fitbest$residuals) 
#dev.off()

#pdf(file = paste0(images,'y5res_acfpacf.pdf'), width = 10, height = 7)
par(mfrow=c(2,1)) 
acf(residuals(y5res_fitbest), lag.max =50, main="ACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
pacf(residuals(y5res_fitbest), lag.max =50, main="ACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
#dev.off()

# Caring for autocorrelations with MA(2)
y5res_ma2 = arima(y5, order=c(0,0,2), xreg=cbind(x1,x2,x4,x5,x6,x7,x8,x11)) 
y5res_ma2

#pdf(file = paste0(images,'y5ma2_acfpacf.pdf'), width = 10, height = 7)
par(mfrow=c(2,1)) 
acf(residuals(y5res_ma2), lag.max =50, main="ACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
pacf(residuals(y5res_ma2), lag.max =50, main="ACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
#dev.off()

# Caring for autocorrelations with MA(46)
# 8 + ma + 1
fx_ma <- rep(0,each=46)
fx_ma[1]<- NA
fx_ma[2]<- NA
fx_ma[40]<- NA
fx_ma[46]<- NA

fx_reg <- rep(NA,each=8)
fx <- c(fx_ma,fx_reg,NA)

y5res_ma46 = arima(y5, order=c(0,0,46), xreg=cbind(x1,x2,x4,x5,x6,x7,x8,x11),fixed=fx) 
y5res_ma46
AIC(y5res_ma46)
BIC(y5res_ma46)


pdf(file = paste0(images,'y5res_ma46_acfpacf.pdf'), width = 10, height = 7)
par(mfrow=c(2,1)) 
acf(residuals(y5res_ma46), lag.max =50, main="ACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
pacf(residuals(y5res_ma46), lag.max =50, main="PACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
dev.off()

pdf(file = paste0(images,'y5res_ma46_histQQ.pdf'), width = 10, height = 7)
par(mfrow=c(2,1))
hist(y5res_ma46$residuals, col='deepskyblue4',main='Histogram of y5 regression residuals', xlab='res',breaks=20) # histogram
lines(density(y5res_ma46$residuals)) # smooth it - ?density for details
qqnorm(y5res_ma46$residuals,main="Normal QQplot of y5 regression residuals") # normal Q-Q plot
qqline(y5res_ma46$residuals) 
dev.off()

# Predictions y5 MA(46) with regression
forecast=predict(y5res_ma46,8, newxreg=cbind(rep(mean(x1),8),rep(mean(x2),8),rep(mean(x4),8),rep(mean(x5),8)
                                             ,rep(mean(x6),8),rep(mean(x7),8),rep(mean(x8),8),rep(mean(x11),8)))
forecast


# plot of forecasts with 1 s.e
pdf(file = paste0(images,'y5reg_ma46_pred.pdf'), width = 10, height = 6)
UL=forecast$pred+forecast$se
LL=forecast$pred-forecast$se
minx = min(y5,LL); maxx = max(y5,UL)
ts.plot(ts(y5,frequency = 1), forecast$pred, main="y5 forecasts with regression and MA(46)")
lines(forecast$pred, col="red", type="o")
lines(UL, col="blue", lty="dashed")
lines(LL, col="blue", lty="dashed")
dev.off()

##$$$$$$$$$$$$$$$$$$$$$$$
## Y8 EMN $$$$$$$$$$$$$$$
##$$$$$$$$$$$$$$$$$$$$$$$
# best regression model:

y8res_fitbest <- lm(y8 ~  x1 + x5 + x7 + x8 + x15,data = ts_data)
summary(y8res_fitbest)


# Diagnostic plots for regression
#pdf(file = paste0(images,'y8res_histQQ.pdf'), width = 10, height = 10)
par(mfrow=c(2,1))
hist(y8res_fitbest$residuals, col='deepskyblue4',main='Histogram of y8 regression residuals', xlab='res',breaks=20) # histogram
lines(density(y8res_fitbest$residuals)) # smooth it - ?density for details
qqnorm(y8res_fitbest$residuals,main="Normal QQplot of y8 regression residuals") # normal Q-Q plot
qqline(y8res_fitbest$residuals) 
#dev.off()

#pdf(file = paste0(images,'y8res_acfpacf.pdf'), width = 10, height = 7)
par(mfrow=c(2,1)) 
acf(residuals(y8res_fitbest), lag.max =50, main="ACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
pacf(residuals(y8res_fitbest), lag.max =50, main="ACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
#dev.off()

# Caring for autocorrelations with AR(2)
y8res_ar2 = arima(y8, order=c(2,0,0), xreg=cbind(x1,x5,x7,x8,x15)) 
y8res_ar2

#pdf(file = paste0(images,'y8ma2_acfpacf.pdf'), width = 10, height = 7)
par(mfrow=c(2,1)) 
acf(residuals(y8res_ma6), lag.max =50, main="ACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
pacf(residuals(y8res_ma6), lag.max =50, main="ACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
#dev.off()

# Caring for autocorrelations with AR(6)
# 8 + ma + 1
fx_ar <- rep(0,each=6)
fx_ar[6]<- NA

fx_reg <- rep(NA,each=5)
fx <- c(fx_ar,fx_reg,NA)

y8res_ar6 = arima(y8, order=c(6,0,0), xreg=cbind(x1,x5,x7,x8,x15),fixed=fx) 
y8res_ar6
AIC(y8res_ar6)
BIC(y8res_ar6)


pdf(file = paste0(images,'y8res_ar6_acfpacf.pdf'), width = 10, height = 7)
par(mfrow=c(2,1)) 
acf(residuals(y8res_ar6), lag.max =50, main="ACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
pacf(residuals(y8res_ar6), lag.max =50, main="PACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
dev.off()

pdf(file = paste0(images,'y8res_ar6_histQQ.pdf'), width = 10, height = 7)
par(mfrow=c(2,1))
hist(y8res_ar6$residuals, col='deepskyblue4',main='Histogram of y8 regression residuals', xlab='res',breaks=20) # histogram
lines(density(y8res_ar6$residuals)) # smooth it - ?density for details
qqnorm(y8res_ar6$residuals,main="Normal QQplot of y8 regression residuals with AR(6)") # normal Q-Q plot
qqline(y8res_ar6$residuals) 
dev.off()

# Predictions y8 AR(6) with regression
forecast=predict(y8res_ar6,8, newxreg=cbind(rep(mean(x1),8),rep(mean(x5),8),rep(mean(x7),8),rep(mean(x8),8),rep(mean(x15),8)))
forecast


# plot of forecasts with 1 s.e
pdf(file = paste0(images,'y8reg_ar6_pred.pdf'), width = 10, height = 6)
UL=forecast$pred+forecast$se
LL=forecast$pred-forecast$se
minx = min(y8,LL); maxx = max(y8,UL)
ts.plot(ts(y8,frequency = 1), forecast$pred, main="y8 forecasts with regression and AR(6)")
lines(forecast$pred, col="red", type="o")
lines(UL, col="blue", lty="dashed")
lines(LL, col="blue", lty="dashed")
dev.off()

