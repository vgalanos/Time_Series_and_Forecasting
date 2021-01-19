############################################################################################################################
## EXERCISE 1 #############################################################################################################


# clear Global Environment
rm(list = ls())
cat("\014")

# import libraries
library(dplyr)
library(zoo)
library(nortest)
library(urca)


# set project path
getwd()
setwd("C:\\Users\\Owner\\Desktop\\Msc Data Science\\Time Series & Forecasting methods\\Assignment-2020-2021\\TimeSeries_Assignment1\\codes")
project_path <- "C:\\Users\\Owner\\Desktop\\Msc Data Science\\Time Series & Forecasting methods\\Assignment-2020-2021\\TimeSeries_Assignment1\\codes"
images <- paste0(project_path,"\\images\\")

############################################################################################################################
## DATA IMPORT #############################################################################################################
############################################################################################################################

# import data from txt file to dataframe and rename column names
#data_path <- "C:\\Users\\Owner\\Desktop\\Msc Data Science\\Time Series & Forecasting methods\\Assignment-2020-2021\\data_assignment.txt"
data_path <- "data_assignment.txt"
col_names <- c('y1'	,'y2'	,'y3','y4'	,'y5'	,'y6'	,'y7'	
               ,'y8'	,'y9'	,'x1','x2'	,'x3'	,'x4'	,'x5'	,'x6'	,'x7'	,'x8'	,'x9'	,'x10'	,'x11'	,'x12'	,'x13'	,'x14'	,'x15')
ts_data <- read.table(data_path, col.names = col_names)
head(ts_data,1)

##$$$$$$$$$$$$$$$$$$$$$$$
## Y8 EMN $$$$$$$$$$$$$$$
##$$$$$$$$$$$$$$$$$$$$$$$

# Convert vector to time series object
y8 <- ts(ts_data$y8, frequency=12, start = c(1990,4),end=c(2005,12))
y8

#Time series plot
pdf(file = paste0(images,'y8_plot.pdf'), width = 15, height = 7)
plot(y8,type="b", col='deepskyblue4', lwd=2, main="Time Series plot of y8 (EMN)"
     , ylab="Monthly returns", xaxt='n')
axis(side = 1, at = as.numeric(floor(as.yearmon(time(y8)))),cex.axis=1)
axis(side = 1, at = as.numeric(as.yearmon(time(y8))),labels=FALSE, lty = 2, lwd = 1)
abline(v = as.numeric(floor(as.yearmon(time(y8)))),lty = 2,col="lightgray")
dev.off()

# Histogram and QQ plot
pdf(file = paste0(images,'y8_histQQ.pdf'), width = 10, height = 10)
par(mfrow=c(2,1))
hist(y8, col='deepskyblue4',main='Histogram of y8', xlab='y8') # histogram
lines(density(y8)) # smooth it - ?density for details
qqnorm(y8,main="Normal QQplot of y8") # normal Q-Q plot
qqline(y8) 
dev.off()

# Normality Test
shapiro.test(y8) 
# For a=0.05 significance we fail to reject the null hypothesis
# We assume that the data are normal

# Mean zero Test
t.test(y8)

# Box test
Box.test(y8,36,type="Box-Pierce")
Box.test(y8,36,type="Ljung-Box")

# automatically fit an AR model using AIC
m=ar(y8)
m
m$order

# perform ADF test
m1=ur.df(y8,type="drift",lags=m$order-1)
m1
summary(m1)

## IDENTIFICATION STEP #####################################################################################################

# Create Autocorrelation and partial autocorrelation plots
pdf(file = paste0(images,'y8_acfpacf.pdf'), width = 10, height = 7)
par(mfrow=c(2,1))        # set up the graphics  
acf(ts(y8,frequency = 1), lag.max =180, main="ACF of y8", xaxt = 'no')      # autocorrelation function plot 
axis(side = 1, at = seq(0, 181, by=5), lty = 1, lwd = 1)
pacf(ts(y8,frequency = 1), lag.max =180, main="PACF of y8", xaxt = 'no')    # partial autocorrelation function 
axis(side = 1, at = seq(0, 181, by=5), lty = 1, lwd = 1)
dev.off()

## ESTIMATION STEP #####################################################################################################

# MA(1)
ma1_fit=arima(y8,order=c(0,0,1)) 
ma1_fit

# MA(6)
ma6_fit=arima(y8,order=c(0,0,6),fixed=c(0,0,0,0,0,NA,NA)) 
ma6_fit

# AR(6)
ar6_fit=arima(y8,order=c(6,0,0),fixed=c(0,0,0,0,0,NA,NA))
ar6_fit

# ARΜΑ(6,6)
arma66_fit=arima(y8,order=c(6,0,6),fixed=c(0,0,0,0,0,NA,0,0,0,0,0,NA,NA))
arma66_fit

# helpfull dynamic vector for fixed
fx <- rep(0,each=27)
fx[6]<- NA
fx[26]<- NA
fx[27]<- NA
fx
# AR(26) only lags 6,26
ar26_fit=arima(y8,order=c(26,0,0),fixed=fx)
ar26_fit

# helpfull dynamic vector for fixed
fx <- rep(0,each=27)
fx[6]<- NA
fx[19]<- NA
fx[26]<- NA
fx[27]<- NA
fx
# AR(26) only lags 6,19,26
ar26_fit2=arima(y8,order=c(26,0,0),fixed=fx)
ar26_fit2
BIC(ar26_fit2)

# helpfull dynamic vector for fixed
fx <- rep(0,each=33)
fx[6]<- NA
fx[26]<- NA
fx[19]<- NA
fx[22]<- NA
fx[32]<- NA
fx[33]<- NA
fx
# AR(32) only lags at 6,19,22,26,32
ar32_fit=arima(y8,order=c(32,0,0),fixed=fx)
ar32_fit

## DIAGNOSTIC PLOTS#########################################################################################
# We are going back and forth this and the previous step in order to find the best choice.

# y8 AR(6) residuals
ar6residuals=ar6_fit$residuals
ar6residuals
residuals=ts(ar6residuals, frequency=12, start = c(1990,4),end=c(2005,12))
residuals

pdf(file = paste0(images,'y8_ar6res_acfpacf.pdf'), width = 15, height = 10)
par(mfrow=c(3,2)) # set up the graphics
acf(ts(residuals,freq=1), 60, main="ACF of AR(6)-y8 residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
pacf(ts(residuals,freq=1), 60, main="PACF of AR(6)-y8 residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
acf(ts(residuals^2,freq=1), 60, main="ACF of AR(6)-y8 squared residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
pacf(ts(residuals^2,freq=1), 60, main="PACF of AR(6)-y8 squared residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
qqnorm(residuals,main="Normal QQplot of AR(6)-y8 residuals")
qqline(residuals)
dev.off()


# y8 AR(26) residuals
ar26residuals=ar26_fit$residuals
ar26residuals
residuals=ts(ar26residuals, frequency=12, start = c(1990,4),end=c(2005,12))
residuals

pdf(file = paste0(images,'y8_ar26res_acfpacf.pdf'), width = 15, height = 10)
par(mfrow=c(3,2)) # set up the graphics
acf(ts(residuals,freq=1), 60, main="ACF of AR(26)-y8 residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
pacf(ts(residuals,freq=1), 60, main="PACF of AR(26)-y8 residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
acf(ts(residuals^2,freq=1), 60, main="ACF of AR(26)-y8 squared residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
pacf(ts(residuals^2,freq=1), 60, main="PACF of AR(26)-y8 squared residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
qqnorm(residuals,main="Normal QQplot of AR(26)-y8 residuals")
qqline(residuals)
dev.off()

# y8 AR(26) with extra y_19 residuals
ar26residuals2=ar26_fit2$residuals
ar26residuals2
residuals=ts(ar26residuals2, frequency=12, start = c(1990,4),end=c(2005,12))
residuals

pdf(file = paste0(images,'y8_ar26res2_acfpacf.pdf'), width = 15, height = 10)
par(mfrow=c(3,2)) # set up the graphics
acf(ts(residuals,freq=1), 60, main="ACF of AR(26)-y8 residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
pacf(ts(residuals,freq=1), 60, main="PACF of AR(26)-y8 residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
acf(ts(residuals^2,freq=1), 60, main="ACF of AR(26)-y8 squared residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
pacf(ts(residuals^2,freq=1), 60, main="PACF of AR(26)-y8 squared residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
qqnorm(residuals,main="Normal QQplot of AR(26)-y8 residuals")
qqline(residuals)
dev.off()

## PREDICTIONS #########################################################################################

# Predictions y8 AR(26)
forecast=predict(ar26_fit2,8)
forecast

# plot of forecasts with 1 s.e
pdf(file = paste0(images,'y8_ar26_pred.pdf'), width = 10, height = 6)
UL=forecast$pred+forecast$se
LL=forecast$pred-forecast$se
minx = min(y8,LL); maxx = max(y8,UL)
ts.plot(y8, forecast$pred, main="y8 forecasts with AR(26)")
lines(forecast$pred, col="red", type="o")
lines(UL, col="blue", lty="dashed")
lines(LL, col="blue", lty="dashed")
dev.off()


##$$$$$$$$$$$$$$$$$$$$$$$
## Y5 EMN $$$$$$$$$$$$$$$
##$$$$$$$$$$$$$$$$$$$$$$$


# Convert vector to time series object
y5 <- ts(ts_data$y5, frequency=12, start = c(1990,4),end=c(2005,12))
y5

#Time series plot
pdf(file = paste0(images,'y5_plot.pdf'), width = 15, height = 7)
plot(y5,type="b", col='deepskyblue4', lwd=2, main="Time Series plot of y5 (ED)"
     , ylab="Monthly returns", xaxt='n')
axis(side = 1, at = as.numeric(floor(as.yearmon(time(y5)))),cex.axis=1)
axis(side = 1, at = as.numeric(as.yearmon(time(y5))),labels=FALSE, lty = 2, lwd = 1)
abline(v = as.numeric(floor(as.yearmon(time(y5)))),lty = 2,col="lightgray")
dev.off()

# Histogram and QQ plot
pdf(file = paste0(images,'y5_histQQ.pdf'), width = 10, height = 10)
par(mfrow=c(2,1))
hist(y5, col='deepskyblue4',main='Histogram of y5', xlab='y5') # histogram
lines(density(y5)) # smooth it - ?density for details
qqnorm(y5,main="Normal QQplot of y5") # normal Q-Q plot
qqline(y5) 
dev.off()

# Normality Test
shapiro.test(y5) 
# For a=0.05 significance we  reject the null hypothesis
# We assume that the data are not normal

# Box test
Box.test(y5,50,type="Box-Pierce")
Box.test(y5,50,type="Ljung-Box")

# automatically fit an AR model using AIC
m=ar(y5)
m
m$order

# perform ADF test
m1=ur.df(y5,type="drift",lags=m$order-1)
m1
summary(m1)

## IDENTIFICATION STEP #####################################################################################################

# Create Autocorrelation and partial autocorrelation plots
pdf(file = paste0(images,'y5_acfpacf.pdf'), width = 10, height = 7)
par(mfrow=c(2,1))        # set up the graphics  
acf(ts(y5,frequency = 1), lag.max =180, main="ACF of y5", xaxt = 'no')      # autocorrelation function plot 
axis(side = 1, at = seq(0, 181, by=5), lty = 1, lwd = 1)
pacf(ts(y5,frequency = 1), lag.max =180, main="PACF of y5", xaxt = 'no')    # partial autocorrelation function 
axis(side = 1, at = seq(0, 181, by=5), lty = 1, lwd = 1)
dev.off()

## ESTIMATION STEP #####################################################################################################

# MA(1)
ma1_fit=arima(y5,order=c(0,0,1)) 
ma1_fit

# AR(1)
ar1_fit=arima(y5,order=c(1,0,0)) 
ar1_fit

# ARMA(1,1)
arma11_fit=arima(y5,order=c(1,0,1))
arma11_fit

# helpfull dynamic vector for fixed
fx <- rep(0,each=50)
fx[1]<- NA
fx[49]<- NA
fx[50]<- NA
fx
# AR(49)
ar49_fit=arima(y5,order=c(49,0,0),fixed=fx)
ar49_fit

fx <- rep(0,each=51)
#fx[1]<- NA
fx[49]<- NA
fx[50]<- NA
fx[51]<- NA
fx
# ARMA(49,1)
arma49_fit=arima(y5,order=c(49,0,1),fixed=fx)
arma49_fit
BIC(arma49_fit)

fx <- rep(0,each=51)
#fx[1]<- NA
fx[39]<- NA
fx[49]<- NA
fx[50]<- NA
fx[51]<- NA
fx
# ARMA(49,1) with ar39
arma49_fit2=arima(y5,order=c(49,0,1),fixed=fx)
arma49_fit2


## DIAGNOSTIC PLOTS#########################################################################################
# We are going back and forth this and the previous step in order to find the best choice.

# y5 AR(1) residuals
ar1residuals=ar1_fit$residuals
ar1residuals
residuals=ts(ar1residuals, frequency=12, start = c(1990,4),end=c(2005,12))
residuals

pdf(file = paste0(images,'y5_ar1res_acfpacf.pdf'), width = 15, height = 10)
par(mfrow=c(3,2)) # set up the graphics
acf(ts(residuals,freq=1), 60, main="ACF of AR(1)-y5 residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
pacf(ts(residuals,freq=1), 60, main="PACF of AR(1)-y5 residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
acf(ts(residuals^2,freq=1), 60, main="ACF of AR(1)-y5 squared residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
pacf(ts(residuals^2,freq=1), 60, main="PACF of AR(1)-y5 squared residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
qqnorm(residuals,main="Normal QQplot of AR(1)-y5 residuals")
qqline(residuals)
dev.off()


# y5 AR(49) residuals
ar49residuals=ar49_fit$residuals
ar49residuals
residuals=ts(ar49residuals, frequency=12, start = c(1990,4),end=c(2005,12))
residuals

pdf(file = paste0(images,'y5_ar49res_acfpacf.pdf'), width = 15, height = 10)
par(mfrow=c(3,2)) # set up the graphics
acf(ts(residuals,freq=1), 60, main="ACF of AR(49)-y5 residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
pacf(ts(residuals,freq=1), 60, main="PACF of AR(49)-y5 residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
acf(ts(residuals^2,freq=1), 60, main="ACF of AR(49)-y5 squared residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
pacf(ts(residuals^2,freq=1), 60, main="PACF of AR(49)-y5 squared residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
qqnorm(residuals,main="Normal QQplot of AR(49)-y5 residuals")
qqline(residuals)
dev.off()

# y5 ARMA(49,1) residuals
arma49residuals=arma49_fit$residuals
arma49residuals
residuals=ts(arma49residuals, frequency=12, start = c(1990,4),end=c(2005,12))
residuals

pdf(file = paste0(images,'y5_arma49res_acfpacf.pdf'), width = 15, height = 10)
par(mfrow=c(3,2)) # set up the graphics
acf(ts(residuals,freq=1), 60, main="ACF of ARMA(49,1)-y5 residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
pacf(ts(residuals,freq=1), 60, main="PACF of ARMA(49,1)-y5 residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
acf(ts(residuals^2,freq=1), 60, main="ACF of ARMA(49,1)-y5 squared residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
pacf(ts(residuals^2,freq=1), 60, main="PACF of ARMA(49,1)-y5 squared residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 60, by=5), lty = 1, lwd = 1)
qqnorm(residuals,main="Normal QQplot of ARMA(49,1)-y5 residuals")
qqline(residuals)
dev.off()

## PREDICTIONS #########################################################################################

# Predictions y5 AR(49)
forecast=predict(arma49_fit,8)
forecast

# plot of forecasts with 1 s.e
pdf(file = paste0(images,'y5_arma49_pred.pdf'), width = 10, height = 6)
UL=forecast$pred+forecast$se
LL=forecast$pred-forecast$se
minx = min(y5,LL); maxx = max(y5,UL)
ts.plot(y5, forecast$pred, main="y5 forecasts with ARMA(49,1)")
lines(forecast$pred, col="red", type="o")
lines(UL, col="blue", lty="dashed")
lines(LL, col="blue", lty="dashed")
dev.off()

