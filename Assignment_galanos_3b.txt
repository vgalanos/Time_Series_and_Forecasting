############################################################################################################################
## EXERCISE 3b #############################################################################################################


# clear Global Environment
rm(list = ls())
cat("\014")

library(fGarch) 
library(rugarch)

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
# Summary Statistics and plots
par(mfrow=c(3,2)) 
plot( ts(ts_data$y5, frequency=12, start = c(1990,4),end=c(2005,12)), type="l", main="intel stock monthly returns")
hist(y5, main="histogram of returns")
acf(y5, 50, main="ACF of returns")   
pacf(y5, 50, main="PACF of returns") 
acf(y5^2,50, main="ACF of squared returns")
pacf(y5^2, 50, main="PACF of squared returns")

Box.test(y5,lag=12,type="Ljung") 
Box.test(y5^2,lag=12,type="Ljung") 

# ARMA-REGRESSION

# ARMA(49,1)
fx <- rep(0,each=51)
fx[11]<- NA
fx[15]<- NA
fx[49]<- NA
fx[50]<- NA
fx[51]<- NA
fx

arma49_fit=arima(y5,order=c(49,0,1),fixed=fx)
arma49_fit

# Diagnostic plots for residuals
par(mfrow=c(2,2)) 
acf(residuals(arma49_fit), 50, main="ACF of returns")   
pacf(residuals(arma49_fit), 50, main="PACF of returns") 
acf(residuals(arma49_fit)^2,50, main="ACF of squared returns")
pacf(residuals(arma49_fit)^2, 50, main="PACF of squared returns")

# Box-test for autocorrelations
Box.test(residuals(arma49_fit),lag=50,type="Ljung") 
Box.test(residuals(arma49_fit)^2,lag=50,type="Ljung") 

# we could stop here, all autocorrelations are insignificant
## ARMA-REGRESSION #########################################################
# ar1, ar2, ar40, ar46, 
fx_ar <- rep(0,each=46)
fx_ar[1]<- NA
fx_ar[2]<- NA
fx_ar[32]<- NA
fx_ar[40]<- NA
fx_ar[46]<- NA

fx_reg <- rep(NA,each=8)
fx <- c(fx_ar,fx_reg,NA)

y5res_ar46 = arima(y5, order=c(46,0,0), xreg=cbind(x1,x2,x4,x5,x6,x7,x8,x11), fixed=fx) 
y5res_ar46

Box.test(residuals(y5res_ar46),lag=50,type="Ljung") 
Box.test(residuals(y5res_ar46)^2,lag=50,type="Ljung") 

# Diagnostic plots for residuals
pdf(file = paste0(images,'y5res_ma46_sres.pdf'), width = 10, height = 7)
par(mfrow=c(2,2)) 
acf(residuals(y5res_ar46), 50, main="ACF of residuals")   
pacf(residuals(y5res_ar46), 50, main="PACF of residuals") 
acf(residuals(y5res_ar46)^2,50, main="ACF of squared residuals")
pacf(residuals(y5res_ar46)^2, 50, main="PACF of squared residuals")
dev.off()

# 1st way: 2-step process. First model the series with ARMA+regression
# and then model the residuals with GARCH.

# pacf lags 33,46 at squared residuals
m1archst=garchFit(~garch(1,1),data= residuals(y5res_ar46),cond.dist="std",trace=F) 
summary(m1archst) 
plot(m1archst)

predict(m1archst,6)

#pdf(file = paste0(images,'y5res_ma46_acfpacf.pdf'), width = 10, height = 7)
par(mfrow=c(2,1)) 
acf(residuals(m1archst)^2, lag.max =50, main="ACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
pacf(residuals(m1archst)^2, lag.max =50, main="ACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
#dev.off()

pdf(file = paste0(images,'y5res_ar46_histQQ2.pdf'), width = 10, height = 7)
par(mfrow=c(2,1))
hist(residuals(m1archst), col='deepskyblue4',main='Histogram of y5 AR(46) regression residuals', xlab='res',breaks=20) # histogram
lines(density(residuals(m1archst))) # smooth it - ?density for details
qqnorm(residuals(m1archst),main="Normal QQplot of y5 AR(46) regression residuals") # normal Q-Q plot
qqline(residuals(m1archst)) 
dev.off()

#########################################################################
# 2nd way: All in one.
#BEST MODEL: REGRESSION+ARMA+GARCH

# dynamically create restrictions
N <- 46
name_arr <- c()
name_arrx <- c()
fx_pars <- list()
gx_pars <- list()
for (i in 1:N){
  name_arr[i] <- paste("ar",i,sep='')
  fx_pars[i] <- 0
  name_arrx[i] <- paste("alpha",i,sep='')
  gx_pars[i] <- 0
  
}
names(fx_pars) <- name_arr
names(gx_pars) <- name_arrx
fx_pars
gx_pars

fx_pars <- within(fx_pars, rm(ar1)) 
fx_pars <- within(fx_pars, rm(ar2)) 
fx_pars <- within(fx_pars, rm(ar32)) 
fx_pars <- within(fx_pars, rm(ar40)) 
fx_pars <- within(fx_pars, rm(ar46)) 
gx_pars <- within(gx_pars, rm(alpha33)) 
gx_pars <- within(gx_pars, rm(alpha46)) 
#fx_pars<- c(fx_pars,shape=4.1)
pars <- c(fx_pars, gx_pars)
pars

X<-matrix(cbind(x1,x2,x4,x5,x6,x7,x8,x11), ncol=8)

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(46,0)), 
                   mean.model = list(armaOrder=c(46,0), include.mean = TRUE
                                     , external.regressors = X)
                   ,fixed.pars=pars
                   ,distribution.model = "ged"
                   )
spec
garch <- ugarchfit(spec = spec, data = ts(y5,frequency = 12, start = c(1990,4),end=c(2005,12))
                   , solver.control = list(trace=0))

garch@fit$matcoef
garch
infocriteria(garch)

pdf(file = paste0(images,'y5res_ma46_garch_acfpacf.pdf'), width = 10, height = 7)
par(mfrow=c(2,1)) 
acf(garch@fit$residuals^2, lag.max =50, main="ACF of residuals^2", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
pacf(garch@fit$residuals^2, lag.max =50, main="PACF of residuals^2", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
dev.off()

pdf(file = paste0(images,'y5res_ma46_garch_histQQ.pdf'), width = 10, height = 7)
par(mfrow=c(2,1))
hist(garch@fit$residuals, col='deepskyblue4',main='Histogram of y5 residuals', xlab='res',breaks=20) # histogram
lines(density(garch@fit$residuals)) # smooth it - ?density for details
qqnorm(garch@fit$residuals,main="Normal QQplot of y5 residuals") # normal Q-Q plot
qqline(garch@fit$residuals) 
dev.off()


forc1 <- ugarchforecast(garch, n.ahead = 8,external.forecasts =list(mean(x1),mean(x2)
                                                           ,mean(x4),mean(x5),mean(x6)
                                                           ,mean(x7),mean(x8),mean(x11))
                        )
# PLOTS ###################################

# using in-built functions
plot(forc1)

# manually
pred_fitted <- ts(as.numeric(fitted(forc1)),frequency = 12, start = c(2006,1))
pred_sigma <- ts(as.numeric(sigma(forc1)),frequency = 12, start = c(2006,1))
y5_ts <-ts(y5,frequency = 12,  start = c(1990,4),end=c(2005,12))

pdf(file = paste0(images,'y5reg_ma46_garch_pred.pdf'), width = 10, height = 6)
UL=pred_fitted+pred_sigma
LL=pred_fitted-pred_sigma
minx = min(y5_ts,LL); maxx = max(y5_ts,UL)
ts.plot(y5_ts
        , pred_fitted
        , main="y5 forecasts with regression, MA(46) and GARCH(46,0)")
lines(pred_fitted, col="red", type="o")
lines(UL, col="blue", lty="dashed")
lines(LL, col="blue", lty="dashed")
dev.off()

##$$$$$$$$$$$$$$$$$$$$$$$
## y8 ED $$$$$$$$$$$$$$$$
##$$$$$$$$$$$$$$$$$$$$$$$
# Summary Statistics and plots
par(mfrow=c(3,2)) 
plot( ts(ts_data$y8, frequency=12, start = c(1990,4),end=c(2005,12)), type="l", main="intel stock monthly returns")
hist(y8, main="histogram of returns")
acf(y8, 50, main="ACF of returns")   
pacf(y8, 50, main="PACF of returns") 
acf(y8^2,50, main="ACF of squared returns")
pacf(y8^2, 50, main="PACF of squared returns")

Box.test(y8,lag=12,type="Ljung") 
Box.test(y8^2,lag=12,type="Ljung") 

# ARMA-REGRESSION
fx_ar <- rep(0,each=6)
fx_ar[6]<- NA

fx_reg <- rep(NA,each=5)
fx <- c(fx_ar,fx_reg,NA)

y8res_ar6 = arima(y8, order=c(6,0,0), xreg=cbind(x1,x5,x7,x8,x15),fixed=fx) 
y8res_ar6

# Diagnostic plots for residuals
pdf(file = paste0(images,'y8res_ar6_sres.pdf'), width = 10, height = 7)
par(mfrow=c(2,2)) 
acf(residuals(y8res_ar6), 50, main="ACF of residuals")   
pacf(residuals(y8res_ar6), 50, main="PACF of residuals") 
acf(residuals(y8res_ar6)^2,50, main="ACF of squared residuals")
pacf(residuals(y8res_ar6)^2, 50, main="PACF of squared residuals")
dev.off()

pdf(file = paste0(images,'y8res_ar6_histQQ2.pdf'), width = 10, height = 7)
par(mfrow=c(2,1))
hist(residuals(y8res_ar6), col='deepskyblue4',main='Histogram of y8 AR(6) regression residuals', xlab='res',breaks=20) # histogram
lines(density(residuals(y8res_ar6))) # smooth it - ?density for details
qqnorm(residuals(y8res_ar6),main="Normal QQplot of y8 AR(6) regression residuals") # normal Q-Q plot
qqline(residuals(y8res_ar6)) 
dev.off()

# Box-test for autocorrelations
Box.test(residuals(y8res_ar6),lag=20,type="Ljung") 
Box.test(residuals(y8res_ar6)^2,lag=20,type="Ljung") 

#########################################################################
# 2nd way: All in one.
#BEST MODEL: REGRESSION+ARMA+GARCH

# dynamically create restrictions
N <- 6
name_arr <- c()
fx_pars <- list()
for (i in 1:N){
  name_arr[i] <- paste("ar",i,sep='')
  fx_pars[i] <- 0
}
names(fx_pars) <- name_arr
pars <- within(fx_pars, rm(ar6))


#X<-matrix(cbind(x1,x5,x7,x8,x15), ncol=5)

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(0,3)), 
                   mean.model = list(armaOrder=c(6,0), include.mean = TRUE
                                     #, external.regressors = X)
                   )
                   ,fixed.pars=pars
                   ,distribution.model = "std"
)
#spec
garch <- ugarchfit(spec = spec, data = ts(y8,frequency = 12, start = c(1990,4),end=c(2005,12))
                   , solver.control = list(trace=0))


garch@fit$matcoef
garch
infocriteria(garch)

Box.test(residuals(garch),lag=10,type="Ljung") 
Box.test(residuals(garch)^2,lag=10,type="Ljung") 


pdf(file = paste0(images,'y8res_ar6_garch_acfpacf.pdf'), width = 10, height = 7)
par(mfrow=c(2,1)) 
acf(garch@fit$residuals^2, lag.max =50, main="ACF of residuals^2", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
pacf(garch@fit$residuals^2, lag.max =50, main="PACF of residuals^2", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
dev.off()

pdf(file = paste0(images,'y8res_ar6_garch_histQQ.pdf'), width = 10, height = 7)
par(mfrow=c(2,1))
hist(garch@fit$residuals, col='deepskyblue4',main='Histogram of y8 residuals', xlab='res',breaks=20) # histogram
lines(density(garch@fit$residuals)) # smooth it - ?density for details
qqnorm(garch@fit$residuals,main="Normal QQplot of y8 residuals") # normal Q-Q plot
qqline(garch@fit$residuals) 
dev.off()


forc1 <- ugarchforecast(garch, n.ahead = 8)

# PLOTS ###################################

# using in-built functions
plot(forc1)

# manually
pred_fitted <- ts(as.numeric(fitted(forc1)),frequency = 12, start = c(2006,1))
pred_sigma <- ts(as.numeric(sigma(forc1)),frequency = 12, start = c(2006,1))
y8_ts <-ts(y8,frequency = 12,  start = c(1990,4),end=c(2005,12))

pdf(file = paste0(images,'y8reg_ar6_garch_pred.pdf'), width = 10, height = 6)
UL=pred_fitted+pred_sigma
LL=pred_fitted-pred_sigma
minx = min(y8_ts,LL); maxx = max(y8_ts,UL)
ts.plot(y8_ts
        , pred_fitted
        , main="y8 forecasts with regression, AR(6) and GARCH(0,3)")
lines(pred_fitted, col="red", type="o")
lines(UL, col="blue", lty="dashed")
lines(LL, col="blue", lty="dashed")
dev.off()
