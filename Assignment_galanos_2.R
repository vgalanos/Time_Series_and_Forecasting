############################################################################################################################
## EXERCISE 2 #############################################################################################################


# clear Global Environment
rm(list = ls())
cat("\014")

# import libraries
#library(dplyr)
#library(zoo)
#library(nortest)
#library(urca)
library(corrplot)

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

#==============================================
# Some plots
#==============================================

pdf(file = paste0(images,'y5_pairs1-5.pdf'), width = 12, height = 12)
CM<-cor(ts_data[, c("y5","x1","x2","x3","x4","x5")])
corrplot(CM, method="ellipse",type="lower")
dev.off()

pdf(file = paste0(images,'y5_pairs6-10.pdf'), width = 12, height = 12)
CM<-cor(ts_data[, c("y5","x6","x7","x8","x9","x10")])
corrplot(CM, method="ellipse",type="lower")
dev.off()

pdf(file = paste0(images,'y5_pairs10-15.pdf'), width = 12, height = 12)
CM<-cor(ts_data[, c("y5","x11","x12","x13","x14","x15")])
corrplot(CM, method="ellipse",type="lower")
dev.off()

#================================================
# Estimate multiple regression using command lm()
#================================================
# Regression with all the independent variables
y5res_full <- lm(y5 ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15)
y5res_full
coef(y5res_full)
alpha <- coef(y5res_full)[1]
alpha
alphase <- sqrt(diag(vcov(y5res_full)))[1]
alphase
tstatalpha <- alpha/alphase
tstatalpha 

summary(y5res_full)

# Regression with only 
y5res_1 <- lm(y5 ~ x1+x3+x4+x5+x7+x10+x14)
summary(y5res_full)

anova(y5res_full,y5res_1)
help(anova)

#====================================================
# Find best model using different elimination methods
#====================================================
y5res_fitall <- lm(y5 ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15, data = ts_data)
summary(y5res_fitall)
n <- length(ts_data$y5)

### Backward Elimination method
stepBE<-step(y5res_fitall, scope=list(lower = ~ 1,
                                upper= ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15),
             direction="backward",k = log(n), criterion = "BIC", data=ts_data)
stepBE$anova
#y5 ~ x1 + x2 + x4 + x5 + x6 + x7 + x8 + x11
stepBE
# Forward
y5res_fitnull<-lm(y5 ~ 1, data = ts_data)
stepFS<-step(y5res_fitnull, scope=list(lower = ~ 1,
                                 upper= ~  x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15),
             direction="forward",k = log(n), criterion = "BIC", data=ts_data)
stepFS$anova
#y5 ~  + x1 + x2 +x4 + x5 + x6 + x7

# stepwise selection method
stepSR<-step(y5res_fitall, scope=list(lower = ~ 1,
                                upper= ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15),
             direction="both",k = log(n), criterion = "BIC", data=ts_data)
stepSR$anova
stepSR
# x1 + x2 + x4 + x5 + x6 + x7 + x8 + x11

# best model:
y5res_fitbest <- lm(y5 ~  x1 + x2 + x4 + x5 + x6 + x7 + x8 + x11,data = ts_data)
summary(y5res_fitbest)
AIC(y5res_fitbest)
BIC(y5res_fitbest)

# Diagnostic plots for regression
pdf(file = paste0(images,'y5res_histQQ.pdf'), width = 10, height = 10)
par(mfrow=c(2,1))
hist(y5res_fitbest$residuals, col='deepskyblue4',main='Histogram of y5 regression residuals', xlab='res',breaks=20) # histogram
lines(density(y5res_fitbest$residuals)) # smooth it - ?density for details
qqnorm(y5res_fitbest$residuals,main="Normal QQplot of y5 regression residuals") # normal Q-Q plot
qqline(y5res_fitbest$residuals) 
dev.off()

pdf(file = paste0(images,'y5res_acfpacf.pdf'), width = 10, height = 7)
par(mfrow=c(2,1)) 
acf(residuals(y5res_fitbest), lag.max =50, main="ACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
pacf(residuals(y5res_fitbest), lag.max =50, main="PACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
dev.off()

##$$$$$$$$$$$$$$$$$$$$$$$
## Y1 HFRI $$$$$$$$$$$$$$
##$$$$$$$$$$$$$$$$$$$$$$$

pdf(file = paste0(images,'y1_pairs1-5.pdf'), width = 12, height = 12)
CM<-cor(ts_data[, c("y1","x1","x2","x3","x4","x5")])
corrplot(CM, method="ellipse",type="lower")
dev.off()

pdf(file = paste0(images,'y1_pairs6-10.pdf'), width = 12, height = 12)
CM<-cor(ts_data[, c("y1","x6","x7","x8","x9","x10")])
corrplot(CM, method="ellipse",type="lower")
dev.off()

pdf(file = paste0(images,'y1_pairs10-15.pdf'), width = 12, height = 12)
CM<-cor(ts_data[, c("y1","x11","x12","x13","x14","x15")])
corrplot(CM, method="ellipse",type="lower")
dev.off()



y1res_fitall <- lm(y1 ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15, data = ts_data)
summary(y1res_fitall)
n <- length(ts_data$y1)

### Backward Elimination method
stepBE<-step(y1res_fitall, scope=list(lower = ~ 1,
                                      upper= ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15),
             direction="backward",k = log(n), criterion = "BIC", data=ts_data)
stepBE$anova
#y1 ~ y1 ~ x1 + x2 + x4 + x5 + x6 + x8 + x13

# Forward
y1res_fitnull<-lm(y1 ~ 1, data = ts_data)
stepFS<-step(y1res_fitnull, scope=list(lower = ~ 1,
                                       upper= ~  x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15),
             direction="forward",k = log(n), criterion = "BIC", data=ts_data)
stepFS$anova
#y1 ~  x1 + x2 + x4+ x5 + x7 + x8 + x13 + x6

# stepwise selection method
stepSR<-step(y1res_fitall, scope=list(lower = ~ 1,
                                      upper= ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15),
             direction="both",k = log(n), criterion = "BIC", data=ts_data)
stepSR$anova
stepSR
#  y1 ~ x1 + x2 + x4 + x5 + x6 + x8 + x13

# best model:
y1res_fitbest <- lm(y1 ~  x1 + x2 + x4 + x5 + x6 + x7 + x8 + x11,data = ts_data)
summary(y1res_fitbest)

# Diagnostic plots for regression
pdf(file = paste0(images,'y1res_histQQ.pdf'), width = 10, height = 10)
par(mfrow=c(2,1))
hist(y1res_fitbest$residuals, col='deepskyblue4',main='Histogram of y1 regression residuals', xlab='res',breaks=20) # histogram
lines(density(y1res_fitbest$residuals)) # smooth it - ?density for details
qqnorm(y1res_fitbest$residuals,main="Normal QQplot of y1 regression residuals") # normal Q-Q plot
qqline(y1res_fitbest$residuals) 
dev.off()

pdf(file = paste0(images,'y1res_acfpacf.pdf'), width = 10, height = 7)
par(mfrow=c(2,1)) 
acf(residuals(y1res_fitbest), lag.max =50, main="ACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
pacf(residuals(y1res_fitbest), lag.max =50, main="ACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
dev.off()

##$$$$$$$$$$$$$$$$$$$$$$$
## y8 EMN $$$$$$$$$$$$$$$
##$$$$$$$$$$$$$$$$$$$$$$$

pdf(file = paste0(images,'y8_pairs1-5.pdf'), width = 12, height = 12)
CM<-cor(ts_data[, c("y8","x1","x2","x3","x4","x5")])
corrplot(CM, method="ellipse",type="lower")
dev.off()

pdf(file = paste0(images,'y8_pairs6-10.pdf'), width = 12, height = 12)
CM<-cor(ts_data[, c("y8","x6","x7","x8","x9","x10")])
corrplot(CM, method="ellipse",type="lower")
dev.off()

pdf(file = paste0(images,'y8_pairs10-15.pdf'), width = 12, height = 12)
CM<-cor(ts_data[, c("y8","x11","x12","x13","x14","x15")])
corrplot(CM, method="ellipse",type="lower")
dev.off()



y8res_fitall <- lm(y8 ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15, data = ts_data)
summary(y8res_fitall)

n <- length(ts_data$y8)

### Backward Elimination method
stepBE<-step(y8res_fitall, scope=list(lower = ~ 1,
                                      upper= ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15),
             direction="backward",k = log(n), criterion = "BIC", data=ts_data)
stepBE$anova

# Forward
y8res_fitnull<-lm(y8 ~ 1, data = ts_data)
stepFS<-step(y8res_fitnull, scope=list(lower = ~ 1,
                                       upper= ~  x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15),
             direction="forward",k = log(n), criterion = "BIC", data=ts_data)
stepFS$anova

# stepwise selection method
stepSR<-step(y8res_fitall, scope=list(lower = ~ 1,
                                      upper= ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15),
             direction="both",k = log(n), criterion = "BIC", data=ts_data)
stepSR$anova
stepSR

# best model:
y8res_fitbest <- lm(y8 ~  x1 + x5 + x7 + x8 + x15,data = ts_data)
summary(y8res_fitbest)
AIC(y8res_fitbest)
BIC(y8res_fitbest)

# Diagnostic plots for regression
pdf(file = paste0(images,'y8res_histQQ.pdf'), width = 10, height = 10)
par(mfrow=c(2,1))
hist(y8res_fitbest$residuals, col='deepskyblue4',main='Histogram of y8 regression residuals', xlab='res',breaks=20) # histogram
lines(density(y8res_fitbest$residuals)) # smooth it - ?density for details
qqnorm(y8res_fitbest$residuals,main="Normal QQplot of y8 regression residuals") # normal Q-Q plot
qqline(y8res_fitbest$residuals) 
dev.off()

pdf(file = paste0(images,'y8res_acfpacf.pdf'), width = 10, height = 7)
par(mfrow=c(2,1)) 
acf(residuals(y8res_fitbest), lag.max =50, main="ACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
pacf(residuals(y8res_fitbest), lag.max =50, main="ACF of residuals", xaxt = 'no')
axis(side = 1, at = seq(0, 50, by=5), lty = 1, lwd = 1)
dev.off()
