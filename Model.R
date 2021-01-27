##-------------Add required libraries-------------
library(readr)
library(ggplot2)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)
library(dlm)

data <- read.csv("J:/Business/Fiverr/Test-Two-Series-R/test_two_series.csv")
nrow(data)
ncol(data)
glimpse(data)
head(data)

##----------------- Time Series Object----------------------
mytimeseries <- ts(data, start = c(1981,5), end = c(2020, 2), frequency = 12)
start(mytimeseries)
end(mytimeseries)
frequency(mytimeseries)
summary(mytimeseries)
cycle(mytimeseries)
plot(aggregate(mytimeseries,FUN = mean))
boxplot(mytimeseries~cycle(mytimeseries))
plot(log(mytimeseries))
plot(diff(log(mytimeseries)))

# Print the timeseries data
print(mytimeseries)
# Give the chart file a name.
png(file = "data.png")
# Plot a graph of the time series.
plot(mytimeseries)
diffts <- diff(mytimeseries)
difftscomponent <- decompose(diffts)
adjusted_diffts <- diffts - difftscomponent$seasonal

acf(log(mytimeseries))
acf(diff(log(mytimeseries)))
pacf(log(mytimeseries))
pacf(diff(log(mytimeseries)))


##------------Abline----------------------------
# Setup up coordinate system (with x == y aspect ratio):
plot(c(-2,3), c(-1,5), type = "n", xlab = "Date", ylab = "series", asp = 1)
## the x- and y-axis, and an integer grid
abline(h = 0, v = 0, col = "gray60")
text(1,0, "abline( h = 0 )", col = "gray60", adj = c(0, -.1))
abline(h = -1:5, v = -2:3, col = "lightgray", lty = 3)
abline(a = 1, b = 2, col = 2)
text(1,3, "abline( 1, 2 )", col = 2, adj = c(-.1, -.1))
## Simple Regression Lines:
require(stats)
sale <- c(mytimeseries)
plot(sale)


#Decomposing the data into its trend, seasonal, and random error components
tsdata_decom <- decompose(mytimeseries, type = "multiplicative")
plot(tsdata_decom)

#Testing the stationarity of the data

#Autocorrelation test
autoplot(acf(mytimeseries,plot=FALSE))+ labs(title="Correlogram of data") 

tsdata_decom$random
autoplot(acf(tsdata_decom$random[7:138],plot=FALSE))+ labs(title="Correlogram of data Random Component")


##----------Seasonal Decomposition------------------- 
fit <- stl(mytimeseries[,1], s.window="period")
plot(fit)

# additional plots
monthplot(mytimeseries)
seasonplot(mytimeseries)


##-------------Simple Exponential Model-----------------
fit <- HoltWinters(mytimeseries, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
#fit <- HoltWinters(mytimeseries, gamma=FALSE)
# triple exponential - models level, trend, and seasonal components
fit <- HoltWinters(mytimeseries)

# predictive accuracy
library(forecast)
accuracy(forecast(fit))

# predict next 12 future values
forecast(fit, 12)
plot(forecast(fit, 12))


##------------ ARIMA Model (a)------------------
fit <- auto.arima(mytimeseries)
          plot(forecast(fit,h=20))
    
          # predictive accuracy
          library(forecast)
          accuracy(forecast(fit))

          # predict next 12 observations
          library(forecast)
          forecast(fit, 12)
          plot(forecast(fit, 12))
          
##------------ARIMA Model (b)-------------------------------

fit <- arima(log(mytimeseries), c(0, 1, 0),seasonal = list(order = c(0, 1, 0), period = 12))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(mytimeseries,2.718^pred$pred, log = "y", lty = c(1,3))
accuracy(forecast(fit))
library(forecast)
forecast(fit, 12)
plot(forecast(fit, 12))

## ----------------VARMA Model------------------

phi=matrix(c(0.2,-0.6,0.3,1.1),2,2); theta=matrix(c(-0.5,0,0,-0.5),2,2)
sigma=diag(2)
m1=VARMAsim(300,arlags=c(1),malags=c(1),phi=phi,theta=theta,sigma=sigma)
zt=m1$series
m2=VARMAsim(zt,p=1,q=1,include.mean=FALSE)

# Fit a sparse VAR model
VARfit<- sparsevar::checkIsVar(mytimeseries)
Lhat <- lagmatrix(fit=VARfit, model="VAR") # get estimated lagmatrix
VARforecast <- directforecast(fit=VARfit, model="VAR", h=1) # get one-step ahead forecasts


VARfit <- sparsevar::checkIsVar(mytimeseries) # sparse VAR
VARforecast <- forecast(fit=VARfit, model="VAR", h=1)


##--------------TBATS Model-------------------
fit <- tbats(mytimeseries)
plot(forecast(fit))
taylor.fit <- tbats(taylor)
plot(forecast(taylor.fit))


##-----------------DLM Model------------------
          
library(dlm)
plot(mytimeseries, type='o', col = c("darkgrey"), xlab = "", ylab = "Level")
mod1 <- dlmModPoly(order = 1, dV = 15100, dW = 755)
NileFilt1 <- dlmFilter(mytimeseries, mod1)
lines(dropFirst(NileFilt1$m), lty = "longdash")
mod2 <- dlmModPoly(order = 1, dV = 15100, dW = 7550)
NileFilt2 <- dlmFilter(mytimeseries, mod2)
lines(dropFirst(NileFilt2$m), lty = "dotdash")
leg <- c("mytimeseries", paste("filtered,  W/V =", format(c(W(mod1) / V(mod1), W(mod2) / V(mod2)))))
legend("bottomright", legend = leg, col=c("darkgrey", "black", "black"),lty = c("solid", "longdash", "dotdash"),pch = c(1, NA, NA), bty = "n")


##---------------------VAR Model-------------------
data <- read.csv("J:/Business/Fiverr/Test-Two-Series-R/test_two_series.csv")
mytimeseries <- ts(data, start = c(1981,5), end = c(2020, 2), frequency = 12)
plot(mytimeseries)
head(data)
h_s <- h_s 
h_s[is.na(h_s)]<- 0
r_s <- r_s 
r_s[is.na(r_s)]<- 0
#tes=cbind(h_s, r_s)
modelvar=VAR(data.frame(h_s,r_s), type ="const", lag.max=2, ic="AIC")
summary(modelvar)
plot(modelvar)
             
             
