## Anual GDP Forecast using ARIMA Model 

setwd("E:/GDP")
data=read.csv("GDP rate Yearly.csv")
head(data,20)
#attach(data)
gdp=data[,2]
class(gdp)
str(gdp)
end(gdp)
frequency(gdp)
sum(is.na(gdp))
summary(gdp)

### To check stationarity
Box.test(gdp)# alternative hypothesis: stationary
## above test we conclude data is stationary 

## Your data are not stationary then we convert the statinary data.
#library(tseries)
 # test stationary signal
# Null hypothesis : data is stationary 
lag.length = 1
ts_diff1 <- diff(gdp, differences = 1)
Box.test(ts_diff1, lag=lag.length, type="Ljung-Box")


library(forecast)
tsdata=ts (gdp, start=c(1952), end=c(2018), frequency=67)# freq=67 for no. of row data.


cycle(ts_diff1) # get a boxplot by cycle.
boxplot(ts_diff1~cycle(ts_diff1,xlab="ts_diff1",ylab="GPD constant LCU in crore Rs"),Main="Yearly ts_diff1 BOXPLOT from 1960 to 2018")


### fitting the model ####
model=auto.arima(ts_diff1)
model
summary(model)
## Let run with trace to compare the information criteria.

auto.arima(ts_diff1,ic="aic",trace =TRUE) # To check model help of AIC and minimum AIC better the model.

##Install.packages("tseries")
library("tseries")

plot.ts(model$residuals)
acf(ts(model$residuals),main="ACF_Residual")
pacf(ts(model$residuals),main="PACF_Residual")

## use the model to forecast for next 10 years
myforecast=forecast(gdp,level = c(95),h=10)
myforecast
plot(myforecast)
summary(myforecast)
myforecast$method
myforecast$fitted
myforecast$series
myforecast$residuals
myforecast$mean


