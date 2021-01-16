## Anual GDP Forecast using ARIMA Model 

setwd("E:/GDP")
data=read.csv("GDP quarterly.csv")
#attach(data)
View(data)
head(data,12)
gdp=data[,3]
class(gdp)
str(gdp)
end(gdp)
frequency(gdp)
sum(is.na(gdp))
summary(gdp)
library(tseries)
### To check stationarity 
adf.test(gdp) # alternative hypothesis: stationary
## above test we conclude data is non stationary 
## To convert data non stationary to stationary

library(tseries)
# test stationary signal
# Null hypothesis : data is stationary 
lag.length =4
ts_diff1 <- diff(gdp, differences = 2)
Box.test(ts_diff1, lag=lag.length, type="Ljung-Box")
# again we use the stationarty by using adf test
adf.test(ts_diff1)
#acf(gdp)
# Above data is stationarty

###
library(forecast)
tsdata=ts(ts_diff1,frequency = 4) # freq=4 for quarterly data.
ddata=decompose(tsdata,"multiplicative") # ddata: decomposition of data
plot(ddata)
plot(ddata$trend)
plot(ddata$seasonal)

plot(ts_diff1)
plot(gdp)
abline(reg = lm(ts_diff1~time(ts_diff1)))
cycle(ts_diff1) # get a boxplot by cycle.
boxplot(ts_diff1~cycle(ts_diff1,xlab="ts_diff1",ylab="GPD growth"),Main="Quarterly ts_diff1 BOXPLOT from 2000-01 to 2019-20")
## To remove outliers
#outlier=match(b,ts_diff1)
#outlier
#ts_diff1=ts_diff1[-c(outlier)]
#plot(ts_diff1_new)

### fitting the model ####
model=auto.arima(ts_diff1)# we replece ts_diff1 too gdp in model
model
summary(model)

## Let run with trace to compare the information criteria.

auto.arima(ts_diff1,ic="aic",trace =TRUE) # To check model help of AIC and minimum AIC better the model.

##Install.packages("tseries")
library("tseries")

plot.ts(model$residuals)
acf(ts_diff1)
acf(ts(model$residuals),main="ACF_Residual")
pacf(ts(model$residuals),main="PACF_Residual")

## use the model to forecast for next 10 years
myforecast=forecast(model,level = c(95),h=4*3)
myforecast
plot(myforecast)
summary(myforecast)
myforecast$method
myforecast$fitted
myforecast$series
myforecast$residuals
myforecast$mean
Box.test(model$resid,lag=5,type = "Ljung-Box")
