milk <- read.csv('milk_prod.csv', sep = ';')
head(milk)
tail(milk)
str(milk)

#Rename colnames and bad last record, remove it
colnames(milk) <- c('Date', 'Production')
milk <- milk[-169,]
tail(milk)
str(milk)

#Plotting data along time
plot(milk$Date, milk$Production)
lines(milk$Date, milk$Production)
#Increasing trend, constant variance and rough seasonality

library(tseries)
library(forecast) 

#Create ts object
milk_ts <- ts(milk$Production,start=c(1962,1),frequency= 12)

#Step 1:  Do a time series plot of the data.  Examine it for features such as trend and seasonality.  
head(milk_ts)
tail(milk_ts)
plot(milk_ts) 
tsdisplay(milk_ts)

#Step 2:  Do any necessary differencing. The general guidelines are:
#If there is both trend and seasonality, apply a seasonal difference to the data and then re-evaluate the trend.  
#If a trend remains, then take first differences. 

milk_ts_d12 <- diff(milk_ts, lag=12, differences=1)
plot(milk_ts_d12)
milk_ts_d12_decompose <- decompose(milk_ts_d12)
plot(milk_ts_d12_decompose) 
#means D=1 in seasonal component (P,1,Q)12

milk_ts_d12and1 <- diff(milk_ts_d12, lag=1, differences=1)
plot(milk_ts_d12and1)
milk_ts_d12and1_d <- decompose(milk_ts_d12and1)
plot(milk_ts_d12and1_d)
#means d=1 in non-seasonal component (p,1,q)

#Step 3:  Examine the ACF and PACF of the differenced data
tsdisplay(milk_ts_d12and1)
#Non-seasonal terms:  Examine the early lags (1, 2, 3, …) to judge non-seasonal terms.  
#Spikes in the ACF (at low lags) indicate non-seasonal MA terms.  (p,1,1)
#Spikes in the PACF (at low lags) indicate possible non-seasonal AR terms. (0,1,1) o (1,1,1)

#Seasonal terms:  Examine the patterns across lags that are multiples of S.
#For example, for monthly data, look at lags 12, 24, 36, and so on.  
#Spikes in the ACF (at high lags) indicate seasonal MA terms. (P,1,1)
#Spikes in the PACF (at high lags) indicate possible seasonal AR terms. (0,1,1), (1,1,1), even (2,1,1)

#Step 4:  Estimate the model(s) that might be reasonable on the basis of Step 3. 
#Candidates:
prueba1 <- Arima(milk_ts, order = c(0,1,1), seasonal = list(order=c(0,1,1)))
prueba2 <- Arima(milk_ts, order = c(1,1,1), seasonal = list(order=c(0,1,1)))
prueba3 <- Arima(milk_ts, order = c(0,1,1), seasonal = list(order=c(1,1,1)))
prueba4 <- Arima(milk_ts, order = c(1,1,1), seasonal = list(order=c(1,1,1)))
prueba5 <- Arima(milk_ts, order = c(0,1,1), seasonal = list(order=c(2,1,1)))
prueba6 <- Arima(milk_ts, order = c(1,1,1), seasonal = list(order=c(2,1,1)))

#Step 5: Compare AIC or BIC values and examine the residuals (with ACF, Box-Pierce...). 
prueba1$aicc
prueba2$aicc
prueba3$aicc
prueba4$aicc
prueba5$aicc
prueba6$aicc
#First model with the lower AICc score (best) Arima(0,1,1)(0,1,1)[12]

summary(prueba1)
tsdisplay(prueba1$residuals)
mean(prueba1$residuals)
#[1] 0.0356593

#Let's do some Box.test. If it is applied to the residuals of a fitted ARIMA model, not the original series, 
#the hypothesis actually being tested is that the residuals from the ARIMA model have no autocorrelation.
#(fitdf parameter corresponds to the sum of p,q,P,Q)
Box.test(prueba1$residuals, lag = 12, fitdf = 2 , type = "Ljung-Box") #p-value = 0.423
Box.test(prueba1$residuals, lag = 24, fitdf = 2 , type = "Ljung-Box") #p-value = 0.7466
Box.test(prueba1$residuals, lag = 36, fitdf = 2 , type = "Ljung-Box") #p-value = 0.9622
Box.test(prueba1$residuals, lag = 48, fitdf = 2 , type = "Ljung-Box") #p-value = 0.9081
Box.test(prueba1$residuals, lag = 60, fitdf = 2 , type = "Ljung-Box") #p-value = 0.8379
Box.test(prueba1$residuals, lag = 72, fitdf = 2 , type = "Ljung-Box") #p-value = 0.9629
#With these p-values we cannot refuse H0 (data non-correlated).

#Step 6: Forecasting production milk (pounds per cow) during next 24 months
forecast_24m <- forecast.Arima(prueba1, h = 24, level = c(80,95))
forecast_24m
plot(forecast_24m)
