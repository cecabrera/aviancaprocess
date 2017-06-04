# https://www.quantinsti.com/blog/forecasting-stock-returns-using-arima-model/

library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)

# Pull data from Yahoo finance 
getSymbols('TECHM.NS', from='2012-01-01', to='2015-01-01')

# Select the relevant close price series
stock_prices = TECHM.NS[,4]

# Compute the log returns for the stock
stock = diff(log(stock_prices),lag=1)
stock = diff(stock_prices,lag=1)
stock = stock[!is.na(stock)]

# Plot log returns 
plot(stock,type='l', main='log returns plot')

# Conduct ADF test on log returns series
print(adf.test(stock))

# Split the dataset in two parts - training and testing
breakpoint = floor(nrow(stock)*(2.9/3))

# Apply the ACF and PACF functions
par(mfrow = c(1,1))
acf.stock = acf(stock[c(1:breakpoint),], main='ACF Plot', lag.max=100)
pacf.stock = pacf(stock[c(1:breakpoint),], main='PACF Plot', lag.max=100)

# Initialzing an xts object for Actual log returns
Actual_series = xts(0,as.Date("2014-11-25","%Y-%m-%d"))

# Initialzing a dataframe for the forecasted return series
forecasted_series = data.frame(Forecasted = numeric())

for (b in breakpoint:(nrow(stock)-1)) {
  
  stock_train = stock[1:b, ]
  stock_test = stock[(b+1):nrow(stock), ]
  
  # Summary of the ARIMA model using the determined (p,d,q) parameters
  fit = arima(stock_train, order = c(2, 0, 2),include.mean=FALSE)
  summary(fit)
  
  # plotting a acf plot of the residuals
  acf(fit$residuals,main="Residuals plot")
  
  # Forecasting the log returns
  arima.forecast = forecast.Arima(fit, h = 1,level=99)
  summary(arima.forecast)
  
  # plotting the forecast
  par(mfrow=c(1,1))
  plot(arima.forecast, main = "ARIMA Forecast")
  
  # Creating a series of forecasted returns for the forecasted period
  forecasted_series = rbind(forecasted_series,arima.forecast$mean[1])
  colnames(forecasted_series) = c("Forecasted")
  
  # Creating a series of actual returns for the forecasted period
  Actual_return = stock[(b+1),]
  Actual_series = c(Actual_series,xts(Actual_return))
  rm(Actual_return)
  
  print(stock_prices[(b+1),])
  print(stock_prices[(b+2),])
  
}

# Incluir la variable categórica: Días de la semana para identificar tendenci
# https://www.r-bloggers.com/forecasting-stock-returns-using-arima-model-with-exogenous-variable-in-r/

