# Load Libraries
library(forecast)
library(tseries)

# Built-in Data
df <- BJsales
plot(df, main = "Time Series Data")

# Dickey-Fuller Test
print(adf.test(df))

# ACF & PACF Plot
acf(df, main = "ACF Plot")
pacf(df, main = "PACF Plot")

# Summary
manual <- arima(df, order = c(1,1,1))
auto <- auto.arima(df)
print(summary(manual))
cat("\n")
print(summary(auto))

# Forecast
plot(forecast(manual, h = 12), main = "Manual ARIMA Forecast")
plot(forecast(auto, h = 12), main = "Auto ARIMA Forecast")