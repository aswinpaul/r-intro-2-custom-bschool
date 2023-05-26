# Time series data and forecasting demo
# Textbook https://otexts.com/fpp2/
# Forecasting: Principles and Practice (2nd ed)
# Rob J Hyndman and George Athanasopoulos

#install.packages("tidyverse")
#install.packages("fpp2")
#install.packages("fBasics")

library(fpp2)
library(tidyverse)
library(fBasics)
library(tseries)

data <- read_csv("real_sales_per_day.csv")
# Monthly sales per day US
# Adjusted for inflation

#Basic statisics
basicStats(data$`Sales Per Day`)

# Test for stationarity
# ADF test
# In statistics, an augmented Dickey–Fuller test (ADF) 
# tests the null hypothesis that a unit root is present in a time series sample.

adf.test(data$`Sales Per Day`) #Non stationary

# Test on log data
ln_sales <- log(data$`Sales Per Day`)
adf.test(ln_sales)

# First difference
fd_sales <- diff(data$`Sales Per Day`)
adf.test((fd_sales))


# Declare as time series data
Y <- ts(data[,2], start = c(1992,1), frequency = 12)

# Preliminary analysis

# timeplot

autoplot(Y) + 
ggtitle("Time plot: Retails sales per day") +
ylab("Millions of dollars (2017)")

# data has a strong trend: Investigate transformations
# take first difference
DY <- diff(Y)

# time plot of difference data
autoplot(DY) + 
  ggtitle("Time plot: Change in retails sales per day") +
  ylab("Millions of dollars (2017)")

# Series appear stationary, investigate seasonality

ggseasonplot(DY) + 
ggtitle("Seasonal Plot: Change in RSales") + 
ylab("Millions of Dollars (2017")

# Another seasonal plo, the subseries plot

ggsubseriesplot(DY) +
ggtitle("Seasonal Plot: Change in RSales") + 
ylab("Millions of Dollars (2017")

# Notes:
# Series Y has trend and seasonality
# First diff removed trends
# The first diff series still had seasonality

# Forecast with various methods

# Benchmark method to forecast

# Mean as benchmark: Bad idea, why?

# Seasonal naive method as benchmark
# y_t = y_(t+s) + r_error

# Seasonal naive method results
fit <- snaive(DY) # Residual SD =  287.06 (Benchmark)
print(summary(fit))
checkresiduals(fit)

##########################
# Fit ETS method (Exponential smoothing)
##########################

fit_ets <- ets(Y) # Uses the data itself
print(summary(fit_ets)) # Residual SD = 218.8133
checkresiduals(fit_ets)

########################
# Fit an ARIMA model
# Autoregressive integrated moving average
#########################
# Data used in ARIMA model needs to be stationery

# 1. Use the regular data
# d = 1 , takes the first-difference
# D = 1, takes the seasonal-difference

fit_arima <- auto.arima(Y, d=1, D=1, stepwise = FALSE, 
                        approximation = FALSE, trace = TRUE)
print(summary(fit_arima)) # Residual SD = sqrt(39129) = 197.8
checkresiduals(fit_arima)

##################################
# Forecast using ARIMA model
#################################

fcst <- forecast(fit_arima, h=24)
autoplot(fcst)
autoplot(fcst, include=60)

summary(fcst)