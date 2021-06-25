######################################################
# This file loads temperature in the Philippines for 125 Years.
# The data will be used to forecast the next fifteen years.
#
#
# Created by: Laine Francis Tomalon 6/15/21
######################################################

# Clear all variables in workspace
rm(list=ls())

# Load the forecasting package
library(fpp2)

# Load the data
data <- read.csv("/Users/User/Documents/Temperature_Philippines_125yrs.csv")

# Declare this as time series
Y <- ts(data[,2],start=c(1887,1),frequency = 12)

######################################################
# Preliminary Analysis
######################################################

# Time Plot
autoplot(Y) + 
  ggtitle("Time Plot: Philippine Temperature in 125 years") +
  xlab("Year") +
  ylab("Temperature")

# The data shows a trend, hence it is not stationary. We'll investigate transformations
# Take the first difference of the date to remove the trend
DY <- diff(Y)

# Time Plot of difference data, differencing
autoplot(DY) + 
  ggtitle("Change in Philippine Temperature in 125 years") +
  xlab("Year") +
  ylab("Temperature")

# After differencing, the series appears trend-stationary, use to investigate seasonality
ggseasonplot(DY) + 
  ggtitle("Seasonal Plot: Change in Philippine Temperature") +
  xlab("Month") +
  ylab("Temperature")

# Let's look at another seasonal plot, the subseries plot
ggsubseriesplot(DY) + 
  ggtitle("Seasonal Subseries Plot: Change in Philippine Temperature") +
  ylab("Temperature")
  
###########################################################
# Our series, Y, shows a trend and seasonality.
# To remove the trend, we take the first difference.
# The first difference series still has seasonality.
#
# Forecast with various methods.
###########################################################

###############
# Use a benchmark method to forecast.
# We'll use the seasonal naive method.
###############

fit <- snaive(DY) # Residual SD = 0.4645
print(summary(fit))
checkresiduals(fit)

#####################
# Fit ETS Method
#####################
fit_ets <- ets(Y) # Residual SD = 0.3048
print(summary(fit_ets))
checkresiduals(fit_ets)

#####################
# Fit an ARIMA model
#####################

fit_arima <- auto.arima(Y,d=1,D=1,stepwise = FALSE, approximation=FALSE, trace=TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)

##########################
# Forecast with ARIMA model
##########################

fcst <- forecast(fit_arima,h=180)
autoplot(fcst)
print(summary(fcst))

