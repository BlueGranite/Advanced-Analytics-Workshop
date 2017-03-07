#
# Time series example in R 
# code and background information from 
# https://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/index.html
# by Avril Chohlan
# organized in this example by Andy Lathrop, BlueGranite, Inc.

# there are 2 main sections to view different time series
# manipulations and plotting: 1. standard and 2. ggplot

#####
# Section 1 - Standard

###
# package info
# ensure package 'pacman' for package management is installed
if (!require("pacman")) install.packages("pacman")

# Packages: "TTR" for time series functions, "forecast" for forecasting, 
# "ggplot2" and "ggfortify" for plotting. "ggfortify" requires the package "devtools"
# and is installed from github
pacman::p_load(TTR, 
               forecast, 
               ggplot2, 
               devtools)

# library(devtools)
install_github('sinhrks/ggfortify')
library(ggfortify)

### 
# load data

# this section included to identify source of original data and export to CSV ---
# initial data are a vector of dollar values of souvenir sales 
souvenir <- data.frame(sales = scan("http://robjhyndman.com/tsdldata/data/fancy.dat"))

##### optional - write CSV to working directory
# write.table(souvenir, file="souvenir-data.csv", row.names=FALSE, col.names=FALSE)
# read from local CSV in working-directory\data path
# souvenir <- read.csv("souvenir-data.csv", 
#                      header=FALSE, 
#                      check.names=FALSE, 
#                      stringsAsFactors=FALSE)

 #####
# EDA - exploratory data analysis prior to forecasting
# convert data to ts (time series) object with monthly values starting in JAN 2008
souvenir.ts <- ts(souvenir, frequency=12, start=c(2009,1))
souvenir.ts
plot.ts(souvenir.ts)

# additive model = fluctuations in the data are roughly constant in size over time.
# In this case, it appears that an additive model is not appropriate 
# for describing this time series, since the size of the seasonal 
# fluctuations and random fluctuations seem to increase with the level 
# of the time series. Thus, we may need to transform the time series 
# in order to get a transformed time series that can be described using 
# an additive model. For example, we can transform the time series by 
# calculating the natural log of the original data:
logsouvenir.ts <- log(souvenir.ts)
plot.ts(logsouvenir.ts)

###
# estimating trend
# To estimate the trend component of a non-seasonal time series that can be described using an additive model,
# it is common to use a smoothing method, such as calculating the simple moving average of the time series.
# estimate the trend, seasonal and irregular components of this time series
# using the SMA() function of the "TTR" package

# simple moving average, order 3
logsouvenir.SMA3 <- SMA(logsouvenir.ts,n=3)
plot.ts(logsouvenir.SMA3)

# plot simple moving average (order 8), with original logsouvenir overlay 
# The data smoothed with a simple moving average of order 8 gives a clearer picture of the trend component
logsouvenir.SMA8 <- SMA(logsouvenir.ts,n=8)
plot.ts(logsouvenir.SMA8, type="l", col="red")
lines(logsouvenir.ts, col="black")
points(logsouvenir.SMA8, col="red")

###
# decomposing seasonal data
# A seasonal time series consists of a trend component, a seasonal component and an irregular component. 
# Decomposing the time series means separating the time series into these three components: 
# that is, estimating these three components. We can do this using the function decompose()

# The function “decompose()” returns a list object as its result, where the estimates of the seasonal component,
# trend component and irregular component are stored in named elements of that list objects, called “seasonal”, 
# “trend”, and “random” respectively.
logcomponents <- decompose(logsouvenir.ts)

# look at seasonal component. Can also do this with $trend and $random
logcomponents$seasonal
plot.ts(logcomponents$seasonal)

# plot the estimated trend, seasonal, and irregular components of 
# the time series by using the “plot()” function
plot(logcomponents)

###
# side bar - seasonally adjusting
# seasonally adjust the time series by estimating the seasonal component, 
# and subtracting the estimated seasonal component from the original time series.
logseasonal.ts <- logsouvenir.ts - logcomponents$seasonal
plot(logseasonal.ts, main="Seasonally Adjusted")

#####
# Forecasting
# we will try different forecasting techniques and perform diagnostic tests

##################################
### Simple Exponential Smoothing #
##################################
# If you have a time series that can be described using an additive model with constant level and no seasonality, 
# you can use simple exponential smoothing to make short-term forecasts.

# Smoothing is controlled by the parameter *alpha*; for the estimate of the level at the current time point. 
# The value of alpha; lies between 0 and 1. Values of alpha that are close to 0 mean that little weight 
# is placed on the most recent observations when making forecasts of future values.

# To make forecasts using simple exponential smoothing in R, we can fit a simple exponential smoothing 
# predictive model using the “HoltWinters()” function in R. To use HoltWinters() for simple exponential smoothing, 
# we need to set the parameters *beta*=FALSE and *gamma*=FALSE in the HoltWinters() function (the beta and gamma parameters
# are used for Holt’s exponential smoothing, or Holt-Winters exponential smoothing, as described below).
# 
# The HoltWinters() function returns a list variable, that contains several named elements
logsouvenir.forecasts <- HoltWinters(logsouvenir.ts, beta=FALSE, gamma=FALSE)
logsouvenir.forecasts

# By default, HoltWinters() just makes forecasts for the same time period covered by our original time series.
# The forecasts made by HoltWinters() are stored in a named element of this list variable called “fitted”, 
# so we can get their values and plot against the original time series by:
logsouvenir.forecasts$fitted
plot(logsouvenir.forecasts)

# The plot shows the original time series in black, and the forecasts as a red line. The forecasts are much
# smoother than the original data, and do not match the trend or seasonal changes very well

# As a measure of the accuracy of the forecasts, we can calculate the sum of squared errors for the in-sample 
# forecast errors, that is, the forecast errors for the time period covered by our original time series. 
# The sum-of-squared-errors is stored in a named element of the list variable “rainseriesforecasts” called “SSE”, 
# so we can get its value by:
logsouvenir.forecasts$SSE

# We can make forecasts for further time points by using the “forecast.HoltWinters()” function in the 
# R “forecast” package. This package was loaded with code at the beginning of the script.

# When using the forecast.HoltWinters() function, as its first argument (input), you pass it the predictive
# model that you have already fitted using the HoltWinters() function. For example, in the case of the
# souvenir time series, we stored the predictive model made using HoltWinters() in the variable 
# “logsouvenir.forecasts”. You specify how many further time points you want to make forecasts for by using
# the “h” parameter in forecast.HoltWinters(). For example, to make a forecast of souvenir sales for the years
# 12 more monthsusing forecast.HoltWinters(), we type:
logsouvenir.forecasts2 <- forecast.HoltWinters(logsouvenir.forecasts, h=12)
logsouvenir.forecasts2
plot.forecast(logsouvenir.forecasts2, main="Simple Exponential Smoothing Forecast (12 months ahead")

# The forecast.HoltWinters() function gives you the forecast for a year, a 80% prediction interval for the
# forecast, and a 95% prediction interval for the forecast.

# Here the forecasts for JAN-DEC 2016 are plotted as a blue line, the 80% prediction interval as a light blue
# shaded area, and the 95% prediction interval as a grey shaded area.

# The in-sample forecast errors are stored in the named element “residuals” of the list variable returned by
# forecast.HoltWinters(). We can use the SSE for later performance comparison with other models. We can
# calculate the SSE from the residuals by:
sum(logsouvenir.forecasts2$residuals^2, na.rm = TRUE)

# Notice this is the same value obtained below from the original HoltWinters() function:
logsouvenir.forecasts$SSE

# If the predictive model cannot be improved upon, there should be no correlations between forecast 
# errors for successive predictions. In other words, if there are correlations between forecast errors 
# for successive predictions, it is likely that the simple exponential smoothing forecasts could be 
# improved upon by another forecasting technique.
acf(logsouvenir.forecasts2$residuals, lag.max=24, na.action = na.pass)

# You can see from the sample correlogram that the autocorrelation at several lags is just touching or past
# the significance bounds, indicating correlation between errors. To test whether there is significant evidence # for non-zero correlations at lags 1-24, we can carry out a Ljung-Box test. This can be done in R using the #“Box.test()”, function. The maximum lag that we want to look at is specified using the “lag” parameter in the 
# Box.test() function.For example, to test whether there are non-zero autocorrelations at lags 1-24, for the 
# in-sample forecast errors for London rainfall data, we type:
Box.test(logsouvenir.forecasts2$residuals, lag=24, type="Ljung-Box")

# Here the Ljung-Box test statistic is 131.68, and the p-value is 2.2e-16, so there is strong evidence of 
# non-zero autocorrelations in the in-sample forecast errors at lags 1-24. 

# It is also a good idea to check whether the forecast errors are normally distributed with mean zero,
# constant variance, and do not exhibit any distinct patterns. To check whether the forecast errors have
# constant variance, we can make a time plot of the in-sample forecast errors:
mean(logsouvenir.forecasts2$residuals, na.rm = TRUE)
plot.ts(logsouvenir.forecasts2$residuals)
# 
# The plot shows that the in-sample forecast errors exhibit a distinct pattern, since the model does not 
# account for seasonality.

# To check whether the forecast errors are normally distributed with mean zero, we can plot a histogram of 
# the forecast errors, with an overlaid normal curve that has mean zero and the same standard deviation as 
# the distribution of forecast errors. To do this, we can define an R function “plotForecastErrors()”, below:

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

# hard code removing NA (1st element in vector)
plotForecastErrors(logsouvenir.forecasts2$residuals[-1])

# The plot shows that the distribution of forecast errors is centered to the right of zero, 
# and appears to be slightly negatively skewed compared to a normal curve. These errors do not 
# indicate strong visual evidence of an approximately normal distribution. 

# Given the poor visual fit of the forecast, strong evidence of non-zero autocorrelation of 
# the forecast errors, and little evidence of Gaussian (normal) errors, We conclude we can 
# probably improve on the simple exponential model.

##################################
### Holt’s Exponential Smoothing #
##################################
# If you have a time series that can be described using an additive model with increasing or decreasing trend
# and no seasonality, you can use Holt’s exponential smoothing to make short-term forecasts.

# Holt’s exponential smoothing estimates the level and slope at the current time point. Smoothing is controlled
# by two parameters, alpha, for the estimate of the level at the current time point, and beta for the estimate 
# of the slope b of the trend component at the current time point. As with simple exponential smoothing, the
# paramters alpha and beta have values between 0 and 1, and values that are close to 0 mean that little weight
# is placed on the most recent observations when making forecasts of future values.

# To use HoltWinters() for Holt’s exponential smoothing, we need to set the parameter gamma=FALSE (the gamma
# parameter is used for Holt-Winters exponential smoothing)
logsouvenir.Holtforecasts <- HoltWinters(logsouvenir.ts, gamma=FALSE)
logsouvenir.Holtforecasts
plot(logsouvenir.Holtforecasts, main="Holt's Exponential Smoothing (in-sample)")

# We can see from the picture that the in-sample forecasts agree pretty well with general trend, but they tend
# to be offset with the periodoc peaks and valleys.

logsouvenir.Holtforecasts2 <- forecast.HoltWinters(logsouvenir.Holtforecasts, h=12)
plot(logsouvenir.Holtforecasts2, main="Holt's Exponential Smoothing (12 months ahead)")

### Plot to compare the different models
# We want to compare the original log time series with the 2 forecast models
# First, notice that the *first fitted* period for the simple exponential model
# is the *second overall* period (February 2009). You can check this with
souvenir.ts 
logsouvenir.forecasts$fitted

# Here we'll create a time series object for the fitted simple exponential model
# using the fitted values, and starting with the second (monthly) period of 2009
logsouvenir.forecasts.ts <- ts(logsouvenir.forecasts$fitted[,"xhat"], 
                               frequency=12, start=c(2009,2))

# The Holt exponential smoothing produces its first fitted value in the
# *third overall* period (March 2009), so our time series object will
# begin with the third period of 2009
logsouvenir.Holtforecasts.ts <- ts(logsouvenir.Holtforecasts$fitted[,"xhat"], 
                               frequency=12, start=c(2009,3))

# prepare plotting elements
xlim <- range(time(souvenir.ts))
ylim <- range(logsouvenir.ts, 
              logsouvenir.forecasts.ts, 
              logsouvenir.Holtforecasts.ts, na.rm=TRUE)

# plot comparing original log values with simple and Holt exponential fitted values
plot(logsouvenir.ts, xlim=xlim, ylim=ylim, 
     type="b", col="grey", lwd=3, 
     main="Comparison of Time Series Forecasting Methods")
lines(logsouvenir.forecasts.ts, type="b", col="red", lwd=2)
lines(logsouvenir.Holtforecasts.ts, type="b", col="dodgerblue", lwd=2)

legend("bottomright", 
       c("Original Log Time Series", "Simple Exponential Smoothing", "Holt's Exponential"), 
       lty=c(2,2,2), lwd=c(3,2.5,2.5), pch="o", pt.cex = 1,
       col=c("grey", "red", "dodgerblue"))

# Compare SSE values
logsouvenir.forecasts$SSE
logsouvenir.Holtforecasts$SSE

# The Holt SSE is larger because of the larger difference in the fitted values at the extreme points;
# the simple exponential model is smoother, and so has smaller residual errors
# However, when we compare the forecasted future values, Holt accounts for the upward trend,
# while the simple exponential model produces a single, flat value

# Diagnotic tests for Holt model. These indicate strong evidence of non-zero autocorrelation
# of the residuals
acf(logsouvenir.Holtforecasts2$residuals, lag.max=24)
Box.test(logsouvenir.Holtforecasts2$residuals, lag=24, type="Ljung-Box")
plot.ts(logsouvenir.Holtforecasts2$residuals)
plotForecastErrors(logsouvenir.Holtforecasts2$residuals)

########################################
### Holt-Winters Exponential Smoothing #
########################################
# If you have a time series that can be described using an additive model with increasing or 
# decreasing trend and seasonality, you can use Holt-Winters exponential smoothing to make 
# short-term forecasts.

# Holt-Winters exponential smoothing estimates the level, slope and seasonal component at 
# the current time point. Smoothing is controlled by three parameters: alpha, beta, and gamma, 
# for the estimates of the level, slope b of the trend component, and the seasonal component, 
# respectively, at the current time point. The parameters alpha, beta and gamma all have 
# values between 0 and 1, and values that are close to 0 mean that relatively little weight 
# is placed on the most recent observations when making forecasts of future values.

logsouvenir.HoltWintersforecasts <- HoltWinters(logsouvenir.ts)
logsouvenir.HoltWintersforecasts
 
# The estimated values of alpha, beta and gamma are 0.41, 0.00, and 0.96, respectively. The 
# value of alpha (0.41) is relatively low, indicating that the estimate of the level at the 
# current time point is based upon both recent observations and some observations in the more 
# distant past. The value of beta is 0.00, indicating that the estimate of the slope b of the 
# trend component is not updated over the time series, and instead is set equal to its initial 
# value. This makes good intuitive sense, as the level changes quite a bit over the time series, 
# but the slope b of the trend component remains roughly the same. In contrast, the value 
# of gamma (0.96) is high, indicating that the estimate of the seasonal component at the 
# current time point is just based upon very recent observations.

plot(logsouvenir.HoltWintersforecasts, main="Holt-Winters Exponential Smoothing (in-sample)")

# We see from the plot that the Holt-Winters exponential method is very successful in predicting 
# the seasonal peaks, which occur roughly in December every year.

# Forecast future values. Note: the forecast for 48 months in the future is for illustration purposes only;
# the further in the future you forecast, the larger the prediction error. A shorter forecast horizon is
# generally recommended
logsouvenir.HoltWintersforecasts2 <- forecast.HoltWinters(logsouvenir.HoltWintersforecasts, h=48)
plot.forecast(logsouvenir.HoltWintersforecasts2, 
              main="Holt-Winters Exponential Smoothing (48 months ahead)")

# Diagnotic tests for Holt-Winters model. These indicate little evidence of non-zero 
# autocorrelation of the residuals, and good evidence of Gaussian errors
acf(logsouvenir.HoltWintersforecasts2$residuals, lag.max=24)
Box.test(logsouvenir.HoltWintersforecasts2$residuals, lag=24, type="Ljung-Box")
plot.ts(logsouvenir.HoltWintersforecasts2$residuals)
plotForecastErrors(logsouvenir.HoltWintersforecasts2$residuals)

# Compare SSE values - Holt-Winters wins for the in-sample error.
logsouvenir.forecasts$SSE
logsouvenir.Holtforecasts$SSE
logsouvenir.HoltWintersforecasts$SSE

#####
# Section 2 - ggplot2 and ggfortify
# ggfortify allows ggplot2 to use ts objects directly in plotting

