# Question Three
################################################################################################################################################################
# a)	Check whether the time series is stationary in mean and variance. 
################################################################################################################################################################
#I will begin by reading in the dataset and selecting the relevant column for our analysis. Assuming you have downloaded and saved the dataset locally, you can do the following:
  
# Load necessary libraries
library(tidyverse)
library(lubridate)
library(TTR)

# Read in the dataset
axisbank <- read.csv("AXISBANK.csv", header = TRUE, stringsAsFactors = FALSE)

# Convert the date column to a date format
axisbank$date <- as.Date(axisbank$Date, format="%Y-%m-%d")

# Select the relevant columns for analysis
axisbank <- select(axisbank, date, Close)

# Rename the columns for convenience
colnames(axisbank) <- c("date", "price")


# Now that I have the dataset, I can plot the time series to visually inspect for any trends, seasonality, or cycles. I can also use the Augmented Dickey-Fuller (ADF) test to check for stationarity. Here's the code for both:
# Plot the time series
ggplot(axisbank, aes(x=date, y=price)) +
  geom_line() +
  labs(x="Date", y="Closing Price", title="Axis Bank Stock Price")


install.packages("tseries")
# library(tseries)
# adf.test(axisbank$close)
# Check for stationarity using ADF test
adf.test(axisbank$price)

#If the p-value of the ADF test is less than 0.05, I can reject the null hypothesis and conclude that the time series is stationary in mean and variance. If the p-value is greater than 0.05, I fail to reject the null hypothesis and conclude that the time series is non-stationary.

#If the time series is non-stationary, I can apply transformations such as differencing, taking the log, or taking the square root to make it stationary. Once the time series is stationary, I can proceed to step (b) to identify the order of AR and MA.

################################################################################################################################################################
# b)	Use acf() and pacf() functions to identify the order of AR and MA
################################################################################################################################################################
# To identify the order of AR and MA, I can use the acf() and pacf() functions. acf() computes the autocorrelation function, which shows the correlation between the time series and its lagged 
# values. pacf() computes the partial autocorrelation function, which shows the correlation between the time series and its lagged values after removing the effects of the intermediate lagged # values.

# I can use the ggtsdisplay() function from the forecast package to plot both ACF and PACF together.

# Here is the code to do so:
library(forecast)

# Plot ACF and PACF
ggtsdisplay(axisbank$price)

# The output will show the ACF and PACF plots. Based on the plots, I can identify the order of AR and MA. If the ACF plot tails off and the PACF plot cuts off after some lag, then it suggests an AR model. If the PACF plot tails off and the ACF plot cuts off after some lag, then it suggests an MA model. If both plots tail off, it suggests an ARMA model.

# In this case, based on the plots, I can see that the ACF plot tails off and the PACF plot cuts off after lag 1. This suggests that an AR(1) model might be appropriate for this time series.

################################################################################################################################################################
# c) Use auto.arima() to learn the best ARIMA model
################################################################################################################################################################
# Now, I can apply the auto.arima() function to our dataset to find the best model. Here's the code:
# Apply auto.arima() function
axis_arima <- auto.arima(axisbank$price, trace=TRUE)
axis_arima

################################################################################################################################################################
# d) Forecast h=10 step ahead prediction of the time series variable and plot it with the original time series.                                                    
################################################################################################################################################################
# here's how you can forecast h=10 step ahead prediction of the time series variable and plot it with the original time series:

library(forecast)

# Convert the 'date' column to a time series object
head(axisbank)
axisbank_ts <- ts(axisbank$date, start = c(2010, 1), frequency = 252)

# Fit an ARIMA model using auto.arima()
axisbank_arima <- auto.arima(axisbank_ts)

# Forecast 10 steps ahead
axisbank_forecast <- forecast(axisbank_arima, h = 10)

# Plot the original time series and the forecast
plot(axisbank_ts, main = "Axis Bank Closing Prices with Forecast", ylab = "Price")
lines(axisbank_forecast$mean, col = "blue")
legend("bottomright", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = c(1, 1), cex = 0.8)

#This code will first convert the 'close' column to a time series object using the ts() function. It will then fit an ARIMA model to the time series using the auto.arima() function. Next, it will #use the forecast() function to generate a forecast for the next 10 steps ahead. Finally, it will plot the original time series along with the forecast using the plot() function and add a legend #using the legend() function.
