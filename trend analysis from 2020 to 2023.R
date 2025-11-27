#=============libraries===============
library(forecast)
library(tseries)
library(timeSeries)
library(xts)
library(timeDate)
library(ggplot2)
#===============data==================
data_2020_to_2023 <- read.csv("~/data_2020_to_2023.csv")
View(data_2020_to_2023)

attach(data_2020_to_2023)

# Example dataset with month and value columns for one year
# Replace this with your actual dataset
data <- data.frame(
  month = month.abb,  # Months
  year = rep(2020, 12),  # Year
  value = ATTENDANCE  # Example values
)

# Convert month and year to a Date object
date <- as.Date("2022-01-01", format = "%Y-%m-%d")

# Convert to time series object
ts_data <- ts(data$value, start = c(2020, 1), frequency = 12)

# Plot the time series using base R plot function
plot(ts_data, main = "Time Series Data", xlab = "Date", ylab = "Value")

# Perform STL decomposition
decomposed <- decompose(ts_data)
# Plot the decomposition components
plot(decomposed)

adf.test(ts_data) # If the p-value is less than the significance level (typically 0.05), you can reject the null hypothesis of non-stationarity.

#==============================ARIMA MODELS================================
# Plot ACF and PACF
acf(ts_data)
pacf(ts_data)

# Model Selection
arima_model_aic <- auto.arima(ts_data, ic = "aic")
arima_model_bic <- auto.arima(ts_data, ic = "bic")

# Print model summaries
summary(arima_model_aic)
summary(arima_model_bic)

# Fitting an ARIMA(1,1,1) model
arima_model <- arima(ts_data, order = c(1,1,1))
arima_model

# Generate forecasts for next 6 years that is from 2023-2029
forecast_values <- forecast(arima_model, h = 6*12)
forecast_values

# Plotting original data, fitted values, and forecasts
plot(forecast_values)

#Evaluating the performance of the ARIMA model using appropriate 
  #metric; Mean Absolute Error (MAE)
mae <- mean(abs(forecast_values$mean - ATTENDANCE))
mae






