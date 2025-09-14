
# Lab 4 – Winters’ Exponential Smoothing
# Dataset: Mean_Temp_IMD_2017.csv (1901–2017 monthly temp)


# Load Libraries
library(readr)
library(dplyr)
library(tidyr)
library(forecast)

# Load the Dataset
data <- read.csv("C:/Users/Neelanjan Dutta/OneDrive/Desktop/Time Series Forecasting/dataset.csv")

# Reshape Wide to Long
monthly_data <- data %>% select(YEAR, JAN:DEC)

long_data <- pivot_longer(monthly_data,
                          cols = JAN:DEC,
                          names_to = "Month",
                          values_to = "Temp")

# Order months correctly
month_levels <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                  "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
long_data$Month <- factor(long_data$Month, levels = month_levels)

# Sort by year and month
long_data <- long_data %>% arrange(YEAR, Month)

# Convert to Time Series Object
ts_data <- ts(long_data$Temp, start = c(1901, 1), frequency = 12)

# Plot the Original Series
plot(ts_data, main = "Monthly Mean Temperature (1901–2017)",
     ylab = "Temperature (°C)", col = "blue")

#Decompose time series into trend, seasonality and random component
decomp <- decompose(ts_data, type = "additive")
plot(decomp)

# Apply Winters’ Exponential Smoothing
# (Handles trend and seasonality internally)
hw_model <- HoltWinters(ts_data)  # Additive model (default)

# Plot fitted model
plot(hw_model, main = "Winters' Exponential Smoothing - Fitted")

# Forecast Next 5 Months
hw_forecast <- forecast(hw_model, h = 5)

# View forecasted values
print(hw_forecast)

# Plot the forecast
plot(hw_forecast, main = "5-Month Forecast using Winters' Method")
