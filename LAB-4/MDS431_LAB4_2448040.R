# Lab 4 – AR(1) Process: Simulation, ACF, PACF

# Load Required Libraries
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

# Plot the Original Time Series
plot(ts_data, main = "Monthly Mean Temperature (1901–2017)",
     ylab = "Temperature (°C)", col = "blue")

# Decompose time series (just to visualize structure)
decomp <- decompose(ts_data, type = "additive")
plot(decomp)

set.seed(123)
AR1_neg <- arima.sim(model = list(ar = -0.9), n = 1000)

# ACF and PACF plots
par(mfrow = c(2, 2))
ts.plot(AR1_neg, main = "AR(1) Time Series (phi = -0.9)", col = "blue")
acf(AR1_neg, main = "ACF: phi = -0.9")
pacf(AR1_neg, main = "PACF: phi = -0.9")

AR1_pos <- arima.sim(model = list(ar = 0.9), n = 1000)

# ACF and PACF plots
par(mfrow = c(2, 2))
ts.plot(AR1_pos, main = "AR(1) Time Series (phi = 0.9)", col = "darkgreen")
acf(AR1_pos, main = "ACF: phi = 0.9")
pacf(AR1_pos, main = "PACF: phi = 0.9")