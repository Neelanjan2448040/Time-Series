#Loading required libraries
library(tseries)
library(forecast)

#Reading the dataset
data <- read.csv("C:/Users/Neelanjan Dutta/OneDrive/Desktop/Time Series Forecasting/dataset.csv")

#View first few rows
head(data)

#Checking the structure and column names
str(data)
names(data)

#Check for missing values
sum(is.na(data))

#Convert wide format (YEAR, JAN–DEC) to a long vector
monthly_temps <- as.vector(t(data[, 2:13]))  # Take JAN–DEC columns row-wise
start_year <- data$YEAR[1]  # Starting year, should be 1901

#Create monthly time series object
temp_ts <- ts(monthly_temps, start = c(start_year, 1), frequency = 12)

#Confirm it's a time series object
class(temp_ts)

#Plot the time series
ts.plot(temp_ts,
        main = "Monthly Mean Temperature (India, 1901–2017)",
        ylab = "Temperature (°C)",
        xlab = "Year",
        col = "darkgreen", lwd = 2)

#Decompose time series into trend, seasonality and random component
decomp <- decompose(temp_ts, type = "additive")
plot(decomp)

#ACF plot to check for autocorrelation
acf(temp_ts, main="ACF of Original Monthly Series")

#ADF test for stationarity
adf_result <- adf.test(temp_ts)

if (adf_result$p.value < 0.05) {
  print("ADF Test: Time series is stationary.")
} else {
  print("ADF Test: Time series is NOT stationary.")
}

#Remove seasonality via seasonal differencing (lag = 12)
temp_seasonal_diff <- diff(temp_ts, lag = 12)

#Remove trend via regular differencing
temp_stationary <- diff(temp_seasonal_diff)

#Plot transformed series
ts.plot(temp_stationary,
        main = "After Seasonal + First Differencing",
        ylab = "Stationary Series",
        col = "blue", lwd = 2)

#ACF plot of differenced series
acf(temp_stationary, main="ACF after Differencing")

#ADF test after differencing
adf_diff_result <- adf.test(temp_stationary)

if (adf_diff_result$p.value < 0.05) {
  print("ADF Test after differencing: Time series is now stationary.")
} else {
  print("ADF Test after differencing: Time series is still NOT stationary.")
}

# Summary statistics
summary(temp_ts)