# Read the dataset
data <- read.csv("C:/Users/Neelanjan Dutta/OneDrive/Desktop/Time Series Forecasting/rainfall dataset.csv")

# View first few rows
head(data)

# Check structure and column names
str(data)
names(data)

# Check for missing values
sum(is.na(data))

# Rename column for clarity
names(data)[names(data) == "JUN.SEP"] <- "Total_Rainfall"

# Count total observations
n <- length(data$Total_Rainfall)
print(paste("Total no. of obs:", n))

# Get start year for time series
start_year <- min(data$YEAR)

# Create time series object (annual frequency = 1)
rain_ts <- ts(data$Total_Rainfall, start = start_year, frequency = 1)

# Confirm it's a time series object
class(rain_ts)

# Plot the raw time series
ts.plot(rain_ts,
        main = "Total Monsoon Rainfall (June–September), India (1901–2021)",
        xlab = "Year", ylab = "Rainfall (mm)",
        col = "blue", lwd = 2)

# Summary statistics
summary(rain_ts)


library(zoo)

# 5-year moving average to smooth out short-term fluctuation
rain_ma <- rollmean(rain_ts, k = 5, fill = NA)

# Plot original series with smoothed trend
plot(rain_ts, col = "blue", lwd = 1.5,
     main = "Rainfall with 5-Year Moving Average Trend",
     ylab = "Rainfall (mm)", xlab = "Year")
lines(rain_ma, col = "red", lwd = 2)
legend("topleft", legend = c("Original", "5-Year Moving Average"),
       col = c("blue", "red"), lty = 1, lwd = 2)