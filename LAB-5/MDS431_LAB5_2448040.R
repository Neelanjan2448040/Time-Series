# =========================================================
# Lab 5: MA(3) Process - Simulation and Real Data Analysis
# =========================================================

# ===== 1. Load required packages =====
library(forecast)   # For ACF/PACF plotting
library(tseries)    # For Augmented Dickey-Fuller test
library(stats)      # For arima.sim

# ---------------------------------------------------------
# PART A: Simulation of MA(3) Process
# ---------------------------------------------------------

cat("\n==============================\n")
cat(" Simulated MA(3) Process \n")
cat("==============================\n")

# Step 1: Simulate MA(3) process
set.seed(123)  
ma3_series <- arima.sim(
  model = list(ma = c(0.5, -0.4, 0.3)),
  n = 500
)

# Step 2: Plot the simulated series
ts.plot(ma3_series,
        main = "Simulated MA(3) Process (n = 500)",
        ylab = "Value",
        col = "blue")

# Step 3: Basic statistics
cat("\n=== Basic Summary Statistics (Simulation) ===\n")
cat("Mean:", mean(ma3_series), "\n")
cat("Variance:", var(ma3_series), "\n")
cat("Standard Deviation:", sd(ma3_series), "\n")

# Step 4: ACF and PACF
acf(ma3_series, main = "ACF - Simulated MA(3)")
pacf(ma3_series, main = "PACF - Simulated MA(3)")

# Step 5: Numerical ACF/PACF
acf_values <- acf(ma3_series, plot = FALSE, lag.max = 10)
pacf_values <- pacf(ma3_series, plot = FALSE, lag.max = 10)
cat("\nACF (first 10 lags):\n", round(acf_values$acf, 3), "\n")
cat("PACF (first 10 lags):\n", round(pacf_values$acf, 3), "\n")

# Step 6: ADF Test
adf_test <- adf.test(ma3_series)
cat("\nADF Test (Simulation):\n")
print(adf_test)

if (adf_test$p.value > 0.05) {
  cat("Conclusion: NON-STATIONARY\n")
} else {
  cat("Conclusion: STATIONARY\n")
}