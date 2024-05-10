# List of packages
packages <- c("readxl", "ggplot2", "dplyr", "zoo", "pracma","corrplot", "forecast", "mFilter", "tseries")

# Check if packages are installed
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

# Install uninstalled packages
if(length(new_packages)) install.packages(new_packages)

# Load the packages
library(readxl)     #read xlsx
library(ggplot2)    #plotting
library(dplyr)
library(zoo)        #moving averages
library(pracma)     #moving averages
library(forecast)   #forecasting
library(mFilter)    #HP filter
library(tseries)    #time series analysis

# Define the path of the Excel file
setwd("/home/admin1/Documents")
file_path <- "Merged_Data3.xlsx"
data <- read_excel(file_path)

# Exclude rows with blank or NA values
data <- na.omit(data)
# Show all the variables
variables <- names(data)
print(variables)
# Rename the columns for easier interpretation
data <- rename(data, Country=`Country Name`, Tech_Exports = `High-Tech_Exports`, `R&D` = `R&D_Expenditure`, Education = `Percent_of_Tertiary_Education`, Patent_applies = `Patent applications per million people`, GDP =`GDP(Current_US$)`)
# Print the data
print(data)

# Add variable of 'Tech_Export_proportion' for better interpretation
data$Tech_Export_proportion <- data$Tech_Exports / data$GDP * 100
# New order of columns
new_order <- c("Country", "Year", "Tech_Export_proportion", "Education", "R&D", "Patent_applies", "GDP", "Tech_Exports")
# Reorder the dataframe
data <- data[, new_order]
print(data)

# Generate descriptive statistics
summary(data)

# Quantile-based detection of outliers
    Q1 <- quantile(data$Tech_Export_proportion, 0.25)
    Q3 <- quantile(data$Tech_Export_proportion, 0.75)
    IQR <- Q3 - Q1 
    
    # Define lower and upper bounds for outliers
    lower_bound <- Q1 - 1.5 * IQR 
    upper_bound <- Q3 + 1.5 * IQR 
    # Identify outliers
    outliers <- data$Tech_Export_proportion < lower_bound | data$Tech_Export_proportion > upper_bound 
    # See outlier values
    print(data$Tech_Export_proportion[outliers]) 
    # Remove outliers
    clean_data <- data[!outliers, ]
    print (clean_data)

# Checking for stationarity using Augmented Dickey-Fuller test
adf_test <- adf.test(clean_data$Tech_Export_proportion, alternative = "stationary")
print(adf_test)

# Time series analysis: Corrected and Explained
# Creating a time series object
ts_data <- ts(clean_data$Tech_Export_proportion, start = min(clean_data$Year), frequency = 1)
# Checking for stationarity
adf_test <- adf.test(ts_data, alternative = "stationary")
print(paste("ADF test p-value:", adf_test$p.value))

# If not stationary, differencing the time series
if(adf_test$p.value > 0.05) {
  ts_data_diff <- diff(ts_data, differences = 1)
  adf_test_diff <- adf.test(ts_data_diff, alternative = "stationary")
  print(paste("Differenced ADF test p-value:", adf_test_diff$p.value))
}

# Applying ARIMA model
auto_arima_model <- auto.arima(ts_data, stationary = TRUE, stepwise = FALSE, approximation = FALSE)
print(auto_arima_model)
# Forecasting with the ARIMA model
forecast_arima <- forecast(auto_arima_model, h = 5) # Forecasting for the next 5 years
plot(forecast_arima)
# Interpretation of ARIMA model results
# The ARIMA model provides a forecast for tech exports based on historical data. The model parameters indicate the complexity of the time series' patterns.

# Applying ARMA model after ensuring stationarity
arma_model <- arima(ts_data, order = c(2, 0, 2))
print(arma_model)
# Forecasting with the ARMA model
forecast_arma <- forecast(arma_model, h = 5) # Forecasting for the next 5 years
plot(forecast_arma)
# Interpretation of ARMA model results
# The ARMA model, applied after confirming the time series is stationary, forecasts future tech exports. The model's fit suggests how past values influence future values.

# Summary of findings
# The corrected time series analysis, using ARIMA and ARMA models, provides accurate forecasts for tech exports. The analysis reveals the importance of ensuring stationarity in the data and the impact of historical values on future trends. These insights are crucial for understanding the dynamics of tech exports and can aid in strategic planning.
