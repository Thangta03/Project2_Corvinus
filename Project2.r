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

############################################################################################################
# interpret data for single year
    # Count the observation by year
    # Filter the data for year 2016
    data_2016 <- filter(clean_data, Year == 2016)
    print(data_2016, n=300)  # 55 observations
    summary(data_2016)

    # Scatter plot of Tech_Export_proportion vs R&D with smooth line
    ggplot(data_2016, aes(x=`R&D`, y=Tech_Export_proportion)) +
        geom_point(aes(color = Education)) +  # Color points by Education
        geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +  # Add a smooth line
        labs(title="Scatterplot of Tech_Export_proportion vs R&D", x="R&D", y="Tech_Export_proportion") +
        theme_minimal()  # Use a minimal theme

    # Scatter plot of Tech_Export_proportion vs Education with smooth line
    ggplot(data_2016, aes(x=Education, y=Tech_Export_proportion)) +
        geom_point(aes(color = `R&D`)) +  # Color points by R&D
        geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +  # Add a smooth line
        labs(title="Scatterplot of Tech_Export_proportion vs Education", x="Education", y="Tech_Export_proportion") +
        theme_minimal()  # Use a minimal theme
    # Scatter plot of Tech_Export_proportion vs Patent_applies with smooth line
    ggplot(data_2016, aes(x=Patent_applies, y=Tech_Export_proportion)) +
        geom_point(aes(color = Education)) +  # Color points by Education
        geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +  # Add a smooth line
        labs(title="Scatterplot of Tech_Export_proportion vs Patent_applies", x="Patent_applies", y="Tech_Export_proportion") +
        theme_minimal()  # Use a minimal theme

    # Scatter plot of Tech_Export_proportion vs GDP with smooth line
    ggplot(data_2016, aes(x=GDP, y=Tech_Export_proportion)) +
        geom_point(aes(color = `R&D`)) +  # Color points by R&D
        geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +  # Add a smooth line
        labs(title="Scatterplot of Tech_Export_proportion vs GDP", x="GDP", y="Tech_Export_proportion") +
        theme_minimal()  # Use a minimal theme

    library(corrplot)
    corr_matrix <- cor(data_2016[,c("Tech_Export_proportion", "R&D", "Education", "Patent_applies", "GDP")], use="complete.obs")
    corrplot(corr_matrix, method="square")
############################################################################################################       
# interpret data for multiple years
    # Filter the data for years 2013 to 2018
    data_2013_2018 <- filter(clean_data, Year >= 2013 & Year <= 2018)

    # Group by Country and count the number of years
    country_year_count <- data_2013_2018 %>% group_by(Country) %>% summarise(n = n_distinct(Year))

    # Filter for countries that have data for all six years
    Cleaned_countries <- filter(country_year_count, n == 6)

    # Print the countries with complete data
    print(Cleaned_countries$Country)  # 36 countries
    # Filter clean_data for the countries in Cleaned_countries
    final_data <- filter(clean_data, Country %in% Cleaned_countries$Country & Year >= 2013 & Year <= 2018)
    # Print the filtered data
    print(final_data, n=300)   #  Country       Year Tech_Export_proportion Education  `R&D` Patent_applies     GDP

    # Create a line plot of Tech Exports Proportion over time
    ggplot(final_data, aes(x=Year, y=Tech_Export_proportion, color=Country)) +
    geom_line() +
    labs(title="Tech Exports over time", x="Year", y="Tech Exports") +
    theme_minimal()
    # The graph were not clear with too many countries, so we will filter for specified countries
    # Filter for specified countries
    specified_countries <- c("Austria", "Belgium", "Brazil", "Canada", "Croatia", "Cyprus", "Denmark", "Finland", "Hungary", "Spain", "Sweden", "Norway")
    specified_data <- final_data[final_data$Country %in% specified_countries,]

    # Create a line plot of Tech Exports Proportion over time for specified countries
    ggplot(specified_data, aes(x=Year, y=Tech_Export_proportion, color=Country)) +
    geom_line() +
    labs(title="Tech Exports Proportion over time", x="Year", y="Tech Exports Proportion") +
    theme_minimal()

############################################################################################################
# Time series analysis: Finalized
# Finalize the deterministic model
final_data$Year <- as.numeric(format(final_data$Year, "%Y"))
final_data$Time <- 1:nrow(final_data)
model <- lm(Tech_Export_proportion ~ Time, data = final_data)

# Refine autocorrelation examination
acf_res <- acf(resid(model), plot = FALSE)
pacf_res <- pacf(resid(model), plot = FALSE)
Box.test(resid(model), type = "Ljung-Box")

# Enhanced forecasting
forecast_model <- auto.arima(final_data$Tech_Export_proportion)
forecast_values <- forecast(forecast_model, h = 2) # Forecast for the next 2 years
plot(forecast_values)

# Update trend-adjusted data calculation
trend_component <- model$coefficients[2]*final_data$Time
final_data$Trend_Adjusted <- final_data$Tech_Export_proportion - trend_component

# Correct application of the HP filter for cycle component determination
hp_result <- hpfilter(final_data$Trend_Adjusted, freq = 14400)
final_data$Cycle <- hp_result$cycle

# Accurate calculation of moving averages
final_data$MA3 <- rollmean(final_data$Tech_Export_proportion, k=3, fill=NA, align = "right")
final_data$CMA3 <- rollmean(final_data$Tech_Export_proportion, k=3, fill=NA, align="center")
final_data$EMA <- movavg(final_data$Tech_Export_proportion, n=3, type="e")

# Create and refine time series objects
ts_data <- ts(final_data$Tech_Export_proportion, start = 2013, frequency = 1)

# Comprehensive checks
wn_check <- Box.test(ts_data, lag = 5, type = "Ljung-Box")
acf_data <- acf(ts_data, lag.max = 5, plot = FALSE)
pacf_data <- pacf(ts_data, lag.max = 5, plot = FALSE)
ar_model <- ar(ts_data, order.max = 2)
arma_model <- arima(ts_data, order = c(2, 0, 2))
check_model <- Box.test(arma_model$residuals, lag = 5, type = "Ljung-Box")
shapiro_test <- shapiro.test(arma_model$residuals)
jarque_bera_test <- jarque.bera.test(arma_model$residuals)

# Refined forecasting using ARMA model
forecast_arma <- forecast(arma_model, h = 2) # Forecast for the next 2 years
plot(forecast_arma)

# Accurate documentation of results
print(summary(forecast_model))
print(summary(arma_model))
print(forecast_values)
print(forecast_arma)
