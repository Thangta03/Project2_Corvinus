# Comprehensive Methodology Summary
# This project encompasses a detailed methodology involving data preparation, analysis, visualization, and time series analysis. It starts with the installation and loading of necessary R packages for data manipulation and visualization. The data is then prepared through loading, cleaning (removing NA values), and renaming for clarity. Descriptive statistics and outlier detection are performed, followed by filtering for specific criteria. Visualization is achieved through scatter plots and line plots to explore relationships and trends over time. Time series analysis includes model creation, autocorrelation checking, and forecasting, with a focus on addressing multicollinearity in regression models. Enhancements include automated testing for data integrity and model accuracy.

# List of packages
packages <- c("readxl", "ggplot2", "dplyr", "zoo", "pracma","corrplot")

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

# Define the path of the Excel file
setwd("/home/admin1/Documents")
file_path <- "Merged_Data3.xlsx"
data <- Merged_Data3

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
    corr_matrix_2 <- cor(data_2016[,c("Tech_Export_proportion", "R&D", "Education", "Patent_applies")], use="complete.obs")
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

    final_data <- rename(data, RD_Expenditure = `R&D`)
    
    # Variance Inflation Factor to check multicollinearity
    model <- lm(Tech_Export_proportion ~ RD_Expenditure + Education + Patent_applies, data = final_data)
    plot(model)  # Residual plots
    shapiro.test(residuals(model))  # Normality of residuals
    vif(model)
    summary(model)
    library(car)  # For vif function
    vif_values <- vif(model)
    print(vif_values)
    
    
   # AIC with combinations to test model with necessary variables
    # Step 1: Create all possible combinations of variables
    variables <- c("RD_Expenditure", "Education", "Patent_applies")
    variable_combinations <- lapply(1:length(variables), function(x) combn(variables, x, simplify = FALSE))
    
    # Step 2: Fit models for each combination and calculate AIC
    aic_results <- lapply(variable_combinations, function(comb) {
      models <- lapply(comb, function(vars) {
        formula <- reformulate(vars, response = "Tech_Export_proportion")
        model <- lm(formula, data = final_data)
        return(model)
      })
      aic_values <- sapply(models, AIC)
      return(data.frame(Variables = sapply(comb, paste, collapse = ","), AIC = aic_values))
    })
    
    # Step 3: Combine AIC results into a single dataframe
    aic_results_df <- bind_rows(aic_results)
    
    # Step 4: Print AIC table sorted by AIC values
    aic_results_df <- aic_results_df %>% arrange(AIC)
    print(aic_results_df)
    ## Model with R&D + Patent_applies with lowest AIC
############################################################################################################
#Time series: # On process
# Create a deterministic model
final_data$Year <- as.numeric(format(final_data$Year, "%Y"))
final_data$Time <- 1:nrow(final_data)
model <- lm(Tech_Export_proportion ~ Time, data = final_data)

# Examine autocorrelation for the residual of the model
acf(resid(model))
Box.test(resid(model), type = "Ljung-Box")

# Make a forecast for 2019
new_data <- data.frame(Time = nrow(final_data) + 1)
forecast <- predict(model, newdata = new_data)
actual <- final_data$Tech_Export_proportion[final_data$Year == 2018]
compare <- data.frame(Forecast = forecast, Actual = actual)

# Calculate the trend-adjusted data
trend_component <- model$coefficients[2]*final_data$Time
final_data$Trend_Adjusted <- final_data$Tech_Export_proportion - trend_component

# Determine the cycle component using the HP filter
library(mFilter)
hp_result <- hpfilter(final_data$Trend_Adjusted, freq = 6)
final_data$Cycle <- hp_result$cycle

# Prepare a textual analysis
# This part is subjective and depends on the specific results obtained.

# Calculate moving averages for Argentina
final_data$MA3 <- rollmean(final_data$Tech_Export_proportion, k=3, fill=NA, align = "right")
final_data$CMA3 <- rollmean(final_data$Tech_Export_proportion, k=3, fill=NA, align="center")
final_data$EMA <- movavg(final_data$Tech_Export_proportion, n=3, type="e") # alpha=2/(n+1), so if alpha is 0.2, then n should be 9

# Plot Tech Exports and the 3 moving averages for Argentina
ggplot(final_data, aes(x=Year))+
    geom_line(aes(y=Tech_Export_proportion, color="Tech Exports"), size=1)+
    geom_line(aes(y=MA3, color="Simple Moving average (3)"), size=1)+
    geom_line(aes(y=CMA3, color="Centered Moving average (3)"), size=1)+
    geom_line(aes(y=EMA, color="Exponential Moving average (alpha=0.2"), size=1)+
    theme_minimal()+
    labs(title="Tech Exports over time for Argentina",
             subtitle = "With different type of Moving Averages",
             y="Tech Exports",
             colour="Time series processes",
             caption="Source: Department of Statistics",
             tag="Figure 1")

# Create a time series object
ts_data <- ts(final_data$Tech_Export_proportion, start = 2013, frequency = 1)

# White noise check
wn_check <- Box.test(ts_data, lag = 5, type = "Ljung-Box")
print(wn_check)

# Autocorrelation check
acf_data <- acf(ts_data, lag.max = 5)
pacf_data <- pacf(ts_data, lag.max = 5)

# AR process
ar_model <- ar(ts_data, order.max = 2)
print(ar_model)

# ARMA model creation
arma_model <- arima(ts_data, order = c(2, 0, 0))
print(arma_model)

# Model checking
check_model <- Box.test(arma_model$residuals, lag = 5, type = "Ljung-Box")
print(check_model)

# Normality tests
shapiro_test <- shapiro.test(arma_model$residuals)
print(shapiro_test)

jarque_bera_test <- jarque.bera.test(arma_model$residuals)
print(jarque_bera_test)

# Forecasting
forecast_data <- forecast(arma_model, h = 1)
print(forecast_data)
