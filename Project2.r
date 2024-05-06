# List of packages
packages <- c("readxl", "ggplot2", "dplyr")

# Check if packages are installed
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

# Install uninstalled packages
if(length(new_packages)) install.packages(new_packages)

# Load the packages
library(readxl)     #read xlsx
library(ggplot2)    #plotting
library(dplyr)

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

# Generate descriptive statistics
summary(data)

# Quantile-based detection of outliers
    Q1 <- quantile(data$Tech_Exports, 0.25)
    Q3 <- quantile(data$Tech_Exports, 0.75)
    IQR <- Q3 - Q1 
    
    # Define lower and upper bounds for outliers
    lower_bound <- Q1 - 1.5 * IQR 
    upper_bound <- Q3 + 1.5 * IQR 
    # Identify outliers
    outliers <- data$Tech_Exports < lower_bound | data$Tech_Exports > upper_bound  
    # Remove outliers
    clean_data <- data[!outliers, ]
    print (clean_data,n=100)

############################################################################################################
# interpret data 
    # Count the observation by year
    # Filter the data for year 2016
    data_2016 <- filter(clean_data, Year == 2016)
    print(data_2016, n=300)
    summary(data_2016)

    # Scatter plot of Tech_Exports vs R&D with smooth line
    ggplot(data_2016, aes(x=`R&D`, y=Tech_Exports)) +
        geom_point(aes(color = Education)) +  # Color points by Education
        geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +  # Add a smooth line
        labs(title="Scatterplot of Tech_Exports vs R&D", x="R&D", y="Tech_Exports") +
        theme_minimal()  # Use a minimal theme

    # Scatter plot of Tech_Exports vs Education with smooth line
    ggplot(data_2016, aes(x=Education, y=Tech_Exports)) +
        geom_point(aes(color = `R&D`)) +  # Color points by R&D
        geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +  # Add a smooth line
        labs(title="Scatterplot of Tech_Exports vs Education", x="Education", y="Tech_Exports") +
        theme_minimal()  # Use a minimal theme
 

