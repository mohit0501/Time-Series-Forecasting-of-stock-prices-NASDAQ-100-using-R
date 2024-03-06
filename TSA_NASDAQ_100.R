# Load necessary libraries
library(tidyverse)
library(lubridate)
library(forecast)

# Step 1: Data Preparation
# Load the data

library(readr)
data <- read_csv("~/Downloads/Nasdaq 100 Historical Data (4).csv")
View(data)

# Load required libraries
library(dplyr)

# Check the structure of the dataframe
str(data)

library(lubridate)
# Convert date column to datetime object
data$Date <- as.Date(data$Date)

# Convert prices to numeric values (assuming the price columns are named 'Open', 'High', 'Low', 'Close')
data$Open <- as.numeric(data$Open)
data$High <- as.numeric(data$High)
data$Low <- as.numeric(data$Low)
data$Close <- as.numeric(data$Close)

# Check for missing values
missing_values <- colSums(is.na(data))
print(missing_values)

# Handle missing values (if any)
# For example, you can replace missing values with the mean or median of the respective column
data$Open[is.na(data$Open)] <- mean(data$Open, na.rm = TRUE)
data$High[is.na(data$High)] <- mean(data$High, na.rm = TRUE)
data$Low[is.na(data$Low)] <- mean(data$Low, na.rm = TRUE)
data$Close[is.na(data$Close)] <- mean(data$Close, na.rm = TRUE)

# Check the structure of the dataframe after preprocessing
str(data)


# 2............................................

# Load required libraries
library(dplyr)
library(ggplot2)

# Assuming 'data' is the preprocessed dataframe containing stock price data

# Summary statistics
summary_stats <- data %>%
  summarise(
    Mean = mean(Open),
    Median = median(Open),
    Std_Dev = sd(Open),
    Min = min(Open),
    Max = max(Open)
  )

print(summary_stats)
# Visualize distribution of stock prices over time (using Open price for example)
ggplot(data, aes(x = Date, y = Open)) +
  geom_line() +
  labs(title = "NASDAQ 100 Stock Prices Over Time", x = "Date", y = "Stock Price") +
  theme_minimal()

# Histogram of stock prices
ggplot(data, aes(x = Open)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of NASDAQ 100 Stock Prices", x = "Stock Price", y = "Frequency") +
  theme_minimal()

# Kernel density plot of stock prices
ggplot(data, aes(x = Open)) +
  geom_density(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Kernel Density Plot of NASDAQ 100 Stock Prices", x = "Stock Price", y = "Density") +
  theme_minimal()

# Identify outliers
# One common method is to use boxplots to visualize outliers
ggplot(data, aes(y = Open)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Boxplot of NASDAQ 100 Stock Prices", y = "Stock Price") +
  theme_minimal()

