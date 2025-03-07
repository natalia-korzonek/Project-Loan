install.packages("readxl")   
install.packages("dplyr")     
install.packages("ggplot2")   
install.packages("ggpubr")    
install.packages("magrittr")  

library(ggplot2)
library(magrittr) 
library(readxl)
library(dplyr)
library(ggpubr)

# Loading the data 
file_path <- "C:/Users/Natalia/Downloads/Dane_pożyczki (1).xlsx" 
data <- read_excel("C:/Users/Natalia/Downloads/Dane_pożyczki (1).xlsx" )

# Displaying the first few rows of data
print("First few rows of the data:")
head(data)

# Checking the structure of the data
str(data)

# Displaying unique values in the 'Month' column
unique_months <- unique(data$data_spr)
print("Unique values in the 'Month' column:")
print(unique_months)

# Filtering data for June 2024
june_data <- data %>% filter(data_spr == '06.2024')

# Displaying data for June 2024
print("Data from June 2024:")
print(june_data)

# Descriptive statistics
summary_stats <- summary(june_data$kw_pozyczki_pln)
print("Descriptive statistics for loan amounts in June 2024:")
print(summary_stats)

# Identifying outliers
Q1 <- quantile(june_data$kw_pozyczki_pln, 0.25)
Q3 <- quantile(june_data$kw_pozyczki_pln, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Outliers
outliers <- june_data %>% filter(kw_pozyczki_pln < lower_bound | kw_pozyczki_pln > upper_bound)

print("Outliers:")
print(outliers)

# Visualization of the loan amount distribution
ggplot(june_data, aes(x = kw_pozyczki_pln)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(aes(y = ..count..), color = "blue") +
  geom_vline(xintercept = lower_bound, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = upper_bound, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of loan amounts in June 2024", x = "Amount", y = "Number of loans") +
  theme_minimal()

# Summary of analysis results
if (nrow(outliers) > 0) {
  cat(sprintf("Identified %d outliers in June 2024.\n", nrow(outliers)))
} else {
  cat("No outliers identified in June 2024.\n")
}

# Splitting data into June 2024 and other months
june_data <- data %>% filter(data_spr == '06.2024')
other_months_data <- data %>% filter(data_spr != '06.2024')

# Calculating the average loan amount for June 2024 and other months
mean_june <- mean(june_data$kw_pozyczki_pln)
mean_other_months <- mean(other_months_data$kw_pozyczki_pln)

cat("Average loan amount in June 2024:", mean_june, "\n")
cat("Average loan amount in other months:", mean_other_months, "\n")

# Student's t-test comparing the average loan amounts in June 2024 with other months
t_test_result <- t.test(june_data$kw_pozyczki_pln, other_months_data$kw_pozyczki_pln)

# Displaying the results of the Student's t-test
print(t_test_result)

# Visualization of the average amount comparison - box plot
ggplot(data, aes(x = data_spr, y = kw_pozyczki_pln, color = data_spr)) +
  geom_boxplot() +
  labs(title = "Comparison of loan amounts in June 2024 and other months", 
       x = "Month", y = "Amount") +
  theme_minimal()

# The result of the Student's t-test indicates a statistically significant difference between 
# the average loan amounts in June 2024 and the other months.

# t = 17.061 – The t-Student statistic value. The higher the t-value, the greater the difference 
# between the means of the compared groups. The value of 17.061 indicates a large difference.

# df = 128.74 – Degrees of freedom. The calculated df results from using Welch's test, 
# which allows for comparing means from groups with different variances.

# p-value < 2.2e-16 – The p-value is much smaller than 0.05 (the standard significance level). 
# The difference between the means is highly statistically significant, ruling out randomness.

# The test examines the alternative hypothesis that the average loan amounts are different 
# in June 2024 and other months. The test results confirm this hypothesis, meaning the averages are not equal.

# According to the confidence interval, there is a 95% probability that the actual difference in average 
# loan amounts between June 2024 and other months ranges from 794.89 to 1003.45 PLN.

# The average loan amount in June 2024 is 3588.506, while in the other months, it is 2689.335. 
# The average for June is significantly higher than in the other months.

# The high t-value and very low p-value indicate a significant difference between the average loan 
# amounts in June 2024 and other months. This difference is substantial enough to suggest that 
# data manipulation or other irregularities may have occurred in June.

# The average loan amount in June is significantly higher than in other months (by around 900 PLN), 
# further supporting the hypothesis of possible anomalies in the data.
