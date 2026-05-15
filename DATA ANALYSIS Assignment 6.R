#Practical NO.6: Data Aggregation, Cross-Tabulation, Exploring 
#Questions
# Load required package

library(dplyr)

# ---------------------------------------------------
# Creating CSV File
# ---------------------------------------------------

set.seed(123)

data <- data.frame(
  CustomerID = 1:100,
  Gender = sample(c("Male", "Female"), 100, replace = TRUE),
  Region = sample(c("North", "South", "East", "West"), 100, replace = TRUE),
  Purchase = sample(c("Yes", "No"), 100, replace = TRUE),
  Amount = sample(1000:10000, 100, replace = TRUE)
)

write.csv(data,
          "customer_data.csv",
          row.names = FALSE)

# Import CSV file

customer_data <- read.csv("customer_data.csv")

# ---------------------------------------------------
# Q1) Sales Performance Analysis
# ---------------------------------------------------

# a) Total sales by region

total_sales_region <- aggregate(
  Amount ~ Region,
  data = customer_data,
  sum
)

# b) Average sales per gender

avg_sales_gender <- aggregate(
  Amount ~ Gender,
  data = customer_data,
  mean
)

# c) Frequency distribution of purchase

purchase_frequency <- table(customer_data$Purchase)

# d) Cross-tab: Region vs Purchase

region_purchase_table <- table(
  customer_data$Region,
  customer_data$Purchase
)

# e) Chi-square test

chisq.test(region_purchase_table)

# ---------------------------------------------------
# Q2) Customer Segmentation
# ---------------------------------------------------

# a) Count customers by region

customer_count_region <- table(customer_data$Region)

# b) Average purchase by region using tapply

avg_purchase_region <- tapply(
  customer_data$Amount,
  customer_data$Region,
  mean
)

# c) Frequency of gender

gender_frequency <- table(customer_data$Gender)

# d) Cross-tab: Gender vs Region

gender_region_table <- table(
  customer_data$Gender,
  customer_data$Region
)

# e) Chi-square test

chisq.test(gender_region_table)

# ---------------------------------------------------
# Q3) Purchase Pattern Analysis
# ---------------------------------------------------

# a) Total purchase amount

total_purchase_amount <- sum(customer_data$Amount)

# b) Row-wise sum using apply

row_sum <- apply(
  customer_data["Amount"],
  1,
  sum
)

# c) Frequency of purchase

purchase_freq <- table(customer_data$Purchase)

# d) Cross-tab: Purchase vs Region

purchase_region_table <- table(
  customer_data$Purchase,
  customer_data$Region
)

# e) Chi-square test

chisq.test(purchase_region_table)

# ---------------------------------------------------
# Q4) High-Value Customer Analysis
# ---------------------------------------------------

# a) Filter customers with Amount > 5000

high_value_customers <- subset(
  customer_data,
  Amount > 5000
)

# b) Mean purchase of filtered group

mean_purchase_filtered <- mean(
  high_value_customers$Amount
)

# c) Frequency of region (filtered)

region_frequency_filtered <- table(
  high_value_customers$Region
)

# d) Cross-tab: Gender vs Purchase (filtered)

gender_purchase_filtered <- table(
  high_value_customers$Gender,
  high_value_customers$Purchase
)

# e) Chi-square test

chisq.test(gender_purchase_filtered)

# ---------------------------------------------------
# Q5) Business Decision Analysis
# ---------------------------------------------------

# a) Total revenue

total_revenue <- sum(customer_data$Amount)

# b) Average revenue per region

avg_revenue_region <- aggregate(
  Amount ~ Region,
  data = customer_data,
  mean
)

# c) Frequency distribution of regions

region_frequency <- table(customer_data$Region)

# d) Cross-tab: Region vs Purchase

region_purchase_cross <- table(
  customer_data$Region,
  customer_data$Purchase
)

# e) Statistical inference (Chi-square test)

chisq_test_result <- chisq.test(region_purchase_cross)

print(chisq_test_result)
