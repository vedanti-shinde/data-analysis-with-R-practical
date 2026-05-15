#Practical NO.8: Working with CSV and Excel Files in R  
#Importing Data from CSV
#Questions
# Load required packages

library(readxl)
library(dplyr)

# ===================================================
# Q1) Retail Sales Analysis
# Dataset: sales
# ===================================================

# Load dataset

sales_data <- read_excel(
  "C:/R practicals/data.xlsx"
)

# a) Display first 10 records

head(sales_data, 10)

# b) Total revenue for each product

total_revenue_product <- sales_data %>%
  group_by(Product) %>%
  summarise(
    TotalRevenue = sum(Revenue)
  )

# c) Top 5 products with highest revenue

top5_products <- sales_data %>%
  group_by(Product) %>%
  summarise(
    TotalRevenue = sum(Revenue)
  ) %>%
  arrange(desc(TotalRevenue)) %>%
  head(5)

# d) Filter sales where quantity sold is greater than 50

high_quantity_sales <- sales_data %>%
  filter(Quantity > 50)

# e) Find average product price by category

avg_price_category <- sales_data %>%
  group_by(Category) %>%
  summarise(
    AveragePrice = mean(Price)
  )

# ===================================================
# Q2) E-Commerce Customer Analysis
# Dataset: customers
# ===================================================

customers_data <- read_excel(
  "C:/R practicals/data.xlsx"
)

# a) Display first 10 records

head(customers_data, 10)

# b) Calculate average purchase amount

mean(customers_data$PurchaseAmount)

# c) Find customers above average purchase amount

above_avg_customers <- customers_data %>%
  filter(
    PurchaseAmount > mean(PurchaseAmount)
  )

# d) Count number of male and female customers

table(customers_data$Gender)

# e) Create age groups

customers_data$AgeGroup <- ifelse(
  customers_data$Age < 25,
  "Youth",
  ifelse(
    customers_data$Age <= 50,
    "Adult",
    "Senior"
  )
)

# f) Find top 10 highest spending customers

top10_customers <- customers_data %>%
  arrange(desc(PurchaseAmount)) %>%
  head(10)

# ===================================================
# Q3) Banking Transaction Analysis
# Dataset: transactions
# ===================================================

transactions_data <- read_excel(
  "C:/R practicals/data.xlsx",
  sheet = "transactions"
)

# a) Display first 10 records

head(transactions_data, 10)

# b) Calculate total deposits

total_deposits <- transactions_data %>%
  filter(Type == "Deposit") %>%
  summarise(
    TotalDeposit = sum(Amount)
  )

# c) Calculate total withdrawals

total_withdrawals <- transactions_data %>%
  filter(Type == "Withdrawal") %>%
  summarise(
    TotalWithdrawal = sum(Amount)
  )

# d) Find accounts with transactions above 10000

high_transactions <- transactions_data %>%
  filter(Amount > 10000)

# e) Calculate average transaction amount

mean(transactions_data$Amount)

# f) Count number of transactions by type

table(transactions_data$Type)

# ===================================================
# Q4) Healthcare Patient Dataset
# Dataset: patients
# ===================================================

patients_data <- read_excel(
  "C:/R practicals/data.xlsx",
  sheet = "patients"
)

# a) Display first 10 records

head(patients_data, 10)

# b) Find patients with high blood pressure (>140)

high_bp_patients <- patients_data %>%
  filter(BloodPressure > 140)

# c) Detect patients with fever (>37°C)

fever_patients <- patients_data %>%
  filter(Temperature > 37)

# d) Calculate average patient age

mean(patients_data$Age)

# e) Find maximum and minimum blood pressure

max(patients_data$BloodPressure)

min(patients_data$BloodPressure)

# f) Count number of patients above 60 years

patients_above60 <- patients_data %>%
  filter(Age > 60)

nrow(patients_above60)

# ===================================================
# Q5) Student Performance Dataset
# Dataset: students
# ===================================================

students_data <- read_excel(
  "C:/R practicals/data.xlsx",
  sheet = "students"
)

# a) Display first 10 records

head(students_data, 10)

# b) Find students who scored above 80

students_above80 <- students_data %>%
  filter(Marks > 80)

# c) Calculate average marks by subject

avg_marks_subject <- students_data %>%
  group_by(Subject) %>%
  summarise(
    AverageMarks = mean(Marks)
  )

# d) Find top scoring student

top_student <- students_data %>%
  arrange(desc(Marks)) %>%
  head(1)

# e) Find students who failed (Marks < 40)

failed_students <- students_data %>%
  filter(Marks < 40)

# f) Count number of students in each subject

table(students_data$Subject)

# ===================================================
# Q6) Social Media Analytics Dataset
# Dataset: posts
# ===================================================

posts_data <- read_excel(
  "C:/R practicals/data.xlsx",
  sheet = "posts"
)

# a) Display first 10 records

head(posts_data, 10)

# b) Calculate total engagement per post

posts_data$TotalEngagement <-
  posts_data$Likes +
  posts_data$Comments +
  posts_data$Shares

# c) Find posts with engagement > 500

high_engagement_posts <- posts_data %>%
  filter(TotalEngagement > 500)

# d) Find most liked post

most_liked_post <- posts_data %>%
  arrange(desc(Likes)) %>%
  head(1)

# e) Calculate average engagement

mean(posts_data$TotalEngagement)

# f) Find posts with low engagement (<100)

low_engagement_posts <- posts_data %>%
  filter(TotalEngagement < 100)
