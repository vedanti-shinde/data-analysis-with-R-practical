#Practical 4 - Data Management and Manipulation in R 

#Questions:
library(readxl)
library(writexl)
library(dplyr)  
# ---------------------------------------------------
# Q1) Importing and Exploring Data
# ---------------------------------------------------

# a) Import dataset from CSV file and Excel file

sales_data <- read.csv("C:/R practicals/sales_data.csv")

sales_data_Excel <- read_excel("C:/R practicals/export_sales_data.xlsx")

# b) Display first 10 rows

head(sales_data, 10)

# c) Display last 10 rows

tail(sales_data, 10)

# d) Check structure of dataset

str(sales_data)

# e) Display summary statistics

summary(sales_data)

# ---------------------------------------------------
# Q2) Exporting Data
# ---------------------------------------------------

# a) Export dataset to CSV file

write.csv(
  sales_data,
  "C:/R practicals/export_sales_data.csv",
  row.names = FALSE
)

# b) Export dataset to Excel file

write_xlsx(
  sales_data,
  "C:/R practicals/export_sales_data.xlsx"
)

# c) Export only Electronics category

electronics_data <- subset(
  sales_data,
  Category == "Electronics"
)

write.csv(
  electronics_data,
  "C:/R practicals/electronics_data.csv",
  row.names = FALSE
)

# d) Export customers from Pune

pune_customers <- subset(
  sales_data,
  City == "Pune"
)

write.csv(
  pune_customers,
  "C:/R practicals/pune_customers.csv",
  row.names = FALSE
)

# e) Export top 20 rows

top20 <- head(sales_data, 20)
write.csv(
  top20,
  "C:/R practicals/top20_rows.csv",
  row.names = FALSE
)

# ---------------------------------------------------
# Q3) Modifying Data and Subset Creation
# ---------------------------------------------------

# 1. Create Profit column
# Example: Profit = 20% of NetAmount

sales_data$Profit <- sales_data$FinalAmount * 0.20

# 2. Rename NetAmount column to NetAmount

names(sales_data)[names(sales_data) == "FinalAmount"] <- "NetAmount"

# 3. Remove DiscountAmount column
# (Only if column exists)

sales_data$DiscountAmount <- NULL

# 4. Add Tax column
# Example: 18% tax

sales_data$Tax <- sales_data$NetAmount * 0.18

# 5. Select orders with Quantity > 5

high_quantity_orders <- subset(
  sales_data,
  Quantity > 5
)

# ---------------------------------------------------
# Q4) Column Selection
# ---------------------------------------------------

# 1. Select CustomerName and Product columns

sales_data[, c("CustomerName", "Product")]

# 2. Select first 5 columns

sales_data[, 1:5]

# 3. Select numeric columns

sales_data[, sapply(sales_data, is.numeric)]

# 4. Remove Discount column

sales_data_no_discount <- sales_data[, !(names(sales_data) %in% "Discount")]

# 5. Select specific columns using index

sales_data[, c(2, 4, 6)]

# ---------------------------------------------------
# Q5) Sorting Data
# ---------------------------------------------------

# 1. Sort by City alphabetically

sort_city <- sales_data[order(sales_data$City), ]

# 2. Sort by City and Revenue

sort_city_revenue <- sales_data[
  order(sales_data$City, sales_data$Revenue),
]

# 3. Average price by city

avg_price_city <- aggregate(
  Price ~ City,
  data = sales_data,
  mean
)

# 4. Maximum order value by city

max_order_city <- aggregate(
  NetAmount ~ City,
  data = sales_data,
  max
)

# 5. Total quantity sold by city

total_quantity_city <- aggregate(
  Quantity ~ City,
  data = sales_data,
  sum
)
