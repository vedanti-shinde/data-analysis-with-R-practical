# Q1 

df <- data.frame(
  name = c("Raj", " Amit", "Neha ", "raj", "Amit"),
  marks = c("80", "90", NA, "85", "90"),
  city = c("Pune", " pune", "Mumbai", "PUNE", "Mumbai")
)

# 1. Remove extra spaces from name column


df$name <- trimws(df$name)

cat("\nNames after removing spaces:\n")
print(df$name)

# 2. Convert all names to lowercase

df$name <- tolower(df$name)

cat("\nNames in lowercase:\n")
print(df$name)

# 3. Convert marks column to numeric and handle missing values

df$marks <- as.numeric(df$marks)

# Replace NA with mean marks
df$marks[is.na(df$marks)] <- mean(df$marks, na.rm = TRUE)

cat("\nMarks after handling missing values:\n")
print(df$marks)


# 4. Standardize city names (lowercase)


df$city <- tolower(trimws(df$city))

cat("\nStandardized City Names:\n")
print(df$city)


# 5. Filter students with marks > 80

high_marks <- subset(df, marks > 80)

cat("\nStudents with Marks > 80:\n")
print(high_marks)

# 6. Create Grade Column


df$grade <- ifelse(df$marks >= 85, "A", "B")

cat("\nFinal Data Frame with Grade:\n")
print(df)
#______________________________________________________________________
# Q2
set.seed(100)

sale <- data.frame(
  product = sample(c("Laptop","Mobile","Tablet"), 50, replace = TRUE),
  city = sample(c("Pune","Mumbai","Delhi"), 50, replace = TRUE),
  quantity = sample(1:5, 50, replace = TRUE),
  price = sample(seq(10000, 50000, 5000), 50, replace = TRUE)
)

# Create Revenue column
sale$Revenue <- sale$quantity * sale$price


# 1. Show records where Revenue > 100000

high_revenue <- subset(sale, Revenue > 100000)

cat("Records where Revenue > 100000:\n")
print(high_revenue)

#-----------------------------------
# 2. Sort data by Revenue in descending order
#-----------------------------------

sorted_data <- sale[order(-sale$Revenue), ]

cat("\nData sorted by Revenue (Descending):\n")
print(sorted_data)


# 3. Find total revenue by city

total_revenue_city <- aggregate(Revenue ~ city, data = sale, sum)

cat("\nTotal Revenue by City:\n")
print(total_revenue_city)


# 4. Find average price by product

avg_price_product <- aggregate(price ~ product, data = sale, mean)

cat("\nAverage Price by Product:\n")
print(avg_price_product)


# 5. Identify the product with highest total revenue
product_revenue <- aggregate(Revenue ~ product, data = sale, sum)

highest_product <- product_revenue[
  which.max(product_revenue$Revenue), ]

cat("\nProduct with Highest Total Revenue:\n")
print(highest_product)
