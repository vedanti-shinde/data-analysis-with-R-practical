#PRACTICAL NO.3: Advanced Data Manipulation Techniques in R Recoding Variables
#Dataset
set.seed(123)
sales_data <- data.frame(
  OrderID = 1:200,
  CustomerName =
  sample(c("Rahul","Amit","Priya","Neha","Arjun","Sneha","Rohit","Kiran"),200,replace=TRUE),
  City = sample(c("Pune","Mumbai","Delhi","Bangalore","Hyderabad"),200,replace=TRUE),
  Product = sample(c("Laptop","Mobile","Tablet","Shoes","Watch"),200,replace=TRUE),
  Category = sample(c("Electronics","Fashion"),200,replace=TRUE),
  Quantity = sample(1:10,200,replace=TRUE),
  Price = sample(seq(500,50000,500),200,replace=TRUE),
  Discount = sample(c(0,5,10,15,20),200,replace=TRUE),
  PaymentMethod = sample(c("Cash","Card","UPI"),200,replace=TRUE)
)
sales_data$Revenue <- sales_data$Quantity * sales_data$Price
sales_data$DiscountAmount <- sales_data$Revenue * sales_data$Discount/100
sales_data$FinalAmount <- sales_data$Revenue - sales_data$DiscountAmount
sales_data 
library(dplyr)
# ---------------------------------------------------
# Q1) Problems on Recoding Variables, Sorting and New Variable Creation
# ---------------------------------------------------

# a) Create OrderSize (Small, Medium, Large)

sales_data$OrderSize <- ifelse(
  sales_data$Quantity <= 3, "Small",
  ifelse(sales_data$Quantity <= 7, "Medium", "Large")
)

# b) Sort by Quantity descending

sort_quantity <- sales_data %>%
  arrange(desc(Quantity))

# c) Sort by City and Revenue

sort_city_revenue <- sales_data %>%
  arrange(City, desc(Revenue))

# d) Create Tax column (Example: 18% tax)

sales_data$Tax <- sales_data$FinalAmount * 0.18

# e) Create TotalBill column

sales_data$TotalBill <- sales_data$FinalAmount + sales_data$Tax

# ---------------------------------------------------
# Q2) Problems on dplyr select() and filter()
# ---------------------------------------------------

# a) Select CustomerName and Product

select_columns <- sales_data %>%
  select(CustomerName, Product)

# b) Select numeric columns

numeric_columns <- sales_data %>%
  select(where(is.numeric))

# c) Remove Discount column

remove_discount <- sales_data %>%
  select(-Discount)

# d) Filter Pune customers

pune_customers <- sales_data %>%
  filter(City == "Pune")

# e) Filter multiple conditions

filtered_data <- sales_data %>%
  filter(Price > 20000 & Quantity > 5)

# ---------------------------------------------------
# Q3) Problem on mutate() and arrange()
# ---------------------------------------------------

mutate_data <- sales_data %>%
  mutate(
    
    # a) Create Tax variable
    Tax = FinalAmount * 0.18,
    
    # b) Create Profit variable (Example: 20% profit)
    Profit = FinalAmount * 0.20,
    
    # c) Create OrderCategory
    OrderCategory = ifelse(
      Quantity <= 3, "Low",
      ifelse(Quantity <= 7, "Medium", "High")
    ),
    
    # d) Create DiscountCategory
    DiscountCategory = ifelse(
      Discount == 0, "No Discount",
      ifelse(Discount <= 10, "Low Discount", "High Discount")
    ),
    
    # e) Create PaymentType variable
    PaymentType = ifelse(
      PaymentMethod == "Cash", "Offline", "Online"
    )
    
  )

# f) Sort by Revenue descending

sorted_revenue <- sales_data %>%
  arrange(desc(Revenue))

# ---------------------------------------------------
# Q4) Using group_by() and summarise()
# ---------------------------------------------------

# a) Total revenue by city

revenue_by_city <- sales_data %>%
  group_by(City) %>%
  summarise(TotalRevenue = sum(Revenue))

# b) Average price by product

avg_price_product <- sales_data %>%
  group_by(Product) %>%
  summarise(AveragePrice = mean(Price))

# c) Total quantity sold by product

quantity_product <- sales_data %>%
  group_by(Product) %>%
  summarise(TotalQuantity = sum(Quantity))

# d) Maximum revenue by city

max_revenue_city <- sales_data %>%
  group_by(City) %>%
  summarise(MaxRevenue = max(Revenue))

# e) Minimum revenue by product

min_revenue_product <- sales_data %>%
  group_by(Product) %>%
  summarise(MinRevenue = min(Revenue))

# ---------------------------------------------------
# Q5) Customer Analysis
# ---------------------------------------------------

# 1. Count orders per customer

orders_customer <- sales_data %>%
  group_by(CustomerName) %>%
  summarise(OrderCount = n())

# 2. Total revenue per customer

revenue_customer <- sales_data %>%
  group_by(CustomerName) %>%
  summarise(TotalRevenue = sum(Revenue))

# 3. Average order value

avg_order_value <- sales_data %>%
  group_by(CustomerName) %>%
  summarise(AverageOrderValue = mean(FinalAmount))

# 4. Top customer

top_customer <- sales_data %>%
  group_by(CustomerName) %>%
  summarise(TotalRevenue = sum(Revenue)) %>%
  arrange(desc(TotalRevenue)) %>%
  slice(1)

# 5. Sort customers by revenue

customer_sorted <- sales_data %>%
  group_by(CustomerName) %>%
  summarise(TotalRevenue = sum(Revenue)) %>%
  arrange(desc(TotalRevenue))
