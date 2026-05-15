#Practical NO.5: Data Visualization with ggplot2 in R 
#Questions:

library(ggplot2)
library(dplyr)

# Import dataset

sales_data <- read.csv("C:/R practicals/sales_data.csv")

# ---------------------------------------------------
# Q1) Basic ggplot Charts
# ---------------------------------------------------

# a) Scatter plot of Price vs Revenue

ggplot(sales_data, aes(x = Price, y = Revenue)) +
  geom_point()

# b) Bar chart for City

ggplot(sales_data, aes(x = City)) +
  geom_bar()

# c) Histogram of Price

ggplot(sales_data, aes(x = Price)) +
  geom_histogram(bins = 10)

# d) Boxplot of Revenue by Category

ggplot(sales_data, aes(x = Category, y = Revenue)) +
  geom_boxplot()

# e) Line chart for Revenue by OrderID

ggplot(sales_data, aes(x = OrderID, y = Revenue)) +
  geom_line()

# ---------------------------------------------------
# Q2) Customized Visualization
# ---------------------------------------------------

# a) Add title to chart

ggplot(sales_data, aes(x = City)) +
  geom_bar() +
  ggtitle("Number of Orders by City")

# b) Change axis labels

ggplot(sales_data, aes(x = Price, y = Revenue)) +
  geom_point() +
  labs(
    x = "Product Price",
    y = "Total Revenue"
  )

# c) Change color of bars

ggplot(sales_data, aes(x = City)) +
  geom_bar(fill = "skyblue")

# d) Change theme style

ggplot(sales_data, aes(x = City)) +
  geom_bar(fill = "orange") +
  theme_minimal()

# e) Rotate x-axis labels

ggplot(sales_data, aes(x = City)) +
  geom_bar(fill = "green") +
  theme(
    axis.text.x = element_text(angle = 45)
  )

# ---------------------------------------------------
# Q3) Multi-Variable Visualization
# ---------------------------------------------------

# a) Price vs Revenue colored by Category

ggplot(sales_data,
       aes(x = Price,
           y = Revenue,
           color = Category)) +
  geom_point()

# b) Price vs Quantity colored by Category

ggplot(sales_data,
       aes(x = Price,
           y = Quantity,
           color = Category)) +
  geom_point()

# c) Revenue vs Discount

ggplot(sales_data,
       aes(x = Discount,
           y = Revenue)) +
  geom_point(color = "red")

# d) Top cities by revenue

city_revenue <- sales_data %>%
  group_by(City) %>%
  summarise(TotalRevenue = sum(Revenue))

ggplot(city_revenue,
       aes(x = City,
           y = TotalRevenue,
           fill = City)) +
  geom_bar(stat = "identity")

# e) Top products by sales

product_sales <- sales_data %>%
  group_by(Product) %>%
  summarise(TotalSales = sum(Revenue))

ggplot(product_sales,
       aes(x = Product,
           y = TotalSales,
           fill = Product)) +
  geom_bar(stat = "identity")

# ---------------------------------------------------
# Q4) Bar Chart, Scatter Plot, Histogram Analysis
# ---------------------------------------------------

# a) Number of orders per city (Bar Chart)

ggplot(sales_data, aes(x = City)) +
  geom_bar(fill = "blue")

# b) Price vs Revenue (Scatter Plot)

ggplot(sales_data,
       aes(x = Price,
           y = Revenue)) +
  geom_point(color = "darkgreen")

# c) Quantity vs Revenue (Scatter Plot)

ggplot(sales_data,
       aes(x = Quantity,
           y = Revenue)) +
  geom_point(color = "purple")

# d) Distribution of Quantity (Histogram)

ggplot(sales_data,
       aes(x = Quantity)) +
  geom_histogram(bins = 10,
                 fill = "orange")

# e) Histogram by Category

ggplot(sales_data,
       aes(x = Revenue,
           fill = Category)) +
  geom_histogram(bins = 10)

# ---------------------------------------------------
# Q5) Box Plot, Line Chart, Faceted Chart Analysis
# ---------------------------------------------------

# a) Revenue by Category (Box Plot)

ggplot(sales_data,
       aes(x = Category,
           y = Revenue,
           fill = Category)) +
  geom_boxplot()

# b) Price by Product (Box Plot)

ggplot(sales_data,
       aes(x = Product,
           y = Price,
           fill = Product)) +
  geom_boxplot()

# c) Revenue trend by OrderID (Line Chart)

ggplot(sales_data,
       aes(x = OrderID,
           y = Revenue)) +
  geom_line(color = "blue")

# d) Discount trend (Line Chart)

ggplot(sales_data,
       aes(x = OrderID,
           y = Discount)) +
  geom_line(color = "red")

# e) City orders by category (Faceted Chart)

ggplot(sales_data,
       aes(x = City,
           fill = Category)) +
  geom_bar() +
  facet_wrap(~Category)
