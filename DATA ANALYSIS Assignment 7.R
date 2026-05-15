#Practical NO.7: Correlation and Probability Distributions and 
#Performing Univariate Analyses
#Questions
# Load required package

library(dplyr)

# ---------------------------------------------------
# Import Dataset
# ---------------------------------------------------

customer_data <- read.csv("customer_data.csv")

# ---------------------------------------------------
# Add Required Columns
# (Because questions use Age, Income, SpendingScore)
# ---------------------------------------------------

set.seed(123)

customer_data$Age <- sample(18:60, 100, replace = TRUE)

customer_data$Income <- sample(30000:100000, 100, replace = TRUE)

customer_data$SpendingScore <- sample(1:100, 100, replace = TRUE)

# Convert Purchase into Binary
# Yes = 1, No = 0

customer_data$PurchaseBinary <- ifelse(
  customer_data$Purchase == "Yes",
  1,
  0
)

# ===================================================
# Q1) Retail Income vs Spending Strategy
# ===================================================

# 1. Pearson correlation

cor(customer_data$Income,
    customer_data$SpendingScore,
    method = "pearson")

# 2. Significance test of correlation

cor.test(customer_data$Income,
         customer_data$SpendingScore)

# 3. Generate normal distribution using Income

income_normal <- rnorm(
  100,
  mean(customer_data$Income),
  sd(customer_data$Income)
)

# 4. One-sample t-test

t.test(customer_data$Income,
       mu = 50000)

# 5. Binomial test

binom.test(
  sum(customer_data$PurchaseBinary),
  nrow(customer_data),
  p = 0.50
)

# ===================================================
# Q2) Customer Segmentation Strategy
# ===================================================

# 1. Correlation matrix

cor_matrix <- cor(
  customer_data[, c("Age",
                    "Income",
                    "SpendingScore")]
)

print(cor_matrix)

# 2. Strongest relationship

cor_matrix

# 3. Normal distribution for SpendingScore

spending_normal <- rnorm(
  100,
  mean(customer_data$SpendingScore),
  sd(customer_data$SpendingScore)
)

# 4. T-test for SpendingScore = 60

t.test(customer_data$SpendingScore,
       mu = 60)

# 5. Kruskal-Wallis test

kruskal.test(
  SpendingScore ~ Region,
  data = customer_data
)

# ===================================================
# Q3) Financial Risk Assessment
# ===================================================

# 1. Correlation between Income and SpendingScore

cor(customer_data$Income,
    customer_data$SpendingScore)

# 2. Statistical significance

cor.test(customer_data$Income,
         customer_data$SpendingScore)

# 3. Poisson distribution

risky_customers <- rpois(100, lambda = 5)

# 4. T-test on Income = 40000

t.test(customer_data$Income,
       mu = 40000)

# 5. Binomial test assuming 60% safe customers

binom.test(
  sum(customer_data$PurchaseBinary),
  nrow(customer_data),
  p = 0.60
)

# ===================================================
# Q4) Product Demand Forecasting
# ===================================================

# 1. Spearman correlation

cor(customer_data$Age,
    customer_data$Income,
    method = "spearman")

# 2. Monotonic relationship explanation
# Positive value = positive monotonic relationship
# Negative value = negative monotonic relationship

# 3. Binomial distribution

purchase_events <- rbinom(
  100,
  size = 10,
  prob = 0.5
)

# 4. T-test on Age = 35

t.test(customer_data$Age,
       mu = 35)

# 5. Chi-square goodness-of-fit

region_table <- table(customer_data$Region)

chisq.test(region_table)

# ===================================================
# Q5) Customer Satisfaction Modeling
# ===================================================

# 1. Pearson correlation

cor(customer_data$Income,
    customer_data$SpendingScore,
    method = "pearson")

# 2. Spearman correlation

cor(customer_data$Income,
    customer_data$SpendingScore,
    method = "spearman")

# 3. Normal distribution for SpendingScore

satisfaction_normal <- rnorm(
  100,
  mean(customer_data$SpendingScore),
  sd(customer_data$SpendingScore)
)

# 4. T-test for satisfaction = 55

t.test(customer_data$SpendingScore,
       mu = 55)

# 5. Wilcoxon test

wilcox.test(customer_data$Age,
            customer_data$SpendingScore)

# ===================================================
# Q6) Sales Probability Analysis
# ===================================================

# 1. Correlation between PurchaseBinary and Income

cor(customer_data$PurchaseBinary,
    customer_data$Income)

# 2. Binomial distribution for purchase trials

purchase_trials <- rbinom(
  100,
  size = 20,
  prob = 0.5
)

# 3. Binomial test

binom.test(
  sum(customer_data$PurchaseBinary),
  nrow(customer_data),
  p = 0.5
)

# 4. T-test on Income = 55000

t.test(customer_data$Income,
       mu = 55000)

# 5. Chi-square test for purchase distribution

purchase_table <- table(customer_data$Purchase)

chisq.test(purchase_table)

# ===================================================
# Q7) Strategic Business Decision Model
# ===================================================

# 1. Correlation matrix among numeric variables

numeric_data <- customer_data[, c(
  "Age",
  "Income",
  "SpendingScore",
  "Amount",
  "PurchaseBinary"
)]

cor(numeric_data)

# 2. Strongest predictor of SpendingScore

cor(numeric_data)

# 3. Poisson distribution for simulated demand

simulated_demand <- rpois(
  100,
  lambda = 10
)

# 4. T-test on Income benchmark = 60000

t.test(customer_data$Income,
       mu = 60000)

# 5. Kruskal-Wallis test for Age across regions

kruskal.test(
  Age ~ Region,
  data = customer_data
)
