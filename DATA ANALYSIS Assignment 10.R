# =========================================================
# Comprehensive Data Analysis Using R Programming
# Dataset: Titanic Dataset from Kaggle
# =========================================================

# ---------------------------
# 1. Install Required Packages
# ---------------------------

install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")

# ---------------------------
# 2. Load Libraries
# ---------------------------

library(readr)
library(dplyr)
library(ggplot2)

# ---------------------------
# 3. Import Dataset
# ---------------------------

# Read Titanic dataset
titanic <- read.csv("titanic.csv")

# Display first 10 records
head(titanic, 10)

# Display structure of dataset
str(titanic)

# Display summary statistics
summary(titanic)

# =========================================================
# 4. Data Cleaning and Preprocessing
# =========================================================

# Check missing values in each column
colSums(is.na(titanic))

# Fill missing Age values using median
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)

# Fill missing Embarked values using mode
mode_embarked <- names(sort(table(titanic$Embarked), decreasing = TRUE))[1]
titanic$Embarked[is.na(titanic$Embarked)] <- mode_embarked

# Remove Cabin column because it contains too many missing values
titanic <- titanic %>% select(-Cabin)

# Convert categorical variables into factors
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)
titanic$Pclass <- as.factor(titanic$Pclass)

# Verify cleaned dataset
summary(titanic)

# =========================================================
# 5. Exploratory Data Analysis (EDA)
# =========================================================

# ---------------------------
# Survival Count Plot
# ---------------------------

ggplot(titanic, aes(x = factor(Survived))) +
  geom_bar(fill = "skyblue") +
  labs(
    title = "Survival Count",
    x = "Survived (0 = No, 1 = Yes)",
    y = "Number of Passengers"
  )

# ---------------------------------------------------------
# Interpretation:
# More passengers died compared to those who survived.
# ---------------------------------------------------------


# ---------------------------
# Gender-wise Survival
# ---------------------------

ggplot(titanic, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Gender-wise Survival",
    x = "Gender",
    y = "Count",
    fill = "Survived"
  )

# ---------------------------------------------------------
# Interpretation:
# Female passengers had much higher survival rates
# than male passengers.
# ---------------------------------------------------------


# ---------------------------
# Age Distribution Histogram
# ---------------------------

ggplot(titanic, aes(x = Age)) +
  geom_histogram(
    binwidth = 5,
    fill = "orange",
    color = "black"
  ) +
  labs(
    title = "Age Distribution of Passengers",
    x = "Age",
    y = "Frequency"
  )

# ---------------------------------------------------------
# Interpretation:
# Most passengers belonged to the age group
# between 20 and 40 years.
# ---------------------------------------------------------


# ---------------------------
# Passenger Class vs Survival
# ---------------------------

ggplot(titanic, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Passenger Class vs Survival",
    x = "Passenger Class",
    y = "Count",
    fill = "Survived"
  )

# ---------------------------------------------------------
# Interpretation:
# First-class passengers had better survival chances
# compared to second and third-class passengers.
# ---------------------------------------------------------


# ---------------------------
# Fare Distribution by Survival
# ---------------------------

ggplot(titanic, aes(x = factor(Survived), y = Fare)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Fare Distribution by Survival",
    x = "Survived",
    y = "Fare"
  )

# ---------------------------------------------------------
# Interpretation:
# Passengers paying higher fares were more likely
# to survive.
# ---------------------------------------------------------


# ---------------------------
# Pie Chart of Embarked Passengers
# ---------------------------

embarked_count <- table(titanic$Embarked)

pie(
  embarked_count,
  main = "Embarked Passenger Distribution",
  col = rainbow(length(embarked_count))
)

# ---------------------------------------------------------
# Interpretation:
# Most passengers boarded from Southampton.
# ---------------------------------------------------------


# =========================================================
# 6. Statistical and Analytical Methods
# =========================================================

# ---------------------------
# Correlation Analysis
# ---------------------------

numeric_data <- titanic %>%
  select(Age, Fare, SibSp, Parch)

correlation_matrix <- cor(numeric_data)

print(correlation_matrix)

# ---------------------------------------------------------
# Interpretation:
# Fare showed some relationship with passenger status,
# while Age had weaker correlation with other variables.
# ---------------------------------------------------------


# ---------------------------
# Survival Rate by Passenger Class
# ---------------------------

survival_rate <- titanic %>%
  group_by(Pclass) %>%
  summarise(
    Survival_Rate = mean(Survived)
  )

print(survival_rate)

# ---------------------------------------------------------
# Interpretation:
# Higher passenger classes had greater survival rates.
# ---------------------------------------------------------


# ---------------------------
# Hypothesis Testing
# ---------------------------

# H0: Gender and survival are independent
# H1: Gender and survival are related

chi_test <- chisq.test(
  table(titanic$Sex, titanic$Survived)
)

print(chi_test)

# ---------------------------------------------------------
# Interpretation:
# If p-value < 0.05, reject H0.
# This means gender significantly affected survival.
# ---------------------------------------------------------


# =========================================================
# 7. Findings
# =========================================================

# ---------------------------------------------------------
# Findings:
#
# 1. Female passengers had significantly higher
#    survival rates than male passengers.
#
# 2. First-class passengers had better survival
#    chances compared to lower classes.
#
# 3. Fare amount influenced survival probability.
#
# 4. Most passengers were between 20 and 40 years old.
#
# 5. Passenger class and gender were important
#    factors affecting survival.
# ---------------------------------------------------------


# =========================================================
# 8. Conclusion
# =========================================================

# ---------------------------------------------------------
# Conclusion:
#
# The Titanic dataset analysis using R programming
# successfully demonstrated data cleaning,
# preprocessing, visualization, and statistical analysis.
#
# The study identified important factors influencing
# survival such as gender, passenger class, and fare.
#
# Female and first-class passengers had higher
# survival probabilities.
#
# This project shows how R programming can be used
# effectively for analyzing real-world datasets and
# deriving meaningful insights using statistical
# and graphical techniques.
# ---------------------------------------------------------