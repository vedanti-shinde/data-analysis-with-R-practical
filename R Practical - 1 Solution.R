# Practical 1 Solution - Introduction to R and Basic Operations


# Q1) Write an R program that: 
#     1.	Takes two numeric values from user. 
#     2.	Performs all arithmetic operations (+, −, *, /, power, modulus). 
#     3.	Stores the results in a vector. 
#     4.	Displays the maximum, minimum, and average value of the results. 

# Solution :

# First Number taken from User
a <- as.numeric(readline(prompt = "Enter a First number : "))

# Second Number taken from User
b <- as.numeric(readline(prompt = "Enter a second number : "))

# Arithmetic operations
add <- a + b
sub <- a - b
mul <- a * b
div <- a / b
power <- a ^ b
mod <- a %% b

# Store results in vector
results <- c(add, sub, mul, div, power, mod)

print(results)

# Numeric analysis
max_value <- max(results)
min_value <- min(results)
avg_value <- mean(results)

print(max_value)
print(min_value)
print(avg_value)


# Q2) Create an R program that:
#     1. Generates 10 random numbers between 1 and 100.
#     2. Calculates square root and then rounded value, ceiling, and floor for each square root value.
#     3. Stores results in a data frame.

# Solution :

# Generate random numbers
numbers <- sample(1:100, 10)

# Numeric functions
sqrt_values <- sqrt(numbers)
rounded_values <- round(sqrt_values)
ceiling_values <- ceiling(sqrt_values)
floor_values <- floor(sqrt_values)

# Create data frame
result_df <- data.frame(
  Numbers = numbers,
  SquareRoot = sqrt_values,
  Rounded = rounded_values,
  Ceiling = ceiling_values,
  Floor = floor_values
)

print(result_df)


# Q3) Write an R program that:
#     1. Takes a list of names.
#     2. Converts all names to uppercase.
#     3. Calculates the number of characters in each name.
#     4. Combines the name and its length into a formatted sentence.

# Solution :

names_list <- c("Rahul", "Sachin", "Amit", "Priya")

# Convert to uppercase
upper_names <- toupper(names_list)

# Count characters
name_length <- nchar(upper_names)

# Combine into sentence
result <- paste(upper_names, "has", name_length, "characters")

print(result)


# Q4) Write an R program that:
#     1. Creates a vector of numbers from 1 to 200.
#     2. Finds numbers that are divisible by 3 and 5.
#     3. Calculates square root and square of those numbers.
#     4. Displays results in a table format.

# Solution :

numbers <- 1:200

# Numbers divisible by 3 and 5
selected <- numbers[numbers %% 3 == 0 & numbers %% 5 == 0]

# Square and square root
square <- selected^2
square_root <- sqrt(selected)

# Create table
result_table <- data.frame(
  Number = selected,
  Square = square,
  SquareRoot = square_root
)

print(result_table)



# Q5) Write an R program to evaluate a quadratic equation 
#     for given a, b, c values and return real or complex roots.

# Formula : 
# For ax² + bx + c = 0
# Discriminant: D = b² − 4ac
# Roots: x = (-b ± √D) / 2a

# Solution :

a <- 1
b <- 4
c <- 5

D <- b^2 - 4*a*c   # Discriminant

root1 <- (-b + sqrt(as.complex(D))) / (2*a)
root2 <- (-b - sqrt(as.complex(D))) / (2*a)

cat("Root 1:", root1, "\n")
cat("Root 2:", root2, "\n")

# Q6) Generate numbers from 1 to 200. 
# Find number:
# Divisible by 3 OR 7
# Divisible by both 3 AND 7
numbers <- 1:200
# Divisible by 3 OR 7
divisible_by_3_OR_7 <- numbers[numbers %% 3 == 0 | numbers %% 7 == 0]

# Divisible by both 3 AND 7
divisible_by_3_AND_7 <- numbers[numbers %% 3 == 0 & numbers %% 7 == 0]

# Print results
cat("Numbers divisible by 3 OR 7:\n")
print(divisible_by_3_OR_7)

cat("\nNumbers divisible by both 3 AND 7:\n")
print(divisible_by_3_AND_7)

# Q7) x <- c(10,20,NA,30,NA,40)
# Find:
# Sum ignoring NA
# Replace NA with mean
# Given vector
x <- c(10, 20, NA, 30, NA, 40)

# 1. Sum ignoring NA
sum_x <- sum(x, na.rm = TRUE)

# 2. Replace NA with mean
mean_x <- mean(x, na.rm = TRUE)
x[is.na(x)] <- mean_x

# Print results
cat("Sum (ignoring NA):", sum_x, "\n")
cat("Vector after replacing NA with mean:\n")
print(x)