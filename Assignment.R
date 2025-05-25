# TP079902 - Mohamed Yaaniu Abdulla
# Dependant Variable - Ratings
# Independant Variable - Shipping Methods
# Objective: To determine the impact of shipping method towards ratings of customers

# Hypothesis
# h0: Shipping method has no impact on Ratings
# h1: Shipping method significantly affect Ratings


# Libraries used
library(data.table)
library(dplyr)
library(ggplot2)
library(DataExplorer) ##

# 1. DATA IMPORT

# Importing the Data into RStudio
retail_Data = read.csv("D:\\Year 2\\PFDA\\Assignment\\retail_data 1.csv")

# Pre-Viewing the structure of the Data in RStudio
str(retail_Data)
head(retail_Data)

# Summarising the number of columns and rows
glimpse(retail_Data)

# Summarising the Entire Data Set
summary(retail_Data)


# 2. DATA CLEANING

## check and missing value
plot_missing(retail_Data)

## Lowercasing all column names
names(retail_Data) = tolower(names(retail_Data))

## converting character columns into factor
retail_Data$shipping_method = as.factor(retail_Data$shipping_method)
retail_Data$order_status = as.factor(retail_Data$order_status)
retail_Data$product_category = as.factor(retail_Data$product_category)
retail_Data$payment_method = as.factor(retail_Data$payment_method)
retail_Data$income = as.factor(retail_Data$income)
retail_Data$feedback = as.factor(retail_Data$feedback)
retail_Data$ratings = as.factor(retail_Data$ratings)



# 2.1. Initial plot of categorical Variables vs Ratings

# Creating a function for the initial Plotting of graphs for Categorical Variables

plot_init_rating_bar = function(data, colname, title = NULL, subtitle = NULL) {
  ggplot(data, aes_string(x = colname, fill = "ratings")) +                     
    geom_bar(position = "dodge", color = "black") +                                                    # side-by-side bars with black outline
    labs(
      title = ifelse(is.null(title), paste(colname, "vs ratings"), title),                             # if no title is provided go with column vs ratings
      subtitle = ifelse(is.null(subtitle), paste("Comparison of Ratings across", colname), subtitle),  # if no subtitle is provided use as default
      x = colname,                                                                                     # axis labels
      y = "Number of Transactions",
      fill = "Customer Ratings"                                                                        # changing legend title from Ratings customer Ratings
    ) +
    theme_minimal(base_size = 12) +                                                                    # cleaner theme
    theme(
      plot.title = element_text(size = 18, face = "bold"),                                             # Changing the Font styles for a more appealing plot
      plot.subtitle = element_text(size = 12, color = "gray40"),
      axis.text.x = element_text(angle = 30, hjust = 0.5),
      axis.text.y = element_text(size = 10),
      legend.position = "top"
    )
}

# Shipping Method vs Ratings
plot_init_rating_bar(retail_Data, "shipping_method")

# Order_Status vs Ratings
plot_init_rating_bar(retail_Data, "order_status")

# Product_Category vs Ratings
plot_init_rating_bar(retail_Data, "product_category")

# Payment_Methods vs Ratings
plot_init_rating_bar(retail_Data, "payment_method")

# Income vs Ratings
plot_init_rating_bar(retail_Data, "income")

# Feedback vs Ratings
plot_init_rating_bar(retail_Data, "feedback")


# Initial Plots for Continuous Variables (couldn't create a function for continuous plot)

# Age vs Ratings

ggplot(retail_Data, aes(x = ratings, y = age, fill = ratings)) +
  geom_boxplot() +
  labs(
    title = "Age vs Ratings",
    subtitle = "Comparison of Age and Ratings",
    ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),                                             
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text.x = element_text(angle = 30, hjust = 0.5),
    axis.text.y = element_text(size = 10),
  )


# Total_Purchases vs Ratings

ggplot(retail_Data, aes(x = ratings, y = total_purchases, fill = ratings)) +
  geom_boxplot() +
  labs(
    title = "Total Purchases vs Ratings",
    subtitle = "Comparison of Total Purchases and Ratings",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),                                             
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text.x = element_text(angle = 30, hjust = 0.5),
    axis.text.y = element_text(size = 10),
  )


# Amount vs Ratings

ggplot(retail_Data, aes(x = ratings, y = amount, fill = ratings)) +
  geom_boxplot() +
  labs(
    title = "Amount vs Ratings",
    subtitle = "Comparison of Amount and Ratings",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),                                             
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text.x = element_text(angle = 30, hjust = 0.5),
    axis.text.y = element_text(size = 10),
  )


# Total_Amount vs Ratings

ggplot(retail_Data, aes(x = ratings, y = total_amount, fill = ratings)) +
  geom_boxplot() +
  labs(
    title = "Total Amount vs Ratings",
    subtitle = "Comparison of Total Amount and Ratings",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),                                             
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text.x = element_text(angle = 30, hjust = 0.5),
    axis.text.y = element_text(size = 10),
  )


# checking Missing Values
colSums(is.na(retail_Data)) # seperately
sum(is.na(retail_Data)) # al;l together

# Replacing Blank Values with NA
retail_Data$age = replace(retail_Data$age, retail_Data$age == "", NA)

retail_Data$income = replace(retail_Data$income, retail_Data$income == "", NA)

retail_Data$total_purchases = replace(retail_Data$total_purchases, retail_Data$total_purchases == "", NA)

retail_Data$amount = replace(retail_Data$amount, retail_Data$amount == "", NA)

retail_Data$total_amount = replace(retail_Data$total_amount, retail_Data$total_amount == "", NA)

retail_Data$product_category = replace(retail_Data$product_category, retail_Data$product_category == "", NA)

retail_Data$feedback = replace(retail_Data$feedback, retail_Data$feedback == "", NA)

retail_Data$shipping_method = replace(retail_Data$shipping_method, retail_Data$shipping_method == "", NA)

retail_Data$payment_method = replace(retail_Data$payment_method, retail_Data$payment_method == "", NA)

retail_Data$order_status = replace(retail_Data$order_status, retail_Data$order_status == "", NA)

retail_Data$ratings = replace(retail_Data$ratings, retail_Data$ratings == "", NA)

# checking NA values after converting blank spaces to NA
colSums(is.na(retail_Data))
sum(is.na(retail_Data))

# Creating a function to get and replace with the mode for the Categorical Variables
mode = function(x){
  uniq = na.omit(unique(x))                                                     # get uniques, non NA values
  uniq[which.max(tabulate(match(x, uniq)))]                                     # Returns most frequent value
}


# Replacing the NA values with the mode Values got from step above

retail_Data$income = replace(retail_Data$income, is.na(retail_Data$income), mode(retail_Data$income))

retail_Data$product_category = replace(retail_Data$product_category, is.na(retail_Data$product_category), mode(retail_Data$product_category))

retail_Data$feedback = replace(retail_Data$feedback, is.na(retail_Data$feedback), mode(retail_Data$feedback))

retail_Data$payment_method = replace(retail_Data$payment_method, is.na(retail_Data$payment_method), mode(retail_Data$payment_method))

retail_Data$order_status = replace(retail_Data$order_status, is.na(retail_Data$order_status), mode(retail_Data$order_status))

retail_Data$shipping_method = replace(retail_Data$shipping_method, is.na(retail_Data$shipping_method), mode(retail_Data$shipping_method))

# checking if NA values has been replaced for the categorical variables

colSums(is.na(retail_Data))
sum(is.na(retail_Data))

# Replacing the NA values with the median Values for the continuous variables

retail_Data$age = replace(retail_Data$age, is.na(retail_Data$age), median(retail_Data$age, na.rm = TRUE))

retail_Data$total_purchases = replace(retail_Data$total_purchases, is.na(retail_Data$total_purchases), median(retail_Data$total_purchases, na.rm = TRUE))

retail_Data$amount = replace(retail_Data$amount, is.na(retail_Data$amount), median(retail_Data$amount, na.rm = TRUE))

retail_Data$total_amount = replace(retail_Data$total_amount, is.na(retail_Data$total_amount), median(retail_Data$total_amount, na.rm = TRUE))

# checking if NA values has been replaced for the continuous variables

colSums(is.na(retail_Data))
sum(is.na(retail_Data))

# Checking NA values for Ratings
sum(is.na(retail_Data$ratings))

# Dropping the rows with NA values for Ratings
retail_Data = retail_Data[!is.na(retail_Data$ratings), ]

# Checking NA values for Ratings after dropping
sum(is.na(retail_Data$ratings))

# Plotting the Categorical variables after cleaning

# Shipping Method vs Ratings
plot_init_rating_bar(retail_Data, "shipping_method")

# Order_Status vs Ratings
plot_init_rating_bar(retail_Data, "order_status")

# Product_Category vs Ratings
plot_init_rating_bar(retail_Data, "product_category")

# Payment_Methods vs Ratings
plot_init_rating_bar(retail_Data, "payment_method")

# Income vs Ratings
plot_init_rating_bar(retail_Data, "income")

# Feedback vs Ratings
plot_init_rating_bar(retail_Data, "feedback")


# Plotting the Continuous variables after cleaning

ggplot(retail_Data, aes(x = ratings, y = age, fill = ratings)) +
  geom_boxplot() +
  labs(
    title = "Age vs Ratings",
    subtitle = "Comparison of Age and Ratings",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),                                             
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text.x = element_text(angle = 30, hjust = 0.5),
    axis.text.y = element_text(size = 10),
  )


# Total_Purchases vs Ratings

ggplot(retail_Data, aes(x = ratings, y = total_purchases, fill = ratings)) +
  geom_boxplot() +
  labs(
    title = "Total Purchases vs Ratings",
    subtitle = "Comparison of Total Purchases and Ratings",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),                                             
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text.x = element_text(angle = 30, hjust = 0.5),
    axis.text.y = element_text(size = 10),
  )


# Amount vs Ratings

ggplot(retail_Data, aes(x = ratings, y = amount, fill = ratings)) +
  geom_boxplot() +
  labs(
    title = "Amount vs Ratings",
    subtitle = "Comparison of Amount and Ratings",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),                                             
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text.x = element_text(angle = 30, hjust = 0.5),
    axis.text.y = element_text(size = 10),
  )


# Total_Amount vs Ratings

ggplot(retail_Data, aes(x = ratings, y = total_amount, fill = ratings)) +
  geom_boxplot() +
  labs(
    title = "Total Amount vs Ratings",
    subtitle = "Comparison of Total Amount and Ratings",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),                                             
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text.x = element_text(angle = 30, hjust = 0.5),
    axis.text.y = element_text(size = 10),
  )















