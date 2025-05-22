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

# 1. DATA IMPORT

# Importing the Data into RStudio
retail_Data = fread("D:\\Year 2\\PFDA\\Assignment\\retail_data 1.csv")

# Pre-Viewing the structure of the Data in RStudio
str(retail_Data)
head(retail_Data)

# Summarising the number of columns and rows
glimpse(retail_Data)

# Summarising the Entire Data Set
summary(retail_Data)


# 2. DATA CLEANING

# 2.1. Initial plot of categorical Variables vs Ratings

# Creating a function for the initial Plotting of graphs for Categorical Variables

plot_init_rating_bar = function(data, colname, title = NULL, subtitle = NULL) {
  ggplot(data, aes_string(x = colname, fill = "Ratings")) +                     
    geom_bar(position = "dodge", color = "black") +                                                    # side-by-side bars with black outline
    labs(
      title = ifelse(is.null(title), paste(colname, "vs Ratings"), title),                             # if no title is provided go with column vs ratings
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
plot_init_rating_bar(retail_Data, "Shipping_Method")

# Order_Status vs Ratings
plot_init_rating_bar(retail_Data, "Order_Status")

# Product_Category vs Ratings
plot_init_rating_bar(retail_Data, "Product_Category")

# Payment_Methods vs Ratings
plot_init_rating_bar(retail_Data, "Payment_Method")

# Income vs Ratings
plot_init_rating_bar(retail_Data, "Income")

# Feedback vs Ratings
plot_init_rating_bar(retail_Data, "Feedback")


# Initial Plots for Continuous Variables (couldn't create a function for continuous plot)

# Age vs Ratings

ggplot(retail_Data, aes(x = Ratings, y = Age, fill = Ratings)) +
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

ggplot(retail_Data, aes(x = Ratings, y = Total_Purchases, fill = Ratings)) +
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

ggplot(retail_Data, aes(x = Ratings, y = Amount, fill = Ratings)) +
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

ggplot(retail_Data, aes(x = Ratings, y = Total_Amount, fill = Ratings)) +
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
colSums(is.na(retail_Data))

# Replaceing Blank Values with NA
retail_Data$Age = replace(retail_Data$Age, retail_Data$Age == "", NA)

retail_Data$Income = replace(retail_Data$Income, retail_Data$Income == "", NA)

retail_Data$Total_Purchases = replace(retail_Data$Total_Purchases, retail_Data$Total_Purchases == "", NA)

retail_Data$Amount = replace(retail_Data$Amount, retail_Data$Amount == "", NA)

retail_Data$Total_Amount = replace(retail_Data$Total_Amount, retail_Data$Total_Amount == "", NA)

retail_Data$Product_Category = replace(retail_Data$Product_Category, retail_Data$Product_Category == "", NA)

retail_Data$Feedback = replace(retail_Data$Feedback, retail_Data$Feedback == "", NA)

retail_Data$Shipping_Method = replace(retail_Data$Shipping_Method, retail_Data$Shipping_Method == "", NA)

retail_Data$Payment_Method = replace(retail_Data$Payment_Method, retail_Data$Payment_Method == "", NA)

retail_Data$Order_Status = replace(retail_Data$Order_Status, retail_Data$Order_Status == "", NA)

retail_Data$Ratings = replace(retail_Data$Ratings, retail_Data$Ratings == "", NA)

# checking NA values after converting blank spaces to NA
colSums(is.na(retail_Data))

# Creating a function to get and replace with the mode for the Categorical Variables
mode = function(x){
  uniq = na.omit(unique(x))                                                     # Remove NA's
  uniq[which.max(tabulate(match(x, uniq)))]                                     # Returns most frequent value
}

mode(retail_Data$Income)
mode(retail_Data$Product_Category)
mode(retail_Data$Feedback)
mode(retail_Data$Payment_Method)
mode(retail_Data$Order_Status)
mode(retail_Data$Shipping_Method)

# Replacing the NA values with the mode Values got from step above

retail_Data$Income = replace(retail_Data$Income, is.na(retail_Data$Income), "Medium")

retail_Data$Product_Category = replace(retail_Data$Product_Category, is.na(retail_Data$Product_Category), "Electronics")

retail_Data$Feedback = replace(retail_Data$Feedback, is.na(retail_Data$Feedback), "Excellent")

retail_Data$Payment_Method = replace(retail_Data$Payment_Method, is.na(retail_Data$Payment_Method), "Credit Card")

retail_Data$Order_Status = replace(retail_Data$Order_Status, is.na(retail_Data$Order_Status), "Delivered")

retail_Data$Shipping_Method = replace(retail_Data$Shipping_Method, is.na(retail_Data$Shipping_Method), "Same-Day")

# checking if NA values has been replaced for the categorical variables

colSums(is.na(retail_Data))


# Replacing the NA values with the median Values for the continuous variables

retail_Data$Age = replace(retail_Data$Age, is.na(retail_Data$Age), median(retail_Data$Age, na.rm = TRUE))

retail_Data$Total_Purchases = replace(retail_Data$Total_Purchases, is.na(retail_Data$Total_Purchases), median(retail_Data$Total_Purchases, na.rm = TRUE))

retail_Data$Amount = replace(retail_Data$Amount, is.na(retail_Data$Amount), median(retail_Data$Amount, na.rm = TRUE))

retail_Data$Total_Amount = replace(retail_Data$Total_Amount, is.na(retail_Data$Total_Amount), median(retail_Data$Total_Amount, na.rm = TRUE))

# checking if NA values has been replaced for the continuous variables

colSums(is.na(retail_Data))

# Checking NA values for Ratings
sum(is.na(retail_Data$Ratings))

# Dropping the rows with NA values for Ratings
retail_Data = retail_Data[!is.na(retail_Data$Ratings), ]

# Checking NA values for Ratings after dropping
sum(is.na(retail_Data$Ratings))

# Plotting the Categorical variables after cleaning

# Shipping Method vs Ratings
plot_init_rating_bar(retail_Data, "Shipping_Method")

# Order_Status vs Ratings
plot_init_rating_bar(retail_Data, "Order_Status")

# Product_Category vs Ratings
plot_init_rating_bar(retail_Data, "Product_Category")

# Payment_Methods vs Ratings
plot_init_rating_bar(retail_Data, "Payment_Method")

# Income vs Ratings
plot_init_rating_bar(retail_Data, "Income")

# Feedback vs Ratings
plot_init_rating_bar(retail_Data, "Feedback")


# Plotting the Continuous variables after cleaning

ggplot(retail_Data, aes(x = Ratings, y = Age, fill = Ratings)) +
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

ggplot(retail_Data, aes(x = Ratings, y = Total_Purchases, fill = Ratings)) +
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

ggplot(retail_Data, aes(x = Ratings, y = Amount, fill = Ratings)) +
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

ggplot(retail_Data, aes(x = Ratings, y = Total_Amount, fill = Ratings)) +
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













