# Analysis
# TP079902 - Mohamed Yaaniu Abdulla
# Dependant Variable - Ratings
# Independant Variable - Shipping Methods
# Objective: To determine the impact of shipping method towards ratings of customers

# Hypothesis
# h0: Shipping method has no impact on Ratings
# h1: Shipping method significantly affect Ratings

#Libraries used
library(dplyr)
library(ggplot2)

#Importing the cleaned dateset
cleaned_date = read.csv("cleaned_data.csv")

#Exploratory analysis
#visualising using table
table(cleaned_date$shipping_method, cleaned_date$ratings)

#visualizing using box plot
ggplot(cleaned_date, aes(x = shipping_method, fill = ratings)) +
  geom_bar(position = "dodge", color = "black") +
  labs(
    title = "shipping_method vs ratings"
  )+
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    legend.position = "top"
  )
