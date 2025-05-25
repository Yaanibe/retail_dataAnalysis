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

#Analysis 1: Is there a relationship between shipping method and ratings

#h0 - There is no relationship between shipping methods and ratings
#h1 - There is a relationship between shipping methods and ratings

# since both the variables are categorical, Using chi-square test of independence

table_analysis1 = table(cleaned_date$shipping_method, cleaned_date$ratings)
table_analysis1

chisq_Analysis1 = chisq.test(table_analysis1)
chisq_Analysis1

# Assume significance level of 0.05 or 5%
# since the p-value is smaller than the significance level, reject the null hypothesis
# Hence it can be concluded that there is relationship between shipping methods and ratings

























