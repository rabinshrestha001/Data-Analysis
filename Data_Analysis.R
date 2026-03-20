# =========================================================================================
# Rabin Shrestha, NP070509
# =========================================================================================
# Hypothesis: Customers who receive orders via faster shipping methods, use digital payment methods,
# and make higher number of purchases are more likely to give High ratings than those
# who use Standard shipping, cash payments, and purchase less.
# =========================================================================================
# Dependent Variable: Ratings 
# Independent Variables: Shipping_Method, Payment_Method, Total_Purchases
# =========================================================================================

library(readr)       # For reading CSV files
library(dplyr)       # For data manipulation 
library(ggplot2)     # For all visualizations

# Load cleaned data
data <- read_csv("/home/college/Documents/3rd-SEM/Assignment/NP2F2511IT_CT127-3-2-PFDA/cleaned_retail_data.csv")

# Convert columns to correct data types
data <- data %>%
  mutate(
    Shipping_Method = as.factor(Shipping_Method),
    Payment_Method  = as.factor(Payment_Method),
    Total_Purchases = as.integer(Total_Purchases),
    Ratings         = as.factor(Ratings)
  )


# ============================================================
# Analysis-1: Shipping method vs Ratings
# ============================================================

# Select only the columns we need for this analysis
shipping_data <- data %>%
  select(Shipping_Method, Ratings)
head(shipping_data)

# Count how many customers gave High/Low ratings per shipping method
shipping_count <- shipping_data %>%
  group_by(Shipping_Method, Ratings) %>%   
  summarise(Count = n(), .groups = "drop") 
print(shipping_count)

# Calculate the percentage of High/Low ratings per shipping method
shipping_pct <- shipping_count %>%
  group_by(Shipping_Method) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2))

print(shipping_pct)

# Bar chart showing count of ratings per shipping method
ggplot(shipping_count, aes(x = Shipping_Method, y = Count, fill = Ratings)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("High" = "blue", "Low" = "red")) +
  labs(title = "Shipping Method vs Customer Ratings",
       x = "Shipping Method",
       y = "Number of Customers",
       fill = "Ratings") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Stacked percentage bar chart
ggplot(shipping_data, aes(x = Shipping_Method, fill = Ratings)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("High" = "blue", "Low" = "red")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Shipping Method vs Ratings (Proportional)",
       x = "Shipping Method",
       y = "Percentage",
       fill = "Ratings") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Frequency table for chi-square test
shipping_table <- table(data$Shipping_Method, data$Ratings)

# Chi-Square test to check if association is statistically significant
chi_shipping <- chisq.test(shipping_table)
print(chi_shipping)

# if p-value is less than 0.05
if (chi_shipping$p.value < 0.05) {
  cat("Result: Significant association between Shipping Method and Ratings\n")
} else {
  cat("Result: No significant association found\n")
}

# Calculate percentage of High ratings per shipping method
impact_shipping <- data %>%
  select(Shipping_Method, Ratings) %>%
  group_by(Shipping_Method) %>%
  summarise(
    Total           = n(),
    High_Ratings    = sum(Ratings == "High"),
    High_Percentage = round(mean(Ratings == "High") * 100, 2),
    .groups = "drop"
  )

print(impact_shipping)

# Bar chart showing High rating percentage per shipping method
ggplot(impact_shipping,
       aes(x = reorder(Shipping_Method, -High_Percentage),
           y = High_Percentage,
           fill = Shipping_Method)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(High_Percentage, "%")),
            vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = c("Same-Day" = "green",
                               "Express"  = "blue",
                               "Standard" = "yellow")) +
  labs(title = "Percentage of High Ratings by Shipping Method",
       x = "Shipping Method",
       y = "% of High Ratings") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")


# ============================================================
# Analysis-2: Payment Method vs Ratings
# ============================================================
# Columns that we only need
payment_data <- data %>%
  select(Payment_Method, Ratings)

# Count ratings per payment method
payment_count <- payment_data %>%
  group_by(Payment_Method, Ratings) %>%
  summarise(Count = n(), .groups = "drop")

print(payment_count)

# Calculate percentage of High/Low ratings per payment method
payment_pct <- payment_count %>%
  group_by(Payment_Method) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2))

print(payment_pct)

# Bar chart showing count of ratings per payment method
ggplot(payment_count, aes(x = Payment_Method, y = Count, fill = Ratings)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("High" = "blue", "Low" = "red")) +
  labs(title = "Payment Method vs Customer Ratings",
       x = "Payment Method",
       y = "Number of Customers",
       fill = "Ratings") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Stacked percentage bar chart
ggplot(payment_data, aes(x = Payment_Method, fill = Ratings)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("High" = "blue", "Low" = "red")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Payment Method vs Ratings (Proportional)",
       x = "Payment Method",
       y = "Percentage",
       fill = "Ratings") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Create frequency table for chi-square test
payment_table <- table(data$Payment_Method, data$Ratings)

# Chi-Square test to check statistical significance
chi_payment <- chisq.test(payment_table)
print(chi_payment)

# Check if association is significant
if (chi_payment$p.value < 0.05) {
  cat("Result: Significant association between Payment Method and Ratings\n")
} else {
  cat("Result: No significant association found\n")
}

# Calculate percentage of High ratings per payment method
impact_payment <- data %>%
  select(Payment_Method, Ratings) %>%
  group_by(Payment_Method) %>%
  summarise(
    Total           = n(),
    High_Ratings    = sum(Ratings == "High"),
    High_Percentage = round(mean(Ratings == "High") * 100, 2),
    .groups = "drop"
  )

print(impact_payment)

# Bar chart showing High rating percentage per payment method
ggplot(impact_payment,
       aes(x = reorder(Payment_Method, -High_Percentage),
           y = High_Percentage,
           fill = Payment_Method)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(High_Percentage, "%")),
            vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = c("Credit Card" = "green",
                               "Debit Card"  = "blue",
                               "PayPal"      = "purple",
                               "Cash"        = "yellow")) +
  labs(title = "Percentage of High Ratings by Payment Method",
       x = "Payment Method",
       y = "% of High Ratings") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")

# ============================================================
# Analysis-3: Total Purchases vs Ratings
# ============================================================
# Columns that we need
purchase_data <- data %>%
  select(Total_Purchases, Ratings)

# Calculate summary statistics for Total Purchases by Ratings
purchase_summary <- purchase_data %>%
  group_by(Ratings) %>%
  summarise(
    Mean_Purchases   = round(mean(Total_Purchases), 2),
    Median_Purchases = median(Total_Purchases),
    SD_Purchases     = round(sd(Total_Purchases), 2),
    Min_Purchases    = min(Total_Purchases),
    Max_Purchases    = max(Total_Purchases),
    .groups = "drop"
  )

print(purchase_summary)

# Boxplot showing distribution of Total Purchases by Ratings
ggplot(purchase_data, aes(x = Ratings, y = Total_Purchases, fill = Ratings)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16) +
  scale_fill_manual(values = c("High" = "blue", "Low" = "red")) +
  labs(title = "Total Purchases Distribution by Ratings",
       x = "Ratings",
       y = "Total Purchases") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")

# Bar chart showing average purchases per rating group
ggplot(purchase_summary, aes(x = Ratings, y = Mean_Purchases, fill = Ratings)) +
  geom_col(width = 0.4) +
  geom_text(aes(label = Mean_Purchases),
            vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = c("High" = "blue", "Low" = "red")) +
  labs(title = "Average Total Purchases by Ratings",
       x = "Ratings",
       y = "Mean Total Purchases") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")

# Binary column for Ratings using mutate()
purchase_data <- purchase_data %>%
  mutate(Ratings_Binary = ifelse(Ratings == "High", 1, 0))

# Point-biserial correlation
correlation <- cor.test(purchase_data$Total_Purchases,
                        purchase_data$Ratings_Binary,
                        method = "pearson")
print(correlation)
cat("Correlation Coefficient:", round(correlation$estimate, 4), "\n")

# Kruskal-Wallis test
kruskal_result <- kruskal.test(Total_Purchases ~ Ratings, data = purchase_data)
print(kruskal_result)

# High rating % for each purchase count
impact_purchases <- purchase_data %>%
  group_by(Total_Purchases) %>%
  summarise(
    Total           = n(),
    High_Ratings    = sum(Ratings == "High"),
    High_Percentage = round(mean(Ratings == "High") * 100, 2),
    .groups = "drop"
  )

print(impact_purchases)

# Line chart showing trend
ggplot(impact_purchases, aes(x = Total_Purchases, y = High_Percentage)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(color = "red", size = 3) +
  geom_text(aes(label = paste0(High_Percentage, "%")),
            vjust = -0.8, size = 3.5) +
  labs(title = "% of High Ratings by Number of Total Purchases",
       x = "Total Purchases",
       y = "% of High Ratings") +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Select all 3 variables and dependent variable
model_data <- data %>%
  select(Shipping_Method, Payment_Method, Total_Purchases, Ratings) %>%
  mutate(Ratings = as.factor(Ratings))

# Split 80% train 20% test
set.seed(123)
sample_index <- sample(1:nrow(model_data), size = 0.8 * nrow(model_data))
train_data   <- model_data[sample_index, ]
test_data    <- model_data[-sample_index, ]

cat("Training set size:", nrow(train_data), "\n")
cat("Testing set size:", nrow(test_data), "\n")

# Train logistic regression model
log_model <- glm(Ratings ~ Shipping_Method + Payment_Method + Total_Purchases,
                 data   = train_data,
                 family = binomial)

print(summary(log_model))

# Predict ratings on test data
predictions_prob <- predict(log_model, newdata = test_data, type = "response")
predictions      <- ifelse(predictions_prob > 0.5, "High", "Low")
predictions      <- as.factor(predictions)
actual           <- as.factor(test_data$Ratings)

# Confusion matrix
conf_matrix <- table(Predicted = predictions, Actual = actual)
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix) * 100
cat("Model Accuracy:", round(accuracy, 2), "%\n")

# Visualize Actual vs Predicted
results_df <- data.frame(
  Actual    = actual,
  Predicted = predictions
)

ggplot(results_df, aes(x = Actual, fill = Predicted)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Low" = "red")) +
  labs(title = "Logistic Regression: Actual vs Predicted Ratings",
       x     = "Actual Ratings",
       y     = "Count",
       fill  = "Predicted") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))