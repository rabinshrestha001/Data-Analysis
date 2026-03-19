# ============================================================
# DATA CLEANING - Retail Transactional Data
# ============================================================

# Load Data
data <- read.csv("/home/college/Documents/3rd-SEM/All Assignment/Data Analysis/Retail_data.csv")
head(data)
dim(data)
str(data)

# Check Missing Values
colSums(is.na(data))

# Remove Missing Values
data <- na.omit(data)
dim(data)

# Remove Duplicates
data <- unique(data)
dim(data)

# Fix Data Type
data$Age  <- as.numeric(data$Age)
data$Amount   <- as.numeric(data$Amount)
data$Total_Amount  <- as.numeric(data$Total_Amount)
data$Total_Purchases  <- as.integer(data$Total_Purchases)
data$Country  <- as.factor(data$Country)
data$Gender   <- as.factor(data$Gender)
data$Income    <- as.factor(data$Income)
data$Customer_Segment <- as.factor(data$Customer_Segment)
data$Feedback  <- as.factor(data$Feedback)
data$Product_Category <- as.factor(data$Product_Category)
data$Shipping_Method  <- as.factor(data$Shipping_Method)
data$Payment_Method <- as.factor(data$Payment_Method)
data$Order_Status  <- as.factor(data$Order_Status)
data$Ratings   <- as.factor(data$Ratings)

# Check Blank String Values
table(data$Country);        table(data$Gender)
table(data$Income);         table(data$Order_Status)
table(data$Payment_Method); table(data$Shipping_Method)
table(data$Product_Category); table(data$Customer_Segment)
table(data$Feedback);       table(data$Ratings)
table(data$City);           table(data$State)

# Remove Blank String Rows
data <- data[data$Country != "", ];          data$Country <- droplevels(data$Country)
data <- data[data$Gender != "", ];           data$Gender <- droplevels(data$Gender)
data <- data[data$Income != "", ];           data$Income <- droplevels(data$Income)
data <- data[data$Order_Status != "", ]
data <- data[data$Payment_Method != "", ]
data <- data[data$Shipping_Method != "", ]
data <- data[data$Product_Category != "", ]
data <- data[data$Customer_Segment != "", ]
data <- data[data$Product_Brand != "", ]
data <- data[data$City != "", ]
data <- data[data$State != "", ]
data <- data[data$Feedback != "", ];         data$Feedback <- droplevels(data$Feedback)
data <- data[data$Ratings != "", ];          data$Ratings <- droplevels(data$Ratings)

# Data Validation
range(data$Age)
range(data$Amount)
range(data$Total_Amount)

# Filter unrealistic age values
data <- data[data$Age >= 18 & data$Age <= 100, ]

# Check
sum(is.na(data))
dim(data)
table(data$Ratings)
cat("Cleaning complete! Final rows:", nrow(data), "\n")

# Save Cleaned Data for Teammates
write.csv(data, "cleaned_retail_data.csv", row.names = FALSE)
cat("Cleaned data saved successfully!\n")
