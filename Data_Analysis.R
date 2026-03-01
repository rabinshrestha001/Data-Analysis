# ============================================================
# Objective 1 — Rabin Shrestha, NP070509
# Hypothesis: Customers from different countries give different product ratings. 
#This suggests that a customer’s country and cultural background influence 
#their satisfaction and expectations toward products.
# Dependent Variable: Ratings
#Independent Variables: Country, Income, Amount
# ============================================================

# Load cleaned data
data <- read.csv("/home/college/Documents/3rd-SEM/Assignment/NP2F2511IT_CT127-3-2-PFDA/cleaned_retail_data.csv")
print(data)
dim(data)
head(data)

# Convert Ratings to numbers (High=1, Low=0)
data$Ratings_Num <- ifelse(data$Ratings == "High", 1, 0)
head(data$Ratings_Num)

# Calculating average rating for each country
usa <- subset(data, Country == "USA")
usa_avg <- mean(usa$Ratings_Num)
cat("USA average rating:", usa_avg, "\n")

uk <- subset(data, Country == "UK")
uk_avg <- mean(uk$Ratings_Num)
cat("UK average rating:", uk_avg, "\n")

aus <- subset(data, Country == "Australia")
aus_avg <- mean(aus$Ratings_Num)
cat("Australia average rating:", aus_avg, "\n")

ger <- subset(data, Country == "Germany")
ger_avg <- mean(ger$Ratings_Num)
cat("Germany average rating:", ger_avg, "\n")

can <- subset(data, Country == "Canada")
can_avg <- mean(can$Ratings_Num)
cat("Canada average rating:", can_avg, "\n")

# Create data for graph
countries <- c("USA", "UK", "Germany", "Australia", "Canada")
averages  <- c(usa_avg, uk_avg, ger_avg, aus_avg, can_avg)

# Bar chart
barplot(averages,
        names.arg = countries,
        main = "Average Rating by Country",
        xlab = "Country",
        ylab = "Average Rating",
        col  = c("red", "steelblue", "gold", 
                 "lightgreen", "plum"),
        ylim = c(0, 1))


















