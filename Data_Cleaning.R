
data<-read.csv("Retail_data.csv")
print(data)
head(data)
dim(data) # Check how many rows and columns
str(data) # Data types of each column

colSums(is.na(data)) # show how many missing values

data<-na.omit(data) #Remove all rows that have missing values
dim(data)

data<-unique(data) #Remove duplicate rows
dim(data)

# Fix data types
data$Age<-as.numeric(data$Age)
data$Amount<-as.numeric(data$Amount)
data$Country<-as.factor(data$Country)
data$Ratings<-as.factor(data$Ratings)
data$Income<-as.factor(data$Income)
data$Gender<-as.factor(data$Gender)

#Blank Values Check
table(data$Country)
table(data$Gender)
table(data$Income)
table(data$Order_Status)
table(data$Payment_Method)
table(data$Shipping_Method)
table(data$Product_Category)
table(data$Customer_Segment)
table(data$Product_Type)
table(data$City)
table(data$State)
table(data$Feedback)


data<- data[data$Country !="",] #Remove rows where Country is blank
data$Country <- droplevels(data$Country) #Drop unused empty 


data<-data[data$Gender !="",] # Remvoe rows whereis Gender blank 
data$Gender <- droplevels(data$Gender)


data<-data[data$Income !="",] # Remove rows where Income is blank
data$Income <- droplevels(data$Income)

data <- data[data$Order_Status != "", ] # Remove blank rows and drop levels

data<-data[data$Payment_Method !="",]

data<-data[data$Shipping_Method !="",]

data<- data[data$Product_Category !="",]

data <- data[data$Customer_Segment != "", ]

data <- data[data$Product_Brand != "", ]

data <- data[data$City != "", ]

data <- data[data$State != "", ]


range(data$Age)      
range(data$Amount)   
table(data$Ratings)  
table(data$Country)  
sum(is.na(data)) 
dim(data)

# Save cleaned data for teammates
write.csv(data, "cleaned_retail_data.csv", row.names = FALSE)


