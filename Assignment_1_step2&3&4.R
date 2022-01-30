#Import data from .csv file
MyRData <- read.csv("C:/Users/Kruthi Tatavarthy/Downloads/archive/Cancer_rate_by_countries_updated.csv", header = TRUE, sep =",", stringsAsFactors = FALSE)
#Display data from .csv file in console
head(MyRData)
#Structure the data
str(MyRData)
#Summary of the Data
summary(MyRData)

#Descriptive statistics
#Apply mean on the data
sapply(MyRData, mean, na.rm=TRUE)
#Apply min & max
min(MyRData$Rank)
min(MyRData$Cancer_Rate)
max(MyRData$Cancer_Rate)
max(MyRData$Cancer_Rate)
#max-min
max(MyRData$Cancer_Rate) - min(MyRData$Cancer_Rate)

#Transform at least one variable
#creating a new dataframe set
Pharmacy <- c("Alpharetta", "Georgia")
Customers <- c(150,100)
Price <- c(20,45)
dataframetest <- data.frame(Pharmacy,Customers,Price)
dataframetest
dataframetest$total <- dataframetest$Customers * dataframetest$Price
dataframetest
dataframetest$total_cost_dollar <- dataframetest$total
dataframetest
dataframetest$total <- NULL
dataframetest
dataframetest$Customers <- ifelse(dataframetest$Customers==150, 100, ifelse(dataframetest$Customers==100, 300, NA))
dataframetest
dataframetest$total <- dataframetest$Customers * dataframetest$Price
dataframetest
dataframetest$total_cost_dollar <- NULL
dataframetest

head(MyRData)
head(dataframetest)
