# Install Packages --------------------------------------------------------

install.packages("readxl")
install.packages("tidyverse")
install.packages("data.table")
install.packages("ggplot2")
install.packages("caret")
install.packages("lubridate")
install.packages("dbscan")
install.packages("mice")

# Load Packages ------------------------------------------------------------

require(readxl)
require(tidyverse)
require(data.table)
require(ggplot2)
require(caret)
require(lubridate)
require(dbscan)
require(mice)

# Import 2017 Data -------------------------------------------------------------

# N
data = fread("Data\\Data_January_2017_2.csv", na.strings = c("-", "NA"))

#remove title, V1, Contract_ID, customer_since, Notice_period and automatic_contract_extension from the data set
data = data[,c("Contract_ID","Client type", "Zip code", "Age", "Duration of customer relationship", "Contract start date", "Minimum contract term", "Consumption", "Payment on account","Annual account", "Bill shock","Online account", "Opt In Mail", "Opt In Post", "Opt In Tel", "Market area","DBII", "Churn")]


# Replace "-" by NA
data[data == "-"] = NA

# Online Account - NA to 0 
data$`Online account`[is.na(data$`Online account`)] = 0

# Recovered - "" to 0 and "X" to 1
data$Recovered[data$`Online account`!="X"] = 0
data$Recovered[data$`Online account`=="X"] = 1

#Transform "Contract start date" to number of months
data$`Contract start date` = dmy(data$`Contract start date`, locale = "English")
data$`Contract start date` = unlist(lapply(data$`Contract start date`, FUN = function(x) interval(ymd(x),ymd('20170131')) %/% months(1)))

#Convert features into right data types
data$`Zip code` = as.character(data$`Zip code`)
data$DBII = as.numeric(gsub(",", ".", gsub("\\.", "", data$DBII)))
data$`Annual account` = as.numeric(gsub(",", ".", gsub("\\.", "", data$`Annual account`)))
data$Contract_ID = as.character(data$Contract_ID)
data$Age = as.integer(data$Age)

data$Consumption = as.numeric(data$Consumption) 
data$Churn = as.factor(data$Churn)
data$Recovered = as.integer(data$Recovered)

data$`Client type` = as.factor(data$`Client type`)
data$`Bill shock` = as.factor(data$`Bill shock`)
data$`Opt In Mail` = as.factor(data$`Opt In Mail`)
data$`Opt In Post` = as.factor(data$`Opt In Post`)
data$`Opt In Tel` = as.factor(data$`Opt In Tel`)

# Impute data ----------------------------------------------------------

imp <- mice(as.data.frame(data[,c("Age","Consumption")]))

imp$imp$Age

# Explore Data I ----------------------------------------------------------

str(data)
summary(data)

summary(data[`Client type`=="1", .(`Client type`, Age)])


# Explore numerical features, detect outliers

# Age
# Check Age feature 
nrow(data[Age < 18])
nrow(data[Age >= 105])
# Consumption
summary(data[, Consumption])
plot(data[, Consumption])

max(data$Consumption)


#Cluster Analysis
hdb <- hdbscan(data, minPts = 4)

unique(data$Title)

# Payment on Account
# Annual Account

# Clean Data --------------------------------------------------------------

# Detect Percentage of NA's per feature

apply(data, 2, function(col)sum(is.na(col))/length(col))


# Data Preparation --------------------------------------------------------

# Customers older than 105 years


# Modeling ----------------------------------------------------------------

myGrid = expand.grid(max_depth =c(5, 10, 15), nrounds =c(50,100), eta = 0.1, gamma = 1, min_child_weight = 2) # insert xgboost parameters here 


myControl = trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)

model = train(Churn ~., data = data, method = "xgbTree", 
              trControl = myControl, tuneGrid = myGrid)


# eta [0.01 - 0.2]
# max_depth [500]
# min_child_weight 

model = train(Churn ~ ., data = data, method = "naive_bayes", trControl = myControl)

plot(model)

# Model Evaluation --------------------------------------------------------

confusionMatrix()


# Import 2018 Data ----------------------------------------------------------------

data_2018 = read_excel("Data\\Data November 2018.xlsx")
# write.csv(data_2018, "Data_November_2018.csv")

data_2018 = fread("Data\\Data_November_2018.csv", na.strings = "-")
