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
require(stringr)
require(mlr)

# Import 2017 Data -------------------------------------------------------------

# N
data = fread("Data\\Data_January_2017_2.csv", na.strings = c("-", "NA"))
#setwd("D:/Julian Hentschel/CRMADM/Churn-Analysis-CRM")

#remove title, V1, Contract_ID, customer_since, Notice_period and automatic_contract_extension from the data set
data = data[,c("Contract_ID", 
               "Client type", 
               "Zip code", 
               "Age", 
               "Duration of customer relationship", 
               "Contract start date", 
               "Minimum contract term", 
               "Consumption", 
               "Payment on account", 
               "Annual account", 
               "Bill shock", 
               "Online account", 
               "Opt In Mail", 
               "Opt In Post", 
               "Opt In Tel", 
               "Market area", 
               "Recovered", 
               "DBII", 
               "Churn")]

#rename cols in order to avoid problems with imputation
names(data)[names(data) == 'Client type'] <- 'Client_type'
names(data)[names(data) == 'Zip code'] <- 'Zip_code'
names(data)[names(data) == 'Duration of customer relationship'] <- 'Duration_of_customer_relationship'
names(data)[names(data) == 'Minimum contract term'] <- 'Minimum_contract_term'
names(data)[names(data) == 'Payment on account'] <- 'Payment_on_account'
names(data)[names(data) == 'Annual account'] <- 'Annual_account'
names(data)[names(data) == 'Bill shock'] <- 'Bill_shock'
names(data)[names(data) == 'Online account'] <- 'Online_account'
names(data)[names(data) == 'Opt In Mail'] <- 'Opt_In_Mail'
names(data)[names(data) == 'Opt In Post'] <- 'Opt_In_Post'
names(data)[names(data) == 'Opt In Tel'] <- 'Opt_In_Tel'

# Replace "-" by NA
data[data == "-"] = NA

# Online Account - NA to 0 
data$`Online_account`[is.na(data$`Online_account`)] = 0

# Recovered - "" to 0 and "X" to 1
data$Recovered[data$`Recovered`!="X"] = 0
data$Recovered[data$`Recovered`=="X"] = 1

#Transform "Contract start date" to number of months
data$`Contract start date` = dmy(data$`Contract start date`, locale = "English")
data$`Contract_since` = unlist(lapply(data$`Contract start date`, FUN = function(x) interval(ymd(x),ymd('20170131')) %/% months(1)))

#Create feature "international"
data$`International` = unlist(gregexpr("[0-9]{5}", data$`Zip_code`))
data$`International`[data$`International` == 1] = 0
data$`International`[data$`International` == -1] = 1

#Transform "Market area" to binary variables
data$`Grundversorger` = unlist(lapply(data$`Market area`, FUN = function(x) ifelse(x=="Grundversorger",1,0) ))
data$`Erweitert` = unlist(lapply(data$`Market area`, FUN = function(x) ifelse(x=="Erweitertes Netzgebiet",1,0) ))
data$`Restlich` = unlist(lapply(data$`Market area`, FUN = function(x) ifelse(x=="Restliches Bundesgebiet",1,0) ))
data = data[,-c("Contract start date", "Market area")]

#Convert features into right data types
data$`Zip_code` = as.character(data$`Zip_code`)
data$Contract_ID = as.character(data$Contract_ID)

data$Age = as.integer(data$Age)

data$DBII = as.numeric(gsub(",", ".", gsub("\\.", "", data$DBII)))
data$`Annual_account` = as.numeric(gsub(",", ".", gsub("\\.", "", data$`Annual_account`)))
data$Payment_on_account = as.numeric(data$Payment_on_account)
data$Consumption = as.numeric(data$Consumption) 

data$Churn = as.factor(data$Churn)
data$Recovered = as.factor(data$Recovered)
data$`Client_type` = as.factor(data$`Client_type`)
data$`Bill_shock` = as.factor(data$`Bill_shock`)
data$`Opt_In_Mail` = as.factor(data$`Opt_In_Mail`)
data$`Opt_In_Post` = as.factor(data$`Opt_In_Post`)
data$`Opt_In_Tel` = as.factor(data$`Opt_In_Tel`)
data$`Grundversorger` = as.factor(data$`Grundversorger`)
data$`Erweitert` = as.factor(data$`Erweitert`)
data$`Restlich` = as.factor(data$`Restlich`)

# Impute data ----------------------------------------------------------

imp <- mice(data)

test <- complete(imp)

summary(test)

# Normalize data ----------------------------------------------------------



test$Payment_on_account = as.numeric(test$Payment_on_account)

normTest = normalizeFeatures(test, cols = c("Age",
                                            "Payment_on_account", 
                                            "Duration_of_customer_relationship", 
                                            "Minimum_contract_term", 
                                            "Consumption", 
                                            "Annual_account", 
                                            "DBII", 
                                            "Contract_since"))

churner_df = normTest[normTest$Churn==1,]

summary(churner_df)

# Explore Data I ----------------------------------------------------------

str(data)
summary(data)

summary(data[`Client type`=="1", .(`Client_type`, Age)])


# Explore numerical features, detect outliers

# Age
# Check Age feature 
nrow(data[Age < 18])
nrow(data[Age >= 105])
# Consumption
summary(data[, Consumption])
plot(data[, Consumption])

max(data$Consumption)

r = data("moons")


# Cluster Analysis ----------------------------------------------------------
#Select subset of features (namely all except for client_ID and Zip_code)
cluster_df = (churner_df[,c("Client_type", 
                            "Age", 
                            "Duration_of_customer_relationship", 
                            "Minimum_contract_term", 
                            "Consumption", 
                            "Payment_on_account", 
                            "Annual_account", 
                            "Bill_shock", 
                            "Online_account",
                            "Opt_In_Mail", 
                            "Opt_In_Post", 
                            "Opt_In_Tel", 
                            "Recovered", 
                            "DBII", 
                            "Contract_since", 
                            "International", 
                            "Grundversorger", 
                            "Erweitert", 
                            "Restlich")])

#Transform to numeric features
cluster_df$Client_type = as.numeric(cluster_df$Client_type)
cluster_df$Age = as.numeric(cluster_df$Age)
cluster_df$Duration_of_customer_relationship = as.numeric(cluster_df$Duration_of_customer_relationship)
cluster_df$Minimum_contract_term = as.numeric(cluster_df$Minimum_contract_term)
cluster_df$Consumption = as.numeric(cluster_df$Consumption)
cluster_df$Payment_on_account = as.numeric(cluster_df$Payment_on_account)
cluster_df$Annual_account = as.numeric(cluster_df$Annual_account)
cluster_df$Bill_shock = as.numeric(cluster_df$Bill_shock)
cluster_df$Online_account = as.numeric(cluster_df$Online_account)
cluster_df$Opt_In_Mail = as.numeric(cluster_df$Opt_In_Mail)
cluster_df$Opt_In_Post = as.numeric(cluster_df$Opt_In_Post)
cluster_df$Opt_In_Tel = as.numeric(cluster_df$Opt_In_Tel)
cluster_df$Recovered = as.numeric(cluster_df$Recovered)
cluster_df$DBII = as.numeric(cluster_df$DBII)
cluster_df$Contract_since = as.numeric(cluster_df$Contract_since)
cluster_df$International = as.numeric(cluster_df$International)
cluster_df$Grundversorger = as.numeric(cluster_df$Grundversorger)
cluster_df$Erweitert = as.numeric(cluster_df$Erweitert)
cluster_df$Restlich = as.numeric(cluster_df$Restlich)

#Execute HDBSCAN clustering algorithm
hdb <- hdbscan(cluster_df, minPts = 4)

plot(hdb)

result = cbind(test[test$Churn==1,], hdb$cluster)


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
