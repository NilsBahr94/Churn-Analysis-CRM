# Install Packages --------------------------------------------------------

install.packages("tidyverse")
install.packages("readxl")
install.packages("tidyverse")
install.packages("data.table")
install.packages("ggplot2")
install.packages("caret")
install.packages("lubridate")
install.packages("xgboost")
install.packages("klaR")
install.packages("rlang")
install.packages("mice")

# Load Packages ------------------------------------------------------------

require(tidyverse)
require(readxl)
require(tidyverse)
require(data.table)
require(ggplot2)
require(caret)
require(lubridate)
require(xgboost)
require(klaR)
require(rlang)
require(mice)

# Import 2017 Data -------------------------------------------------------------

# data_2017 = read_excel("Data/Data January 2017.xlsx", na = "-")
# write.csv(data_2017, "Data_January_2017.csv")

# N
data = fread("Data\\Data_January_2017.csv", na.strings = "NA")

# L
data = fread("Data/Data_January_2017.csv", na.strings = "NA")

#remove title and V1 from the data set
data = data[,c("Contract_ID", 
               "Client type", 
               "Zip code", 
               "Age", 
               "Duration of customer relationship", 
               "Customer since",
               "Contract start date", 
               "Minimum contract term", 
               "Notice period",
               "Automatic contract extension",
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
names(data)[names(data) == 'Client type'] <- 'Client_type' # Customer since, Notice Period, Automatic Contract extension
names(data)[names(data) == 'Zip code'] <- 'Zip_code'
names(data)[names(data) == 'Duration of customer relationship'] <- 'Duration_of_customer_relationship'  
names(data)[names(data) == 'Customer since' ] <- 'Customer_since' 
names(data)[names(data) == 'Contract start date' ] <- 'Contract_start_date' 
names(data)[names(data) == 'Minimum contract term'] <- 'Minimum_contract_term'
names(data)[names(data) == 'Notice period' ] <- 'Notice_period' 
names(data)[names(data) == 'Automatic contract extension' ] <- 'Automatic_contract_extension' 
names(data)[names(data) == 'Payment on account'] <- 'Payment_on_account'
names(data)[names(data) == 'Annual account'] <- 'Annual_account'
names(data)[names(data) == 'Bill shock'] <- 'Bill_shock'
names(data)[names(data) == 'Online account'] <- 'Online_account'
names(data)[names(data) == 'Opt In Mail'] <- 'Opt_In_Mail'
names(data)[names(data) == 'Opt In Post'] <- 'Opt_In_Post'
names(data)[names(data) == 'Opt In Tel'] <- 'Opt_In_Tel'
names(data)[names(data) == 'Market area'] <- 'Market_area'

# Online Account - NA to 0 
data$`Online_account`[is.na(data$`Online_account`)] = 0

# Recovered - "" to 0 and "X" to 1
data$Recovered[data$`Recovered`=="X"] = 1
data$`Recovered`[is.na(data$`Recovered`)] = 0

#Transform "Customer_since" to number of months
data$`Customer_since` = ymd(data$`Customer_since`)
data$`Customer_since_interval` = interval(ymd(data$`Customer_since`), ymd(20170201)) %/% months(1)

#Transform "Contract_start_date" to number of months
data$`Contract_start_date` = ymd(data$`Contract_start_date`)
data$`Contract_start_date_interval` = interval(ymd(data$`Contract_start_date`), ymd(20170201)) %/% months(1)

#Transform "Market area" to binary variables
data$`Grundversorger` = unlist(lapply(data$`Market area`, FUN = function(x) ifelse(x=="Grundversorger",1,0) ))
data$`Erweitert` = unlist(lapply(data$`Market area`, FUN = function(x) ifelse(x=="Erweitertes Netzgebiet",1,0) ))
data$`Restlich` = unlist(lapply(data$`Market area`, FUN = function(x) ifelse(x=="Restliches Bundesgebiet",1,0) ))

#Convert features into right data types
data$Contract_ID = as.character(data$Contract_ID)
data$`Zip_code` = as.character(data$`Zip_code`)
data$`Client_type` = as.factor(data$`Client_type`)
data$Age = as.integer(data$Age)
data$Minimum_contract_term = as.integer(data$Minimum_contract_term)
data$Notice_period = as.integer(data$Notice_period)
data$Automatic_contract_extension = as.integer(data$Automatic_contract_extension)
data$Consumption = as.numeric(data$Consumption) 
data$Payment_on_account = as.numeric(data$Payment_on_account)
data$`Annual_account` = as.numeric(gsub(",", ".", gsub("\\.", "", data$`Annual_account`)))
data$`Bill_shock` = as.factor(data$`Bill_shock`)
data$`Online_account` = as.factor(data$`Online_account`)
data$`Opt_In_Mail` = as.factor(data$`Opt_In_Mail`)
data$`Opt_In_Post` = as.factor(data$`Opt_In_Post`)
data$`Opt_In_Tel` = as.factor(data$`Opt_In_Tel`)
data$`Grundversorger` = as.factor(data$`Grundversorger`)
data$`Erweitert` = as.factor(data$`Erweitert`)
data$`Restlich` = as.factor(data$`Restlich`)
data$Recovered = as.factor(data$Recovered)
data$Churn = as.factor(data$Churn)
data$DBII = as.numeric(gsub(",", ".", gsub("\\.", "", data$DBII)))
data$Customer_since_interval = as.integer(data$Customer_since_interval)
data$Contract_start_date_interval = as.integer(data$Contract_start_date_interval)


# Feature engineering -------------------------------------------------------------

#Create feature "international"
data$`International` = unlist(gregexpr("[0-9]{5}", data$`Zip_code`)) # Nochmal überprüfen, kurze PLZ müssen nicht unbedingt im Ausland sein
data$`International`[data$`International` == 1] = 0
data$`International`[data$`International` == -1] = 1

data$`Actual_payment` = data$Payment_on_account * 12 + data$Annual_account

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
max(data$Consumption, na.rm= TRUE)

# Payment on Account
# Annual Account

# Clean Data --------------------------------------------------------------

# Detect Percentage of NA's per feature

apply(data, 2, function(col)sum(is.na(col))/length(col))
md.pattern(data, plot= T)

# Data Preparation --------------------------------------------------------

# Customers older than 105 years


# Modeling ----------------------------------------------------------------

# a) Naive Bayes

model = train(Churn ~ ., data = data, method = "naive_bayes", trControl = myControl)

plot(model)

# b) Random Forest

# c) Support Vector Machine

# d) XGBoost
myGrid = expand.grid(max_depth =c(5, 10, 15), nrounds =c(50,100), eta = 0.1, gamma = 1, min_child_weight = 2) # insert xgboost parameters here 

myControl = trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)

model = train(Churn ~., data = data, method = "xgbTree", 
              trControl = myControl, tuneGrid = myGrid)

# e) Catboost

fit_control <- trainControl(method = "cv",
                            number = 5,
                            classProbs = TRUE)

grid <- expand.grid(depth = c(4, 6, 8),
                    learning_rate = 0.1,
                    iterations = 100,
                    l2_leaf_reg = 0.1,
                    rsm = 0.95,
                    border_count = 64)

model <- train(x, as.factor(make.names(y)),
               method = catboost.caret,
               logging_level = 'Silent', preProc = NULL,
               tuneGrid = grid, trControl = fit_control)

print(model)

# f) Logistic Regression



# Model Evaluation --------------------------------------------------------

confusionMatrix()


# Import 2018 Data ----------------------------------------------------------------

data_2018 = read_excel("Data\\Data November 2018.xlsx", na = "-")
# write.csv(data_2018, "Data_November_2018.csv")

data_2018 = fread("Data\\Data_November_2018.csv", na.strings = "-")
