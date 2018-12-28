# Install Packages --------------------------------------------------------

install.packages("tidyverse")
install.packages("readxl")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("data.table")
install.packages("ggplot2")
install.packages("caret")
install.packages("lubridate")
install.packages("xgboost")
install.packages("klaR")
install.packages("rlang")
install.packages("mice")
install.packages("corrplot")
install.packages("dplyr")
install.packages("factoextra")
install.packages("lattice")
install.packages("VIM")
install.packages("purrr")


# Load Packages ------------------------------------------------------------

require(readxl)
require(tidyverse)
require(dplyr)
require(data.table)
require(ggplot2)
require(caret)
require(lubridate)
require(xgboost)
require(klaR)
require(rlang)
require(mice)
require(corrplot)
require(dplyr)
require(factoextra)
require(lattice)
require(VIM)
require(purrr)


# Import 2017 Data -------------------------------------------------------------

# N
# data_2017 = read_excel("Data\\Data January 2017.xlsx", na = "-")
# write.csv(data_2017, "Data\\Data_January_2017.csv")

# # L
# data_2017 = read_excel("Data/Data January 2017.xlsx", na = "-")
# write.csv(data_2017, "Data/Data_January_2017.csv")

# L
original_data = fread("Data/Data_January_2017.csv", na.strings = "NA")
data = original_data

# N
original_data = fread("Data\\Data_January_2017.csv", na.strings = "NA")
data = original_data
  
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


# Data Preparation --------------------------------------------------------
  
# Online Account - NA to 0 
data$Online_account[is.na(data$Online_account)] = 0

# Recovered - "" to 0 and "X" to 1
data$Recovered[data$`Recovered`=="X"] = 1
data$Recovered[is.na(data$`Recovered`)] = 0

#Transform "Customer_since" to number of months
data$`Customer_since` = ymd(data$`Customer_since`) # failed to parse bei wenigen - wahrscheinlich nicht im ymd Format
data$`Customer_since_interval` = interval(ymd(data$`Customer_since`), ymd(20170201)) %/% months(1)

#Transform "Contract_start_date" to number of months
data$`Contract_start_date` = ymd(data$`Contract_start_date`)
data$`Contract_start_date_interval` = interval(ymd(data$`Contract_start_date`), ymd(20170201)) %/% months(1)

#Transform "Market area" to binary variables
data$`MA_Grundversorger` = unlist(lapply(data$Market_area, FUN = function(x) ifelse(x=="Grundversorger",1,0)))
data$`MA_Erweitert` = unlist(lapply(data$Market_area, FUN = function(x) ifelse(x=="Erweitertes Netzgebiet",1,0)))
data$`MA_Restlich` = unlist(lapply(data$Market_area, FUN = function(x) ifelse(x=="Restliches Bundesgebiet",1,0)))

## Feature Conversion
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
data$`MA_Grundversorger` = as.factor(data$`MA_Grundversorger`)
data$`MA_Erweitert` = as.factor(data$`MA_Erweitert`)
data$`MA_Restlich` = as.factor(data$`MA_Restlich`)
data$Recovered = as.factor(data$Recovered)
data$Churn = as.factor(data$Churn)
data$DBII = as.numeric(gsub(",", ".", gsub("\\.", "", data$DBII)))
data$Customer_since_interval = as.integer(data$Customer_since_interval)
data$Contract_start_date_interval = as.integer(data$Contract_start_date_interval)


## Modifications

#At 51 observations "Customer_since" starts later than "Contract start date" --> Replace "Customer_since" by "Contract_start_date"  
nrow(subset(data,data$Customer_since>data$Contract_start_date)) 
data$Customer_since <- if_else(data$Customer_since>data$Contract_start_date, data$Contract_start_date, data$Customer_since)

#If "Contract_start_date"==NA, insert "Customer_since" as "Contract_start_date"
data$Contract_start_date <- if_else(is.na(data$Contract_start_date), data$Customer_since, data$Contract_start_date)

#If "Customer_since"==NA, calculate "Customer_since" based on "Duration_of_customer_relationship"
data$Customer_since <- if_else(is.na(data$Customer_since),ymd(20170301)- months(data$Duration_of_customer_relationship),data$Customer_since)

#At 4 observations, the contract starts later than 2017-01-01 --> Delete cases
nrow(subset(data, data$Customer_since <= ymd(20170101)))
data <- subset(data, data$Customer_since <= ymd(20170101))

# Feature Engineering -------------------------------------------------------------

# Create feature "not_opted" 
# data[Opt_In_Mail == "0" & Opt_In_Post == "0" & Opt_In_Tel == "0", .N, by= Churn] 
#However, only non-churners have this combination, therefore this new feature would not be meaningful

#Create feature "international"
data$`International` = unlist(gregexpr("[0-9]{5}", data$`Zip_code`)) # Nochmal überprüfen, kurze PLZ müssen nicht unbedingt im Ausland sein
data$`International`[data$`International` == 1] = 0
data$`International`[data$`International` == -1] = 1

#Create feature "annual payment"
data$`Actual_payment` = data$Payment_on_account * 12 + data$Annual_account

#Create feature "Continuous relationship"
data$`Continuous_relationship` = ifelse(data$Contract_start_date==data$Customer_since, 1,0)
data$Continuous_relationship = as.factor(data$Continuous_relationship)

#Create feature "Digitally_savvy"
# data$Digitally_savvy = ifelse(data$Online_account=="1" & data$Opt_In_Mail=="1" & data$Age <= 50, 1,0) # 81 cases und alle Nicht-Churner 
# data$Digitally_savvy = as.factor(data$Digitally_savvy)
# data[Online_account == "1" & Opt_In_Mail == "1", .N, by = Churn] # Ebenfalls alle nicht Churner, wenn Age rausgenommen

# Imputation for Missing Values --------------------------------------------------------------

# Detect Percentage of NA's per feature
apply(data, 2, function(col)sum(is.na(col))/length(col))
md.pattern(data, plot= T)
md.pairs(data)

# Multiple Imputation 
imp_data = mice(data, m=5) # Fehlermeldung wg. nicht installiertem lattice package


# Outlier  ----------------------------------------------------------------

# Outlier Detection & Elimination

## Age
# Customers older than 105 years
data[Age >= 105, .N, by = Churn] # 14 Customers are 105 years old or older 
# Customers younger than 18 years
data[Age < 18, .N, by = Churn] # 9 Customers are younger than 18 years 
# Set to NA, since we interprete these factors as not meaningful
data[Age >= 105] = NA
data[Age >= 105] = NA


# Consumption
summary(data[, Consumption])
plot(data[, Consumption])
max(data$Consumption, na.rm= TRUE)
data[Consumption > 30000, .N, by = Client_type] # important to clarify what is really meant by consumption - counter reading at a given point in time which also factors in the consumption of the past years or really the consumption for a given time horizon by a single customer

# Payment on Account
# Annual Account


# PCA ---------------------------------------------------------------------

data_pca <- select_if(data, is.numeric)

pca <- prcomp(na.omit(data_pca), center = TRUE, scale = TRUE)
str(pca)
summary(pca)
fviz_eig(pca)
fviz_pca_biplot(pca)
biplot(pca)
loadings(pca)
pca$rotation
cor(data$age,data$Duration_of_customer_relationship)

# Data Exploration ----------------------------------------------------------

str(data)
summary(data)

summary(data[Client_type=="1", .(Client_type, Age)])

# Correlation Plot

# Churn Distribution Plot 
# Categorical features 
# Numerical features

# Explore numerical features, detect outliers
# Kriterium gut begründen


# Final Feature Dataset ----------------------------------------------------------




# Quick & Dirty Modeling ----------------------------------------------------------------

# Try Data Partitioning in case CV did not work
set.seed(42)
index <- createDataPartition(data$V1, p = 0.7, list = FALSE)
train_data <- data[index, ]
test_data  <- data[-index, ]



# Extensive Modeling --------------------------------------------------------

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

# define training control
train_control <- trainControl(method = "cv", number = 5)

# train the model on training set
model <- train(Churn ~ Client_type + Duration_of_customer_relationship,
               data = data,
               trControl = train_control,
               method = "glm",
               na.action = na.pass,
               family=binomial())

# print cv scores
summary(model)


# Import 2018 Data ----------------------------------------------------------------

data_2018 = read_excel("Data\\Data November 2018.xlsx", na = "-")
# write.csv(data_2018, "Data_November_2018.csv")

data_2018 = fread("Data\\Data_November_2018.csv", na.strings = "-")
