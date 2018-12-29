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
install.packages("corrplot")
install.packages("gplots")
install.packages("rsample")
install.packages("yardstick")
install.packages("ggthemes")
install.packages("e1071")
install.packages("MLmetrics")
install.packages("ISLR")
install.packages("randomForest")


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
require(corrplot)
require(gplots)
library(readr)     # for fast reading of input files
library(mice)      # mice package for Multivariate Imputation by Chained Equations (MICE)
library(rsample)   # for splitting training and test data
library(recipes)   # for preprocessing
library(yardstick) # for evaluation
library(ggthemes)  # for additional plotting themes
library(e1071)
library(MLmetrics)
library(ISLR)
library(randomForest)


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
data$Churn = as.character(data$Churn)
data$Churn[data$Churn == "0"] = "No"
data$Churn[data$Churn == "1"] = "Yes"
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

data[ International == 1, .N, by = MA_Erweitert] # Auch checken, ob erweiterten Netzgebiet
data[ International == 1, .N, by = MA_Restlich] 

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
# imp_data = mice(data) # Fehlermeldung wg. nicht installiertem lattice package
# train_data_impute <- complete(imp_data, "long")

# Outlier ----------------------------------------------------------------

# Outlier Detection & Elimination

summary(data)

## Age
plot(data[, Age])
# Customers older than 105 years
data[Age >= 105, .N, by = Churn] # 14 Customers are 105 years old or older 
# Customers younger than 18 years
data[Age < 18, .N, by = Churn] # 9 Customers are younger than 18 years 
# Set to NA, since we interprete these factors as not meaningful
data$Age[data$Age >= 105] = NA 
data$Age[data$Age < 18] = NA

## Consumption
plot(data[, Consumption])
summary(data[, Consumption])
data[Consumption > 30000, .N, by = Client_type] # important to clarify what is really meant by consumption - counter reading at a given point in time which also factors in the consumption of the past years or really the consumption for a given time horizon by a single customer
data[Consumption < 100, .N] # 233

## Payment on Account
plot(data[, Payment_on_account])
summary(data[, Payment_on_account])
data[Payment_on_account > 20000, .N, by = Client_type] 
#All Clients with a Payment larger than 20,000 are firms, which is plausable
#Therefore, these observations should not be classified as outliers and not eliminated 
data[Payment_on_account == 0, .N, by = Client_type] #61 observations that do not have a payment on account, maybe eliminate?
View(data[Payment_on_account == 0]) # However, those customers have consumed sth indeed - but they just did not have to pay for it
data[Payment_on_account == 0, .N, by = Churn] # 15 Churners here of 61

## Actual Payment
plot(data[, Actual_payment])
# Very high Payment
plot(data[Actual_payment <= 1000000, Actual_payment])
data[Actual_payment > 1000000, .N]
data[Actual_payment > 1000000000] # Actual Payment is too high, this must be a fault. Annual_Account is also that high
data[Actual_payment > 1000000000, .N]
data[Contract_ID == "3018420", .N]

# To Do Eliminate

# Very low Payments
data[Actual_payment < -100000] # does that make sense?
data[Actual_payment < -100000, .N, by = Churn] 

# Filter out observation with unreasonably high and low Actual Payment
# data = data[Actual_payment < 10000000000 & Actual_payment > -100000]

## Annual Account
plot(data[, Annual_account]) # Same picture as for Annual Payment

## DBII
plot(data[, DBII]) # some observations with neg. DBII, but do not appear to be outliers 
data[DBII < 0]

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

# Correlation Plot
corrplot.mixed(corr=cor(data[, .(Age, 
                                 Duration_of_customer_relationship, 
                                 Notice_period,
                                 Consumption, 
                                 Payment_on_account,
                                 Annual_account, 
                                 DBII, 
                                 Minimum_contract_term, 
                                 Customer_since_interval,
                                 Contract_start_date_interval, 
                                 Actual_payment)], use="complete.obs", method="pearson"), 
               upper="ellipse", 
               tl.pos="lt")


# Churn Distribution for Categorical Features
data %>%
  select(-Age, 
         -Duration_of_customer_relationship, 
         -Consumption, 
         -Notice_period, 
         -Annual_account, 
         -DBII, 
         -Minimum_contract_term, 
         -Customer_since_interval,
         -Contract_start_date_interval)  %>%
  select(Churn, everything()) %>%
  gather(x, y, gender:PaymentMethod) %>%
  count(Churn, x, y) %>%
  ggplot(aes(x = y, y = n, fill = Churn, color = Churn)) +
  facet_wrap(~ x, ncol = 4, scales = "free") +
  geom_bar(stat = "identity", alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_color_tableau() +
  scale_fill_tableau()


# Churn Distribution for Continous Features 

ggplot(data= data, aes(x = Age, fill = Churn, color = Churn))+
  geom_density(alpha = 0.5)  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_color_tableau() +
  scale_fill_tableau()


data[,.(Age, 
        Duration_of_customer_relationship, 
        Consumption, 
        Notice_period, 
        Annual_account, 
        DBII, 
        Minimum_contract_term, 
        Customer_since_interval,
        Contract_start_date_interval)] %>%
ggplot(aes(x = Age, fill = Churn, color = Churn))+
  geom_density(alpha = 0.5) + facet_wrap(~ Churn, ncol = 3, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_color_tableau() +
  scale_fill_tableau()

data[,.(Age, 
         Duration_of_customer_relationship, 
         Consumption, 
         Notice_period, 
         Annual_account, 
         DBII, 
         Minimum_contract_term, 
         Customer_since_interval,
         Contract_start_date_interval)] %>%
  gather(x, y, MonthlyCharges:TotalCharges) %>%
  ggplot(aes(x = y, fill = Churn, color = Churn)) +
  facet_wrap(~ x, ncol = 3, scales = "free") +
  geom_density(alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_color_tableau() +
  scale_fill_tableau()


## Detailled Exploration of Single Features

#Age 
#Violin plot
ggplot(data= data[complete.cases(data)], aes(x=Churn, y= Age)) + geom_violin() + ggtitle("Age Distribution of Non-Churners and Churners")

#Consumption
ggplot(data= data[complete.cases(data) & Consumption <= 10000], aes(x=Churn, y= Consumption)) + geom_violin() + ggtitle("Consumption Distribution of Non-Churners and Churners")


# Final Dataset for Modeling ----------------------------------------------------------




# Quick & Dirty Modeling ----------------------------------------------------------------

# First do simple Training / Test Split
set.seed(456) 
smp_siz = floor(0.70*nrow(data)) # 70% of data for training, 30% for testing
train_ind = sample(seq_len(nrow(data)), size = smp_siz) 
train_data=data[train_ind,]
test_data=data[-train_ind,]  

# 1) Naive Bayes ----------------------------------------------------------

nb_classifier = naiveBayes(Churn ~ . , data = train_data, laplace=1)
nb_pred = predict(nb_classifier, newdata = test_data[-22])
test_data$Churn_pred = predict(nb_classifier, newdata = test_data[-22])
# test_data[, .N, by = Churn]
# test_data[, .N, by = Churn_pred]

ConfusionMatrix(y_pred = nb_pred, y_true=test_data$Churn) # nicht die selbe length: "Supplied 21683 items to be assigned to 21684 items of column 'Churn_pred' (recycled leaving remainder of 1 items)."


get_model_performance = function (predict_variable, y_reference = test_data$Churn){
  require(MLmetrics)
  require(caret)
  
  Table_Predictions = table(predict_variable)
  Confusion_Matrix = confusionMatrix(data = predict_variable, reference = y_reference)
  Precision_Score = Precision(y_true = y_reference, y_pred = predict_variable)
  Recall_Score = Recall(y_true = y_reference, y_pred = predict_variable)
  F1 = F1_Score(y_true = y_reference, y_pred = predict_variable)
  Accuracy_Score = Accuracy(y_true = y_reference, y_pred = predict_variable)
  
  print(list(Table_Predictions=Table_Predictions, Confusion_Matrix=Confusion_Matrix, Precision_Score=Precision_Score, Recall_Score=Recall_Score, F1=F1, Accuracy_Score=Accuracy_Score))
}

get_model_performance(predict_variable = nb_pred, y_reference = test_data$Churn)



# 2) Random Forest --------------------------------------------------------

randomForest(Churn ~ ., data=train_data, ntree = 300)


# Extensive Modeling --------------------------------------------------------

# a) Naive Bayes -------------------

myControl = trainControl(method = "cv",
                         number = 5,
                         classProbs = TRUE)

model = train(Churn ~ ., data = data, method = "naive_bayes", trControl = myControl, na.action = na.pass)

plot(model)

# b) Random Forest -------------------

# c) Support Vector Machine -------------------

# d) XGBoost -------------------

# train_data = train_data[, -19]
# test_data = test_data[, -19]
# 
# train_data$Churn = as.character(train_data$Churn)
# train_data$Churn[train_data$Churn == "No"] = "0"
# train_data$Churn[train_data$Churn == "Yes"] = "1"
# train_data$Churn = as.numeric(train_data$Churn)
# 
# train_data = as.matrix(train_data)
# train_data = xgb.DMatrix(train_data)

params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

xgbcv <- xgb.cv( params = params, data = as.matrix(train_data), nrounds = 100, nfold = 5)

# Get the evaluation log 
elog <- as.data.frame(xgboost_training$evaluation_log)
elog

# Determine and print how many trees minimize training and test error
number_trees <- elog %>% 
  summarize(ntrees.train = which.min(train_rmse_mean),   # find the index of min(train_rmse_mean)
            ntrees.test  = which.min(test_rmse_mean))   # find the index of min(test_rmse_mean)

# The number of trees to use, as determined by xgb.cv
set.seed(123)
ntrees_train <- number_trees$ntrees.train
ntrees_test <- number_trees$ntrees.test

# Train XGBoost Model 
xgb_model <- xgboost(data = as.matrix(train_data), # training data as matrix
                     label = train_data$Churn,  # column of outcomes
                     nrounds = ntrees_train,   # number of trees to build
                     objective = "binary:logistic", # objective
                     # the higher, the lower the rmse
                     eta = 0.3,
                     verbose = 0,  # silent,
                     eval_metric="error",
                     # set booster from default (="gbtree") to "dart" to enable further parameter config
                     booster = "gbtree",
                     sample_type = "weighted")

# Predict on the Training Data 
training_data$pred <- predict(xgb_model, newdata=as.matrix(train_data[,-21]))

# e) Catboost -------------------

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
