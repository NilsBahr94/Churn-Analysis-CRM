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
install.packages("gridExtra")
# Install dependencies for H20
install.packages("RCurl")
install.packages("bitops")
install.packages("rjson")
install.packages("statmod")
install.packages("tools")
install.packages("yaml")
install.packages("ModelMetrics")
# Install H20
# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
# # Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}
# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-xu/1/R")

# Finally, let's load H2O and start up an H2O cluster

# Load Packages ------------------------------------------------------------

require(readxl)
require(tidyverse)
require(dplyr)
require(data.table)
require(ggplot2)
require(caret)

require(xgboost)
require(klaR)
require(rlang)
require(mice)
require(corrplot)
require(dplyr)
require(factoextra)
require(VIM)
require(purrr)
require(corrplot)
require(gplots)
library(readr)     # for fast reading of input files
library(rsample)   # for splitting training and test data
library(recipes)   # for preprocessing
library(ggthemes)  # for additional plotting themes
library(e1071)
library(MLmetrics)
library(ISLR)
library(randomForest)
require(lubridate)

# Dependencies for H20
require(RCurl)
require(bitops)
require(rjson)
require(statmod)
require(tools)
require(yaml)
library(h2o)

# Import 2017 Data -------------------------------------------------------------

data_2017 = read_excel("Data/Data January 2017.xlsx", na = "-", col_types = c("text","guess","guess","text","guess","guess","guess","guess","guess","guess","guess","guess","guess","guess","guess","guess","guess","guess","guess","guess","guess","guess","guess"))
write.csv2(data_2017, "Data/Data_January_2017_3.csv")

# L
data = fread("Data/Data_January_2017_3.csv", na.strings = "NA", dec = ",")

# N
data = fread("Data\\Data_January_2017_3.csv", na.strings = "NA", dec = ",")


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

#Transform "Customer_since" to number of months
data$`Customer_since` = ymd(data$`Customer_since`) 
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
data$`Annual_account` = as.numeric(data$`Annual_account`)
data$`Bill_shock` = as.factor(data$`Bill_shock`)
data$`Online_account` = as.factor(data$`Online_account`)
data$`Opt_In_Mail` = as.factor(data$`Opt_In_Mail`)
data$`Opt_In_Post` = as.factor(data$`Opt_In_Post`)
data$`Opt_In_Tel` = as.factor(data$`Opt_In_Tel`)
data$Market_area = as.factor(data$Market_area)
data$`MA_Grundversorger` = as.factor(data$`MA_Grundversorger`)
data$`MA_Erweitert` = as.factor(data$`MA_Erweitert`)
data$`MA_Restlich` = as.factor(data$`MA_Restlich`)
data$Recovered = as.factor(data$Recovered)
data$Churn = as.character(data$Churn)
data$Churn[data$Churn == "0"] = "No"
data$Churn[data$Churn == "1"] = "Yes"
data$Churn = as.factor(data$Churn)
data$DBII = as.numeric(data$DBII)
data$Customer_since_interval = as.integer(data$Customer_since_interval)
data$Contract_start_date_interval = as.integer(data$Contract_start_date_interval)

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

# Imputation  --------------------------------------------------------------

# Detect Percentage of NA's per feature
apply(data, 2, function(col)sum(is.na(col))/length(col))
md.pattern(data, plot= T)
md.pairs(data)

# Multiple Imputation (only for private customers)
private_customers = subset(data[which(data$Client_type==0),])
corporate_customers = subset(data[which(data$Client_type==1),])

imp_data = mice(private_customers) 
private_customers <- complete(imp_data)

data <- rbind(private_customers,corporate_customers)


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

# Bar Chart 
data[, .(Client_type, 
         Bill_shock, 
         Online_account, 
         Opt_In_Mail, 
         Opt_In_Post, 
         Opt_In_Tel, 
         Market_area, 
         Recovered, 
         Continuous_relationship, 
         Churn)] %>% drop_na() %>%
  gather(x, y, Client_type:Continuous_relationship) %>%
  count(Churn, x, y) %>%
  ggplot(aes(x = y, y = n, fill = Churn, color = Churn)) +
  facet_wrap(~ x, ncol = 3, scales = "free") +
  geom_bar(stat = "identity", alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_color_tableau() +
  scale_fill_tableau()


# Churn Distribution for Continuous Features 

# Density Plots
data[,.(Age, 
        Duration_of_customer_relationship, 
        Consumption, 
        Notice_period, 
        Annual_account, 
        DBII, 
        Minimum_contract_term, 
        Customer_since_interval,
        Contract_start_date_interval, Churn)] %>% drop_na() %>%
  gather(x, y, Age:Contract_start_date_interval) %>%
  ggplot(aes(x = y, fill = Churn, color = Churn)) +
  facet_wrap(~ x, ncol = 3, scales = "free") +
  geom_density(alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_color_tableau() +
  scale_fill_tableau()


## Detailled Exploration of Single Features

#Age

  ggplot(data = data, aes(x = Age, fill = Churn, color = Churn)) +
  geom_density(alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_color_tableau() +
  scale_fill_tableau()

#Age 
#Violin plot
ggplot(data= data[complete.cases(data)], aes(x=Churn, y= Age)) + geom_violin() + ggtitle("Age Distribution of Non-Churners and Churners")

#Consumption
ggplot(data= data[complete.cases(data) & Consumption <= 10000], aes(x=Churn, y= Consumption)) + geom_violin() + ggtitle("Consumption Distribution of Non-Churners and Churners")


# Final Dataset for Modeling ----------------------------------------------------------

data = data[, .(Churn, 
                Client_type, 
                Bill_shock, 
                Online_account, 
                Opt_In_Mail, 
                Opt_In_Post, 
                Opt_In_Tel, 
                MA_Grundversorger,
                MA_Erweitert,
                MA_Restlich,
                Recovered, 
                Continuous_relationship, 
                Age, 
                Duration_of_customer_relationship, 
                Consumption, 
                Notice_period, 
                Annual_account, 
                DBII, 
                Minimum_contract_term, 
                Customer_since_interval,
                Contract_start_date_interval)]

# Create random training, validation, and test sets

# Set some input variables to define the splitting.
# Input 1. The data frame that you want to split into training, validation, and test.
# data

set.seed(562)

# Input 2. Set the fractions of the dataframe you want to split into training, 
# validation, and test.
fractionTraining   <- 0.60
fractionValidation <- 0.20
fractionTest       <- 0.20

# Compute sample sizes.
sampleSizeTraining   <- floor(fractionTraining   * nrow(data))
sampleSizeValidation <- floor(fractionValidation * nrow(data))
sampleSizeTest       <- floor(fractionTest       * nrow(data))

# Create the randomly-sampled indices for the dataframe. Use setdiff() to
# avoid overlapping subsets of indices.
indicesTraining    <- sort(sample(seq_len(nrow(data)), size=sampleSizeTraining))
indicesNotTraining <- setdiff(seq_len(nrow(data)), indicesTraining)
indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))
indicesTest        <- setdiff(indicesNotTraining, indicesValidation)

# Finally, output the three dataframes for training, validation and test.
train_data <- data[indicesTraining, ]
valid_data <- data[indicesValidation, ]
test_data  <- data[indicesTest, ]

dim(train_data)
dim(test_data)
dim(valid_data)

str(train_data)
str(valid_data)
str(test_data)


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

randomForest(Churn ~ ., data=train_data[, -4], ntree = 300)


# Extensive Modeling --------------------------------------------------------

# a) Naive Bayes -------------------

myControl = trainControl(method = "cv",
                         number = 5,
                         classProbs = TRUE)

model = train(Churn ~ ., data = data, method = "naive_bayes", trControl = myControl, na.action = na.pass)

plot(model)

# b) Random Forest -------------------

# c) XGBoost -------------------

## Data Preprocessing XGBOOst --------

# Create XGBoost specific dataset

xgb_data = data

## 1. Remove information about the target variable from the training data

# Not applicable here

## 2. Reduce the amount of redundant information

# Already done (deleted Customer_ID, V1, and so on)

## 3. Convert categorical information (like country) to a numeric format

# train_data$Churn = as.character(train_data$Churn)
# train_data$Churn[train_data$Churn == "No"] = "0"
# train_data$Churn[train_data$Churn == "Yes"] = "1"
# train_data$Churn = as.numeric(train_data$Churn)

## 4. Split dataset into testing and training subsets
## 5. Convert the cleaned dataframe to a Dmatrix

# train_data = train_data[, -19]
# test_data = test_data[, -19]
# 

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


# d) Logistic Regression ------------

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


# e) H2o ---------------------------------------------------------------------

h2o.init(nthreads = -1)

h2o.no_progress()

train_hf <- as.h2o(train_data)
valid_hf <- as.h2o(valid_data) 
test_hf <- as.h2o(test_data)

response <- "Churn"
features <- setdiff(colnames(train_hf), response)

summary(train_hf$Churn, exact_quantiles = TRUE) 
summary(valid_hf$Churn, exact_quantiles = TRUE) 
summary(test_hf$Churn, exact_quantiles = TRUE) 

aml <- h2o.automl(x = features, 
                  y = response,
                  training_frame = train_hf,
                  validation_frame = valid_hf,
                  balance_classes = F,
                  sort_metric = "mean_per_class_error", # or leave AUC and respecify what should be interpreted as positive
                  max_runtime_secs = 28000)

# View the AutoML Leaderboard
lb <- aml@leaderboard

best_model <- aml@leader

h2o.saveModel(best_model, "C:\\Users\\nilsb\\sciebo\\Master\\3. Semester\\CRM and Direct Marketing\\Project\\Churn-Analysis-CRM", force = TRUE)

# Prediction
pred <- h2o.predict(best_model, test_hf[, -1])

# Evaluation
# https://www.rdocumentation.org/packages/h2o/versions/2.4.3.11/topics/h2o.performance
# Optimize for 3*Sensitivity (Recall) + Specificity (Selectivity)

# Mean per class error
h2o.mean_per_class_error(best_model, train = TRUE, valid = TRUE, xval = TRUE)
h2o.auc(best_model, train = TRUE)
h2o.auc(best_model, valid = TRUE)
h2o.auc(best_model, xval = TRUE)

# Confusion matrix on validation data
h2o.confusionMatrix(best_model, valid = TRUE)
perf <- h2o.performance(best_model, test_hf) 
h2o.confusionMatrix(perf)
plot(perf)

# Metrics Overview
metrics <- as.data.frame(h2o.metric(perf))
View(metrics)

# Metrics Plot
metrics %>%
  gather(x, y, f1:tpr) %>%
  ggplot(aes(x = threshold, y = y, group = x)) +
  facet_wrap(~ x, ncol = 2, scales = "free") +
  geom_line()

# Specific Metrics
recall = h2o.recall() #insert H20ModelMetrics Object
specificity = h2o.specificity()
# Compute final metric
final_metric = 3*recall+specificity 


# Logistic Regression -----------------------------------------------------


# Cluster Analysis --------------------------------------------------------

#Select subset of features (namely all except for client_ID and Zip_code)
churners = data[which(data$Churn=="Yes"),]

cluster_df = (churners[,c("Client_type", 
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
                          
                          "International", 
                          "MA_Grundversorger", 
                          "MA_Erweitert", 
                          "MA_Restlich")])

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
cluster_df$International = as.numeric(cluster_df$International)
cluster_df$MA_Grundversorger = as.numeric(cluster_df$MA_Grundversorger)
cluster_df$MA_Erweitert = as.numeric(cluster_df$MA_Erweitert)
cluster_df$MA_Restlich = as.numeric(cluster_df$MA_Restlich)

#Execute HDBSCAN clustering algorithm
hdb <- hdbscan(cluster_df, minPts = 10)

hdb$cluster

plot(hdb)

result = cbind(test[test$Churn==1,], hdb$cluster)

# Import 2018 Data ----------------------------------------------------------------

data_2018 = read_excel("Data\\Data November 2018.xlsx", na = "-")
# write.csv(data_2018, "Data_November_2018.csv")

data_2018 = fread("Data\\Data_November_2018.csv", na.strings = "-")





# Deprecated --------------------------------------------------------------


# Multiple Imputation 
imp_data = mice(data[data$Client_type == 0], method = "cart") # Imputation only for private customers
data_impute <- complete(imp_data)
write.csv(data_impute, "Data/Data_January_2017_Imputed_CART.csv")
apply(data_impute, 2, function(col)sum(is.na(col))/length(col))

data_firms = data[Client_type == 1]
imputed_data = base::rbind(data_impute, imp_data)
