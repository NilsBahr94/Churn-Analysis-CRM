# Install Packages --------------------------------------------------------

install.packages("kernlab")
install.packages("DMwR")
install.packages("ROSE")
install.packages("glmnet")
install.packages("rpart")
install.packages("hdbscan")
install.packages("plyr")
install.packages("DMwR")
install.packages("readxl")
install.packages("tidyverse")
install.packages("data.table")
install.packages("ggplot2")
install.packages("caret", dependencies = TRUE)
install.packages("lubridate")
install.packages("xgboost")
# install.packages("klaR", dependencies = TRUE)
install.packages("rlang")
install.packages("mice", dependencies = TRUE)
install.packages("corrplot")
install.packages("dplyr")
install.packages("factoextra")
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

require(kernlab)
require(rpart)
require(dbscan)
require(plyr)
require(DMwR)
require(readxl)
require(tidyverse)
require(data.table)
require(ggplot2)
require(caret)
require(DMwR)
require(ROSE)
require(glmnet)
require(gridExtra)

require(xgboost)
# require(klaR)
require(rlang)
require(mice)
require(corrplot)
require(dplyr)
require(factoextra)
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


# data_2017 = read_excel("Data/Data January 2017.xlsx", na = "-", col_types = c("text","guess","guess","text","guess","guess","guess","guess","guess","guess","guess","numeric","guess","guess","guess","guess","guess","guess","guess","guess","guess","guess","guess"))
# write.csv2(data_2017, "Data/Data_January_2017_3.csv")


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

#If "Contract_start_date"==NA, insert "Customer_since" as "Contract_start_date"
data$`Customer_since` = ymd(data$`Customer_since`) 
data$`Contract_start_date` = ymd(data$`Contract_start_date`)
data$Contract_start_date <- if_else(is.na(data$Contract_start_date), data$Customer_since, data$Contract_start_date)

#If "Customer_since"==NA, calculate "Customer_since" based on "Duration_of_customer_relationship"
data$Customer_since <- if_else(is.na(data$Customer_since),ymd(20170301)- months(data$Duration_of_customer_relationship),data$Customer_since)

#At 4 observations, the contract starts later than 2017-01-01 --> Delete cases
nrow(subset(data, data$Customer_since <= ymd(20170101)))
data <- subset(data, data$Customer_since <= ymd(20170101))

#At 51 observations "Customer_since" starts later than "Contract start date" --> Replace "Customer_since" by "Contract_start_date"  
nrow(subset(data,data$Customer_since>data$Contract_start_date)) 
data$Customer_since <- if_else(data$Customer_since>data$Contract_start_date, data$Contract_start_date, data$Customer_since)

#Transform "Customer_since" to number of months
data$`Customer_since_interval` = interval(ymd(data$`Customer_since`), ymd(20170201)) %/% months(1)

#Transform "Contract_start_date" to number of months
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
data$Age = as.numeric(data$Age)
data$Duration_of_customer_relationship = as.numeric(data$Duration_of_customer_relationship)
data$Minimum_contract_term = as.numeric(data$Minimum_contract_term)
data$Notice_period = as.numeric(data$Notice_period)
data$Automatic_contract_extension = as.numeric(data$Automatic_contract_extension)
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
data$Customer_since_interval = as.numeric(data$Customer_since_interval)
data$Contract_start_date_interval = as.numeric(data$Contract_start_date_interval)

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
# apply(data, 2, function(col)sum(is.na(col))/length(col))
# md.pattern(data, plot= T)
# md.pairs(data)
# 
# # Multiple Imputation (only for private customers)
# private_customers = subset(data[which(data$Client_type==0),])
# corporate_customers = subset(data[which(data$Client_type==1),])
# 
# imp_data = mice(private_customers) 
# private_customers <- complete(imp_data)
# 
# data <- rbind(private_customers,corporate_customers)


# Outlier ----------------------------------------------------------------

# Outlier Detection & Elimination
summary(data)

# Set to NA, since we interprete these factors as not meaningful
data$Age[data$Age >= 105] = NA 
data$Age[data$Age < 18] = NA

## Age
plot(data[, Age])
# Customers older than 105 years
data[Age >= 105, .N, by = Churn] # 14 Customers are 105 years old or older 
# Customers younger than 18 years
data[Age < 18, .N, by = Churn] # 9 Customers are younger than 18 years 


## Consumption
plot(data[, Consumption])
summary(data[, Consumption])
data[Consumption > 20000, .N, by = Client_type] # important to clarify what is really meant by consumption - counter reading at a given point in time which also factors in the consumption of the past years or really the consumption for a given time horizon by a single customer
data[Consumption < 100, .N] # 233

## Payment on Account
plot(data[, Payment_on_account])
summary(data[, Payment_on_account])
data[Payment_on_account > 20000, .N, by = Client_type] 
#All Clients with a Payment larger than 20,000 are firms, which is plausable
#Therefore, these observations should not be classified as outliers and not eliminated 
data[Payment_on_account == 0, .N, by = Client_type] #61 observations that do not have a payment on account, maybe eliminate?
View(data[Payment_on_account == 0]) # However, those customers have consumed sth indeed - but they just did not have to pay for it, maybe they produced their own power (solar cells) and sold it again to the provider 
data[Payment_on_account == 0, .N, by = Churn] # 15 Churners here of 61

## Actual Payment
plot(data[, Actual_payment])
# Very high Payment
plot(data[Actual_payment <= 6000000, Actual_payment])
data[Actual_payment > 1000000, .N]
# data[Actual_payment > 1000000000] # Actual Payment is too high, this must be a fault. Annual_Account is also that high
# data[Actual_payment > 1000000000, .N]
# data[Contract_ID == "3018420", .N]

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

table(data$Churn) / nrow(data)

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
p1 = data[Age > 18 & Age <= 105 ] %>% drop_na() %>%
  ggplot(aes(x = Age, fill = Churn, color = Churn)) +
  geom_density(alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_color_tableau() +
  scale_fill_tableau()

# Consumption
p2 = data[Consumption <= 15000] %>% drop_na() %>%
  ggplot(aes(x = Consumption, fill = Churn, color = Churn)) +
  geom_density(alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_color_tableau() +
  scale_fill_tableau()

# Consumption
p3 = data %>% drop_na() %>%
  ggplot(aes(x = Duration_of_customer_relationship, fill = Churn, color = Churn)) +
  geom_density(alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top") +
  scale_color_tableau() +
  scale_fill_tableau()

grid.arrange(p1, p2, p3, ncol = 3)

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
                Consumption, 
                Notice_period, 
                Annual_account, 
                DBII, 
                Minimum_contract_term, 
                Customer_since_interval,
                Contract_start_date_interval)]


# Convert features of the preparaed data into right format for modeling 
data$Churn = as.factor(data$Churn)
data$Client_type = as.numeric(as.character(data$Client_type))
data$Bill_shock = as.numeric(as.character(data$Bill_shock))
data$Online_account = as.numeric(as.character(data$Online_account))
data$Opt_In_Mail = as.numeric(as.character(data$Opt_In_Mail))
data$Opt_In_Post = as.numeric(as.character(data$Opt_In_Post))
data$Opt_In_Tel = as.numeric(as.character(data$Opt_In_Tel))
data$MA_Grundversorger = as.numeric(as.character(data$MA_Grundversorger))
data$MA_Erweitert = as.numeric(as.character(data$MA_Erweitert))
data$MA_Restlich = as.numeric(as.character(data$MA_Restlich))
data$Recovered = as.numeric(as.character(data$Recovered))
data$Continuous_relationship = as.numeric(as.character(data$Continuous_relationship))
data$Age = as.numeric(as.character(data$Age))
data$Duration_of_customer_relationship = as.numeric(as.character(data$Duration_of_customer_relationship))

str(data)

# Change order of factor levels such that "Yes" is interpreted as positive and "No" is interpreted as negative 
levels(data$Churn)
data$Churn = factor(data$Churn, levels = c("Yes", "No"))
levels(data$Churn)

# Do *one* of the next two approaches
# 1) Create training, validation and test set
set.seed(156)
split1 <- createDataPartition(data$Churn, p=.7, list=FALSE)
train_data <- data[split1,]
test_data  <- data[-split1,]

# 2) Training, validation, test set
set.seed(365)
split1 <- createDataPartition(data$Churn, p=.6, list=FALSE)
train_data <- data[split1,]
other  <- data[-split1,]

set.seed(234)
split2 <- createDataPartition(other$Churn, p=.5, list=FALSE)
eval_data = other[split2,]
test_data = other[-split2,]

# Extensive Modeling --------------------------------------------------------


# a) GLM ---------------------------------------------------------------------

# Also have different presampling methods available and try them out one after the other
# GLM work good with small dataset, with resampling dataset becomes small indeed

glm_grid <- expand.grid(alpha= c(0, 0.2, 0.4, 0.6, 0.8),
                        lambda = c(0.0001, 0.001, 0.1))

glm_tc = trainControl(method = "cv", number = 5,
                      summaryFunction = twoClassSummary,
                      classProbs = TRUE, # Super important!
                      verboseIter = FALSE,
                      sampling = "down")

# GLM Model only works when NA's are excluded
train_data_ex_NA = na.omit(train_data)

set.seed(42)
glm_model = train(x= train_data_ex_NA[, -1],
                 y= train_data_ex_NA$Churn,
                 method="glmnet",
                 trControl=glm_tc,
                 tuneGrid=glm_grid,
                 metric="ROC") # ROC, Kappa does not work

print(glm_model)
plot(glm_model)

# Predicting the Test set results
test_data_ex_NA = na.omit(test_data)
glm_pred = predict(glm_model, newdata = test_data_ex_NA[,-1], type = 'raw')

confusionMatrix(glm_pred, test_data_ex_NA$Churn)

glm_model_results = as.data.table(glm_model$results)
glm_model_results$crm_eval = 3*glm_model_results$Sens + glm_model_results$Spec
max(glm_model_results$crm_eval)

# b) SVM -------------------

# Data Preprocessing for SVM: scale data to [-1, 1]
# Training Data
scale_train_data = as.data.table(ReScaling(train_data[, 13:21], t.mn = 0, t.mx = 1)) # Scale continuous feature
scale_train_data = cbind(train_data[, 1:12], scale_train_data)

# Test Data
scale_test_data = as.data.table(ReScaling(test_data[, 13:21], t.mn = 0, t.mx = 1)) # Scale continuous feature
scale_test_data = cbind(test_data[, 1:12], scale_test_data)

set.seed(743) 
svm_grid_radial <- expand.grid(sigma = c(0.05, 0.0456, 0.0577), C = c(1.5,1.596,1.65,1.89,1.95,2,2.2,2.44))

svm_tc <- trainControl(method = "cv", number = 5,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE) 

svm_Radial_Grid <- train(x = scale_train_data[,-1], 
                         y = scale_train_data$Churn, 
                         method = "svmRadial",
                         trControl=svm_tc,
                         tuneGrid = svm_grid_radial)

#Use the predictions on the data
svm_pred <- predict(model_svm, newdata = scale_test_data[, -1], type ="raw")

print(svm_model)
plot(svm_model)

# Predicting the Test set results
svm_pred = predict(svm_model, newdata = scale_test_data[,-1])

confusionMatrix(svm_pred, scale_test_data$Churn)

svm_model_results = as.data.table(svm_model$results)
svm_model_results$crm_eval = 3*svm_model_results$Sens + svm_model_results$Spec
max(svm_model_results$crm_eval)

# c) Decision Tree -------------------

dt_grid <- expand.grid(cp= seq(0, 0.8, length = 10)) 
dt_tc <- trainControl(method = "cv", number = 5,
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE,
                           allowParallel=TRUE,
                           sampling = "down") # Use Subsampling due to class imbalance

dt_model = train(x= train_data[, -1],
        y= train_data$Churn,
        method="rpart",
        trControl=dt_tc,
        tuneGrid=dt_grid,
        metric="ROC") # ROC, Kappa does not work

print(dt_model)
plot(dt_model)

# Predicting the Test set results
y_pred = predict(dt_model, newdata = test_data[,-1], type = 'raw')

confusionMatrix(y_pred, test_data$Churn)

dt_model_results = as.data.table(dt_model$results)
dt_model_results$crm_eval = 3*dt_model_results$Sens + dt_model_results$Spec
max(dt_model_results$crm_eval)


# d) Random Forest -------------------

# Only possible with imputed data because NAs are no accepted

imp_data= fread("C:\\Users\\nilsb\\sciebo\\Master\\3. Semester\\CRM and Direct Marketing\\Project\\Churn-Analysis-CRM\\Data\\Data_January_2017_3_imputed.csv")

str(imp_data)

## Feature Conversion
#Convert features into right data types
imp_data$Contract_ID = as.character(imp_data$Contract_ID)
imp_data$`Zip_code` = as.character(imp_data$`Zip_code`)
imp_data$`Client_type` = as.factor(imp_data$`Client_type`)
imp_data$Age = as.numeric(imp_data$Age)
imp_data$Duration_of_customer_relationship = as.numeric(imp_data$Duration_of_customer_relationship)
imp_data$Minimum_contract_term = as.numeric(imp_data$Minimum_contract_term)
imp_data$Notice_period = as.numeric(imp_data$Notice_period)
imp_data$Automatic_contract_extension = as.numeric(imp_data$Automatic_contract_extension)
imp_data$Consumption = as.numeric(imp_data$Consumption) 
imp_data$Payment_on_account = gsub( ",", ".", imp_data$Payment_on_account)
imp_data$Payment_on_account = as.numeric(imp_data$Payment_on_account)
imp_data$Annual_account = gsub( ",", ".", imp_data$Annual_account)
imp_data$`Annual_account` = as.numeric(imp_data$`Annual_account`)
imp_data$`Bill_shock` = as.factor(imp_data$`Bill_shock`)
imp_data$`Online_account` = as.factor(imp_data$`Online_account`)
imp_data$`Opt_In_Mail` = as.factor(imp_data$`Opt_In_Mail`)
imp_data$`Opt_In_Post` = as.factor(imp_data$`Opt_In_Post`)
imp_data$`Opt_In_Tel` = as.factor(imp_data$`Opt_In_Tel`)
imp_data$Market_area = as.factor(imp_data$Market_area)
imp_data$`MA_Grundversorger` = as.factor(imp_data$`MA_Grundversorger`)
imp_data$`MA_Erweitert` = as.factor(imp_data$`MA_Erweitert`)
imp_data$`MA_Restlich` = as.factor(imp_data$`MA_Restlich`)
imp_data$Recovered = as.factor(imp_data$Recovered)
imp_data$Churn = as.character(imp_data$Churn)
imp_data$Churn[imp_data$Churn == "0"] = "No"
imp_data$Churn[imp_data$Churn == "1"] = "Yes"
imp_data$Churn = as.factor(imp_data$Churn)
imp_data$DBII = gsub(",", ".", imp_data$DBII)
imp_data$DBII = as.numeric(imp_data$DBII)
imp_data$Customer_since_interval = as.numeric(imp_data$Customer_since_interval)
imp_data$Contract_start_date_interval = as.numeric(imp_data$Contract_start_date_interval)

# Feature Engineering
# Create feature "not_opted" 
# data[Opt_In_Mail == "0" & Opt_In_Post == "0" & Opt_In_Tel == "0", .N, by= Churn] 
#However, only non-churners have this combination, therefore this new feature would not be meaningful

#Create feature "international"
imp_data$`International` = unlist(gregexpr("[0-9]{5}", imp_data$`Zip_code`)) # Nochmal überprüfen, kurze PLZ müssen nicht unbedingt im Ausland sein
imp_data$`International`[imp_data$`International` == 1] = 0
imp_data$`International`[imp_data$`International` == -1] = 1

imp_data[ International == 1, .N, by = MA_Erweitert] # Auch checken, ob erweiterten Netzgebiet
imp_data[ International == 1, .N, by = MA_Restlich] 

#Create feature "annual payment"
imp_data$`Actual_payment` = imp_data$Payment_on_account * 12 + imp_data$Annual_account

#Create feature "Continuous relationship"
imp_data$`Continuous_relationship` = ifelse(imp_data$Contract_start_date==imp_data$Customer_since, 1,0)
imp_data$Continuous_relationship = as.factor(imp_data$Continuous_relationship)


# Final Set of Features 

imp_data = imp_data[, .(Churn, 
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

data_rf = imp_data
data_rf$Churn = as.factor(data_rf$Churn)

apply(data_rf[Client_type == 0], 2, function(col)sum(is.na(col))/length(col))
# Because for firms there are NA's in age, eliminate those from those dataset 
data_rf_no_na = data_rf[Client_type == 0]
apply(data_rf_no_na, 2, function(col)sum(is.na(col))/length(col))

# Convert factor names of Churn to caret compatible format (1 and 0 are not allowed)
data_rf_no_na$Churn = as.character(data_rf_no_na$Churn)
data_rf_no_na$Churn[data_rf_no_na$Churn == "0"] = "No"
data_rf_no_na$Churn[data_rf_no_na$Churn == "1"] = "Yes"
data_rf_no_na$Churn = as.factor(data_rf_no_na$Churn)

# Change order of factor levels such that "Yes" is interpreted as positive and "No" is interpreted as negative 
levels(data_rf_no_na$Churn)
data_rf_no_na$Churn = factor(data_rf_no_na$Churn, levels = c("Yes", "No"))
levels(data_rf_no_na$Churn)

# Do Training and Test Split for Random Forest

# Create training, validation and test set
set.seed(156)
split1 <- createDataPartition(data_rf_no_na$Churn, p=.7, list=FALSE)
data_rf_train <- data_rf_no_na[split1,]
data_rf_test  <- data_rf_no_na[-split1,]

# Downsampling
cv_rf <- trainControl(method = "cv", number = 5,
                              summaryFunction = twoClassSummary,
                              classProbs = TRUE,
                              allowParallel=TRUE,
                              sampling = "rose") # Use Subsampling due to class imbalance


# Define Hyperparameter Grid; changeable parameters: nrounds, max_depth, eta, gamma, colsample_bytree, min_child_weight, subsample
rf_grid <- expand.grid(mtry= c(10, 20, 25, 30, 35, 40, 50)) 

# Train Model
set.seed(45)
rf_model <- train(x= data_rf_train[, -1],
                  y= as.factor(data_rf_train$Churn),
                  method="rf",
                  trControl=cv_rf,
                  tuneGrid=rf_grid,
                  metric="ROC")

rf_model
plot(rf_model)
rf_model_results = as.data.table(rf_model$results)
rf_model_results$crm_eval = 3*rf_model_results$Sens + rf_model_results$Spec
max(rf_model_results$crm_eval)

pred_rf = predict(rf_model, newdata = data_rf_test[,-1])
confusionMatrix(pred_rf, data_rf_test$Churn)

# e) XGBoost Caret -------------------

xgb_train_data = train_data
# xgb_eval_data = eval_data
xgb_test_data = test_data

nrow(xgb_train_data)
xgb_train_data[,.N, by = Churn]
# nrow(xgb_eval_data)
# xgb_eval_data[,.N, by = Churn]
nrow(xgb_test_data)
xgb_test_data[,.N, by = Churn]

# Different forms of resampling to deal with unbalanced classes 
# Down sampling
cv_ctrl_down <- trainControl(method = "cv", number = 10,
                            summaryFunction = twoClassSummary,
                            classProbs = TRUE,
                            allowParallel=TRUE,
                            sampling = "down") # Use Subsampling due to class imbalance

# Up sampling
cv_ctrl_up <- trainControl(method = "cv", number = 10,
                             summaryFunction = twoClassSummary,
                             classProbs = TRUE,
                             allowParallel=TRUE,
                             sampling = "up") # Use Subsampling due to class imbalance

# SMOTE
cv_ctrl_smote <- trainControl(method = "cv", number = 10,
                              summaryFunction = twoClassSummary,
                              classProbs = TRUE,
                              allowParallel=TRUE,
                              sampling = "smote") # Use Subsampling due to class imbalance

# ROSE
cv_ctrl_rose <- trainControl(method = "cv", number = 5,
                              summaryFunction = twoClassSummary,
                              classProbs = TRUE,
                              allowParallel=TRUE,
                              sampling = "rose")

# No resampling
cv_ctrl_no <- trainControl(method = "cv", number = 5,
                             summaryFunction = twoClassSummary,
                             classProbs = TRUE,
                             allowParallel=TRUE)


# Different Hyperparameter Grid 
# changeable parameters: nrounds, max_depth, eta, gamma, colsample_bytree, min_child_weight, subsample
xgb_grid1 <- expand.grid(nrounds = c(10, 20, 50, 100),
                        max_depth = c(1, 2, 3, 4),
                        eta = c(0.005, 0.01, 0.05, 0.1),
                        gamma = c(0, 0.1, 0.2),
                        colsample_bytree = c(0.4, 0.6, 1),
                        min_child_weight = c(1, 2),
                        subsample = 1)

xgb_grid2 <- expand.grid(nrounds = c(20, 50, 100, 150),
                        max_depth = c(1, 2, 4, 6),
                        eta = c(0.01, 0.05, 0.1),
                        gamma = c(0, 0.1),
                        colsample_bytree = c(0.5, 1),
                        min_child_weight = 1,
                        subsample = 1)

# Train Model
set.seed(45)

xgb_model_resampling = function(resampling_method, tune_grid){
  xgb_model = 
        train(x= as.matrix(xgb_train_data[, -1]),
        y= as.factor(xgb_train_data$Churn),
        method="xgbTree",
        trControl=resampling_method,  #change between cv_ctrl_smote, cv_ctrl_down, cv_ctrl_up, cv_ctrl_rose 
        tuneGrid=tune_grid,
        verbose=T,
        metric="ROC", # ROC, Kappa does not work
        nthread =3)
  return(xgb_model)
} 

xgb_tune = xgb_model_resampling(resampling_method=cv_ctrl_no, tune_grid = xgb_grid2)
xgb_tune

# Predict with final model
xgb_pred = predict(xgb_tune, newdata = xgb_test_data[,-1])
confusionMatrix(xgb_pred, xgb_test_data$Churn)

###  Model Predictions and Performance
# Evaluate performance
xgb_tune_results = as.data.table(xgb_tune$results)
xgb_tune_results$crm_eval = 3*xgb_tune_results$Sens + xgb_tune_results$Spec
max(xgb_tune_results$crm_eval)

View(xgb_tune_results)
plot(xgb_tune)
xgb_tune$bestTune

evalResults <- data.frame(Churn = xgb_test_test$Churn)
evalResults$xgb <- predict(xgb_tune,
                           newdata = xgb_test_data[,-1],
                           type = "prob")


# f) H2o ---------------------------------------------------------------------

# 2) Training, validation, test set
set.seed(365)
split1 <- createDataPartition(data$Churn, p=.6, list=FALSE)
train_data <- data[split1,]
other  <- data[-split1,]

set.seed(234)
split2 <- createDataPartition(other$Churn, p=.5, list=FALSE)
eval_data = other[split2,]
test_data = other[-split2,]

# Reset train, eval and test data here
h2o_train_data = train_data
h2o_train_data$Churn = as.character(h2o_train_data$Churn)
h2o_train_data$Churn[h2o_train_data$Churn == "Yes"] = "Churn"
h2o_train_data$Churn[h2o_train_data$Churn == "No"] = "No_Churn"
h2o_train_data$Churn = as.factor(h2o_train_data$Churn)
h2o_eval_data = eval_data
h2o_eval_data$Churn = as.character(h2o_eval_data$Churn)
h2o_eval_data$Churn[h2o_eval_data$Churn == "Yes"] = "Churn"
h2o_eval_data$Churn[h2o_eval_data$Churn == "No"] = "No_Churn"
h2o_eval_data$Churn = as.factor(h2o_eval_data$Churn)
h2o_test_data = test_data
h2o_test_data$Churn = as.character(h2o_test_data$Churn)
h2o_test_data$Churn[h2o_test_data$Churn == "Yes"] = "Churn"
h2o_test_data$Churn[h2o_test_data$Churn == "No"] = "No_Churn"
h2o_test_data$Churn = as.factor(h2o_test_data$Churn)

#Execute h2o
h2o.init(nthreads = -1)
h2o.no_progress()

train_hf <- as.h2o(h2o_train_data)
valid_hf <- as.h2o(h2o_eval_data) 
test_hf <- as.h2o(h2o_test_data)

response <- "Churn"
features <- setdiff(colnames(train_hf), response)

summary(train_hf$Churn, exact_quantiles = TRUE) 
summary(valid_hf$Churn, exact_quantiles = TRUE) 
summary(test_hf$Churn, exact_quantiles = TRUE) 

# Define what is the positive class
train_hf$Churn = h2o.relevel(train_hf$Churn, "No_Churn")
valid_hf$Churn = h2o.relevel(valid_hf$Churn, "No_Churn")
test_hf$Churn = h2o.relevel(test_hf$Churn, "No_Churn")

# Modeling
aml <- h2o.automl(x = features, 
                  y = response,
                  training_frame = train_hf,
                  # nfolds = 5,
                  validation_frame = valid_hf,
                  balance_classes = F, # Make sure that set to TRUE
                  sort_metric = "AUC", 
                  max_runtime_secs = 3600)

# View the AutoML Leaderboard
lb <- aml@leaderboard
lb

# Save model that is best in regards of crm_eval_correct
aml@leaderboard # To Do 
best_model <- aml@leader
h2o.saveModel(best_model, "C:\\Users\\nilsb\\sciebo\\Master\\3. Semester\\CRM and Direct Marketing\\Project\\Churn-Analysis-CRM\\Models", force = TRUE)

# Prediction
pred <- h2o.predict(best_model, test_hf[, -1])

# Evaluation
# Confusion matrix on validation data
h2o.confusionMatrix(best_model, valid = TRUE)
perf <- h2o.performance(best_model, test_hf) 
h2o.confusionMatrix(perf)
plot(perf)

# Metrics Overview
metrics <- as.data.table(h2o.metric(perf))
metrics$crm_eval_correct = 3*(metrics$tns/(metrics$tns+metrics$fps))+(metrics$tps)/(metrics$tps+metrics$fns) # max 1. 3.094811
max(metrics$crm_eval_correct)
metrics$crm_eval_final = 3*metrics$recall + metrics$specificity # Calculation is not correct 
max(metrics$crm_eval_final)
View(metrics) 

metrics$crm_eval_correct = NULL

# Metrics Plot
metrics %>%
  gather(x, y, f1:crm_eval_final) %>%
  ggplot(aes(x = threshold, y = y, group = x)) +
  facet_wrap(~ x, ncol = 2, scales = "free") +
  geom_line()


# Logistic Regression -----------------------------------------------------


# Cluster Analysis --------------------------------------------------------

#Select subset of features (namely all except for client_ID and Zip_code)
churners = data[which(data$Churn=="Yes"),]

private_churners <- churners[which(churners$Client_type==0),]

cluster_df = (private_churners[,c("Age", 
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
                          "MA_Grundversorger", 
                          "MA_Erweitert", 
                          "MA_Restlich")])

#Transform to numeric features
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
cluster_df$MA_Grundversorger = as.numeric(cluster_df$MA_Grundversorger)
cluster_df$MA_Erweitert = as.numeric(cluster_df$MA_Erweitert)
cluster_df$MA_Restlich = as.numeric(cluster_df$MA_Restlich)

#Scale values for clustering algorithm
scaled_ds <- scale(cluster_df)

#Execute HDBSCAN clustering algorithm for private customers churners
hdb <- hdbscan(scaled_ds, minPts = 10)
plot(hdb, show_flat = TRUE)

cs_result = cbind(churners[churners$Client_type==0,], "Cluster" = hdb$cluster)

# Import 2018 Data ----------------------------------------------------------------

data_2018 = read_excel("Data\\Data November 2018.xlsx", na = "-", col_types = c("text","guess","guess","text","guess","guess","guess","guess","guess","guess","guess","numeric","guess","guess","guess","guess","guess","guess","guess","guess","guess","guess"))
write.csv2(data_2018, "Data\\Data_November_2018.csv")

data_2018 = fread("Data\\Data_November_2018.csv", na.strings = "NA", dec = ",")

# Data Preparation
#remove title and V1 from the data set
data_2018 = data_2018[,c("Contract_ID", 
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
               "DBII")]

#rename cols in order to avoid problems with imputation

names(data_2018)[names(data_2018) == 'Client type'] <- 'Client_type' # Customer since, Notice Period, Automatic Contract extension
names(data_2018)[names(data_2018) == 'Zip code'] <- 'Zip_code'
names(data_2018)[names(data_2018) == 'Duration of customer relationship'] <- 'Duration_of_customer_relationship'  
names(data_2018)[names(data_2018) == 'Customer since' ] <- 'Customer_since' 
names(data_2018)[names(data_2018) == 'Contract start date' ] <- 'Contract_start_date' 
names(data_2018)[names(data_2018) == 'Minimum contract term'] <- 'Minimum_contract_term'
names(data_2018)[names(data_2018) == 'Notice period' ] <- 'Notice_period' 
names(data_2018)[names(data_2018) == 'Automatic contract extension' ] <- 'Automatic_contract_extension' 
names(data_2018)[names(data_2018) == 'Payment on account'] <- 'Payment_on_account'
names(data_2018)[names(data_2018) == 'Annual account'] <- 'Annual_account'
names(data_2018)[names(data_2018) == 'Bill shock'] <- 'Bill_shock'
names(data_2018)[names(data_2018) == 'Online account'] <- 'Online_account'
names(data_2018)[names(data_2018) == 'Opt In Mail'] <- 'Opt_In_Mail'
names(data_2018)[names(data_2018) == 'Opt In Post'] <- 'Opt_In_Post'
names(data_2018)[names(data_2018) == 'Opt In Tel'] <- 'Opt_In_Tel'
names(data_2018)[names(data_2018) == 'Market area'] <- 'Market_area'

#If "Contract_start_date"==NA, insert "Customer_since" as "Contract_start_date"
data_2018$`Customer_since` = ymd(data_2018$`Customer_since`) 
data_2018$`Contract_start_date` = ymd(data_2018$`Contract_start_date`)
data_2018$Contract_start_date <- if_else(is.na(data_2018$Contract_start_date), data_2018$Customer_since, data_2018$Contract_start_date)

#If "Customer_since"==NA, calculate "Customer_since" based on "Duration_of_customer_relationship"
data_2018$Customer_since <- if_else(is.na(data_2018$Customer_since),ymd(20181201)- months(data_2018$Duration_of_customer_relationship),data_2018$Customer_since)

#At 4 observations, the contract starts later than 2017-01-01 --> Delete cases
nrow(subset(data_2018, data_2018$Customer_since <= ymd(20181101)))
data_2018 <- subset(data_2018, data_2018$Customer_since <= ymd(20181201))

#At 51 observations "Customer_since" starts later than "Contract start date" --> Replace "Customer_since" by "Contract_start_date"  
nrow(subset(data_2018,data_2018$Customer_since>data_2018$Contract_start_date)) 
data_2018$Customer_since <- if_else(data_2018$Customer_since>data_2018$Contract_start_date, data_2018$Contract_start_date, data_2018$Customer_since)

#Transform "Customer_since" to number of months
data_2018$`Customer_since_interval` = interval(ymd(data_2018$`Customer_since`), ymd(20181201)) %/% months(1)

#Transform "Contract_start_date" to number of months
data_2018$`Contract_start_date_interval` = interval(ymd(data_2018$`Contract_start_date`), ymd(20181201)) %/% months(1)

## Feature Conversion
#Convert features into right data types
data_2018$Contract_ID = as.character(data_2018$Contract_ID)
data_2018$`Zip_code` = as.character(data_2018$`Zip_code`)
data_2018$`Client_type` = as.factor(data_2018$`Client_type`)
data_2018$Age = as.numeric(data_2018$Age)
data_2018$Duration_of_customer_relationship = as.numeric(data_2018$Duration_of_customer_relationship)
data_2018$Minimum_contract_term = as.numeric(data_2018$Minimum_contract_term)
data_2018$Notice_period = as.numeric(data_2018$Notice_period)
data_2018$Automatic_contract_extension = as.numeric(data_2018$Automatic_contract_extension)
data_2018$Consumption = as.numeric(data_2018$Consumption) 
data_2018$Payment_on_account = as.numeric(data_2018$Payment_on_account)
data_2018$`Annual_account` = as.numeric(data_2018$`Annual_account`)
data_2018$`Bill_shock` = as.factor(data_2018$`Bill_shock`)
data_2018$`Online_account` = as.factor(data_2018$`Online_account`)
data_2018$`Opt_In_Mail` = as.factor(data_2018$`Opt_In_Mail`)
data_2018$`Opt_In_Post` = as.factor(data_2018$`Opt_In_Post`)
data_2018$`Opt_In_Tel` = as.factor(data_2018$`Opt_In_Tel`)
data_2018$Market_area = as.factor(data_2018$Market_area)
data_2018$`MA_Grundversorger` = as.factor(data_2018$`MA_Grundversorger`)
data_2018$`MA_Erweitert` = as.factor(data_2018$`MA_Erweitert`)
data_2018$`MA_Restlich` = as.factor(data_2018$`MA_Restlich`)
data_2018$Recovered = as.factor(data_2018$Recovered)
data_2018$Churn = as.character(data_2018$Churn)
data_2018$Churn[data_2018$Churn == "0"] = "No"
data_2018$Churn[data_2018$Churn == "1"] = "Yes"
data_2018$Churn = as.factor(data_2018$Churn)
data_2018$DBII = as.numeric(data_2018$DBII)
data_2018$Customer_since_interval = as.numeric(data_2018$Customer_since_interval)
data_2018$Contract_start_date_interval = as.numeric(data_2018$Contract_start_date_interval)

# Feature Engineering 
#Create feature "international"
data_2018$`International` = unlist(gregexpr("[0-9]{5}", data_2018$`Zip_code`)) # Nochmal überprüfen, kurze PLZ müssen nicht unbedingt im Ausland sein
data_2018$`International`[data_2018$`International` == 1] = 0
data_2018$`International`[data_2018$`International` == -1] = 1

data_2018[ International == 1, .N, by = MA_Erweitert] # Auch checken, ob erweiterten Netzgebiet
data_2018[ International == 1, .N, by = MA_Restlich] 

#Create feature "annual payment"
data_2018$`Actual_payment` = data_2018$Payment_on_account * 12 + data_2018$Annual_account

#Create feature "Continuous relationship"
data_2018$`Continuous_relationship` = ifelse(data_2018$Contract_start_date==data_2018$Customer_since, 1,0)
data_2018$Continuous_relationship = as.factor(data_2018$Continuous_relationship)

# Outlier Detection
data_2018$Age[data_2018$Age >= 105] = NA 
data_2018$Age[data_2018$Age < 18] = NA

# Data Exploration
summary(data_2018)
str(data_2018)

# Correlation Plot
corrplot.mixed(corr=cor(data_2018[, .(Age, 
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

# Final Dataset
data_2018 = data_2018[, .(Churn, 
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
                      Consumption, 
                      Notice_period, 
                      Annual_account, 
                      DBII, 
                      Minimum_contract_term, 
                      Customer_since_interval,
                      Contract_start_date_interval)]

# Convert features of the preparaed data into right format for modeling 
data_2018$Churn = as.factor(data_2018$Churn)
data_2018$Client_type = as.numeric(as.character(data_2018$Client_type))
data_2018$Bill_shock = as.numeric(as.character(data_2018$Bill_shock))
data_2018$Online_account = as.numeric(as.character(data_2018$Online_account))
data_2018$Opt_In_Mail = as.numeric(as.character(data_2018$Opt_In_Mail))
data_2018$Opt_In_Post = as.numeric(as.character(data_2018$Opt_In_Post))
data_2018$Opt_In_Tel = as.numeric(as.character(data_2018$Opt_In_Tel))
data_2018$MA_Grundversorger = as.numeric(as.character(data_2018$MA_Grundversorger))
data_2018$MA_Erweitert = as.numeric(as.character(data_2018$MA_Erweitert))
data_2018$MA_Restlich = as.numeric(as.character(data_2018$MA_Restlich))
data_2018$Recovered = as.numeric(as.character(data_2018$Recovered))
data_2018$Continuous_relationship = as.numeric(as.character(data_2018$Continuous_relationship))
data_2018$Age = as.numeric(as.character(data_2018$Age))
data_2018$Duration_of_customer_relationship = as.numeric(as.character(data_2018$Duration_of_customer_relationship))

str(data_2018)

# Change order of factor levels such that "Yes" is interpreted as positive and "No" is interpreted as negative 
levels(data_2018$Churn)
data_2018$Churn = factor(data_2018$Churn, levels = c("Yes", "No"))
levels(data_2018$Churn)


# Deprecated --------------------------------------------------------------

# Multiple Imputation 
imp_data = mice(data[data$Client_type == 0], method = "cart") # Imputation only for private customers
data_impute <- complete(imp_data)
write.csv(data_impute, "Data/Data_January_2017_Imputed_CART.csv")
apply(data_impute, 2, function(col)sum(is.na(col))/length(col))

data_firms = data[Client_type == 1]
imputed_data = base::rbind(data_impute, imp_data)


# a) Naive Bayes 
myControl = trainControl(method = "cv",
                         number = 5,
                         classProbs = TRUE)

model = train(Churn ~ ., data = data, method = "naive_bayes", trControl = myControl, na.action = na.pass)
plot(model)

# 1) Naive Bayes 

nb_classifier = naiveBayes(x = train_data ~ . , data = train_data[, -1], laplace=1)
nb_pred = predict(nb_classifier, newdata = test_data[,-1])
test_data$Churn_pred = predict(nb_classifier, newdata = test_data[,-1])
# test_data[, .N, by = Churn]
# test_data[, .N, by = Churn_pred]

ConfusionMatrix(y_pred = nb_pred, y_true=test_data$Churn) # nicht die selbe length: "Supplied 21683 items to be assigned to 21684 items of column 'Churn_pred' (recycled leaving remainder of 1 items)."

# First Approaches XGB

# Check whether all variables are numeric
str(xgb_data)

## 4. Split dataset into testing and training subsets

# Do train, test data splitting once again
attach(xgb_data)
smp_siz = floor(0.70*nrow(xgb_data))
smp_siz  

set.seed(123)   
train_ind = sample(seq_len(nrow(xgb_data)),size = smp_siz) 
xgb_train =xgb_data[train_ind,] 
xgb_test=xgb_data[-train_ind,] 

## 5. Convert the cleaned dataframe to a Dmatrix

# xgb_train_c = xgb.DMatrix(as.matrix(xgb_train))
# xgb_test_c = xgb.DMatrix(as.matrix(xgb_test))

xgb_train_m = as.matrix(xgb_train)
xgb_test_m = as.matrix(xgb_test)

## Approach 1

xgboost_model <- xgboost(data = xgb_train_m[ , -1],
                         label = xgb_train_m[ , 1],
                         eta = 0.1,
                         max_depth = 10,
                         objective = "binary:logistic",
                         nrounds = 200,
                         verbose = FALSE,
                         prediction = TRUE)
xgboost_model

predict(xgboost_model,
        as.matrix(xgb_test_m[, -1])) %>%
  as.tibble() %>%
  mutate(prediction = round(value),
         label = xgb_test_m[ , 1]) %>%
  count(prediction, label)

# Approach 2

xgb_train_c = xgb.DMatrix(as.matrix(xgb_train), label = xgb_train$Churn)
xgb_test_c = xgb.DMatrix(as.matrix(xgb_test), label = xgb_test$Churn)

params <- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eta=0.3, 
               gamma=0, 
               max_depth=6)

watchlist <- list(train = xgb_train_c, eval = xgb_test_c)

bst_model <- xgb.train(params = params, 
                       data = xgb_train_c, 
                       nrounds = 200, 
                       watchlist = watchlist,
                       verbose = FALSE,
                       prediction = TRUE)
bst_model

# Approach 3 

cv <- xgb.cv(data = xgb_train_m[ , -1],
             label = xgb_train_m[ , 1],
             nrounds = 150,
             nfold = 5,
             objective = "binary:logistic",
             eta = 0.01,
             max_depth = 6,
             verbose = 0    # silent
)

# Get the evaluation log 
elog <- as.data.frame(cv$evaluation_log)
elog

# Determine and print how many trees minimize training and test error
elog %>% 
  summarize(ntrees.train = which.min(train_error_mean),   # find the index of min
            ntrees.test  = which.min(test_error_mean))   # find the index of min

# The number of trees to use, as determined by xgb.cv
set.seed(123)
ntrees_train <- elog$ntrees.train
ntrees_test <- elog$ntrees.test

ntrees = ntrees_test

xgb_model_with_cv = xgboost(data = xgb_train_m[ , -1],
                            label = xgb_train_m[ , 1],  # column of outcomes
                            nrounds = 100,       # number of trees to build
                            objective = "binary:logistic", # objective
                            eta = 0.01,
                            depth = 6,
                            verbose = 0,
                            eval_metrix = "auc")  # silent

# Predict on the Training Data 
pred_xgb_model_with_cv <- predict(xgb_model_with_cv, newdata=xgb_test_m[,-1])

# To Do: Convert Probabilities back into labels 
predict(xgb_model_with_cv, newdata=xgb_test_m[,-1]) %>%
  as.tibble() %>%
  mutate(prediction = round(value),
         label = xgb_test_m[ , 1]) %>%
  count(prediction, label)


# Show Confusion Matrix
ConfusionMatrix(pred_xgb_model_with_cv, xgb_test_m[,1])



# Change "Yes" and "No" to "Ja" and "Nein" in order to restructure the alphabetical order, which might lead to a right calculation of the confusion matrix rates 

h2o_data = xgb_data

h2o_data$Churn = as.character(h2o_data$Churn)
h2o_data$Churn[h2o_data$Churn == "Yes"] = "Ja"
h2o_data$Churn[h2o_data$Churn == "No"] = "Nein"
h2o_data$Churn = as.factor(h2o_data$Churn)

levels(h2o_data$Churn)
h2o_data$Churn = factor(h2o_data$Churn, levels = c("Ja", "Nein"))
levels(h2o_data$Churn)

# Create training, validation and test set
set.seed(789)
split1 <- createDataPartition(h2o_data$Churn, p=.6, list=FALSE)
h2o_train_data <- h2o_data[split1,]
other  <- h2o_data[-split1,]

set.seed(2342)
split2 <- createDataPartition(other$Churn, p=.5, list=FALSE)
h2o_eval_data = other[split2,]
h2o_test_test = other[-split2,]

## 1. Remove information about the target variable from the training data
# Not applicable here
## 2. Reduce the amount of redundant information
# Already done (deleted Customer_ID, V1, and so on)

## 3. Convert categorical information (like country) to a numeric format

# xgb_data$Churn = as.character(xgb_data$Churn)
# xgb_data$Churn[xgb_data$Churn == "No"] = "0"
# xgb_data$Churn[xgb_data$Churn == "Yes"] = "1"
# xgb_data$Churn = as.numeric(xgb_data$Churn)
# xgb_data$Churn = as.character(xgb_data$Churn)
# xgb_data$Churn[xgb_data$Churn == "0"] = "No"
# xgb_data$Churn[xgb_data$Churn == "1"] = "Yes"
# xgb_data$Churn = as.factor(xgb_data$Churn)


xgb_data = data


xgb_data$Churn = as.factor(xgb_data$Churn)
xgb_data$Client_type = as.numeric(as.character(xgb_data$Client_type))
xgb_data$Bill_shock = as.numeric(as.character(xgb_data$Bill_shock))
xgb_data$Online_account = as.numeric(as.character(xgb_data$Online_account))
xgb_data$Opt_In_Mail = as.numeric(as.character(xgb_data$Opt_In_Mail))
xgb_data$Opt_In_Post = as.numeric(as.character(xgb_data$Opt_In_Post))
xgb_data$Opt_In_Tel = as.numeric(as.character(xgb_data$Opt_In_Tel))
xgb_data$MA_Grundversorger = as.numeric(as.character(xgb_data$MA_Grundversorger))
xgb_data$MA_Erweitert = as.numeric(as.character(xgb_data$MA_Erweitert))
xgb_data$MA_Restlich = as.numeric(as.character(xgb_data$MA_Restlich))
xgb_data$Recovered = as.numeric(as.character(xgb_data$Recovered))
xgb_data$Continuous_relationship = as.numeric(as.character(xgb_data$Continuous_relationship))
xgb_data$Age = as.numeric(as.character(xgb_data$Age))
xgb_data$Duration_of_customer_relationship = as.numeric(as.character(xgb_data$Duration_of_customer_relationship))

str(xgb_data)

# Change order of factor levels such that "Yes" is interpreted as positive and "No" is interpreted as negative 
levels(xgb_data$Churn)
xgb_data$Churn = factor(xgb_data$Churn, levels = c("Yes", "No"))
levels(xgb_data$Churn)



# Create training, validation and test set
set.seed(156)
split1 <- createDataPartition(xgb_data$Churn, p=.6, list=FALSE)
xgb_train_data <- xgb_data[split1,]
other  <- xgb_data[-split1,]

set.seed(234)
split2 <- createDataPartition(other$Churn, p=.5, list=FALSE)
xgb_eval_data = other[split2,]
xgb_test_test = other[-split2,]


# h2o_data$Churn = as.character(h2o_data$Churn)
# h2o_data$Churn[h2o_data$Churn == "Yes"] = "Ja"
# h2o_data$Churn[h2o_data$Churn == "No"] = "Nein"
# h2o_data$Churn = as.factor(h2o_data$Churn)



# Mean per class error
# h2o.mean_per_class_error(best_model, train = TRUE, valid = TRUE, xval = TRUE)
# h2o.auc(best_model, train = TRUE)
# h2o.auc(best_model, valid = TRUE)
# h2o.auc(best_model, xval = TRUE)

# f3) GLM 

h2o_glm = h2o.glm(x = features, 
                  y = response,
                  training_frame = train_hf,
                  nfolds = 5,
                  # validation_frame = valid_hf,
                  balance_classes = TRUE, # Make sure that set to TRUE
                  max_runtime_secs = 240)

pred <- h2o.predict(h2o_glm, test_hf[, -1])

# Confusion matrix on validation data
perf <- h2o.performance(h2o_glm, test_hf) 
h2o.confusionMatrix(perf)
plot(perf)


# f_2) Gradient Boosting Machine

h2o_gbm <- h2o.gbm(x = features, 
                   y = response,
                   training_frame = train_hf,
                   nfolds = 5,
                   # validation_frame = valid_hf,
                   balance_classes = TRUE, # Make sure that set to TRUE
                   max_runtime_secs = 240)


pred <- h2o.predict(h2o_gbm, test_hf[, -1])

# Confusion matrix on validation data
perf <- h2o.performance(h2o_gbm, test_hf) 
h2o.confusionMatrix(perf)
plot(perf)
