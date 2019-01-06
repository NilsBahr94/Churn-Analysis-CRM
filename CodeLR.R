# Install Packages --------------------------------------------------------

install.packages("tidyverse")
install.packages("car")
install.packages("MASS")
install.packages("readxl")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("data.table")
install.packages("ggplot2")
install.packages("caret")
install.packages("pROC")
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
install.packages('zeligverse')
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
require(car)
require(MASS)
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
require(zeligverse)
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

L
data = fread("Data/Data_January_2017_3.csv", na.strings = "NA", dec = ",")

# N
#data = fread("Data\\Data_January_2017_3.csv", na.strings = "NA", dec = ",")


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
data$Recovered = as.factor(data$Recovered)
data$Churn = as.factor(data$Churn)
data$DBII = as.numeric(data$DBII)
data$`Customer_since` = ymd(data$`Customer_since`) 
data$`Contract_start_date` = ymd(data$`Contract_start_date`)

#If "Contract_start_date"==NA, insert "Customer_since" as "Contract_start_date"
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
data$Customer_since_interval = as.integer(data$Customer_since_interval)

#Transform "Contract_start_date" to number of months
data$`Contract_start_date_interval` = interval(ymd(data$`Contract_start_date`), ymd(20170201)) %/% months(1)
data$Contract_start_date_interval = as.integer(data$Contract_start_date_interval)

# Feature Engineering -------------------------------------------------------------

# Create feature "not_opted" 
# data[Opt_In_Mail == "0" & Opt_In_Post == "0" & Opt_In_Tel == "0", .N, by= Churn] 
#However, only non-churners have this combination, therefore this new feature would not be meaningful

#Create feature "international"
data$`International` = unlist(gregexpr("[0-9]{5}", data$`Zip_code`))
data$`International`[data$`International` == 1] = 0
data$`International`[data$`International` == -1] = 1
data$`International` = as.factor(data$`International`)

#Create feature "annual payment"
data$`Actual_payment` = data$Payment_on_account * 12 + data$Annual_account

#Create feature "Continuous relationship"
data$`Continuous_relationship` = ifelse(data$Contract_start_date==data$Customer_since, 1,0)
data$Continuous_relationship = as.factor(data$Continuous_relationship)

#Delete Customer_since, Contract_start_date, and Duration_of_customer_relationship

data$Duration_of_customer_relationship <- NULL
data$Customer_since <- NULL
data$Contract_start_date <- NULL

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

#Datasets for LR -------------
#Dataset without ContractID, Zip_code, and Age
data1 = data[, .(Churn, 
                     Client_type, 
                     Bill_shock, 
                     Online_account, 
                     Opt_In_Mail, 
                     Opt_In_Post, 
                     Opt_In_Tel, 
                     Recovered, 
                     Continuous_relationship, 
                     Consumption, 
                     Notice_period, 
                     Payment_on_account,
                     Annual_account,
                     Actual_payment,
                     International,
                     Market_area,
                     DBII,
                     Automatic_contract_extension,
                     Minimum_contract_term, 
                     Customer_since_interval,
                     Contract_start_date_interval)]

#Delete observations with missing values as Logistic Regression requires a complete dataset 72282
data1 = na.omit(data1) #reduction from 72282 observations to 72173 observations (-109 observations)


#Classic Logistic Regression -----------------------------------

#Influence of single IVs on DV -------------------------

ctrl <- trainControl(method = "cv", number = 10, savePredictions = "final")

m_Clienttype <- train(Churn ~ Client_type ,
                data = data1,
                trControl = ctrl,
                method = "glm",
                family=binomial())

summary(m_Clienttype)

m_Bill_shock <- train(Churn ~ Bill_shock,
                      data = data1,
                      trControl = ctrl,
                      method = "glm",
                      family=binomial())

summary(m_Bill_shock)

m_Online_account <- train(Churn ~ Online_account,
                      data = data1,
                      trControl = ctrl,
                      method = "glm",
                      family=binomial())

summary(m_Online_account)

m_Opt_In_Mail <- train(Churn ~ Opt_In_Mail,
                          data = data1,
                          trControl = ctrl,
                          method = "glm",
                          family=binomial())

summary(m_Opt_In_Mail)

m_Opt_In_Post <- train(Churn ~ Opt_In_Post,
                       data = data1,
                       trControl = ctrl,
                       method = "glm",
                       family=binomial())

summary(m_Opt_In_Post)

m_Opt_In_Tel <- train(Churn ~ Opt_In_Tel,
                       data = data1,
                       trControl = ctrl,
                       method = "glm",
                       family=binomial())

summary(m_Opt_In_Tel)

m_Recovered <- train(Churn ~ Recovered,
                      data = data1,
                      trControl = ctrl,
                      method = "glm",
                      family=binomial())

summary(m_Recovered)

m_Continuous_relationship <- train(Churn ~  Continuous_relationship,
                     data = data1,
                     trControl = ctrl,
                     method = "glm",
                     family=binomial())

summary(m_Continuous_relationship)

m_Consumption <- train(Churn ~  Consumption,
                                   data = data1,
                                   trControl = ctrl,
                                   method = "glm",
                                   family=binomial())

summary(m_Consumption)

m_Notice_period <- train(Churn ~   Notice_period,
                       data = data1,
                       trControl = ctrl,
                       method = "glm",
                       family=binomial())

summary(m_Notice_period)

m_Payment_on_account <- train(Churn ~   Payment_on_account,
                         data = data1,
                         trControl = ctrl,
                         method = "glm",
                         family=binomial())

summary(m_Payment_on_account)

m_Annual_account <- train(Churn ~   Annual_account,
                              data = data1,
                              trControl = ctrl,
                              method = "glm",
                              family=binomial())

summary(m_Annual_account)

m_Actual_payment <- train(Churn ~   Actual_payment,
                          data = data1,
                          trControl = ctrl,
                          method = "glm",
                          family=binomial())

summary(m_Actual_payment)

m_International <- train(Churn ~   International,
                          data = data1,
                          trControl = ctrl,
                          method = "glm",
                          family=binomial())

summary(m_International)

m_Market_area <- train(Churn ~   Market_area,
                         data = data1,
                         trControl = ctrl,
                         method = "glm",
                         family=binomial())

summary(m_Market_area)

m_DBII <- train(Churn ~   DBII,
                       data = data1,
                       trControl = ctrl,
                       method = "glm",
                       family=binomial())

summary(m_DBII)

m_Automatic_contract_extension <- train(Churn ~   Automatic_contract_extension,
                data = data1,
                trControl = ctrl,
                method = "glm",
                family=binomial())

summary(m_Automatic_contract_extension)

m_Minimum_contract_term <- train(Churn ~   Minimum_contract_term,
                                        data = data1,
                                        trControl = ctrl,
                                        method = "glm",
                                        family=binomial())

summary(m_Minimum_contract_term)

m_Customer_since_interval <- train(Churn ~   Customer_since_interval,
                                 data = data1,
                                 trControl = ctrl,
                                 method = "glm",
                                 family=binomial())

summary(m_Customer_since_interval)

m_Contract_start_date_interval <- train(Churn ~   Contract_start_date_interval,
                                   data = data1,
                                   trControl = ctrl,
                                   method = "glm",
                                   family=binomial())

summary(m_Contract_start_date_interval)

# Data split (70/30 split)
smp_size = floor(0.7*nrow(data1))
set.seed(123)
sample = sample(seq_len(nrow(data1)),size = smp_size)
train_data =data1[sample,]
test_data =data1[-sample,]

summary(train_data) #189 churners; 50332 non-churners
summary(test_data) #90 churners; 21562 non-churners

#Model building -----------------------

#Logistic regression including all variables

m_all <- glm(Churn ~., data = train_data, family = binomial)
summary(m_all)
anova(m_all, test="Chisq")

#Serious issues of multicollinearity --> PCA to reduce features and inspect VIF values

vif(m_all)

data_pca <- select_if(data1, is.numeric) #10 variables remain

pca <- prcomp(na.omit(data_pca), center = TRUE, scale = TRUE)
str(pca)
summary(pca)
#Scree plot
fviz_eig(pca)
fviz_pca_biplot(pca)
biplot(pca)
pca$rotation

#Model without Consumption and Actual_payment

train_data$Consumption <- NULL
train_data$Actual_payment <- NULL
test_data$Consumption <- NULL
test_data$Actual_payment <- NULL

m_2 <- glm(Churn ~., data = train_data, family = binomial)
summary(m_2)
anova(m_2, test="Chisq")
vif(m_2)

#Model backward selection

m_2_bw <- stepAIC(m_2, direction="backward", trace = T)
summary(m_2_bw)
vif(m_2_bw)
g_2_bth$anova


#Model forward selection 

m_3 <- glm(Churn ~ 1, data = train_data, family = binomial) #Null model
summary(m_3)
m_3_fw <- stepAIC(m_3, direction="forward", trace = T, scope=list(upper=m_2,lower=m_3))
vif(m_3_fw)

#Model forward and backward selection

m_3_both <- stepAIC(m_3, direction="both", trace = T, scope=list(upper=m_2,lower=m_3))
vif(m_3_both)

#Rare events logistic regression (ReLogit) -------------

install.packages("Zelig")
require(Zelig)

#Influence of single IVs on DV -------------------------

test_data$Churn = as.integer(test_data$Churn) #Required to run relogit model
test_data$Churn = test_data$Churn-1

train_data$Churn = as.integer(train_data$Churn) #Required to run relogit model
train_data$Churn = train_data$Churn-1

zelig(Churn ~  Client_type, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

zelig(Churn ~ Bill_shock, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

zelig(Churn ~  Online_account, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

zelig(Churn ~  Opt_In_Mail, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

zelig(Churn ~  Opt_In_Post, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

zelig(Churn ~  Opt_In_Tel, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

zelig(Churn ~  Recovered, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

zelig(Churn ~  Consumption, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

zelig(Churn ~  Actual_payment, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

zelig(Churn ~  Continuous_relationship, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

zelig(Churn ~  Payment_on_account, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

zelig(Churn ~  Annual_account, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

zelig(Churn ~  International, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

zelig(Churn ~  Market_area, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

zelig(Churn ~  DBII, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

zelig(Churn ~  Minimum_contract_term, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

zelig(Churn ~  Customer_since_interval, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

zelig(Churn ~  Contract_start_date_interval, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)


#Build models with multiple variables in the model based on selection methods from classic logistic regression------

m_2_re <- zelig(Churn ~  Client_type + Bill_shock + Online_account + Opt_In_Mail + Opt_In_Post + Opt_In_Tel + Recovered + Continuous_relationship + Notice_period + Payment_on_account + Annual_account + International + Market_area + DBII + Automatic_contract_extension + Minimum_contract_term + Customer_since_interval + Contract_start_date_interval, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

test_data$Prob_m_2_re = predict(m_2_re, test_data, type="response") 

m_2_bw_re <- zelig(Churn ~ Bill_shock + Opt_In_Post + Recovered + Annual_account + 
                     Market_area + DBII + Minimum_contract_term + Customer_since_interval + 
                     Contract_start_date_interval, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

test_data$Prob_m_2_bw_re = predict(m_2_bw_re, test_data, type="response")

m_3_bw_re <- zelig(Churn ~ Bill_shock + Opt_In_Post + Recovered + Consumption + 
                     Payment_on_account + Annual_account + Market_area + Minimum_contract_term + 
                     Customer_since_interval + Contract_start_date_interval, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

summary(m_3_bw_re)

test_data$Prob_m_3_bw_re = predict(m_3_bw_re, test_data, type="response")

m_3_fw_re <- zelig(Churn ~ Customer_since_interval + Market_area + Contract_start_date_interval + 
                     DBII + Actual_payment + Consumption + Opt_In_Post + Recovered, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

test_data$Prob_m_3_fw_re = predict(m_3_fw_re, test_data, type="response")

m_3_bth_re <- zelig(Churn ~ Customer_since_interval + Market_area + Contract_start_date_interval + 
                      Actual_payment + Consumption + Opt_In_Post + Recovered, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

test_data$Prob_m_3_bth_re = predict(m_3_bth_re, test_data, type="response")

#Evaluate model performances ---------------------

library(ggplot2)
require(caret)
library(pROC)
library(gplots)

test_data$Churn <- as.factor(test_data$Churn)
str(test_data)

#m_2_re -----------------

test_data$Pred_m_2_re <- ifelse(test_data$Prob_m_2_re > 0.05,"0", "1")
test_data$Pred_m_2_re <- as.factor(test_data$Pred_m_2_re)
confusionMatrix(data = test_data$Pred_m_2_re, reference= test_data$Churn, positive = "1")

#Area under curve
auc(test_data$Churn, test_data$Prob_m_2_re)

#ROC
ROCPred_m_2_re <- prediction(test_data$Prob_m_2_re,test_data$Churn)
ROCPerf_m_2_re <- performance(ROCPred_m_2_re, measure="tpr", x.measure="fpr")
plot(ROCPerf_m_2_re, colorize=T, print.cutoffs.at = seq(0.1, by = 0.1))
abline(a=0, b= 1)

#Calculate best threshold
roc_m_2_re <- roc(response = test_data$Churn, predictor = test_data$Prob_m_2_re)
coords(roc_m_2_re, x = "best", best.method = "y", input = "threshold")
coords(roc_m_2_re, x = "best", best.method = "c", input = "threshold")

# Determine Threshold which minimizes Misspecification Costs
coords(roc, x= "best", best.weights = c(3,0.3), input = "threshold")

#m_2_bw_re -----------------

test_data$Pred_m_2_bw_re <- ifelse(test_data$Prob_m_2_bw_re > 0.05,"0", "1")
test_data$Pred_m_2_bw_re <- as.factor(test_data$Pred_m_2_bw_re)
confusionMatrix(data = test_data$Pred_m_2_bw_re, reference= test_data$Churn, positive = "1")

#Area under curve
auc(test_data$Churn, test_data$Prob_m_2_bw_re)

#m_3_bw_re ------------------

test_data$Pred_m_3_bw_re <- ifelse(test_data$Prob_m_3_bw_re < 0.004,"0", "1")
test_data$Pred_m_3_bw_re <- as.factor(test_data$Pred_m_3_bw_re)
confusionMatrix(data = test_data$Pred_m_3_bw_re, reference= test_data$Churn, positive = "1")

#Area under curve = 0.6987 (best AUC)
auc(test_data$Churn, test_data$Prob_m_3_bw_re)

#Calculate optimal threshold
roc_m_3_bw_re <- roc(response = test_data$Churn, predictor = test_data$Prob_m_3_bw_re)
coords(roc_m_3_bw_re, x= "best", best.method = "y", input = "threshold", ret=c("threshold", "specificity", "sensitivity"))
coords(roc_m_3_bw_re, x = "best", best.method = "c", input = "threshold")

#m_3_fw_re ------------------

test_data$Pred_m_3_fw_re <- ifelse(test_data$Prob_m_3_fw_re > 0.05,"0", "1")
test_data$Pred_m_3_fw_re <- as.factor(test_data$Pred_m_3_fw_re)
confusionMatrix(data = test_data$Pred_m_3_fw_re, reference= test_data$Churn, positive = "1")

#Area under curve
auc(test_data$Churn, test_data$Prob_m_3_fw_re)

#m_3_bth_re ------------------

test_data$Pred_m_3_bth_re <- ifelse(test_data$Prob_m_3_bth_re > 0.05,"0", "1")
test_data$Pred_m_3_bth_re <- as.factor(test_data$Pred_m_3_bth_re)
confusionMatrix(data = test_data$Pred_m_3_bth_re, reference= test_data$Churn, positive = "1")

#Area under curve
auc(test_data$Churn, test_data$Prob_m_3_bth_re)

#Logistic regression for private customers only -------------

#Delete Zip_code, Contract_ID and CLient_type from training and test dataset

private_customers$Zip_code <- NULL
private_customers$Contract_ID <- NULL
private_customers$Client_type <- NULL

# Data split (70/30 split)
set.seed(123)
split1 <- createDataPartition(private_customers$Churn, p=.7, list=FALSE)
train_data_private <- private_customers[split1,]
test_data_private  <- private_customers[-split1,]

summary(train_data_private) #194 churners and 49637 non-churners
summary(test_data_private) #82 churners and 21272 non-churners


#Full model
private_full <- glm(Churn ~., data = train_data_private, family = binomial)
summary(private_full)
vif(private_full) #Issue with multicollinearity

#Backward selection
private_full_bw <- stepAIC(private_full, direction="backward", trace = T)
summary(private_full_bw)

vif(private_full_bw)

#Forward selection
private_null <- glm(Churn ~ 1, data = train_data_private, family = binomial) #Null model

#Forward and backward selection
private_null_fw <- stepAIC(private_null, direction="forward", trace = T, scope=list(upper=private_full,lower=private_null))

vif(private_null_fw)

#Model forward and backward selection
private_null_bth <- stepAIC(private_null, direction="both", trace = T, scope=list(upper=private_full,lower=private_null))
vif(private_null_bth)

#Build models with multiple variables in the model based on selection methods from classic logistic regression------

test_data_private$Churn = as.integer(test_data_private$Churn) #Required to run relogit model
test_data_private$Churn = test_data_private$Churn-1

train_data_private$Churn = as.integer(train_data_private$Churn) #Required to run relogit model
train_data_private$Churn = train_data_private$Churn-1

private_full_bw_re <- zelig(Churn ~ Age + Minimum_contract_term + Consumption + Annual_account + 
                           Bill_shock + Market_area + Customer_since_interval + Contract_start_date_interval
                         , tau = 194/49831, model = "relogit",  case.control = "prior", bias.correct = TRUE, data = train_data_private)

summary(private_full_bw_re)

private_null_fw_re <- zelig(Churn ~ Customer_since_interval + Contract_start_date_interval + 
                              Market_area + Consumption + Age + Annual_account + Bill_shock + 
                              Minimum_contract_term
                            , tau = 194/49831, model = "relogit",  case.control = "prior", bias.correct = TRUE, data = train_data_private)

summary(private_null_fw_re)

#Model evaluation ---------------

#Private_full_bw_re
test_data_private$Churn <- as.factor(test_data_private$Churn)

test_data_private$Prob_private_full_bw_re = predict(private_full_bw_re, test_data_private, type="response")
test_data_private$Pred_private_full_bw_re <- ifelse(test_data_private$Prob_private_full_bw_re < 0.05,"0", "1")
test_data_private$Pred_private_full_bw_re <- as.factor(test_data_private$Pred_private_full_bw_re)
confusionMatrix(data = test_data_private$Pred_private_full_bw_re, reference= test_data_private$Churn, positive = "1")

  #Area under curve
auc(test_data_private$Churn, test_data_private$Prob_private_full_bw_re)

#Private_null_fw_re
test_data_private$Prob_private_null_fw_re = predict(private_null_fw_re, test_data_private, type="response")
test_data_private$Pred_private_null_fw_re <- ifelse(test_data_private$Prob_private_null_fw_re < 0.05,"0", "1")
test_data_private$Pred_private_null_fw_re <- as.factor(test_data_private$Pred_private_null_fw_re)
confusionMatrix(data = test_data_private$Pred_private_null_fw_re, reference= test_data_private$Churn, positive = "1")

  #Area under curve
auc(test_data_private$Churn, test_data_private$Prob_private_null_fw_re)