#Install Packages --------------------

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
install.packages("RCurl")
install.packages("bitops")
install.packages("rjson")
install.packages("statmod")
install.packages("tools")
install.packages("yaml")
install.packages("ModelMetrics")
install.packages("Zelig")
install.packages("recipes")   
install.packages("ggthemes")
install.packages("DescTools")
install.packages("ResourceSelection")
install.packages("pROC")

#Load Packages --------------------

require("tidyverse")
require("car")
require("MASS")
require("readxl")
require("tidyverse")
require("dplyr")
require("data.table")
require("ggplot2")
require("caret")
require("pROC")
require("lubridate")
require("xgboost")
require("klaR")
require("rlang")
require("mice")
require("corrplot")
require("dplyr")
require("factoextra")
require("VIM")
require("purrr")
require("corrplot")
require("gplots")
require("rsample")
require("yardstick")
require("ggthemes")
require("e1071")
require("MLmetrics")
require("ISLR")
require("randomForest")
require("gridExtra")
require("RCurl")
require("bitops")
require("rjson")
require("statmod")
require("tools")
require("yaml")
require("ModelMetrics")
require("Zelig")
require("recipes")   
require("ggthemes")
require("DescTools")
require("ResourceSelection")
require("pROC")

#Import 2017 Data --------------------

data_2017 = read_excel("Data/Data January 2017.xlsx", na = "-", col_types = c("text","guess","guess","text","guess","guess","guess","guess","guess","guess","guess","numeric","guess","guess","guess","guess","guess","guess","guess","guess","guess","guess","guess"))
write.csv2(data_2017, "Data/Data_January_2017_3.csv")

data = fread("Data/Data_January_2017_3.csv", na.strings = "NA", dec = ",")

#Remove "title" and "V1" from the data set
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

#Rename cols in order to avoid problems with imputation

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

#Data Preparation --------------------

# Online Account: NA to 0 
data$Online_account[is.na(data$Online_account)] = 0

#Recovered: "" to 0 and "X" to 1
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

# Feature Engineering --------------------

#Create feature "annual payment"
data$`Actual_payment` = data$Payment_on_account * 12 + data$Annual_account

#Create feature "Continuous relationship"
data$`Continuous_relationship` = ifelse(data$Contract_start_date==data$Customer_since, 1,0)
data$Continuous_relationship = as.factor(data$Continuous_relationship)

#Delete Customer_since, Contract_start_date, and Duration_of_customer_relationship
data$Duration_of_customer_relationship <- NULL
data$Customer_since <- NULL
data$Contract_start_date <- NULL

#Imputation  --------------------

# Detect Percentage of NA's per feature
apply(data, 2, function(col)sum(is.na(col))/length(col))
md.pattern(data, plot= T)
md.pairs(data)

#Multiple Imputation (only for private customers)
private_customers = subset(data[which(data$Client_type==0),])
corporate_customers = subset(data[which(data$Client_type==1),])

imp_data = mice(private_customers) 
private_customers <- complete(imp_data)

data <- rbind(private_customers,corporate_customers)

#Outlier --------------------

#Outlier Detection & Elimination
summary(data)

#Set to NA, since we interprete these factors as not meaningful
data$Age[data$Age >= 105] = NA 
data$Age[data$Age < 18] = NA
private_customers$Age[private_customers$Age >= 105] = NA 
private_customers$Age[private_customers$Age < 18] = NA

#Datasets for LR (corporate and private customers) --------------------

#Dataset without "ContractID", "Zip_code", and "Age"
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

#Classic Logistic Regression --------------------

#Influence of single IVs on DV --------------------

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

#Data split (70/30 split)

smp_size = floor(0.7*nrow(data1))
set.seed(123)
sample = sample(seq_len(nrow(data1)),size = smp_size)
train_data =data1[sample,]
test_data =data1[-sample,]

summary(train_data) #189 churners; 50332 non-churners
summary(test_data) #90 churners; 21562 non-churners

#Model building --------------------

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

#Calcualte model fit statistics
PseudoR2(m_2, which = "all")
hoslem.test(m_2$y, fitted(m_2, g=10))

#Model backward selection

m_2_bw <- stepAIC(m_2, direction="backward", trace = T)
summary(m_2_bw)
vif(m_2_bw)
g_2_bth$anova

#Calcualte Model fit statistics
PseudoR2(m_2_bw, which = "all")
hoslem.test(m_2_bw$y, fitted(m_2_bw), g=10)

#Model forward selection 

m_3 <- glm(Churn ~ 1, data = train_data, family = binomial) #Null model
summary(m_3)
m_3_fw <- stepAIC(m_3, direction="forward", trace = T, scope=list(upper=m_2,lower=m_3))
summary(m_3_fw)
vif(m_3_fw)

#Calcualte Model fit statistics
PseudoR2(m_3_fw, which = "all")
hoslem.test(m_3_fw$y, fitted(m_3_fw, g=10))

#Model forward and backward selection

m_3_both <- stepAIC(m_3, direction="both", trace = T, scope=list(upper=m_2,lower=m_3))
summary(m_3_both)
vif(m_3_both)

#Calcualte Model fit statistics
PseudoR2(m_3_both, which = "all")
hoslem.test(m_3_both$y, fitted(m_3_fw, g=10))

#Rare events logistic regression (ReLogit) --------------------

#Influence of single IVs on DV --------------------

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


#Build models with multiple variables in the model based on selection methods from classic logistic regression --------------------

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
exp(coef(m_3_bw_re))

#Box-Tidwell Test
train_data$lnConsumption <- log(train_data$Consumption)
train_data$lnPayment_on_account <- log(train_data$Payment_on_account)
train_data$lnAnnual_account <- log(train_data$Annual_account)
train_data$lnMinimum_contract_term <- log(train_data$Minimum_contract_term)
train_data$lnCustomer_since_interval <- log(train_data$Customer_since_interval)
train_data$lnContract_start_date_interval <- log(train_data$Contract_start_date_interval)

boxtidwell <- zelig(Churn ~ Bill_shock + Opt_In_Post + Recovered + Consumption + Consumption*lnConsumption +
                      Payment_on_account + Payment_on_account*lnPayment_on_account + Annual_account + Market_area + Minimum_contract_term + Minimum_contract_term*lnMinimum_contract_term +
                      Customer_since_interval + Customer_since_interval*lnCustomer_since_interval + Contract_start_date_interval + Contract_start_date_interval*lnContract_start_date_interval, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = test_data)

summary(boxtidwell)

test_data$Prob_m_3_bw_re = predict(m_3_bw_re, test_data, type="response")

m_3_fw_re <- zelig(Churn ~ Customer_since_interval + Market_area + Contract_start_date_interval + 
                     DBII + Actual_payment + Consumption + Opt_In_Post + Recovered, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

test_data$Prob_m_3_fw_re = predict(m_3_fw_re, test_data, type="response")

m_3_bth_re <- zelig(Churn ~ Customer_since_interval + Market_area + Contract_start_date_interval + 
                      Actual_payment + Consumption + Opt_In_Post + Recovered, tau = 189/50521, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data)

test_data$Prob_m_3_bth_re = predict(m_3_bth_re, test_data, type="response")

#Evaluate model performances --------------------

#m_2_re --------------------

test_data$Pred_m_2_re <- ifelse(test_data$Prob_m_2_re < 0.003,"0", "1")
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

#m_2_bw_re --------------------

test_data$Pred_m_2_bw_re <- ifelse(test_data$Prob_m_2_bw_re < 0.003,"0", "1")
test_data$Pred_m_2_bw_re <- as.factor(test_data$Pred_m_2_bw_re)
confusionMatrix(data = test_data$Pred_m_2_bw_re, reference= test_data$Churn, positive = "1")

#Area under curve
auc(test_data$Churn, test_data$Prob_m_2_bw_re)

#m_3_bw_re --------------------

test_data$Pred_m_3_bw_re <- ifelse(test_data$Prob_m_3_bw_re < 0.003,"0", "1")
test_data$Pred_m_3_bw_re <- as.factor(test_data$Pred_m_3_bw_re)
confusionMatrix(data = test_data$Pred_m_3_bw_re, reference= test_data$Churn, positive = "1")

#Area under curve = 0.6987 (best AUC)
auc(test_data$Churn, test_data$Prob_m_3_bw_re)

#Calculate optimal threshold
roc_m_3_bw_re <- roc(response = test_data$Churn, predictor = test_data$Prob_m_3_bw_re)
coords(roc_m_3_bw_re, x= "best", best.method = "y", input = "threshold", ret=c("threshold", "specificity", "sensitivity"))
coords(roc_m_3_bw_re, x = "best", best.method = "c", input = "threshold")

#m_3_fw_re --------------------

test_data$Pred_m_3_fw_re <- ifelse(test_data$Prob_m_3_fw_re < 0.003,"0", "1")
test_data$Pred_m_3_fw_re <- as.factor(test_data$Pred_m_3_fw_re)
confusionMatrix(data = test_data$Pred_m_3_fw_re, reference= test_data$Churn, positive = "1")

#Area under curve
auc(test_data$Churn, test_data$Prob_m_3_fw_re)

#m_3_bth_re --------------------

test_data$Pred_m_3_bth_re <- ifelse(test_data$Prob_m_3_bth_re < 0.003,"0", "1")
test_data$Pred_m_3_bth_re <- as.factor(test_data$Pred_m_3_bth_re)
confusionMatrix(data = test_data$Pred_m_3_bth_re, reference= test_data$Churn, positive = "1")

#Area under curve
auc(test_data$Churn, test_data$Prob_m_3_bth_re)

#Logistic regression for private customers only --------------------
summary(private_customers)

#Omit variables with missing values (due to Age under 18 or over 104)
private_customers = na.omit(private_customers)

#Delete Zip_code, Contract_ID and CLient_type from training and test dataset

private_customers$Zip_code <- NULL
private_customers$Contract_ID <- NULL
private_customers$Client_type <- NULL

#Data split (70/30 split)
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
PseudoR2(private_full_bw, which = "all")
hoslem.test(private_full_bw$y, fitted(private_full_bw, g=10))

#Forward selection
private_null <- glm(Churn ~ 1, data = train_data_private, family = binomial) #Null model

#Forward and backward selection
private_null_fw <- stepAIC(private_null, direction="forward", trace = T, scope=list(upper=private_full,lower=private_null))

vif(private_null_fw)

#Model forward and backward selection
private_null_bth <- stepAIC(private_null, direction="both", trace = T, scope=list(upper=private_full,lower=private_null))
vif(private_null_bth)

#Build models with multiple variables in the model based on selection methods from classic logistic regression------

train_data_private$Churn = as.integer(train_data_private$Churn) #Required to run relogit model
train_data_private$Churn = train_data_private$Churn-1

private_full_bw_re <- zelig(Churn ~ Age + Minimum_contract_term + Consumption + Annual_account + 
                           Bill_shock + Market_area + Customer_since_interval + Contract_start_date_interval
                         , tau = 194/49831, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data_private)

summary(private_full_bw_re)

#Box-Tidwell Test
train_data_private$lnAge <- log(train_data_private$Age)
train_data_private$lnMinimum_contract_term <- log(train_data_private$Minimum_contract_term)
train_data_private$lnConsumption <- log(train_data_private$Consumption)
train_data_private$lnAnnual_account <- log(train_data_private$Annual_account)
train_data_private$lnCustomer_since_interval <- log(train_data_private$Customer_since_interval)
train_data_private$lnContract_start_date_interval <- log(train_data_private$Contract_start_date_interval)


boxtidwell_private <- zelig(Churn ~ Age + Age*lnAge + Minimum_contract_term + Minimum_contract_term*lnMinimum_contract_term + Consumption + Consumption*lnConsumption + Annual_account + 
                              Annual_account*lnAnnual_account + Bill_shock + Market_area + Customer_since_interval + Customer_since_interval*lnCustomer_since_interval + Contract_start_date_interval + Contract_start_date_interval*lnContract_start_date_interval
                            , tau = 194/49831, model = "relogit",  case.control = "weighting", bias.correct = TRUE, data = train_data_private)
summary(boxtidwell_private)

#Model evaluation --------------------

#Private_full_bw_re
Prob_private_full_bw_re <- predict(private_full_bw_re, test_data_private, type="response")
test_data_private$Prob_private_full_bw_re <- unlist(Prob_private_full_bw_re, use.names=FALSE)
test_data_private$Pred_private_full_bw_re <- ifelse(test_data_private$Prob_private_full_bw_re < 0.003,"0", "1")
test_data_private$Pred_private_full_bw_re <- as.factor(test_data_private$Pred_private_full_bw_re)
confusionMatrix(data = test_data_private$Pred_private_full_bw_re, reference= test_data_private$Churn, positive = "1")

#Area under curve = 0.7427
auc(test_data_private$Churn, test_data_private$Prob_private_full_bw_re)

#Import 2018 data --------------------

data_2018 = read_excel("Data/Data November 2018.xlsx", na = "-", col_types = c("text","guess","guess","text","guess","guess","guess","guess","guess","guess","guess","numeric","guess","guess","guess","guess","guess","guess","guess","guess","text","guess"))
write.csv2(data_2018, "Data/Data_November_2018.csv")

data_2018 = fread("Data/Data_November_2018.csv", na.strings = "NA", dec = ",")

#Data Preparation --------------------
#Remove "title" and "V1" from the data set
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

#Rename cols in order to avoid problems with imputation

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

# Online Account: NA to 0 
data_2018$Online_account[is.na(data_2018$Online_account)] = 0

# Recovered: "" to 0 and "X" to 1
data_2018$Recovered[data_2018$Recovered=="X"] = 1
data_2018$Recovered[is.na(data_2018$Recovered)] = 0

#If "Contract_start_date"==NA, insert "Customer_since" as "Contract_start_date"
data_2018$`Customer_since` = ymd(data_2018$Customer_since) 
data_2018$`Contract_start_date` = ymd(data_2018$Contract_start_date)
data_2018$Contract_start_date <- if_else(is.na(data_2018$Contract_start_date), data_2018$Customer_since, data_2018$Contract_start_date)

#If "Customer_since"==NA, calculate "Customer_since" based on "Duration_of_customer_relationship"
data_2018$Customer_since <- if_else(is.na(data_2018$Customer_since),ymd(20181201)- months(data_2018$Duration_of_customer_relationship),data_2018$Customer_since)

#At 59 observations "Customer_since" starts later than "Contract start date" --> Replace "Customer_since" by "Contract_start_date"  
nrow(subset(data_2018,data_2018$Customer_since>data_2018$Contract_start_date)) 
data_2018$Customer_since <- if_else(data_2018$Customer_since>data_2018$Contract_start_date, data_2018$Contract_start_date, data_2018$Customer_since)

#Transform "Customer_since" to number of months
data_2018$`Customer_since_interval` = interval(ymd(data_2018$`Customer_since`), ymd(20181201)) %/% months(1)

#Transform "Contract_start_date" to number of months
data_2018$`Contract_start_date_interval` = interval(ymd(data_2018$`Contract_start_date`), ymd(20181201)) %/% months(1)

#Remove customer_since, contract_start_date, and duration_of_customer_relationship
data_2018$Customer_since <- NULL
data_2018$Contract_start_date <- NULL
data_2018$Duration_of_customer_relationship <- NULL

#Feature Conversion
#Convert features into right data types
data_2018$Contract_ID = as.character(data_2018$Contract_ID)
data_2018$`Zip_code` = as.character(data_2018$`Zip_code`)
data_2018$`Client_type` = as.factor(data_2018$`Client_type`)
data_2018$Age = as.numeric(data_2018$Age)
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
data_2018$Recovered = as.factor(data_2018$Recovered)
data_2018$DBII = as.numeric(data_2018$DBII)
data_2018$Customer_since_interval = as.numeric(data_2018$Customer_since_interval)
data_2018$Contract_start_date_interval = as.numeric(data_2018$Contract_start_date_interval)

#Feature Engineering --------------------
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

#Reset customer_since_interval -22 months as values in 2018 dataset are higher and, thus, LR leads to wrong predictions
data_2018$Customer_since_interval = ifelse(data_2018$Customer_since_interval<23,0,data_2018$Customer_since_interval-22)
data_2018$Contract_start_date_interval = ifelse(data_2018$Contract_start_date_interval<23,0,data_2018$Contract_start_date_interval-22)

#Apply final LR model --------------------
data_2018$Prob_final <- predict(m_3_bw_re, data_2018, type="response")
data_2018$Pred_final <- unlist(data_2018$Prob_final, use.names=FALSE)
data_2018$Pred_final <- ifelse(data_2018$Pred_final < 0.003 ,"0", "1")
data_2018$Pred_final <- as.factor(data_2018$Pred_final)
table(data_2018$Pred_final)
