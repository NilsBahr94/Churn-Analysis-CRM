# Set working directory
install.packages('readxl')
install.packages('rockchalk')
install.packages("MASS")
install.packages("caret")
install.packages("e1071")
install.packages("car")
install.packages("HH")
install.packages("DescTools")
install.packages("ResourceSelection")
install.packages("lmtest")
install.packages("pROC")
install.packages("ROCR")
install.packages("gplots")
install.packages("ggrepel")
install.packages("gridExtra")
install.packages("GGally")
install.packages("glmnet")
setwd("~/Dropbox/Master/Seminar Marketing in the Age of Big Data/R/Dataset")

# Import data from Excel file (xlsx)
library(readxl)
data <- read_excel('Telco-Churn-Train.xlsx')
data_test <- read_excel('Telco-Churn-Test.xlsx')

# Get overview of data
summary(data)
str(data)

#________________________________DATA_PREPARATION_TRAIN_DATA______________________________________

# Convert character variables to factors
data$gender <- as.factor(data$gender)
data$SeniorCitizen <- as.factor (data$SeniorCitizen)
data$Partner <- as.factor(data$Partner)
data$Dependents <- as.factor(data$Dependents)
data$PhoneService <- as.factor(data$PhoneService)
data$MultipleLines <- as.factor (data$MultipleLines)
data$InternetService <- as.factor(data$InternetService)
data$OnlineSecurity <- as.factor(data$OnlineSecurity)
data$OnlineBackup <- as.factor (data$OnlineBackup)
data$DeviceProtection <- as.factor(data$DeviceProtection)
data$TechSupport <- as.factor(data$TechSupport)
data$StreamingTV <- as.factor(data$StreamingTV)
data$StreamingMovies <- as.factor (data$StreamingMovies)
data$Contract <- as.factor(data$Contract)
data$PaperlessBilling <- as.factor(data$PaperlessBilling)
data$PaymentMethod <- as.factor(data$PaymentMethod)
data$Churn <- as.factor(data$Churn)

# Change factor levels of SeniorCitizen from 0 and 1 to No and Yes
levels(data$SeniorCitizen) <- c("No","Yes")

# Check whether dataset contains missing values
sum(is.na(data))

# Convert missing values from variable Total charges to 0
data$TotalCharges[is.na(data$TotalCharges)] <- 0

# Check correlation between data$InternetService == "No" and factor levels "No internet service" and data$PhoneService == "No" and factor level "No phone service"
cor(data$InternetService == "No", data$OnlineSecurity == "No internet service")
cor(data$InternetService == "No", data$OnlineBackup == "No internet service")
cor(data$InternetService == "No", data$DeviceProtection == "No internet service")
cor(data$InternetService == "No", data$TechSupport == "No internet service")
cor(data$InternetService == "No", data$StreamingTV == "No internet service")
cor(data$InternetService == "No", data$StreamingMovies == "No internet service")
cor(data$PhoneService == "No", data$MultipleLines == "No phone service")

# Replace "No internet service" and "No phone service" by "No"
library(rockchalk)
data$OnlineSecurity <- combineLevels(data$OnlineSecurity, levs = c("No", "No internet service"), newLabel = "No")
data$OnlineBackup <- combineLevels(data$OnlineBackup, levs = c("No", "No internet service"), newLabel = "No")
data$DeviceProtection <- combineLevels(data$DeviceProtection, levs = c("No", "No internet service"), newLabel = "No")
data$TechSupport <- combineLevels(data$TechSupport, levs = c("No", "No internet service"), newLabel = "No")
data$StreamingTV <- combineLevels(data$StreamingTV, levs = c("No", "No internet service"), newLabel = "No")
data$StreamingMovies <- combineLevels(data$StreamingMovies, levs = c("No", "No internet service"), newLabel = "No")
data$MultipleLines <- combineLevels(data$MultipleLines, levs = c("No", "No phone service"), newLabel = "No")

#________________________________DATA_PREPARATION_TEST_DATA______________________________________
# Convert character variables to factors
data_test$gender <- as.factor(data_test$gender)
data_test$SeniorCitizen <- as.factor (data_test$SeniorCitizen)
data_test$Partner <- as.factor(data_test$Partner)
data_test$Dependents <- as.factor(data_test$Dependents)
data_test$PhoneService <- as.factor(data_test$PhoneService)
data_test$MultipleLines <- as.factor (data_test$MultipleLines)
data_test$InternetService <- as.factor(data_test$InternetService)
data_test$OnlineSecurity <- as.factor(data_test$OnlineSecurity)
data_test$OnlineBackup <- as.factor (data_test$OnlineBackup)
data_test$DeviceProtection <- as.factor(data_test$DeviceProtection)
data_test$TechSupport <- as.factor(data_test$TechSupport)
data_test$StreamingTV <- as.factor(data_test$StreamingTV)
data_test$StreamingMovies <- as.factor (data_test$StreamingMovies)
data_test$Contract <- as.factor(data_test$Contract)
data_test$PaperlessBilling <- as.factor(data_test$PaperlessBilling)
data_test$PaymentMethod <- as.factor(data_test$PaymentMethod)
data_test$Churn <- as.factor(data_test$Churn)

# Change factor levels of SeniorCitizen from 0 and 1 to No and Yes
levels(data_test$SeniorCitizen) <- c("No","Yes")

# Check whether data_testset contains missing values
sum(is.na(data_test))

# Convert missing values from variable Total charges to 0
data_test$TotalCharges[is.na(data_test$TotalCharges)] <- 0

# Check correlation between data_test$InternetService == "No" and factor levels "No internet service" and data_test$PhoneService == "No" and factor level "No phone service"
cor(data_test$InternetService == "No", data_test$OnlineSecurity == "No internet service")
cor(data_test$InternetService == "No", data_test$OnlineBackup == "No internet service")
cor(data_test$InternetService == "No", data_test$DeviceProtection == "No internet service")
cor(data_test$InternetService == "No", data_test$TechSupport == "No internet service")
cor(data_test$InternetService == "No", data_test$StreamingTV == "No internet service")
cor(data_test$InternetService == "No", data_test$StreamingMovies == "No internet service")
cor(data_test$PhoneService == "No", data_test$MultipleLines == "No phone service")

# Replace "No internet service" and "No phone service" by "No"
library(rockchalk)
data_test$OnlineSecurity <- combineLevels(data_test$OnlineSecurity, levs = c("No", "No internet service"), newLabel = "No")
data_test$OnlineBackup <- combineLevels(data_test$OnlineBackup, levs = c("No", "No internet service"), newLabel = "No")
data_test$DeviceProtection <- combineLevels(data_test$DeviceProtection, levs = c("No", "No internet service"), newLabel = "No")
data_test$TechSupport <- combineLevels(data_test$TechSupport, levs = c("No", "No internet service"), newLabel = "No")
data_test$StreamingTV <- combineLevels(data_test$StreamingTV, levs = c("No", "No internet service"), newLabel = "No")
data_test$StreamingMovies <- combineLevels(data_test$StreamingMovies, levs = c("No", "No internet service"), newLabel = "No")
data_test$MultipleLines <- combineLevels(data_test$MultipleLines, levs = c("No", "No phone service"), newLabel = "No")

#________________________________EXPLORATORY_DATA_ANALYSIS_______________________
library(ggplot2)
library(ggrepel)
summary(data)
str(data)
summary(data_test)
str(data_test)

ggplot(data, aes(Churn, fill = factor(Churn))) + ggtitle("Churn") + ylab("Quantity") +
  scale_fill_manual(values=c("No"="royalblue1", "Yes"="indianred1")) + 
  theme(legend.position="none") +
  geom_bar(width = 0.5) +
  geom_text_repel(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), force = 0, size = 7) +
  coord_flip()

p1 <- ggplot(data, aes(gender, fill = Churn)) + ggtitle("Gender") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  scale_fill_manual(values=c("royalblue1","indianred1")) + 
  coord_flip()

p2 <- ggplot(data, aes(SeniorCitizen, fill = Churn)) + ggtitle("Senior Citizen") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  scale_fill_manual(values=c("royalblue1","indianred1")) + 
  coord_flip()

p3 <- ggplot(data, aes(Partner, fill = Churn)) + ggtitle("Partner") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  scale_fill_manual(values=c("royalblue1","indianred1")) + 
  coord_flip()

p4 <- ggplot(data, aes(Dependents, fill = Churn)) + ggtitle("Dependents") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  scale_fill_manual(values=c("royalblue1","indianred1")) + 
  coord_flip()

p5 <- ggplot(data, aes(PhoneService, fill = Churn)) + ggtitle("Phone Service") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  scale_fill_manual(values=c("royalblue1","indianred1")) + 
  coord_flip()

p6 <- ggplot(data, aes(MultipleLines, fill = Churn)) + ggtitle("Multiple Lines") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  scale_fill_manual(values=c("royalblue1","indianred1")) + 
  coord_flip()

p7 <- ggplot(data, aes(InternetService, fill = Churn)) + ggtitle("Internet Service") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  scale_fill_manual(values=c("royalblue1","indianred1")) + 
  coord_flip()

p8 <- ggplot(data, aes(OnlineSecurity, fill = Churn)) + ggtitle("Online Security") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  scale_fill_manual(values=c("royalblue1","indianred1")) + 
  coord_flip()

p9 <- ggplot(data, aes(OnlineBackup, fill = Churn)) + ggtitle("Online Backup") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  scale_fill_manual(values=c("royalblue1","indianred1")) + 
  coord_flip()

p10 <- ggplot(data, aes(DeviceProtection, fill = Churn)) + ggtitle("Device Protection") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  scale_fill_manual(values=c("royalblue1","indianred1")) + 
  coord_flip()

p11 <- ggplot(data, aes(TechSupport, fill = Churn)) + ggtitle("Tech Support") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  scale_fill_manual(values=c("royalblue1","indianred1")) + 
  coord_flip()

p12 <- ggplot(data, aes(StreamingTV, fill = Churn)) + ggtitle("Streaming TV") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  scale_fill_manual(values=c("royalblue1","indianred1")) + 
  coord_flip()

p13 <- ggplot(data, aes(StreamingMovies, fill = Churn)) + ggtitle("Streaming Movies") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  scale_fill_manual(values=c("royalblue1","indianred1")) + 
  coord_flip()

p14 <- ggplot(data, aes(Contract, fill = Churn)) + ggtitle("Contract") + ylab("Count") +
  geom_bar(width = 0.5) +
  scale_fill_manual(values=c("royalblue1","indianred1")) + 
  coord_flip()

p15 <- ggplot(data, aes(PaperlessBilling, fill = Churn)) + ggtitle("Paperless Billing") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  scale_fill_manual(values=c("royalblue1","indianred1")) + 
  coord_flip()

p16 <- ggplot(data, aes(PaymentMethod, fill = Churn)) + ggtitle("Payment Method") + ylab("Quantity") + 
  geom_bar(width = 0.5) +
  scale_fill_manual(values=c("royalblue1","indianred1")) + 
  coord_flip()

library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, ncol=4)

ggplot(data, aes(tenure, fill = Churn)) + ggtitle("Tenure") + xlab("Months") + ylab("Percentage") +
  geom_histogram(binwidth = 1, position = position_fill()) + 
  scale_fill_manual(values=c("royalblue1","indianred1")) 

ggplot(data, aes(MonthlyCharges, fill = Churn)) + ggtitle("Monthly Charges") + xlab("Months") + ylab("Percentage") +
  geom_histogram(binwidth = 1, position = position_stack()) + 
  scale_fill_manual(values=c("royalblue1","indianred1")) 

cor(data$TotalCharges, data$tenure)

boxplot(data$Churn, data$tenure, ylab = "Months")

p15 <- ggplot(data, aes(Churn, tenure, fill = Churn)) + ylab("Tenure") + 
  geom_boxplot() + 
  scale_fill_manual(values=c("royalblue1","indianred1"))

p16 <- ggplot(data, aes(Churn, MonthlyCharges, fill = Churn)) + ylab("Monthly Charges") + 
  geom_boxplot() + 
  scale_fill_manual(values=c("royalblue1","indianred1"))

p17 <- ggplot(data, aes(Churn, TotalCharges, fill = Churn)) + ylab("Total Charges") + 
  geom_boxplot() + 
  scale_fill_manual(values=c("royalblue1","indianred1"))

grid.arrange(p15,p16,p17, nrow = 1)


plot(data$tenure, data$TotalCharges, xlab="Tenure", ylab="TotalCharges")
plot(data$MonthlyCharges, data$TotalCharges, xlab="MonthlyCharges", ylab="TotalCharges")
cor(data$InternetService=="Fiber optic", data$MonthlyCharges)
cor(data$StreamingTV=="Yes", data$MonthlyCharges)

p21 <- ggplot(data_test, aes(gender)) + ggtitle("Gender") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  coord_flip()

p22 <- ggplot(data_test, aes(SeniorCitizen)) + ggtitle("Senior Citizen") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  coord_flip()

p23 <- ggplot(data_test, aes(Partner)) + ggtitle("Partner") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  coord_flip()

p24 <- ggplot(data_test, aes(Dependents)) + ggtitle("Dependents") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  coord_flip()

p25 <- ggplot(data_test, aes(PhoneService)) + ggtitle("Phone Service") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  coord_flip()

p26 <- ggplot(data_test, aes(MultipleLines)) + ggtitle("Multiple Lines") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  coord_flip()

p27 <- ggplot(data_test, aes(InternetService)) + ggtitle("Internet Service") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  coord_flip()

p28 <- ggplot(data_test, aes(OnlineSecurity)) + ggtitle("Online Security") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  coord_flip()

p29 <- ggplot(data_test, aes(OnlineBackup)) + ggtitle("Online Backup") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  coord_flip()

p30 <- ggplot(data_test, aes(DeviceProtection)) + ggtitle("Device Protection") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  coord_flip()

p31 <- ggplot(data_test, aes(TechSupport)) + ggtitle("Tech Support") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  coord_flip()

p32 <- ggplot(data_test, aes(StreamingTV)) + ggtitle("Streaming TV") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  coord_flip()

p33 <- ggplot(data_test, aes(StreamingMovies)) + ggtitle("Streaming Movies") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  coord_flip()

p34 <- ggplot(data_test, aes(Contract)) + ggtitle("Contract") + ylab("Count") +
  geom_bar(width = 0.5) +
  coord_flip()

p35 <- ggplot(data_test, aes(PaperlessBilling)) + ggtitle("Paperless Billing") + ylab("Quantity") +
  geom_bar(width = 0.5) +
  coord_flip()

p36 <- ggplot(data_test, aes(PaymentMethod)) + ggtitle("Payment Method") + ylab("Quantity") + 
  geom_bar(width = 0.5) +
  coord_flip()

library(gridExtra)
grid.arrange(p21, p22, p23, p24, p25, p26, p27, p28, p29, p30, p31, p32, p33, p34, p35, p36, ncol=4)

summary(data_test)


#________________________________DATA_SPLIT______________________________________

# Split data set into training and test data set 
smp_size = floor(0.8*nrow(data))
set.seed(123)
sample = sample(seq_len(nrow(data)),size = smp_size)
train_data =data[sample,]
test_data =data[-sample,]

#________________________________TEST_MODELS______________________________________

#______________________________INFLUENCE_OF_SINGLE_IVS_ON_DV______________________
glm_gender <- glm(Churn ~ gender, data = train_data, family=binomial)
summary(glm_gender)

glm_SeniorCitizen <- glm(Churn ~ SeniorCitizen, data = train_data, family=binomial)
summary(glm_SeniorCitizen)

glm_Partner <- glm(Churn ~ Partner, data = train_data, family=binomial)
summary(glm_Partner)

glm_Dependents <- glm(Churn ~ Dependents, data = train_data, family=binomial)
summary(glm_Dependents)

glm_tenure <- glm(Churn ~ tenure, data = train_data, family=binomial)
summary(glm_tenure)

glm_PhoneService <- glm(Churn ~ PhoneService, data = train_data, family=binomial)
summary(glm_PhoneService)

glm_MultipleLines <- glm(Churn ~ MultipleLines, data = train_data, family=binomial)
summary(glm_MultipleLines)

glm_InternetService <- glm(Churn ~ InternetService, data = train_data, family=binomial)
summary(glm_InternetService)

glm_OnlineSecurity <- glm(Churn ~ OnlineSecurity, data = train_data, family=binomial)
summary(glm_OnlineSecurity)

glm_OnlineBackup <- glm(Churn ~ OnlineBackup, data = train_data, family=binomial)
summary(glm_OnlineBackup)

glm_DeviceProtection <- glm(Churn ~ DeviceProtection, data = train_data, family=binomial)
summary(glm_DeviceProtection)

glm_TechSupport <- glm(Churn ~ TechSupport, data = train_data, family=binomial)
summary(glm_TechSupport)

glm_StreamingTV <- glm(Churn ~ StreamingTV, data = train_data, family=binomial)
summary(glm_StreamingTV)

glm_StreamingMovies <- glm(Churn ~ StreamingMovies, data = train_data, family=binomial)
summary(glm_StreamingMovies)

glm_Contract <- glm(Churn ~ Contract, data = train_data, family=binomial)
summary(glm_Contract)

glm_PaperlessBilling <- glm(Churn ~ PaperlessBilling, data = train_data, family=binomial)
summary(glm_PaperlessBilling)

glm_PaymentMethod <- glm(Churn ~ PaymentMethod, data = train_data, family=binomial)
summary(glm_PaymentMethod)

glm_MonthlyCharges <- glm(Churn ~ MonthlyCharges, data = train_data, family=binomial)
summary(glm_MonthlyCharges)

glm_TotalCharges <- glm(Churn ~ TotalCharges, data = train_data, family=binomial)
summary(glm_TotalCharges)
#___________________________________LASSO_________________________________________ (Quelle: https://eight2late.wordpress.com/2017/07/11/a-gentle-introduction-to-logistic-regression-and-lasso-regularisation-using-r/)
library(glmnet)

#convert training data to matrix format
x<- model.matrix(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, train_data)

#convert class to numerical variable for train data
y <- ifelse(train_data$Churn=="Yes",1,0)


#perform grid search to find optimal value of lambda
#family= binomial => logistic regression, alpha=1 => lasso
cv.glmmod <- cv.glmnet(x, y, alpha=1, family="binomial")

#plot results
plot(cv.glmmod)

#min value of lambda
lambda_min <- cv.glmmod$lambda.min

#best value of lambda
lambda_1se <- cv.glmmod$lambda.1se

#regression coefficients
coef(cv.glmmod,s=lambda_1se)

# Variance-Analysis
library(DescTools)
library(car)
glm.lasso_var <-glm(Churn ~  SeniorCitizen + Dependents + tenure + PhoneService + InternetService + OnlineSecurity + OnlineBackup + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod, data = train_data, family=binomial)
summary(glm.lasso_var)
# Run Anova
anova(glm.lasso_var, test="Chisq")

# Assess Model Fit 
# Pseudo R2
library(DescTools)
PseudoR2(glm.lasso_var, which = "all")
# Brier Score
BrierScore(glm.lasso_var, scaled = FALSE)
# Hosmer-Lemeshow-test
library(ResourceSelection)
h02lasso <- hoslem.test(glm.lasso_var$y, fitted(glm.lasso_var), g=10)
print(h02lasso)
# Chi2 test
library(lmtest)
lrtest(glm.lasso_var)

  #VIF-values
vif(glm.lasso_var)


# Assess Model Performance - Accuracy, Sensitivity, Specificity, AUC
# Confusion Matrix (Threshold = 0.5)
library(caret)
library(e1071)
prob_lasso <- predict(glm.lasso_var, test_data, type="response")
pred_lasso <- ifelse(prob_lasso > 0.5,"Yes", "No")
pred_lasso <- as.factor(pred_lasso)
confusionMatrix(data = pred_lasso, positive = "Yes", reference= test_data$Churn)
#AUC
library(pROC)
auc(test_data$Churn, prob_lasso)


# Determine Threshold which maximizes Sensitivity/Specificity
library(pROC)
library(ROCR)
library(gplots)
ROCpred_lasso <- prediction(prob_lasso,test_data$Churn)
ROCperf_lasso <- performance(ROCpred_lasso, measure="tpr", x.measure="fpr")
plot(ROCperf_lasso, colorize=T, print.cutoffs.at = seq(0.1, by = 0.1))
abline(a=0, b= 1)
str(ROCperf_lasso)
roc_lasso <- roc(response = test_data$Churn, predictor = prob_lasso)

coords(roc_lasso, x= "best", best.method = "y", input = "threshold")
coords(roc_lasso, x= "best", best.method = "c", input = "threshold")

# Confusion Matrix (Threshold = 0.2956042)
pred_lasso_acc <- ifelse(prob_lasso > 0.2956042,"Yes", "No")
pred_lasso_acc <- as.factor(pred_lasso_acc)
confusionMatrix(data = pred_lasso_acc, positive = "Yes", reference= test_data$Churn)

# Determine Threshold which minimizes Misspecification Costs
coords(roc_lasso, x= "best", best.method = "y", best.weights = c(5.5,1278/4799), input = "threshold")

# Create Cost Vector
costvector <- c(0,1,5.5,0)

# Confusion Matrix (Threshold = 0.1765539)
pred_lasso_mc <- ifelse(prob_lasso > 0.1765539 ,"Yes", "No")
pred_lasso_mc <- as.factor(pred_lasso_mc)
confusionMatrix(data = pred_lasso_mc, positive = "Yes", reference= test_data$Churn)

# Calculate Misspecification Costs
matrixglm.lasso <- table(pred_lasso, test_data$Churn)
sum(matrixglm.lasso*costvector)
matrixglm.lasso <- table(pred_lasso_acc, test_data$Churn)
sum(matrixglm.lasso*costvector)
matrixglm.lasso <- table(pred_lasso_mc, test_data$Churn)
sum(matrixglm.lasso*costvector)

#________________________________GLM00____________________________________________

# Logistic regression including all variables (glm00)
glm00 <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, data = train_data, family=binomial)
summary(glm00)

# Run Anova
anova(glm00, test="Chisq")

# Assess Model Fit 
  # Pseudo R2
library(DescTools)
PseudoR2(glm00, which = "all")
  # Brier Score
BrierScore(glm00, scaled = FALSE)
  # Hosmer-Lemeshow-test
library(ResourceSelection)
h00 <- hoslem.test(glm00$y, fitted(glm00), g=10)
print(h00)
  # Chi2 test
library(lmtest)
lrtest(glm00)
  # VIF-values
library(DescTools)
library(car)
vif(glm00)




# Assess Model Performance - Accuracy, Sensitivity, Specificity, AUC
  # Confusion Matrix (Threshold = 0.5)
library(caret)
library(e1071)
prob00 <- predict(glm00, test_data, type="response")
pred00 <- ifelse(prob00 > 0.5,"Yes", "No")
pred00 <- as.factor(pred00)
confusionMatrix(data = pred00, positive = "Yes", reference= test_data$Churn)
auc(test_data$Churn, prob00)

# Calculate Misspecification Costs
m1glm00 <- table(pred00, test_data$Churn)
sum(m1glm00*costvector)

  # Determine Threshold which maximizes Sensitivity/Specificity
library(pROC)
library(ROCR)
library(gplots)
ROCpred00 <- prediction(prob00,test_data$Churn)
ROCperf00 <- performance(ROCpred00, measure="tpr", x.measure="fpr")
plot(ROCperf00, colorize=T, print.cutoffs.at = seq(0.1, by = 0.1))
abline(a=0, b= 1)
str(ROCperf00)
roc00 <- roc(response = test_data$Churn, predictor = prob00)
coords(roc00, x= "best", best.method = "y", input = "threshold")
coords(roc00, x= "best", best.method = "c", input = "threshold")

  # Confusion Matrix (Threshold = 0.3243960)
pred00.1 <- ifelse(prob00 > 0.3243960,"Yes", "No")
pred00.1 <- as.factor(pred00.1)
confusionMatrix(data = pred00.1, positive = "Yes", reference= test_data$Churn)

# Calculate Misspecification Costs
m2glm00 <- table(pred00.1, test_data$Churn)
sum(m2glm00*costvector)

  # Determine Threshold which minimizes Misspecification Costs
coords(roc00, x= "best", best.method = "y", best.weights = c(5.5,1278/4799), input = "threshold")

  # Create Cost Vector
costvector <- c(0,1,5.5,0)

  # Confusion Matrix (Threshold = 0.1615181)
pred00.2 <- ifelse(prob00 > 0.1615181 ,"Yes", "No")
pred00.2 <- as.factor(pred00.2)
confusionMatrix(data = pred00.2, positive = "Yes", reference= test_data$Churn)

  # Calculate Misspecification Costs
matrixglm00 <- table(pred00.2, test_data$Churn)
s00 <- sum(matrixglm00*costvector)

#________________________________GLM00.FWD_________________________________________
library(MASS)
glm00.fwd <- stepAIC(glm00, direction="forward", trace = FALSE)
glm00.fwd$anova

# Assessment of model fit and model performance not needed because glm00.fwd is the same model as glm00

#________________________________GLM00.BWD_________________________________________
library(MASS)
glm00.bwd <- stepAIC(glm00, direction="backward", trace = FALSE)
glm00.bwd$anova
summary(glm00.bwd)

# Assess Model Fit 
# Pseudo R2
library(DescTools)
PseudoR2(glm00.bwd, which = "all")
# Brier Score
BrierScore(glm00.bwd, scaled = FALSE)
# Hosmer-Lemeshow-test
library(ResourceSelection)
h00.bwd <- hoslem.test(glm00.bwd$y, fitted(glm00.bwd), g=10)
print(h00.bwd)
# Chi2 test
library(lmtest)
lrtest(glm00.bwd)
# VIF-values
library(DescTools)
library(car)
vif(glm00.bwd)

# Assess Model Performance - Accuracy, Sensitivity, Specificity, AUC
# Confusion Matrix (Threshold = 0.5)
library(caret)
library(e1071)
prob00.bwd <- predict(glm00.bwd, test_data, type="response")
pred00.bwd <- ifelse(prob00.bwd > 0.5,"Yes", "No")
pred00.bwd <- as.factor(pred00.bwd)
confusionMatrix(data = pred00.bwd, positive = "Yes", reference= test_data$Churn)
auc(test_data$Churn, prob00.bwd)

# Calculate Misspecification Costs
m1glm00.bwd <- table(pred00.bwd, test_data$Churn)
sum(m1glm00.bwd*costvector)

# Determine Threshold which maximizes Sensitivity/Specificity
library(pROC)
library(ROCR)
library(gplots)
ROCpred00.bwd <- prediction(prob00.bwd,test_data$Churn)
ROCperf00.bwd <- performance(ROCpred00.bwd, measure="tpr", x.measure="fpr")
plot(ROCperf00.bwd, colorize=T, print.cutoffs.at = seq(0.1, by = 0.1))
abline(a=0, b= 1)
str(ROCperf00.bwd)
roc00.bwd <- roc(response = test_data$Churn, predictor = prob00.bwd)
coords(roc00.bwd, x= "best", best.method = "y", input = "threshold")
coords(roc00.bwd, x= "best", best.method = "c", input = "threshold")

# Confusion Matrix (Threshold = 0.3078156)
pred00.bwd.1 <- ifelse(prob00.bwd > 0.3078156,"Yes", "No")
pred00.bwd.1 <- as.factor(pred00.bwd.1)
confusionMatrix(data = pred00.bwd.1, positive = "Yes", reference= test_data$Churn)

# Calculate Misspecification Costs
m2glm00.bwd <- table(pred00.bwd.1, test_data$Churn)
sum(m2glm00.bwd*costvector)

# Determine Threshold which minimizes Misspecification Costs
coords(roc00, x= "best", best.method = "y", best.weights = c(5.5,1278/4799), input = "threshold")

# Confusion Matrix (Threshold = 0.1615181)
pred00.bwd.2 <- ifelse(prob00.bwd > 0.1615181,"Yes", "No")
pred00.bwd.2 <- as.factor(pred00.bwd.2)
confusionMatrix(data = pred00.bwd.2, positive = "Yes", reference= test_data$Churn)

# Calculate Misspecification Costs
m3glm00.bwd <- table(pred00.bwd.2, test_data$Churn)
sum(m3glm00.bwd*costvector)

#________________________________GLM00.BTH_________________________________________
library(MASS)
glm00.bth <- stepAIC(glm00, trace = FALSE)
glm00.bth$anova

# Assessment of model fit and model performance not needed because glm00.bth is the same model as glm00.bwd

#________________________________GLM01____________________________________________
# Check whether IVs tenure, MonthlyCharges and TotalCharges are correlated
cor(train_data$tenure*train_data$MonthlyCharges, train_data$TotalCharges)
# Result shows that IVs are highly correlated. As MonthlyCharges is insignificant in the full model and highly correlated with TotalCharges, the variable is omitted in the next model

# Logistic regression excluding MonthlyCharges
glm01 <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + TotalCharges, data = train_data, family=binomial)
summary(glm01)

# Run Anova
anova(glm01, test="Chisq")

# Assess Model Fit 
# Pseudo R2
library(DescTools)
PseudoR2(glm01, which = "all")
# Brier Score
BrierScore(glm01, scaled = FALSE)
# Hosmer-Lemeshow-test
library(ResourceSelection)
h01 <- hoslem.test(glm01$y, fitted(glm01), g=10)
print(h01)
# Chi2 test
library(lmtest)
lrtest(glm01)
# VIF-values
library(DescTools)
library(car)
vif(glm01)

# Assess Model Performance - Accuracy, Sensitivity, Specificity, AUC
# Confusion Matrix (Threshold = 0.5)
library(caret)
library(e1071)
prob01 <- predict(glm01, test_data, type="response")
pred01 <- ifelse(prob01 > 0.5,"Yes", "No")
pred01 <- as.factor(pred01)
confusionMatrix(data = pred01, positive = "Yes", reference= test_data$Churn)
auc(test_data$Churn, prob01)

# Calculate Misspecification Costs
m1glm01 <- table(pred01, test_data$Churn)
sum(m1glm01*costvector)

# Determine Threshold which maximizes Sensitivity/Specificity
library(pROC)
library(ROCR)
library(gplots)
ROCpred01 <- prediction(prob01,test_data$Churn)
ROCperf01 <- performance(ROCpred01, measure="tpr", x.measure="fpr")
plot(ROCperf01, colorize=T, print.cutoffs.at = seq(0.1, by = 0.1))
abline(a=0, b= 1)
str(ROCperf01)
roc01 <- roc(response = test_data$Churn, predictor = prob01)
coords(roc01, x= "best", best.method = "y", input = "threshold")
coords(roc01, x= "best", best.method = "c", input = "threshold")

# Confusion Matrix (Threshold = 0.3121001)
pred01.1 <- ifelse(prob01 > 0.3121001,"Yes", "No")
pred01.1 <- as.factor(pred01.1)
confusionMatrix(data = pred01.1, positive = "Yes", reference= test_data$Churn)

# Calculate Misspecification Costs
m2glm01.1 <- table(pred01.1, test_data$Churn)
sum(m2glm01.1*costvector)

# Determine Threshold which minimizes Misspecification Costs
coords(roc01, x= "best", best.method = "y", best.weights = c(5.5,1278/4799), input = "threshold")

# Confusion Matrix (Threshold = 0.1588196)
pred01.2 <- ifelse(prob01 > 0.1588196,"Yes", "No")
pred01.2 <- as.factor(pred01.2)
confusionMatrix(data = pred01.2, positive = "Yes", reference= test_data$Churn)

# Calculate Misspecification Costs
matrixglm01 <- table(pred01.2, test_data$Churn)
s01 <- sum(matrixglm01*costvector)

#________________________________GLM01.FWD_________________________________________
library(MASS)
glm01.fwd <- stepAIC(glm01, direction="forward", trace = FALSE)
glm01.fwd$anova

# Assessment of model fit and model performance not needed because glm01.fwd is the same model as glm01

#________________________________GLM01.BWD_________________________________________
library(MASS)
glm01.bwd <- stepAIC(glm01, direction="backward", trace = FALSE)
glm01.bwd$anova
summary(glm01.bwd)

# Assess Model Fit 
# Pseudo R2
library(DescTools)
PseudoR2(glm01.bwd, which = "all")
# Brier Score
BrierScore(glm01.bwd, scaled = FALSE)
# Hosmer-Lemeshow-test
library(ResourceSelection)
h01.bwd <- hoslem.test(glm01.bwd$y, fitted(glm01.bwd), g=10)
print(h01.bwd)
# Chi2 test
library(lmtest)
lrtest(glm01.bwd)
# VIF-values
library(DescTools)
library(car)
vif(glm01.bwd)

# Assess Model Performance - Accuracy, Sensitivity, Specificity, AUC
# Confusion Matrix (Threshold = 0.5)
library(caret)
library(e1071)
prob01.bwd <- predict(glm01.bwd, test_data, type="response")
pred01.bwd <- ifelse(prob01.bwd > 0.5,"Yes", "No")
pred01.bwd <- as.factor(pred01.bwd)
confusionMatrix(data = pred01.bwd, positive = "Yes", reference= test_data$Churn)
auc(test_data$Churn, prob01.bwd)

# Calculate Misspecification Costs
m1glm01.bwd <- table(pred01.bwd, test_data$Churn)
sum(m1glm01.bwd*costvector)

# Determine Threshold which maximizes Sensitivity/Specificity
library(pROC)
library(ROCR)
library(gplots)
ROCpred01.bwd <- prediction(prob01.bwd,test_data$Churn)
ROCperf01.bwd <- performance(ROCpred01.bwd, measure="tpr", x.measure="fpr")
plot(ROCperf01.bwd, colorize=T, print.cutoffs.at = seq(0.1, by = 0.1))
abline(a=0, b= 1)
str(ROCperf01.bwd)
roc01.bwd <- roc(response = test_data$Churn, predictor = prob01.bwd)
coords(roc01.bwd, x= "best", best.method = "y", input = "threshold")
coords(roc01.bwd, x= "best", best.method = "c", input = "threshold")

# Confusion Matrix (Threshold = 0.3046870)
pred01.bwd.1 <- ifelse(prob01.bwd > 0.3046870,"Yes", "No")
pred01.bwd.1 <- as.factor(pred01.bwd.1)
confusionMatrix(data = pred01.bwd.1, positive = "Yes", reference= test_data$Churn)

# Calculate Misspecification Costs
m2glm01.bwd <- table(pred01.bwd.1, test_data$Churn)
sum(m2glm01.bwd*costvector)

# Determine Threshold which minimizes Misspecification Costs
coords(roc01, x= "best", best.method = "y", best.weights = c(5.5,1278/4799), input = "threshold")

# Confusion Matrix (Threshold = 0.1588196)
pred01.bwd.2 <- ifelse(prob01.bwd > 0.1588196,"Yes", "No")
pred01.bwd.2 <- as.factor(pred01.bwd.2)
confusionMatrix(data = pred01.bwd.2, positive = "Yes", reference= test_data$Churn)

# Calculate Misspecification Costs
matrixglm01.bwd <- table(pred01.bwd.2, test_data$Churn)
s01.bwd <- sum(matrixglm01.bwd*costvector)

#________________________________GLM01.BTH_________________________________________
library(MASS)
glm01.bth <- stepAIC(glm01, trace = FALSE)
glm01.bth$anova

# Assessment of model fit and model performance not needed because glm01.bth is the same model as glm01.bwd

#________________________________GLM02____________________________________________
# Check whether IVs tenure and TotalCharges are correlated
cor(train_data$tenure, train_data$TotalCharges)
# Result shows that IVs are highly correlated. As TotalCharges indirectly includes tenure, the variable tenure is omitted in the next model

# Logistic regression excluding MonthlyCharges and tenure
glm02 <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + TotalCharges, data = train_data, family=binomial)
summary(glm02)

# Run Anova
anova(glm02, test="Chisq")

# Assess Model Fit 
# Pseudo R2
library(DescTools)
PseudoR2(glm02, which = "all")
# Brier Score
BrierScore(glm02, scaled = FALSE)
# Hosmer-Lemeshow-test
library(ResourceSelection)
h02 <- hoslem.test(glm02$y, fitted(glm02), g=10)
print(h02)
# Chi2 test
library(lmtest)
lrtest(glm02)
# VIF-values
library(DescTools)
library(car)
vif(glm02)

# Assess Model Performance - Accuracy, Sensitivity, Specificity, AUC
# Confusion Matrix (Threshold = 0.5)
library(caret)
library(e1071)
prob02 <- predict(glm02, test_data, type="response")
pred02 <- ifelse(prob02 > 0.5,"Yes", "No")
pred02 <- as.factor(pred02)
confusionMatrix(data = pred02, positive = "Yes", reference= test_data$Churn)
auc(test_data$Churn, prob02)

# Calculate Misspecification Costs
m1glm02 <- table(pred02, test_data$Churn)
sum(m1glm02*costvector)

# Determine Threshold which maximizes Sensitivity/Specificity
library(pROC)
library(ROCR)
library(gplots)
ROCpred02 <- prediction(prob02,test_data$Churn)
ROCperf02 <- performance(ROCpred02, measure="tpr", x.measure="fpr")
plot(ROCperf02, colorize=T, print.cutoffs.at = seq(0.1, by = 0.1))
abline(a=0, b= 1)
str(ROCperf02)
roc02 <- roc(response = test_data$Churn, predictor = prob02)
coords(roc02, x= "best", best.method = "y", input = "threshold")
coords(roc02, x= "best", best.method = "c", input = "threshold")

# Confusion Matrix (Threshold = 0.3114642)
pred02.1 <- ifelse(prob02 > 0.3114642,"Yes", "No")
pred02.1 <- as.factor(pred02.1)
confusionMatrix(data = pred02.1, positive = "Yes", reference= test_data$Churn)

# Calculate Misspecification Costs
m2glm02 <- table(pred02.1, test_data$Churn)
sum(m2glm02*costvector)

# Determine Threshold which minimizes Misspecification Costs
coords(roc02, x= "best", best.method = "y", best.weights = c(5.5,1278/4799), input = "threshold")

# Confusion Matrix (Threshold = 0.08503122)
pred02.2 <- ifelse(prob02 > 0.08503122,"Yes", "No")
pred02.2 <- as.factor(pred02.2)
confusionMatrix(data = pred02.2, positive = "Yes", reference= test_data$Churn)

# Calculate Misspecification Costs
m3glm02 <- table(pred02.2, test_data$Churn)
s02 <- sum(m3glm02*costvector)

#________________________________GLM02.FWD_________________________________________
library(MASS)
glm02.fwd <- stepAIC(glm02, direction="forward", trace = FALSE)
glm02.fwd$anova

# Assessment of model fit and model performance not needed because glm02.fwd is the same model as glm02

#________________________________GLM02.BWD_________________________________________
library(MASS)
glm02.bwd <- stepAIC(glm02, direction="backward", trace = FALSE)
glm02.bwd$anova
summary(glm02.bwd)

# Assess Model Fit 
# Pseudo R2
library(DescTools)
PseudoR2(glm02.bwd, which = "all")
# Brier Score
BrierScore(glm02.bwd, scaled = FALSE)
# Hosmer-Lemeshow-test
library(ResourceSelection)
h02.bwd <- hoslem.test(glm02.bwd$y, fitted(glm02.bwd), g=10)
print(h02.bwd)
# Chi2 test
library(lmtest)
lrtest(glm02.bwd)
# VIF-values
library(DescTools)
library(car)
vif(glm02.bwd)

# Assess Model Performance - Accuracy, Sensitivity, Specificity, AUC
# Confusion Matrix (Threshold = 0.5)
library(caret)
library(e1071)
prob02.bwd <- predict(glm02.bwd, test_data, type="response")
pred02.bwd <- ifelse(prob02.bwd > 0.5,"Yes", "No")
pred02.bwd <- as.factor(pred02.bwd)
confusionMatrix(data = pred02.bwd, positive = "Yes", reference= test_data$Churn)
auc(test_data$Churn, prob02.bwd)

# Calculate Misspecification Costs
m1glm02.bwd <- table(pred02.bwd, test_data$Churn)
sum(m1glm02.bwd*costvector)

# Determine Threshold which maximizes Sensitivity/Specificity
library(pROC)
library(ROCR)
library(gplots)
ROCpred02.bwd <- prediction(prob02.bwd,test_data$Churn)
ROCperf02.bwd <- performance(ROCpred02.bwd, measure="tpr", x.measure="fpr")
plot(ROCperf02.bwd, colorize=T, print.cutoffs.at = seq(0.1, by = 0.1))
abline(a=0, b= 1)
str(ROCperf02.bwd)
roc02.bwd <- roc(response = test_data$Churn, predictor = prob02.bwd)
coords(roc02.bwd, x= "best", best.method = "y", input = "threshold")
coords(roc02.bwd, x= "best", best.method = "c", input = "threshold")

# Confusion Matrix (Threshold =  0.2844892)
pred02.bwd.1 <- ifelse(prob02.bwd >  0.2844892,"Yes", "No")
pred02.bwd.1 <- as.factor(pred02.bwd.1)
confusionMatrix(data = pred02.bwd.1, positive = "Yes", reference= test_data$Churn)

# Calculate Misspecification Costs
m2glm02.bwd <- table(pred02.bwd.1, test_data$Churn)
sum(m2glm02.bwd*costvector)

# Determine Threshold which minimizes Misspecification Costs
coords(roc02.bwd, x= "best", best.method = "y", best.weights = c(5.5,1278/4799), input = "threshold")

# Confusion Matrix (Threshold =   0.08508848)
pred02.bwd.2 <- ifelse(prob02.bwd >   0.085088482,"Yes", "No")
pred02.bwd.2 <- as.factor(pred02.bwd.2)
confusionMatrix(data = pred02.bwd.2, positive = "Yes", reference= test_data$Churn)

# Calculate Misspecification Costs
matrixglm02.bwd <- table(pred02.bwd.2, test_data$Churn)
s02.bwd <- sum(matrixglm02.bwd*costvector)

#________________________________GLM02.BTH_________________________________________
library(MASS)
glm02.bth <- stepAIC(glm02, trace = FALSE)
glm02.bth$anova

# Assessment of model fit and model performance not needed because glm02.bth is the same model as glm02.bwd

#_________________________________________________________________________________

#Logistic regressions excluding non-significant variables one after another
glm00nogender <- glm(Churn ~ SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, data = train_data, family=binomial)
summary(glm00nogender)

glm00noPartner <- glm(Churn ~ gender + SeniorCitizen + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, data = train_data, family=binomial)
summary(glm00noPartner)

glm00noDependents <- glm(Churn ~ gender + SeniorCitizen + Partner + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, data = train_data, family=binomial)
summary(glm00noDependents)

glm00noPhoneService <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, data = train_data, family=binomial)
summary(glm00noPhoneService)

glm00noOnlineSecurity <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, data = train_data, family=binomial)
summary(glm00noOnlineSecurity)

glm00noOnlineBackup <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, data = train_data, family=binomial)
summary(glm00noOnlineBackup)

glm00noDeviceProtection <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, data = train_data, family=binomial)
summary(glm00noDeviceProtection)

glm00noTechSupport <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, data = train_data, family=binomial)
summary(glm00noTechSupport)

glm00noMonthlyCharges <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + TotalCharges, data = train_data, family=binomial)
summary(glm00noMonthlyCharges)

#____________________________________CROSS_VALIDATION_________________________________
#Holdout Method
# Confusion Matrix (Threshold = 0.1765539)
library(caret)
library(e1071)
prob_lasso_mc_train <- predict(glm.lasso_var, train_data, type="response")
pred_lasso_mc_train <- ifelse(prob_lasso_mc_train > 0.1765539 ,"Yes", "No")
pred_lasso_mc_train <- as.factor(pred_lasso_mc_train)
confusionMatrix(data = pred_lasso_mc_train, positive = "Yes", reference= train_data$Churn)
library(pROC)
auc(train_data$Churn, prob_lasso_mc_train)

#Misclassification Costs
matrixglm.lasso_train <- table(pred_lasso_mc_train, train_data$Churn)
sum(matrixglm.lasso_train*costvector)

#K-fold cross-validation
#Split train_data in k=5 subsets (as we already have one subset of 1/5, we split the remaining 4/5 into 1/5 each)
set.seed(7)
ss <- sample(1:4,size=nrow(train_data),replace=T,prob=c(0.2,0.2,0.2,0.2))
k_1 <- train_data[ss==1,]
k_2 <- train_data[ss==2,]
k_3 <- train_data[ss==3,]
k_4 <- train_data[ss==4,]

library(caret)
createDataPartition(1:4, train_data)

#Apply model on test dataset

prob_lasso_test <- predict(glm.lasso_var, data_test, type="response")
pred_lasso_test <- ifelse(prob_lasso_test > 0.1765539 ,"Yes", "No")
pred_lasso_test <- as.factor(pred_lasso_test)
table(pred_lasso_test)
churners <- subset(data_test, pred_lasso_test=="Yes")
churners$customerID
plot(pred_lasso_test, prob_lasso_test, ylab="Probability to Churn")
ggplot(data=data_test, aes(prob_lasso_test,pred_lasso_test)) + 
  geom_jitter()

ggplot(data=data_test, aes(nrow(data_test),prob_lasso_test, color=pred_lasso_test)) + 
  xlab("Customer Index") + 
  ylab("Predicted Churn Probability") + 
  geom_jitter() + 
  geom_hline(yintercept = 0.1765539) + 
  scale_color_manual(values = c("Yes" = "indianred1", "No" = "royalblue1"), name = "Predicted Class", labels=c("Non-Churner", "Churner"))

ggplot(data=test_data, aes(nrow(test_data),prob_lasso, color=test_data$Churn)) + 
  xlab("Customer Index") + 
  ylab("Predicted Churn Probability") + 
  geom_jitter() + 
  geom_hline(yintercept = 0.1765539) + 
  scale_color_manual(values = c("Yes" = "indianred1", "No" = "royalblue1"), name = "Actual Class", labels=c("Non-Churner", "Churner"))
  
 

