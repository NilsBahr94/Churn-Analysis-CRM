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

# data_2017 = read_excel("Data\\Data January 2017.xlsx")
# write.csv(data_2017, "Data_January_2017.csv")

# N
data = fread("Data\\Data_January_2017.csv", na.strings = c("-", "NA"))

# Replace "-" by NA
data[data == "-"] = NA

#Convert features into right data types
data$V1 = as.character(data$V1)
data$Title = as.factor(data$Title)
data$`Zip code` = as.character(data$`Zip code`)
data$DBII = as.numeric(data$DBII)
data$Contract_ID = as.character(data$Contract_ID)
data$Age = as.integer(data$Age)
data$`Customer since` = ymd(data$`Customer since`)
data$`Contract start date` = ymd(data$`Contract start date`)
data$Consumption = as.numeric(data$Consumption) 
data$Churn = as.factor(data$Churn)

data$`Client type` = as.factor(data$`Client type`)
data$`Bill shock` = as.factor(data$`Bill shock`)
data$`Opt In Mail` = as.factor(data$`Opt In Mail`)
data$`Opt In Post` = as.factor(data$`Opt In Post`)
data$`Opt In Tel` = as.factor(data$`Opt In Tel`)

# Online Account - NA to 0 

setnames(data, `Client type`, Client_type)


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

data_2018 = read_excel("Data\\Data November 2018.xlsx")
# write.csv(data_2018, "Data_November_2018.csv")

data_2018 = fread("Data\\Data_November_2018.csv", na.strings = "-")
