# Install Packages --------------------------------------------------------

install.packages("readxl")
install.packages("tidyverse")
install.packages("data.table")
install.packages("ggplot2")
install.packages("caret")
install.packages("lubridate"))

# Load Packages ------------------------------------------------------------

require(readxl)
require(tidyverse)
require(data.table)
require(ggplot2)
require(caret)
require(lubridate)

# Import 2017 Data -------------------------------------------------------------

data_2017 = read_excel("Data\\Data January 2017.xlsx")
# write.csv(data_2017, "Data_January_2017.csv")

data = fread("Data\\Data_January_2017.csv", na.strings = c("-", "NA"))

# Replace "-" by NA
data[data == "-"] = NA

#Convert features into right data types
data$V1 = as.character(data$V1)
data$`Zip code` = as.character(data$`Zip code`)
data$DBII = as.numeric(data$DBII)
data$Contract_ID = as.character(data$Contract_ID)
data$Age = as.integer(data$Age)
data$`Customer since` = ymd(data$`Customer since`)
data$`Contract start date` = ymd(data$`Contract start date`)
data$Consumption = as.numeric(data$Consumption) 


# Explore Data I ----------------------------------------------------------

str(data)
summary(data)

# Clean Data --------------------------------------------------------------

# Detect Percentage of NA's per feature

apply(data, 2, function(col)sum(is.na(col))/length(col))


# Data Preparation --------------------------------------------------------

# Customers older than 105 years


# Modeling ----------------------------------------------------------------

myGrid = data.frame()

myControl = trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary)

model = train(churn ~., data = data, method = "xgbLinear", 
              trControl = myControl, tuneGrid = myGrid
              )

plot(model)

# Model Evaluation --------------------------------------------------------

confusionMatrix()


# Import 2018 Data ----------------------------------------------------------------

data_2018 = read_excel("Data\\Data November 2018.xlsx")
# write.csv(data_2018, "Data_November_2018.csv")

data_2018 = fread("Data\\Data_November_2018.csv", na.strings = "-")
