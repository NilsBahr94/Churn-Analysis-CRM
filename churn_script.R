# Install Packages --------------------------------------------------------

install.packages("readxl")
install.packages("tidyverse")
install.packages("data.table")
install.packages("ggplot2")
install.packages("caret")
install.packages("lubridate")

# Load Packages ------------------------------------------------------------

require(readxl)
require(tidyverse)
require(data.table)
require(ggplot2)
require(caret)
require(lubridate)

# Import Data I -------------------------------------------------------------

data_2017 = read_excel("Data\\Data January 2017.xlsx")
# write.csv(data_2017, "Data_January_2017.csv")

data = fread("Data\\Data_January_2017.csv", na.strings = c("-", "NA"))

# Replace "-" by NA
data[data == "-"] = NA

#Convert data into right types
data$DBII = as.numeric(data$DBII) # Warning as output
data$Contract_ID = as.character(data$Contract_ID)
data$Age = as.integer(data$Age)
data$`Customer since` = ymd(data$`Customer since`)
data$`Contract start date` = ymd(data$`Contract start date`)
data$Consumption = as.numeric(data$Consumption) # Take care

# Check whether new NA's have been introduced by data type conversion
sum(is.na(data$DBII))





# Explore Data I ----------------------------------------------------------

str(data)
summary(data)


# Clean Data --------------------------------------------------------------

# NA's instead of "-"


# Detect NA's per feature

apply(data, 2, function(col)sum(is.na(col))/length(col))




# Test set ----------------------------------------------------------------

data_2018 = read_excel("Data\\Data November 2018.xlsx")
# write.csv(data_2018, "Data_November_2018.csv")

data_2018 = fread("Data\\Data_November_2018.csv", na.strings = "-")
