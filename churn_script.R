# Install Packages --------------------------------------------------------

install.packages("readxl")
install.packages("tidyverse")
install.packages("data.table")
install.packages("ggplot2")
install.packages("caret")

# Load Packages ------------------------------------------------------------

require(readxl)
require(tidyverse)
require(data.table)
require(ggplot2)
require(caret)

# Import Data -------------------------------------------------------------

data_2017 = read_excel("Data\\Data January 2017.xlsx")
# write.csv(data_2017, "Data_January_2017.csv")
data_2018 = read_excel("Data\\Data November 2018.xlsx")
# write.csv(data_2018, "Data_November_2018.csv")

data_2017 = fread("Data\\Data_January_2017.csv", na.strings = "-")
data_2018 = fread("Data\\Data_November_2018.csv", na.strings = "-")


# Explore Data I ----------------------------------------------------------

str(data_2017)
summary(data_2017)

# Clean Data --------------------------------------------------------------

# NA's instead of "-"


# Detect NA's per feature
