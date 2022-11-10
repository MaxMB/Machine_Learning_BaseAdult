library(fastDummies)
library(dplyr)

shell("cls") #cat("\014") --- clear console
rm(list=ls()) # clear variables
setwd("C:/Users/Marcelo/Documents/Poli/POS/2022-P1/PCS5024 - Aprendizado estatistico/Trab")

### PREPROCESSING TRAIN DATA

Train_Data <- read.table(
  file = "adult.data",
  header = TRUE,
  sep = ",",
  strip.white = TRUE,
  na.strings = "?",
  stringsAsFactors = TRUE,
  col.names = c("age",
                "work",
                "fnlwgt",
                "edu",
                "edu_num",
                "marital",
                "occup",
                "relat",
                "race",
                "sex",
                "capital_gain",
                "capital_loss",
                "hours_week",
                "country",
                "out50k")
)

Train_Data <- na.omit(Train_Data[,-4]) # Remove missing values rows and EDU column
Train_Data <- distinct(Train_Data) # remove duplicate rows

Train_Data$sex <- ifelse(Train_Data$sex == "Male", 1, 0)
Train_Data$out50k <- ifelse(Train_Data$out50k == ">50K", 1, 0)

## Train Scaling - Z-score (or Standard Score)
age_mn <- mean(Train_Data$age)
age_sd <- sd(Train_Data$age)
Train_Data$age <- (Train_Data$age - age_mn) / age_sd
# Train_Data$age <- as.numeric(scale(Train_Data$age))
fnlwgt_mn <- mean(Train_Data$fnlwgt)
fnlwgt_sd <- sd(Train_Data$fnlwgt)
Train_Data$fnlwgt <- (Train_Data$fnlwgt - fnlwgt_mn) / fnlwgt_sd
# Train_Data$fnlwgt <- as.numeric(scale(Train_Data$fnlwgt))
edu_num_mn <- mean(Train_Data$edu_num)
edu_num_sd <- sd(Train_Data$edu_num)
Train_Data$edu_num <- (Train_Data$edu_num - edu_num_mn) / edu_num_sd
# Train_Data$edu_num <- as.numeric(scale(Train_Data$edu_num))
capital_gain_mn <- mean(Train_Data$capital_gain)
capital_gain_sd <- sd(Train_Data$capital_gain)
Train_Data$capital_gain <- (Train_Data$age - capital_gain_mn) / capital_gain_sd
# Train_Data$capital_gain <- as.numeric(scale(Train_Data$capital_gain))
capital_loss_mn <- mean(Train_Data$capital_loss)
capital_loss_sd <- sd(Train_Data$capital_loss)
Train_Data$capital_loss <- (Train_Data$capital_loss - capital_loss_mn) / capital_loss_sd
# Train_Data$capital_loss <- as.numeric(scale(Train_Data$capital_loss))
hours_week_mn <- mean(Train_Data$hours_week)
hours_week_sd <- sd(Train_Data$hours_week)
Train_Data$hours_week <- (Train_Data$hours_week - hours_week_mn) / hours_week_sd
# Train_Data$hours_week <- as.numeric(scale(Train_Data$hours_week))

# Create Dummy Columns (a type of one-hot encoding)
Train_Data_PP <- dummy_cols(
  Train_Data,
  select_columns = c('work',
                     'marital',
                     'occup',
                     'relat',
                     'race',
                     'country'),
  remove_most_frequent_dummy = TRUE,
  remove_selected_columns = TRUE
)

Train_Data_PP <- Train_Data_PP[ , order(names(Train_Data_PP))]




### PREPROCESSING TEST DATA

Test_Data <- read.table(
  file = "adult.test",
  header = TRUE,
  sep = ",",
  strip.white = TRUE,
  na.strings = "?",
  stringsAsFactors = TRUE,
  col.names = c("age",
                "work",
                "fnlwgt",
                "edu",
                "edu_num",
                "marital",
                "occup",
                "relat",
                "race",
                "sex",
                "capital_gain",
                "capital_loss",
                "hours_week",
                "country",
                "out50k")
)

Test_Data <- na.omit(Test_Data[,-4]) # Remove missing values rows and EDU column
Test_Data <- distinct(Test_Data) # remove duplicate rows

Test_Data$sex <- ifelse(Test_Data$sex == "Male", 1, 0)
Test_Data$out50k <- ifelse(Test_Data$out50k == ">50K.", 1, 0)

## Test Scaling - Z-score (or Standard Score) with Train Statistics
Test_Data$age <- (Test_Data$age - age_mn) / age_sd
Test_Data$fnlwgt <- (Test_Data$fnlwgt - fnlwgt_mn) / fnlwgt_sd
Test_Data$edu_num <- (Test_Data$edu_num - edu_num_mn) / edu_num_sd
Test_Data$capital_gain <- (Test_Data$age - capital_gain_mn) / capital_gain_sd
Test_Data$capital_loss <- (Test_Data$capital_loss - capital_loss_mn) / capital_loss_sd
Test_Data$hours_week <- (Test_Data$hours_week - hours_week_mn) / hours_week_sd

# Test_Data$age <- as.numeric(scale(Test_Data$age))
# Test_Data$fnlwgt <- as.numeric(scale(Test_Data$fnlwgt))
# Test_Data$edu_num <- as.numeric(scale(Test_Data$edu_num))
# Test_Data$capital_gain <- as.numeric(scale(Test_Data$capital_gain))
# Test_Data$capital_loss <- as.numeric(scale(Test_Data$capital_loss))
# Test_Data$hours_week <- as.numeric(scale(Test_Data$hours_week))

# Create Dummy Columns (a type of one-hot encoding)
Test_Data_PP <- dummy_cols(
  Test_Data,
  select_columns = c('work',
                     'marital',
                     'occup',
                     'relat',
                     'race',
                     'country'),
  remove_most_frequent_dummy = TRUE,
  remove_selected_columns = TRUE
)

Test_Data_PP <- Test_Data_PP[ , order(names(Test_Data_PP))]




### OUTPUT TXT
# Remove columns from Train_Data that isn't in Test_Data
for (i in names(Train_Data_PP)) {
  if (!(i %in% names(Test_Data_PP))) {
    Train_Data_PP <- Train_Data_PP[,-which(names(Train_Data_PP) %in% i)]
  }
}
# Remove columns from Test_Data that isn't in Train_Data
for (i in names(Test_Data_PP)) {
  if (!(i %in% names(Train_Data_PP))) {
    Test_Data_PP <- Test_Data_PP[,-which(names(Test_Data_PP) %in% i)]
  }
}
# Write the output files
write.table(Train_Data_PP, file="adult_PP.data", append=FALSE, sep=" ", col.names=TRUE)
write.table(Test_Data_PP, file="adult_PP.test", append=FALSE, sep=" ", col.names=TRUE)

print("Operations completed")
