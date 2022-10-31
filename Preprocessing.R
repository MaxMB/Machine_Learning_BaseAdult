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

Train_Data$age <- as.numeric(scale(Train_Data$age))
Train_Data$fnlwgt <- as.numeric(scale(Train_Data$fnlwgt))
Train_Data$edu_num <- as.numeric(scale(Train_Data$edu_num))
Train_Data$capital_gain <- as.numeric(scale(Train_Data$capital_gain))
Train_Data$capital_loss <- as.numeric(scale(Train_Data$capital_loss))
Train_Data$hours_week <- as.numeric(scale(Train_Data$hours_week))

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

Test_Data$age <- as.numeric(scale(Test_Data$age))
Test_Data$fnlwgt <- as.numeric(scale(Test_Data$fnlwgt))
Test_Data$edu_num <- as.numeric(scale(Test_Data$edu_num))
Test_Data$capital_gain <- as.numeric(scale(Test_Data$capital_gain))
Test_Data$capital_loss <- as.numeric(scale(Test_Data$capital_loss))
Test_Data$hours_week <- as.numeric(scale(Test_Data$hours_week))

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

for (i in names(Train_Data_PP)) {
  if (!(i %in% names(Test_Data_PP))) {
    Train_Data_PP <- Train_Data_PP[,-which(names(Train_Data_PP) %in% i)]
  }
}

for (i in names(Test_Data_PP)) {
  if (!(i %in% names(Train_Data_PP))) {
    Test_Data_PP <- Test_Data_PP[,-which(names(Test_Data_PP) %in% i)]
  }
}

write.table(Train_Data_PP, file="adult_PP.data", append=FALSE, sep=" ", col.names=TRUE)
write.table(Test_Data_PP, file="adult_PP.test", append=FALSE, sep=" ", col.names=TRUE)



# MV_train <- 0
# for (i in 1:nrow(Train_Data)) {
#   for (j in 1:ncol(Train_Data)) {
#     if (Train_Data[i,j] == "?") {
#       MV_train <- MV_train + 1
#       i <- i + 1
#       j <- 0
#     }
#   }
# }
# 
# MV_train # 2531 --> 2531 / 32561 = 7.773103 %

# MV_test <- 0
# for (i in 1:nrow(Test_Data)) {
#   for (j in 1:ncol(Test_Data)) {
#     if (Test_Data[i,j] == "?") {
#       MV_test <- MV_test + 1
#       i <- i + 1
#       j <- 0
#     }
#   }
# }
# 
# MV_test # 1293 --> 1293 / 16281 = 7.941773 %