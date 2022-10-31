library(randomForest)

shell("cls") #cat("\014") --- clear console
rm(list=ls()) # clear variables
setwd("C:/Users/Marcelo/Documents/Poli/POS/2022-P1/PCS5024 - Aprendizado estatistico/Trab")

Train_Data <- read.table("adult_PP.data", header=TRUE)
Test_Data <- read.table("adult_PP.test", header=TRUE)

set.seed(1)
# Random Forest
rf.fits <- randomForest(out50k ~ ., data=Train_Data, mtry=round(sqrt(ncol(Train_Data)-1)), importance=TRUE)

i <- which(names(Train_Data) %in% "out50k")
rf.test <- predict(object=rf.fits, newdata=Test_Data[,-i])
 
rf.pred <- rep(0, nrow(Test_Data))
rf.pred[rf.test > 0.5] = 1

CM <- table(rf.pred, Test_Data$out50k) # Confusion Matrix
acc <- (CM[1,1] + CM[2,2]) / sum(CM)
prec <- CM[1,1] / (CM[1,1] + CM[2,1])
sn <- CM[1,1] / (CM[1,1] + CM[1,2])

# Accuracy: ACC = (TP + TN) / (P + N)
# Precision (positive predictive value): PREC = TP / (TP + FP)
# Sensitivity (Recall or True positive rate): SN = TP / P = TP / (TP + FN)
# Specificity (True negative rate): SP = TN / N = TN / (TN + FP)
# False positive rate: FPR = FP / N = FP / (TN + FP) = 1 - SP

Results <- data.frame(acc, prec, sn)
write.table(Results, file="Classification_Random_Forest.txt", append=FALSE, sep=" ", col.names=TRUE)