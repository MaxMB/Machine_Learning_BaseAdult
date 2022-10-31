library(e1071)

shell("cls") #cat("\014") --- clear console
rm(list=ls()) # clear variables
setwd("C:/Users/Marcelo/Documents/Poli/POS/2022-P1/PCS5024 - Aprendizado estatistico/Trab")

Train_Data <- read.table("adult_PP.data", header=TRUE)
Test_Data <- read.table("adult_PP.test", header=TRUE)

# Support Vector Machine
svm.fits <- svm(out50k ~ ., data=Train_Data, kernel="linear", cost=2, scale=FALSE)

i <- which(names(Train_Data) %in% "out50k")
svm.test <- predict(object=svm.fits, newdata=Test_Data[,-i], type="response")

svm.pred <- rep(0, nrow(Test_Data))
svm.pred[svm.test > 0.5] = 1

CM <- table(svm.pred, Test_Data$out50k) # Confusion Matrix
acc <- (CM[1,1] + CM[2,2]) / sum(CM)
prec <- CM[1,1] / (CM[1,1] + CM[2,1])
sn <- CM[1,1] / (CM[1,1] + CM[1,2])

# Accuracy: ACC = (TP + TN) / (P + N)
# Precision (positive predictive value): PREC = TP / (TP + FP)
# Sensitivity (Recall or True positive rate): SN = TP / P = TP / (TP + FN)
# Specificity (True negative rate): SP = TN / N = TN / (TN + FP)
# False positive rate: FPR = FP / N = FP / (TN + FP) = 1 - SP

Results <- data.frame(acc, prec, sn)
write.table(Results, file="Classification_SVM.txt", append=FALSE, sep=" ", col.names=TRUE)