library(class)

shell("cls") #cat("\014") --- clear console
rm(list=ls()) # clear variables
setwd("C:/Users/Marcelo/Documents/Poli/POS/2022-P1/PCS5024 - Aprendizado estatistico/Trab")

Train_Data <- read.table("adult_PP.data", header=TRUE)
Test_Data <- read.table("adult_PP.test", header=TRUE)

set.seed(1)
i <- which(names(Train_Data) %in% "out50k")

n <- 30 # grid search 1 to n
acc <- rep(0, n)
prec <- rep(0, n)
sn <- rep(0, n)
for (j in 1:n) {
  knn.fit <- knn(train=Train_Data[,-i], test=Test_Data[,-i], cl=Train_Data[,i], k=j)
  CM <- table(knn.fit, Test_Data$out50k) # Confusion Matrix
  acc[j] <- (CM[1,1] + CM[2,2]) / sum(CM)
  prec[j] <- CM[1,1] / (CM[1,1] + CM[2,1])
  sn[j] <- CM[1,1] / (CM[1,1] + CM[1,2])
}
k <- 1:n

# Accuracy: ACC = (TP + TN) / (P + N)
# Precision (positive predictive value): PREC = TP / (TP + FP)
# Sensitivity (Recall or True positive rate): SN = TP / P = TP / (TP + FN)
# Specificity (True negative rate): SP = TN / N = TN / (TN + FP)
# False positive rate: FPR = FP / N = FP / (TN + FP) = 1 - SP

Results <- data.frame(k, acc, prec, sn)
write.table(Results, file="Classification_kNN.txt", append=FALSE, sep=" ", col.names=TRUE)