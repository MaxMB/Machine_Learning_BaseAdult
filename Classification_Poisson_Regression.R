shell("cls") #cat("\014") --- clear console
rm(list=ls()) # clear variables
setwd("C:/Users/Marcelo/Documents/Poli/POS/2022-P1/PCS5024 - Aprendizado estatistico/Trab")

Train_Data <- read.table("adult_PP.data", header=TRUE)
Test_Data <- read.table("adult_PP.test", header=TRUE)

# Poisson Regression
pr.fits <- glm(out50k ~ ., data=Train_Data, family=poisson)

i <- which(names(Train_Data) %in% "out50k")
pr.test <- predict(object=pr.fits, newdata=Test_Data[,-i], type="response")

pr.pred <- rep(0, nrow(Test_Data))
pr.pred[pr.test > 0.5] = 1

CM <- table(pr.pred, Test_Data$out50k) # Confusion Matrix
acc <- (CM[1,1] + CM[2,2]) / sum(CM)
prec <- CM[1,1] / (CM[1,1] + CM[2,1])
sn <- CM[1,1] / (CM[1,1] + CM[1,2])

# Accuracy: ACC = (TP + TN) / (P + N)
# Precision (positive predictive value): PREC = TP / (TP + FP)
# Sensitivity (Recall or True positive rate): SN = TP / P = TP / (TP + FN)
# Specificity (True negative rate): SP = TN / N = TN / (TN + FP)
# False positive rate: FPR = FP / N = FP / (TN + FP) = 1 - SP

Results <- data.frame(acc, prec, sn)
write.table(Results, file="Classification_Poisson_Regression.txt", append=FALSE, sep=" ", col.names=TRUE)
