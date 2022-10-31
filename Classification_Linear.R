shell("cls") #cat("\014") --- clear console
rm(list=ls()) # clear variables
setwd("C:/Users/Marcelo/Documents/Poli/POS/2022-P1/PCS5024 - Aprendizado estatistico/Trab")

Train_Data <- read.table("adult_PP.data", header=TRUE)
Test_Data <- read.table("adult_PP.test", header=TRUE)

# least squares linear regression -> linear model
lm.fits <- lm(out50k ~ ., data=Train_Data)

i <- which(names(Train_Data) %in% "out50k")
lm.test <- predict(object=lm.fits, newdata=Test_Data[,-i], type="response")

lm.pred <- rep(0, nrow(Test_Data))
lm.pred[lm.test > 0.5] = 1

CM <- table(lm.pred, Test_Data$out50k) # Confusion Matrix
acc <- (CM[1,1] + CM[2,2]) / sum(CM)
prec <- CM[1,1] / (CM[1,1] + CM[2,1])
sn <- CM[1,1] / (CM[1,1] + CM[1,2])

# Accuracy: ACC = (TP + TN) / (P + N)
# Precision (positive predictive value): PREC = TP / (TP + FP)
# Sensitivity (Recall or True positive rate): SN = TP / P = TP / (TP + FN)
# Specificity (True negative rate): SP = TN / N = TN / (TN + FP)
# False positive rate: FPR = FP / N = FP / (TN + FP) = 1 - SP

Results <- data.frame(acc, prec, sn)
write.table(Results, file="Classification_Linear.txt", append=FALSE, sep=" ", col.names=TRUE)