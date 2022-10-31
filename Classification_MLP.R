library(RSNNS)

shell("cls") #cat("\014") --- clear console
rm(list=ls()) # clear variables
setwd("C:/Users/Marcelo/Documents/Poli/POS/2022-P1/PCS5024 - Aprendizado estatistico/Trab")

Train_Data <- read.table("adult_PP.data", header=TRUE)
Test_Data <- read.table("adult_PP.test", header=TRUE)

i <- which(names(Train_Data) %in% "out50k")

set.seed(1)
mlp.fits <- mlp(x=Train_Data[,-i],
                y=Train_Data[,i],
                size = c(10, 4), # 2 hidden layers with 10 and 4 nodes
                maxit = 200, # max iterations
                initFunc = "Randomize_Weights",
                initFuncParams = c(-0.3, 0.3),
                learnFunc = "Std_Backpropagation",
                learnFuncParams = c(0.2, 0),
                updateFunc = "Topological_Order",
                updateFuncParams = c(0.1),
                hiddenActFunc = "Act_Logistic")

mlp.test <- predict(object=mlp.fits, newdata=Test_Data[,-i], type="response")

mlp.pred <- rep(0, nrow(Test_Data))
mlp.pred[mlp.test > 0.5] = 1

CM <- table(mlp.pred, Test_Data$out50k) # Confusion Matrix
acc <- (CM[1,1] + CM[2,2]) / sum(CM)
prec <- CM[1,1] / (CM[1,1] + CM[2,1])
sn <- CM[1,1] / (CM[1,1] + CM[1,2])

# Accuracy: ACC = (TP + TN) / (P + N)
# Precision (positive predictive value): PREC = TP / (TP + FP)
# Sensitivity (Recall or True positive rate): SN = TP / P = TP / (TP + FN)
# Specificity (True negative rate): SP = TN / N = TN / (TN + FP)
# False positive rate: FPR = FP / N = FP / (TN + FP) = 1 - SP

Results <- data.frame(acc, prec, sn)
write.table(Results, file="Classification_MLP.txt", append=FALSE, sep=" ", col.names=TRUE)