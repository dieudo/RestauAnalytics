library(data.table)
library(dplyr)
set.seed(100)
EngData3=read.csv("EngineeredCleanData.csv")
head(EngData3)
subsetdata=EngData3%>%select(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33)
colnames(subsetdata) <- paste("INSPECTION", 1:length(subsetdata), sep = "")
head(subsetdata)
NewSubset=cbind(EngData3%>%select(CAMIS,DBA,BORO,BUILDING,STREET,ZIPCODE,CUISINE.DESCRIPTION),subsetdata)
#write.csv(NewSubset,file="InspectionsInOrder.csv")
formula=INSPECTION33~.
trainingRowIndex <- sample(1:nrow(subsetdata), 0.8*nrow(subsetdata))
trainingData <- subsetdata[trainingRowIndex, ]  # model training data
testData  <- subsetdata[-trainingRowIndex, ] # test data
head(testData)
################################library(randomForest)########################################

fitRF <- randomForest(formula,data=trainingData)
importance(fitRF) # importance of each predictor
fitRF$importance
varImpPlot(fitRF,main="Important variables in the model")
write.csv(importance(fitRF),"VariableImportances.csv",row.names = FALSE)
predtRF= predict(fitRF,testData)# ###Predict on the test dataset
mean((predRF-testData$INSPECTION33)^2)# 13.56 #

pred_all_RF= predict(fitRF,NewSubset)
length(pred_all_RF)
RF_Predict=round(pred_all_RF)
mean((RF_Predict-NewSubset$INSPECTION33)^2) #5.06

##################Linear Regression##################################################

fitLR<-lm(formula,data=trainingData)
summary(fitLR)
predtLR= predict(fitLR,testData)#Predict on test dataset
mean((predtLR-testData$INSPECTION33)^2) #

pred_all_LR= predict(fitLR,NewSubset) ##Predict All
mean((pred_all_LR-NewSubset$INSPECTION33)^2)
LR_Predict=round(pred_all_LR)
mean((LR_Predict-NewSubset$INSPECTION33)^2)# 4.72



#######################BASE_LINE-CARRYING "32nd to 33 rd "#####################################################

BL_Predict<-NewSubset$INSPECTION32
mean((BL_Predict-NewSubset$INSPECTION33)^2)# 37.33505

#Decision Trees

library(rpart)
fitDT <- rpart(formula, data = trainingData,method="anova")
summary(fitDT)
#Predict Output 
predDT= predict(fitDT,testData)
#predicted
mean((predDT-testData$INSPECTION33)^2) # #17.61355
DT_Predict<-predict(fitDT,NewSubset)
mean((DT_Predict-NewSubset$INSPECTION33)^2)###17.31

##########################GRADIENT BOOSTING MACHINE###################################

library(gbm)
fitGBM=gbm(formula,data =trainingData,distribution = "gaussian",n.trees = 1000,
           shrinkage = 0.01, interaction.depth = 4)
fitGBM
predtGBM <- predict(fitGBM, n.trees = fitGBM$n.trees, testData)
mean((predtGBM-testData$INSPECTION33)^2)  #16.569 on the test
pred_All_GBM <- predict(fitGBM, n.trees = fitGBM$n.trees, NewSubset)
head(pred_All_GBM)
mean((pred_All_GBM-NewSubset$INSPECTION33)^2)  #14.0079
GBM_Predict<-round(pred_All_GBM)
mean((GBM_Predict-NewSubset$INSPECTION33)^2)
SCORE<-NewSubset$INSPECTION33


#################################PREDICTIONS TABLES####################################

PREDICTION.TABLE<-cbind(RF_Predict,BL_Predict,GBM_Predict,LR_Predict,SCORE,NewSubset)
write.csv(PREDICTION.TABLE,file="Data_With_Predictions.csv",row.names = FALSE)
ONLY.PREDICTIONS<-cbind(RF_Predict,BL_Predict,GBM_Predict,LR_Predict,SCORE)
write.csv(ONLY.PREDICTIONS,file="Predictions_By_Model.csv",row.names = FALSE)
