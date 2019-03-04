library(data.table)
library(dplyr)
library(randomForest)
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
################################randomForest########################################

fitRF <- randomForest(formula,data=trainingData)
importance(fitRF) # importance of each predictor
fitRF$importance
varImpPlot(fitRF,main="Important variables in the model")
#write.csv(importance(fitRF),"VariableImportances.csv",row.names = FALSE)
predtRF= predict(fitRF,testData)# ###Predict on the test dataset# #
mean(abs(predtRF-testData$INSPECTION33))

pred_all_RF= predict(fitRF,NewSubset)
RF_Predict=round(pred_all_RF)
#MEAN ABSOLUTE ERROR
mean(abs(RF_Predict-NewSubset$INSPECTION33)) #1.213
#AVERAGE SCORES
mean(NewSubset$INSPECTION33) #11.12

#PLOT TO SEE ERRORS DISTRIBUTIONS 
plot(NewSubset$INSPECTION33,(RF_Predict-NewSubset$INSPECTION33),col="blue",main="Errors Vs True values for Random Forest")
#THE DISTRIBUTION AROUND THE TRUE VALUES LOOKS FINE 
plot(RF_Predict-NewSubset$INSPECTION33,col="red")

##################Linear Regression##################################################

fitLR<-lm(formula,data=trainingData)
summary(fitLR)
predtLR= predict(fitLR,testData)#Predict on test dataset
mean(abs(predtLR-testData$INSPECTION33)) #

pred_all_LR= predict(fitLR,NewSubset) ##Predict All
mean(abs(pred_all_LR-NewSubset$INSPECTION33))
LR_Predict=round(pred_all_LR)
mean(abs(LR_Predict-NewSubset$INSPECTION33))# 1.4941
plot(NewSubset$INSPECTION33,(LR_Predict-NewSubset$INSPECTION33),col="red",main="Errors for Linear Regression")


#######################BASE_LINE-CARRYING "32nd to 33 rd "#####################################################

BL_Predict<-NewSubset$INSPECTION32
mean(abs(BL_Predict-NewSubset$INSPECTION33)) #3.21


#######################DECSISON TREES#####################################################

fitDT <- rpart(formula, data = trainingData,method="anova")
summary(fitDT)
# 
predDT= predict(fitDT,testData)
#
mean(abs(predDT-testData$INSPECTION33)) #2.66# #
DT_Predict<-predict(fitDT,NewSubset)
mean(abs(DT_Predict-NewSubset$INSPECTION33))###2.64


##########################GRADIENT BOOSTING MACHINE###################################

library(gbm)
fitGBM=gbm(formula,data =trainingData,distribution = "gaussian",n.trees = 1000,
           shrinkage = 0.01, interaction.depth = 4)
fitGBM
predtGBM <- predict(fitGBM, n.trees = fitGBM$n.trees, testData)
mean(abs(predtGBM-testData$INSPECTION33))  #2.49
pred_All_GBM <- predict(fitGBM, n.trees = fitGBM$n.trees, NewSubset)
head(pred_All_GBM)
mean(abs(pred_All_GBM-NewSubset$INSPECTION33))  #2.40
GBM_Predict<-round(pred_All_GBM)
mean(abs(GBM_Predict-NewSubset$INSPECTION33)) # 2.39

#################################PREDICTIONS TABLES####################################

####PLEASE RUN IT ONLY 1 TIME TO SAVE VALUES INTO CSV FILES######

#PREDICTION.TABLE<-cbind(RF_Predict,BL_Predict,GBM_Predict,LR_Predict,SCORE,NewSubset)
#write.csv(PREDICTION.TABLE,file="Data_With_Predictions.csv",row.names = FALSE)
#ONLY.PREDICTIONS<-cbind(RF_Predict,BL_Predict,GBM_Predict,LR_Predict,SCORE)
#write.csv(ONLY.PREDICTIONS,file="Predictions_By_Model.csv",row.names = FALSE)
