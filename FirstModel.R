library(ERSA)
WideCleanData<-read.csv("Restaurants_WideData.csv")
#colnames(WideCleanData)
f=lm(X6 ~ CUISINE.DESCRIPTION+CRITICAL.FLAG+BORO+X1+X2+X3+X4+X5,data=WideCleanData)
exploreReg(f,WideCleanData)
