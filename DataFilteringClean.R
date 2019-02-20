library(readr)
library(randomForest)
library(rpart)
library(tidyverse)
my_data<-read.csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv")
MyCount=my_data %>% count(CAMIS)
Insp <- my_data %>%
  filter((INSPECTION.TYPE %in% 
            c('Cycle Inspection / Re-inspection'
              ,'Pre-permit (Operational) / Re-inspection')
          |(INSPECTION.TYPE %in%
              c('Cycle Inspection / Initial Inspection'
                ,'Pre-permit (Operational) / Initial Inspection')) 
          & SCORE <= 13)
         | (INSPECTION.TYPE %in%  
              c('Pre-permit (Operational) / Reopening Inspection'
                ,'Cycle Inspection / Reopening Inspection'))
         & GRADE %in% c('A', 'B', 'C', 'P', 'Z')) %>%
  
  select(CAMIS,DBA,BUILDING,STREET,SCORE,INSPECTION.DATE)


#Select distinct Insp
Insp_Distinct <- distinct(Insp)
head(Insp_Distinct)

#recent inspection date
LastRecentInsp <- Insp_Distinct %>%
  group_by(CAMIS) %>%
  slice(which.max(as.Date(INSPECTION.DATE,'%m/%d/%y')))

df11=LastRecentInsp%>%full_join(MyCount,by="CAMIS")
#head(df11)

write.csv(LastRecentInsp,"LastRecentInsp.csv",row.names = FALSE)

df10=my_data%>%inner_join(LastRecentInsp, by = c("CAMIS","INSPECTION.DATE","SCORE"))
#head(df10)
write.csv(df10,"Restaurant_Most_Recent_Insp.csv",row.names = FALSE)
#retrieve restaurants inspection data based recent inspection date
Final <- my_data %>% inner_join(LastRecentInsp) %>%
  filter((INSPECTION.TYPE%in% 
            c('Cycle Inspection / Re-inspection'
              ,'Pre-permit (Operational) / Re-inspection'
              , 'Pre-permit (Operational) / Reopening Inspection' 
              ,'Cycle Inspection / Reopening Inspection')
          |(INSPECTION.TYPE %in%
              c('Cycle Inspection / Initial Inspection'
                ,'Pre-permit (Operational) / Initial Inspection')) 
          & SCORE <= 13)) %>%
  
  select(CAMIS,DBA,INSPECTION.DATE,GRADE,INSPECTION.TYPE,SCORE)

head(my_data)
mydata$INSPECTION.DATE<-(as.Date(mydata$INSPECTION.DATE, "%m/%d/%y"))
head(mydata)
mydata$GRADE.DATE<-(as.Date(mydata$GRADE.DATE, "%m/%d/%y"))
mydata$RECORD.DATE<-(as.Date(mydata$RECORD.DATE, "%m/%d/%y"))
class(mydata$RECORD.DATE)
mydata$GRADE <- factor(mydata$GRADE, levels = c("A", "B", "C", "Z", "Not Yet Graded"), ordered = TRUE)
(levels(mydata$GRADE))
mydata$CRITICAL.FLAG <-   factor(mydata$CRITICAL.FLAG, levels = c("Critical", "Not Critical", "Not Applicable"), ordered = T)
levels(mydata$CRITICAL.FLAG)
mydata$CAMIS <- as.numeric(mydata$CAMIS)
mydata$SCORE <- as.numeric(mydata$SCORE)
n_distinct(mydata$CAMIS)
range(mydata$INSPECTION.DATE)
#Remove restaurants without inspection yet
mydata<-filter(mydata, INSPECTION.DATE != "2000-01-01")
range(mydata$INSPECTION.DATE)

Graded<- c("Cycle Inspection / Initial Inspection" , "Cycle Inspection / Re-inspection", 
               "Pre-permit (Operational) / Initial Inspection", "Pre-permit (Operational) / Re-inspection")

mydata <- (filter(mydata, INSPECTION.TYPE %in% Graded))

grades <- c("A", "B", "C")
nrow(filter(mydata, GRADE %in% grades))
df=filter(mydata, GRADE %in% grades)

#Distinct Inspection
dfnew=mydata%>%select(CAMIS, DBA, BORO, BUILDING, STREET, ZIPCODE, CUISINE.DESCRIPTION, INSPECTION.DATE, ACTION, SCORE, GRADE, INSPECTION.TYPE)
nrow(df2)
str(df2$CUISINE.DESCRIPTION)
df2=distinct(dfnew)
str(df2$ACTION)
df2$ZIPCODE
PriorityRanks <- arrange(filter(df2, GRADE %in% grades), desc(SCORE))[,c("DBA","CAMIS","SCORE","INSPECTION.DATE","BUILDING","STREET","ZIPCODE","ACTION")]
write.csv(PriorityRanks,"PriorityRanks.csv",row.names = FALSE)

library(lattice)
filter1 <- filter(df2, GRADE %in% grades)
filter(df2, GRADE %in% grades)$GRADE%>%
  factor(levels = c("A", "B", "C"), ordered= T) %>%
  table()%>%
  barchart(horizontal = F, main = "Grades of Restaurant Allowed to Stay Open")

InitialGrade <- select(filter(df2, INSPECTION.TYPE=="Cycle Inspection / Initial Inspection" | INSPECTION.TYPE == "Pre-permit (Operational) / Initial Inspection"), CAMIS, SCORE, INSPECTION.DATE)

GRADEHYP <- cut(InitialGrade$SCORE, breaks = c(0, 13, 27, 200), 
                labels = c("A", "B", "C"), include.lowest = T)

firstGrades <- (mutate (InitialGrade,GRADEHYP))
firstGrades
df3<- df2%>%left_join(firstGrades, by = c("CAMIS", "SCORE", "INSPECTION.DATE"))

firstGrades$GRADEHYP%>%
  factor(levels = c("A", "B", "C"), ordered= T) %>%
  table()%>%
  barchart(horizontal = F, main = "Frequency of Estimated Grades upon Initial Inspection")


#head(df)
#185K rows 18 variables
#write.csv(df,"UniqueScoredData.csv",row.names = FALSE)
read.csv("UniqueScoredData.csv")
#
#
my_visits_data=read.csv("VisitRestaurants.csv")
head(my_visits_data)
nrow(my_visits_data)
MyCount2=my_visits_data %>% count(CAMIS)
MyCount2
dfCount=data_wide%>%full_join(MyCount2,by="CAMIS")
write.csv(dfCount,"Visits_counted_Data.csv",row.names = FALSE)
str(dfCount)

#INSPECTION COUNTED WITH  EACH SCORE WIDE FORMAT DATASET
Visits_counted_Data <- read.csv("Visits_counted_Data.csv")
head(Visits_counted_Data)
OpenRestaurants=df%>%full_join(MyCount2,by="CAMIS")
write.csv(OpenRestaurants,"OpenRestaurants.csv",row.names = FALSE)

###OPEN RESTAURANTS WITH SINGLE INSPECTION SCORE 

OpenRestaurantsData<-read.csv("OpenRestaurants.csv")
head(OpenRestaurantsData)


### CASTING THE DATA INTO WIDE FORMAT 
library(reshape2)

data_wide <- dcast(my_visits_data,CAMIS+BORO+CUISINE.DESCRIPTION+ZIPCODE+GRADE~ Visit, value.var="SCORE")
data_wide <- dcast(my_visits_data,CAMIS+DBA+GRADE+BUILDING+STREET~ Visit, value.var="SCORE")
write.csv(data_wide, file = "Inspection_Ranks.csv",row.names=FALSE)
#nrow(data_wide)
data_wide_new=dcast(my_visits_data,CAMIS~ Visit,value.var ="SCORE")
write.csv(data_wide_new,"CleanWideData.csv",row.names =FALSE)

# ROW IMPUTATIONS

##### Imputation of multiple rows (i.e. the whole data frame) #####

for(i in 1:nrow(Newdf)) {
  Newdf[i, ][is.na(Newdf[i, ])] <- mean(as.numeric(Newdf[i, ]), na.rm = TRUE)
}
head(Newdf) 
FinalWideData=cbind(data_wide_new$CAMIS,round(Newdf))
write.csv(FinalWideData, file = "FinalWideData.csv",row.names=FALSE)
#write.csv(FinalWideData, file = "EngineeredData.csv",row.names=FALSE)
Finaldata=read.csv("FinalWideData.csv")
head(Finaldata)
head(Finaldata)
Xdata=Finaldata[,-1]

Inspections2 <- my_data %>%
  filter((INSPECTION.TYPE %in% 
            c('Cycle Inspection / Re-inspection'
              ,'Pre-permit (Operational) / Re-inspection')
          |(INSPECTION.TYPE %in%
              c('Cycle Inspection / Initial Inspection'
                ,'Pre-permit (Operational) / Initial Inspection')) 
          & SCORE <= 13)
         | (INSPECTION.TYPE %in%  
              c('Pre-permit (Operational) / Reopening Inspection'
                ,'Cycle Inspection / Reopening Inspection'))
         & GRADE %in% c('A', 'B', 'C', 'P', 'Z')) %>%
  
  select(SCORE,INSPECTION.DATE,CAMIS,DBA,BUILDING,STREET,CUISINE.DESCRIPTION,BORO,ZIPCODE)

#Select distinct inspections
Inspections_Distinct2 <- distinct(Inspections2)
head(Inspections_Distinct2)

#Select most recent inspection date
MostRecentInsp2 <- Inspections_Distinct2%>%
  group_by(CAMIS) %>%
  slice(which.max(as.Date(INSPECTION.DATE,'%m/%d/%y')))
head(MostRecentInsp2)

library(data.table)

#setnames(Finaldata, "data_wide_new.CAMIS", "CAMIS")

Interdf2=Finaldata%>%left_join(MostRecentInsp2, by="CAMIS")
colnames(Interdf2)

write.csv(Interdf2,"EngineeredData.csv",row.names = FALSE)
EngData=read.csv("EngineeredData.csv")


#####COMPARING METHODS
#Random forest
formula=SCORE~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22+X23+X24+X25+X26+X27+X28+X29+X30+X31+X32
trainingRowIndex <- sample(1:nrow(EngData), 0.8*nrow(EngData))  # row indices for training data
trainingData <- EngData[trainingRowIndex, ]  # model training data
testData  <- EngData[-trainingRowIndex, ]   # test data
fitRF <- randomForest(formula,data=trainingData)
importance(fitRF) # importance of each predictor
predRF= predict(fitRF,testData)#
mean((predRF-testData$SCORE)^2)  #

#Linear Regression
fitLR<-lm(formula,data=trainingData)
summary(fitLR)
predLR= predict(fitLR,testData)
mean((predLR-testData$SCORE)^2) #

#head(newdata)
#Decision Trees
library(rpart)
fitDT <- rpart(formula, data = trainingData,method="anova")
summary(fitDT)
#Predict Output 
predDT= predict(fitDT,testData)
#predicted
mean((predDT-testData$SCORE)^2) # 
###gbm

fit.gbm <- train(formula, data=EngData, method = 'gbm', trControl=fitControl,tuneGrid=Grid,metric='RMSE',maximize=FALSE)

summary(fit.gbm)
predgbm= predict(fit.gbm,EngData)
mean((predgbm-EngData$SCORE)^2)  

set.seed(123)
tc <- trainControl("oob")
Grid <- expand.grid(mtry = seq(4,16,4))
#formula <- lgcount ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + wday + month + year
fit.rf <- train(formula, data=EngData , method='rf', trControl=tc,tuneGrid=Grid,metric='MSE')
predfit.rf= predict(fit.rf,EngData)
predfit.rf
mean((predfit.rf-EngData$SCORE)^2)


