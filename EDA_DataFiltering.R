library(readr)
library(tidyverse)
# Download DOHMH NYC Restaurant Inspection Results data set and save as CSV file: NYC_Insp_Results.csv
#Open_Data_Sample <- read.csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv",
                           #  col_types  =  cols(ZIPCODE = col_character())
#)
my_data<-read.csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv")
Open_Data_Sample=my_data
#count(my_data,c('CAMIS'))
MyCount=my_data %>% count(CAMIS)
MyCount

head(Open_Data_Sample)
#View(Open_Data_Sample)
#Filter on inspection type, score, grade
Inspections <- Open_Data_Sample %>%
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


#Select distinct inspections
Inspections_Distinct <- distinct(Inspections)
head(Inspections_Distinct)

#Select most recent inspection date
MostRecentInsp <- Inspections_Distinct %>%
  group_by(CAMIS) %>%
  slice(which.max(as.Date(INSPECTION.DATE,'%m/%d/%y')))

df11=MostRecentInsp%>%full_join(MyCount,by="CAMIS")
head(df1)

#write.csv(MostRecentInsp,"MostRecentInspections.csv",row.names = FALSE)




df10=Open_Data_Sample%>%inner_join(MostRecentInsp, by = c("CAMIS","INSPECTION.DATE","SCORE"))
head(df10)
write.csv(df10,"Restaurant_Most_Recent_Inspections.csv",row.names = FALSE)
#Select restaurant inspection data based on most recent inspection date
Final <- Open_Data_Sample %>% inner_join(MostRecentInsp) %>%
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









mydata<-tbl_df(my_data)
head(my_data)
#glimpse(mydata)
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
#class(mydata$CAMIS)
#class(mydata$SCORE)
n_distinct(mydata$CAMIS)
range(mydata$INSPECTION.DATE)
mydata<-filter(mydata, INSPECTION.DATE != "2000-01-01")

range(mydata$INSPECTION.DATE)
#levels(as.factor(mydata$`INSPECTION TYPE`)) 

Graded<- c("Cycle Inspection / Initial Inspection" , "Cycle Inspection / Re-inspection", 
               "Pre-permit (Operational) / Initial Inspection", "Pre-permit (Operational) / Re-inspection")

mydata <- (filter(mydata, INSPECTION.TYPE %in% Graded))
#levels(as.factor(mydata$INSPECTION.TYPE))

grades <- c("A", "B", "C")
nrow(filter(mydata, GRADE %in% grades))
nrow(mydata)
df=filter(mydata, GRADE %in% grades)
head(df)
#185K rows 18 variables
write.csv(df,"UniqueScoredData.csv",row.names = FALSE)

#

my_visits_data=read.csv("VisitRestaurants.csv")
head(my_visits_data)
nrow(my_visits_data)
MyCount2=my_visits_data %>% count(CAMIS)
MyCount2
dfCount=data_wide%>%full_join(MyCount2,by="CAMIS")
#write.csv(dfCount,"Visits_counted_Data.csv",row.names = FALSE)
str(dfCount)
Visits_counted_Data <- read.csv("~/Visits_counted_Data.csv")
head(Visits_counted_Data)
library(reshape2)

#data_wide <- dcast(my_visits_data,CAMIS+BORO+CUISINE.DESCRIPTION+ZIPCODE+GRADE~ Visit, value.var="SCORE")
data_wide <- dcast(my_visits_data,CAMIS+DBA+GRADE+BUILDING+STREET~ Visit, value.var="SCORE")
#write.csv(data_wide, file = "Inspection_Ranks.csv",row.names=FALSE)
head(data_wide)

#view(data_wide)




df=mydata%>%filter(GRADE %in% grades)%>%select(CAMIS, DBA, BORO, BUILDING, STREET, ZIPCODE, CUISINE.DESCRIPTION, INSPECTION.DATE, ACTION, SCORE, GRADE, INSPECTION.TYPE)
df2=unique(df)


Worst_Restaurants <- arrange(filter(df2, GRADE %in% grades), desc(SCORE))[1:20,c(2,10)]

groupCuisine <- group_by(df2, CUISINE.DESCRIPTION)
cuisineSummary <- summarize(groupCuisine,
                            "A's" = sum(GRADE == "A", na.rm = T)/sum(GRADE == "A" | GRADE == "B" | GRADE == "C", na.rm = T),
                            "B's" = sum(GRADE == "B", na.rm = T)/sum(GRADE == "A" | GRADE == "B" | GRADE == "C", na.rm = T),
                            "C's" = sum(GRADE == "C", na.rm = T)/sum(GRADE == "A" | GRADE == "B" | GRADE == "C", na.rm = T),
                            "Totals" = sum(GRADE == "A" | GRADE == "B" | GRADE == "C", na.rm = T)
)


