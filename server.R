

require(data.table)
require(lattice)
require(randomForest)
require(tidyr)
library(ggplot2)
library(dplyr)
library(shiny)
library(psych)
library(pander)


my_data<-read.csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv")
my_data<-tbl_df(my_data)  
#Finaldata=read.csv("FinalWideData.csv") ###
#Visits_counted_Data <- read.csv("Visits_counted_Data.csv")####
#OpenRestaurantsData<-read.csv("OpenRestaurants.csv")#####
#PR=read.csv("PriorityRanks.csv")
EngData=read.csv("EngineeredData.csv")
mydata=my_data
mydata$INSPECTION.DATE<-(as.Date(mydata$INSPECTION.DATE, "%m/%d/%y"))
mydata$GRADE.DATE<-(as.Date(mydata$GRADE.DATE, "%m/%d/%y"))
mydata$RECORD.DATE<-(as.Date(mydata$RECORD.DATE, "%m/%d/%y"))
mydata$GRADE <- factor(mydata$GRADE, levels = c("A", "B", "C", "Z", "Not Yet Graded"), ordered = TRUE)
(levels(mydata$GRADE))
mydata$CRITICAL.FLAG <-   factor(mydata$CRITICAL.FLAG, levels = c("Critical", "Not Critical", "Not Applicable"), ordered = T)
levels(mydata$CRITICAL.FLAG)
mydata$CAMIS <- as.numeric(mydata$CAMIS)
mydata$SCORE <- as.numeric(mydata$SCORE)
#Removing dates where there is no inspection records yet
mydata<-filter(mydata, INSPECTION.DATE != "2000-01-01")
#Keeping the graded cases
Graded<- c("Cycle Inspection / Initial Inspection" , "Cycle Inspection / Re-inspection", 
           "Pre-permit (Operational) / Initial Inspection", "Pre-permit (Operational) / Re-inspection")

mydata <- (filter(mydata, INSPECTION.TYPE %in% Graded))
#levels(as.factor(mydata$INSPECTION.TYPE))
grades <- c("A", "B", "C")
df=filter(mydata, GRADE %in% grades)

#Distinct Inspection
dfnew=mydata%>%select(CAMIS, DBA, BORO, BUILDING, STREET, ZIPCODE, CUISINE.DESCRIPTION, INSPECTION.DATE, ACTION, SCORE, GRADE, INSPECTION.TYPE)
df2=distinct(dfnew)

#PriorityRanks <- arrange(filter(df2, GRADE %in% grades), desc(SCORE))[,c("DBA","CAMIS","SCORE","BUILDING","STREET","ACTION")]
Worst_Restaurants <- arrange(filter(df2, GRADE %in% grades), desc(SCORE))[1:15,c(2,10)]

Results <- my_data%>%
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
  
  select(SCORE,INSPECTION.DATE,CAMIS,DBA,BUILDING,STREET)

#Select distinct Results
Results_Distinct <- distinct(Results)
#head(Results_Distinct)

#Select most recent inspection date
LastInspResult <- Results_Distinct %>%
  group_by(CAMIS) %>%
  slice(which.max(as.Date(INSPECTION.DATE,'%m/%d/%y')))

#formula=
#fitRF=lm(SCORE~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22+X23+X24+X25+X26+X27+X28+X29+X30+X31+X32,data=EngData)
formula=SCORE~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22+X23+X24+X25+X26+X27+X28+X29+X30+X31+X32
trainingRowIndex <- sample(1:nrow(EngData), 0.8*nrow(EngData))  # row indices for training data
trainingData <- EngData[trainingRowIndex, ]  # model training data
testData  <- EngData[-trainingRowIndex, ]   # test data
fitRF <- randomForest(formula,data=trainingData)
  
predt= predict(fitRF,EngData)

dataBind=cbind(round(predt),EngData$SCORE)
colnames(dataBind) <- c("PREDICTED.VALUE","ACTUAL.VALUE")
subset=select(EngData,DBA,CAMIS,BORO,ZIPCODE,CUISINE.DESCRIPTION,BUILDING,STREET)
Pred=cbind(dataBind,subset)


subset2=select(EngData,DBA,CAMIS,BORO,ZIPCODE,CUISINE.DESCRIPTION,BUILDING,STREET)
subset3=cbind(round(predt),EngData$X32)
colnames(subset3) <- c("PREDICTED.VALUE","PREVIOUS.VALUE")
Pred2=cbind(subset3,subset2)

Final <- Pred2 %>%
  filter((PREDICTED.VALUE>27)&(PREVIOUS.VALUE<14))

#Final # from A to C

Final1 <- Pred2 %>%
  filter((PREDICTED.VALUE>27)&(PREVIOUS.VALUE<28))
#Final1 # from A|B to C

#Even they were C they are moving
Final2<- Pred2 %>%
  filter((PREDICTED.VALUE<14)&(PREVIOUS.VALUE>27))
#Final2

Violation_codes<- my_data %>%
  group_by(VIOLATION.CODE) %>%
  summarise(count = n()) %>%arrange(desc(count))
p <- ggplot(Violation_codes[1:10,], aes(x=reorder(VIOLATION.CODE, count), y=count)) +
  geom_bar(stat='identity') +
  coord_flip()+
  theme_minimal() + labs(x='10 Most Violations Codes', y='Count')

Violation_counts <- my_data%>%
  group_by(VIOLATION.DESCRIPTION) %>%
  summarise(count = n())



#Violations=my_data %>% filter(GRADE %in% c("A","B","C"),CRITICAL.FLAG %in% "Critical") %>%
#  group_by(BORO,GRADE,VIOLATION.DESCRIPTION) %>% 
#  summarise(Count=n()) %>% mutate(Percent=Count/sum(Count)*100) %>% 
#  filter(Percent >= 10)

# Distribution of score variable
plot10 <- Results_Distinct %>% select(SCORE) %>% filter(SCORE != "NA") %>% group_by(SCORE) %>% tally()
dataBoroCount10 <- plot10
myPlot3 <- ggplot(data = dataBoroCount10, aes(x=SCORE,y=n))+ggtitle("Distribution of Scores")+ labs(x="Score",y="Frequency of restaurants")
myPlot3 + geom_bar(stat="identity", fill = "blue")+ xlim(-2,100) + theme(plot.title = element_text(hjust = 0.8))





shinyServer(function(input, output){
  inputData=my_data
  subset=select(my_data,BORO,CUISINE.DESCRIPTION,ZIPCODE,SCORE,GRADE)
  
  
  formulaText1<-reactive({
    paste(" ",input$variable)
  })
  
  
  output$caption1<-renderText({
    paste(" ",formulaText1())
  })
  output$caption2<-renderText({
    paste(" ",formulaText1())
  })
  output$caption3<-renderText({
    paste(" ",formulaText1())
  })
  
  output$view<-renderDataTable({
    
    if(input$variable=="Predicted Violations"){
      i<-1
    }
    if(input$variable=='Predicted from A to C'){
      i<-2
    }
    if(input$variable=='Predicted from A|B to C'){
      i<-3
    }
    
    if(input$variable=='Predicted from C to A'){
      i<-4
    }
    
    if(input$variable=="Entire Records"){
      i=5
    }
    
    
    if(i==1) {
      
      Pred
      
    }else {
      if(i==2){
        
        #Worst_Restaurants
        Final
        #Violations
        
        #dataCleanM
      } else{
        
        if(i==3){
          Final1
          #(arrange(GradesByCuisines, desc(`A's`)))[1:85,]
         
          
        } else{
          
          if(i==4){
            Final2
            
          }else{
          my_data
          }
          
        }
      }
      
    }
    
    
    
  })
  
  
  output$plot<-renderPlot({ 
  
  if(input$variable=="Predicted Violations"){
    i<-1
  }
  if(input$variable=='Predicted from A to C'){
    i<-2
  }
    if(input$variable=='Predicted from A|B to C'){
      i<-3
    } 
    
  if(input$variable=='Predicted from C to A'){
    i<-4
  }
  if(input$variable=="Entire Records"){
    i<-5
  }
  
  
  
  if(i==1) {
    
    # Violations%>%
    #   ggplot(.,aes(x="",y=Percent,color=VIOLATION.DESCRIPTION)) + 
    #   geom_point(aes(size=Percent),position=position_dodge(0.2),alpha=0.8) + 
    #   facet_grid(GRADE~BORO) + 
    #   scale_y_continuous(breaks = seq(10, 25, by = 5)) +
    #   labs(x="",y="Percent Common Violations") + theme_bw() + guides(size = FALSE) +
    #   theme(legend.position="bottom", 
    #         legend.direction="vertical",
    #         legend.key = element_rect(fill=NA),
    #         legend.title=element_blank(),
    #         legend.text=element_text(size=4),
    #         legend.justification = c(1, 0)) 
    
    
    #plot(fitRF,col="Blue",main="Error vs number of trees used")
   # plot(ouput$PREDICTED.VALUE,output$ACTUAL.VALUE,col="blue")
    plot(dataBind,col="blue",main="Predicted Values vs Actual Values")
 
    
    
    
  } else {
    if(i==2){
      
      myPlot3 + geom_bar(stat="identity", fill = "blue")+ xlim(-2,100) + theme(plot.title = element_text(hjust = 0.8))
      
      
    } else {
      if(i==3){
        
        
        
       p 
        
        
      } else{
        
        if(i==4){
          
          my_data %>% filter(GRADE %in% c("A","B","C"))%>%count(BORO,GRADE) %>%
            ggplot(.,aes(x=BORO,y=n,fill=GRADE)) + geom_bar(stat="identity",width=0.8,alpha=1,color="black") + facet_grid(GRADE~.,scale="free") +
            geom_text(aes(label=n),hjust=-.6,vjust=1.0) +
            labs(x="",y="Counts") +coord_flip()+theme_bw()
        
        }else{
          
       
          Worst_Restaurants$DBA <- factor(Worst_Restaurants$DBA, levels = rev(Worst_Restaurants$DBA))
          ggplot(Worst_Restaurants, aes(x = `DBA`, y = `SCORE`)) +
            geom_bar(stat = 'identity', aes(fill = `SCORE`)) +
            scale_fill_gradient(
              low = "lightgreen",
              high = "green3", guide = 'none') +
            geom_hline(yintercept = 14, color = "yellow") +
            geom_hline(yintercept = 27, color = "red") +
            theme(text = element_text(size=14),
                  axis.text.x = element_text(angle=45, vjust=1, color = "black")) +
            labs(title="Latest highest Scores")+coord_flip()
        
             
        }
      }
    } 
  }   
  
  })
  
  output$summary <- renderPrint({
    
    if(input$variable=="Predicted Violations"){
      i<-1
    }
    if(input$variable=='Predicted from A to C'){
      i<-2
    }
    if(input$variable=='Predicted from A|B to C'){
      i<-3
    }
    if(input$variable=='Predicted from C to A'){
      i<-4
    }
    if (input$variable=="Entire Records"){
      i<-5
    }
    
    
    if(i==1) {
     
      pander(summary(dataBind))
      
    } else {
      
      if(i==2){
        
        pander(summary(Final[,c(1,2)]))
        
      } else{
        if(i==3){
         
         pander(summary(Final1[,c(1,2)]))
          
        } else{
          #Final2
          if(i==4){
            #
            pander(summary(Final2[,c(1,2)]))
            
          }else{
            #pander(PR)
          pander(Violation_counts%>%arrange(desc(count)))
            }
          
          
        }
      }
      
    }
  })
  
})