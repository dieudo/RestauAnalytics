require(data.table)
require(lattice)
require(taRifx)#.geo
require(gdata)
require(tidyr)
require(arules)
require(arulesViz)
require(colorspace)
library(foreach)
library(ggplot2)
library(readr)
library(lubridate)
library(tidyverse)
library(dplyr)
library(shiny)
require(maps) || install.packages("maps")
require(ggmap) || install.packages("ggmap")
require(grid) || install.packages("grid")
library(psych)

RestauAnalytics<-read.csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv")
#Start Siny Server!
my_data=RestauAnalytics
WideCleanData<-read.csv("Restaurants_WideData.csv")
#colnames(WideCleanData)
f=lm(X6 ~ CUISINE.DESCRIPTION+CRITICAL.FLAG+BORO+X1+X2+X3+X4+X5,data=WideCleanData)


shinyServer(function(input, output){
  inputData=RestauAnalytics
  subset=select(RestauAnalytics,BORO,CUISINE.DESCRIPTION,ZIPCODE,SCORE,GRADE)

  formulaText1<-reactive({
    paste("Category",input$variable)
  })


  output$caption1<-renderText({
    paste("Restaurants Analytics in NYC ",formulaText1())
  })
  output$caption2<-renderText({
    paste(" metrics",formulaText1())
  })
  output$caption3<-renderText({
    paste("Search Table",formulaText1())
  })

  output$plot<-renderPlot({
    
    if(input$variable=="GRADE"){
      i<-1
    }
    if(input$variable=='SCORE'){
      i<-2
    }
    if(input$variable=='CUISINE.DESCRIPTION'){
      i<-3
    }
    
    
    
    
    if(i==1) {
    
    
      my_data %>% filter(GRADE %in% c("A","B","C","D"))%>%count(BORO,GRADE) %>%
        ggplot(.,aes(x=BORO,y=n,fill=GRADE)) + geom_bar(stat="identity",width=0.4,alpha=0.8,color="black") + facet_grid(GRADE~.,scale="free") +
        geom_text(aes(label=n),hjust=-.6,vjust=1.0) +
        labs(x="",y="Counts") +coord_flip()+theme_bw()
      
    } else {
      if(i==2){
        
        # Critical Violation by borough
        
        my_data %>% filter(GRADE %in% c("A","B","C"),!CRITICAL.FLAG %in% "Not Applicable") %>% count(BORO,GRADE,CRITICAL.FLAG) %>%
          ggplot(.,aes(x=BORO,y=n,fill=CRITICAL.FLAG)) +
          geom_bar(stat="identity",width=0.4,alpha=0.8,color="black") +
          facet_grid(GRADE~.,scale="free") +
          labs(x="",y="Count") + theme_bw()
      } else {
        if(i==3){
        
        
          
          my_data %>% filter(GRADE %in% c("A","B","C")) %>% group_by(BORO,GRADE) %>% summarise(count=n()) %>% mutate(Percent=count/sum(count)*100) %>%
            ggplot(.,aes(x=BORO,y=Percent,fill=GRADE)) + 
            geom_histogram(aes(group=BORO),stat="identity",width=0.4,alpha=0.8,color="black") + 
            labs(x="",y="Percent of restaurants") + coord_flip() +theme_bw()
          # 
        } else{
          
          Raw_data <- fread("DOHMH_New_York_City_Restaurant_Inspection_Results.csv", header = T, 
                            sep=',', na.strings=c("NA","N/A",""), colClasses = "character")
          inspecttbl<-tbl_df(Raw_data)
          #glimpse(inspecttbl)
          inspecttbl$"INSPECTION DATE"<-(as.Date(inspecttbl$'INSPECTION DATE', "%m/%d/%y"))
          inspecttbl$"GRADE DATE"<-(as.Date(inspecttbl$'GRADE DATE', "%m/%d/%y"))
          inspecttbl$"RECORD DATE"<-(as.Date(inspecttbl$'RECORD DATE', "%m/%d/%y"))
          class(inspecttbl$'RECORD DATE')
          inspecttbl$GRADE <- factor(inspecttbl$GRADE, levels = c("A", "B", "C", "Z", "Not Yet Graded"), ordered = TRUE)
          (levels(inspecttbl$GRADE))
          inspecttbl$`CRITICAL FLAG` <-   factor(inspecttbl$`CRITICAL FLAG`, levels = c("Critical", "Not Critical", "Not Applicable"), ordered = T)
          levels(inspecttbl$`CRITICAL FLAG`)
          inspecttbl$CAMIS <- as.numeric(inspecttbl$CAMIS)
          inspecttbl$SCORE <- as.numeric(inspecttbl$SCORE)
          #class(inspecttbl$CAMIS)
          #class(inspecttbl$SCORE)
          #n_distinct(inspecttbl$CAMIS)
          #range(inspecttbl$`INSPECTION DATE`)
          #levels(as.factor(inspecttbl$`INSPECTION TYPE`)) 
          
          gradables <- c("Cycle Inspection / Initial Inspection" , "Cycle Inspection / Re-inspection", 
                         "Pre-permit (Operational) / Initial Inspection", "Pre-permit (Operational) / Re-inspection")
          
          inspecttbl <- (filter(inspecttbl, `INSPECTION TYPE` %in% gradables))
          #levels(as.factor(inspecttbl$`INSPECTION TYPE`))
          
          grades <- c("A", "B", "C")
          #nrow(filter(inspecttbl, GRADE %in% grades))
          
          df=inspecttbl%>%filter(GRADE %in% grades)%>%select(CAMIS, DBA, BORO, BUILDING, STREET, ZIPCODE, `CUISINE DESCRIPTION`, `INSPECTION DATE`, ACTION, SCORE, GRADE, `INSPECTION TYPE`)
          df2=unique(df)
          
          
          Worst_Restaurants <- arrange(filter(df2, GRADE %in% grades), desc(SCORE))[1:20,c(2,10)]
          
          Worst_Restaurants$DBA <- factor(Worst_Restaurants$DBA, levels = rev(Worst_Restaurants$DBA))
          ggplot(Worst_Restaurants, aes(x = `DBA`, y = `SCORE`)) +
            geom_bar(stat = 'identity', aes(fill = SCORE)) +
            scale_fill_gradient(
              low = "lightgreen",
              high = "green3", guide = 'none') +
            geom_hline(yintercept = 14, color = "yellow") +
            geom_hline(yintercept = 27, color = "red") +
            theme(text = element_text(size=10),
                  axis.text.x = element_text(angle=45, vjust=1, color = "black")) +
            labs(title="Worst Restaurant")
          
          
        }
      
      }
     
    }
  
   
  output$summary <- renderPrint({
    
    if(input$variable=="GRADE"){
      i<-1
    }
    if(input$variable=='SCORE'){
      i<-2
    }
    if(input$variable=='CUISINE.DESCRIPTION'){
      i<-3
    }
    
     if(i==1) {
      
      summary(f)
    
    } else {
      
      if(i==2){
      
    groupCuisine <- group_by(new_DF, `CUISINE DESCRIPTION`)
    print(cuisineSummary <- summarize(groupCuisine,
                                      "A's" = sum(GRADE == "A", na.rm = T)/sum(GRADE == "A" | GRADE == "B" | GRADE == "C", na.rm = T),
                                      "B's" = sum(GRADE == "B", na.rm = T)/sum(GRADE == "A" | GRADE == "B" | GRADE == "C", na.rm = T),
                                      "C's" = sum(GRADE == "C", na.rm = T)/sum(GRADE == "A" | GRADE == "B" | GRADE == "C", na.rm = T),
                                      "Totals" = sum(GRADE == "A" | GRADE == "B" | GRADE == "C", na.rm = T)
    ))
    } else{
    #best cuisines withmost A
    arrange(cuisineSummary, desc(`A's`))[1:20,]
    
    #Cusines with most C
    arrange(cuisineSummary, desc(`C's`))[c(1:20),]
    # Total A(Inspection and resinpection)
    summary(new_DF$GRADE)
    
    }
    
    }
  })
    
  })

  
  output$view<-renderDataTable({
    Phone_number<-inputData[,which(names(inputData)==input$variable)-1]
    CriticalFlag<-inputData[,which(names(inputData)==input$variable)+5]
    data<-cbind(subset,Phone_number,CriticalFlag)
    data<-data[,-which(names(data) %in%"ZIPCODE")]
    data
  })
})


