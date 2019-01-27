require(shiny) || install.packages("shiny")
require(maps) || install.packages("maps")
require(ggmap) || install.packages("ggmap")
require(grid) || install.packages("grid")
require(dplyr) || install.packages("dplyr")
HPI_INCOME<-read.csv("income.csv")
DF <- map_data("state")
map<-ggplot(DF)+geom_polygon(aes(x=long, y=lat,  group = group),
  fill="grey90",color=I("white"), size=1, data=DF) + 
  theme_bw() + theme(axis.text=element_blank(), 
                     axis.title=element_blank(),
                     axis.line=element_blank(),
                     axis.ticks=element_blank(),
                     panel.border=element_blank(),
                     panel.grid=element_blank(),
                     aspect.ratio=1/1.5,
                     plot.margin=unit(c(-1,-1,-1,-1), units="cm"))
  
##############################################################################################
#Start Siny Server!
shinyServer(function(input, output){
  inputData=HPI_INCOME
  subset=select(HPI_INCOME,city,state,lat,long)
  
  formulaText1<-reactive({
    paste("HPI~",input$variable)
  })
  
  
  output$caption1<-renderText({
    paste("Relationship between HPI and Personal 
          Income for US largest cities||",formulaText1()) 
  })
  output$caption2<-renderText({
    paste("A summary of dataset for",formulaText1()) 
  })
  output$caption3<-renderText({
    paste("Show table of dataset and sort cities by ",formulaText1()) 
  })
  
  output$plot<-renderPlot({
    income<-inputData[,which(names(inputData)==input$variable)]
    hpi<-inputData[,which(names(inputData)==input$variable)+5]
    data<-cbind(subset,income,hpi)
    p<-map+geom_point(data=data,mapping=aes(x=long, y=lat,color=hpi,size=income),alpha=I(input$alphapoint))+
      scale_colour_gradient("HPI change",low=("blue"),high=("red"))+  
      scale_size_continuous("Income Change", range = c(4,input$max))
    
    if (input$label) {
      if(input$legend){
        print(p+theme(legend.position="none"))
      }else{
        print(p)
      }
    } else {
      if(input$legend){
        print(p+annotate("text",x=data$long+3,y=data$lat,label=as.character(data$city),
                         size=abs(data$income)/max(abs(data$income))*input$scale+5,
                         alpha=I(input$alphatext))+theme(legend.position="none") )
      }else{
        print(p+annotate("text",x=data$long+3,y=data$lat,label=as.character(data$city),
                         size=abs(data$income)/max(abs(data$income))*input$scale+5,
                         alpha=I(input$alphatext)) )
      }
      
    }
    
  })
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    income<-inputData[,which(names(inputData)==input$variable)]
    hpi<-inputData[,which(names(inputData)==input$variable)+5]
    data<-cbind(subset,income,hpi)
    summary(data[,-which(names(data) %in% c("lat","long"))])
  })
  
  #output$view<-renderTable({
  output$view<-renderDataTable({
    income<-inputData[,which(names(inputData)==input$variable)]
    hpi<-inputData[,which(names(inputData)==input$variable)+5]
    data<-cbind(subset,income,hpi)
    data<-data[,-which(names(data) %in% c("lat","long"))]
    data
  })
  
})