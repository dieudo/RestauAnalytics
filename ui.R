library(shiny)
library(shinyWidgets)
shinyUI(fluidPage(tags$head(
  
 # includeCSS("styles.css"),

   setBackgroundColor(
   color = c("#F7FBFF", "#2171B4"),
 gradient = "radial",
  direction = c("top", "left")
   ),
  
 tags$style(
    ".title {margin: auto; width: 50px}"
  )
),
tags$div(class="title", titlePanel("RestauAnalytics Dieudonne Ouedraogo")),

  #Top Part-Title
  #titlePanel("RESTAU ANALYTICS"),
  br(),

  #Left Part-SideBar Panel
  sidebarPanel(

    selectInput( "variable", "CHOOSE AN OPTION:",
                c("Predicted Violations" = "Predicted Violations",
                  "Predicted from A to C" = "Predicted from A to C",
                  "Predicted from A|B to C" = "Predicted from A|B to C",
                  "Predicted from C to A" = "Predicted from C to A",
                  "Entire Records"="Entire Records"

                )),

    br(),

    sliderInput("obs", "SCALE", 
                min = 1, max = 1000, value = 500),
    
    br()
    
  ),




 #Right Part-Main Panel
 
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("PREDICTED VALUES & DATA",h3(textOutput("caption3")),dataTableOutput("view"),width=9), 
                #tabPanel("PLOT", h4(textOutput("caption1")), plotOutput("plot")),
                tabPanel("DESCRIPTIONS",h3(textOutput("caption2")),verbatimTextOutput("summary")),
               # tabPanel("SEARCH TABLE",h4(textOutput("caption3")),
                tabPanel("PLOTS", h3(textOutput("caption1")), plotOutput("plot", width = "auto",click = "plot_click"))
                         
  ))

  
 
))


