
# #UI.R
# library(shiny)
# shinyUI(fluidPage(
#   titlePanel("RestauAnalytics"),
#   sidebarLayout(
#     sidebarPanel(
#       radioButtons("p", "Select an Option:",
#                    list("Grades by Borough"='a', "Presence of Critical Flag"='b', "Percentage by Borough"='c', "Summary"='d')),
#       sliderInput("bins",
#                   "Number of bins:",
#                   min = 1,
#                   max = 50,
#                   value = 30)
#     ),
#     mainPanel(
#       plotOutput("distPlot")
#     )
#   )
# ))


library(shiny)

shinyUI(fluidPage(

  #Top Part-Title
  titlePanel("WELCOME To RestauAnalytics! The 'Safety food' App "),
  br(),

  #Left Part-SideBar Panel
  sidebarPanel(

    selectInput("variable", "SELECT TYPE :",
                c("GRADE" = "GRADE",
                  "SCORE" = "SCORE",
                  "CUISINE.DESCRIPTION" = "CUISINE.DESCRIPTION",
                  "BORO"="BORO"

                )),

    br(),


    checkboxInput("legend", "Hide Map Legends", FALSE),
    br(),

    # Specification of range of Point Size and Transparency
    sliderInput("max", "Choose Size of Points:",
                min = 6, max = 30, value = 15),
    br(),

    sliderInput("alphapoint", "Choose Transparency of Points:",
                min = 0.1, max = 0.8, value = 0.5),
    br(),

    #Show or Hide of borough Labels map and Control These Labels
    checkboxInput("label", "Hide Labels", FALSE),
    br(),

    sliderInput("scale", "Choose Size of Labels:",
                min = 1, max = 10, value = 5),
    br(),
    sliderInput("alphatext", "Choose Transparency of Labels:",
                min = 0.1, max = 0.8, value = 0.5),
    br(),

    helpText("YOUR TRUSTED ADVISOR FOR SAFE RESTAUTRANTS"),
    br()
    #submitButton("Update View")
  ),

  #Right Part-Main Panel
  mainPanel(
    tabsetPanel(type = "tabs",

                tabPanel("PLOT", h4(textOutput("caption1")), plotOutput("plot")),
                tabPanel("LINEAR REGRESSION ",h4(textOutput("caption2")),verbatimTextOutput("summary")),
                tabPanel("SEARCH TABLE",h4(textOutput("caption3")),

                         dataTableOutput("view"),width=9)
    ))

))




# ui.R

# library(shiny)
# library(shinythemes)
# 
# 
# fig.width <- 600
# fig.height <- 450
# 
# 
# shinyUI(fluidPage(theme =  shinytheme("cerulean"),
#                   titlePanel("Linear Regression Model"),
#                   #list(tags$head(tags$style("body {background-color: gray; }"))),
#                   sidebarLayout(
#                     sidebarPanel(
#                       
#                       # sidepbar panel has the Input
#                       h4("Note that"),
#                       h4("the input data set shuold contain",
#                          "predictor in fisrt column and outcome in second column",style = "font-family: 'times'; font-si16pt"),
#                       helpText("the type of file you upload should be .asv or .txt",style = "font-family: 'times'; font-si16pt"),
#                       
#                       br(),
#                       # Check box for selection of header and type of file option
#                       checkboxInput('header','Header', value = F),
#                       checkboxInput('default','Default Data Set', value = T),
#                       br(),
#                       radioButtons('sep', 'separator', c(comma = ',', semicolon = ';', tab = '\t'), selected = NULL, inline = FALSE),
#                       br(),
#                       
#                       # take the file from user
#                       fileInput('data', 'Choose file to upload', multiple = FALSE, accept = c('.text/ comma-separated-values',
#                                                                                               '.csv',
#                                                                                               '.xlsx',
#                                                                                               '.txt',
#                                                                                               '.text/ tab-separated-values')),
#                       
#                       helpText("Deselect default data set when you upload your data set"),
#                       
#                       numericInput("obs", "Observations:", 20,
#                                    min = 1, max = 100),
#                       
#                       
#                       
#                       
#                       #    sliderInput("intercept",
#                       #                strong("Intercept"),
#                       #                min=-2, max=6, step=.5,
#                       #                value=sample(seq(-2, 6, .5), 1), ticks=FALSE),
#                       #    sliderInput("slope", 
#                       #                strong("Slope"),
#                       #                min=-1, max=3, step=.25, 
#                       #                value=sample(seq(-1, 3, .25), 1), ticks=FALSE),
#                       br(),
#                       wellPanel(
#                         h5("Contact Info:"),
#                         h5("Kunal Jagtap"),
#                         helpText(   a("View My LinkedIn Profile",href="https://www.linkedin.com/in/kunaljagtap")),
#                         helpText("srkunaljagtap@gmail.com")
#                       ),
#                       br(),br(),br(),br()
#                     ),
#                     
#                     
#                     mainPanel(
#                       tabsetPanel(
#                         
#                         tabPanel("Table", tableOutput("table")),
#                         tabPanel("Plot", plotOutput("plot")),
#                         tabPanel("Linear Regression", plotOutput("LinearPlot")),
#                         tabPanel("Summary", verbatimTextOutput("summary")),
#                         tabPanel("Linear Regression Summary", verbatimTextOutput("Linearsummary"))
#                       )
#                     )
#                     
#                   )  
# )
# )
# 
# 
