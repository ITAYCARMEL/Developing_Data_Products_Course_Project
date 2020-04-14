 

# The application will let the user to compare the covid spreading rate within cities around the world

library(shiny)
library(plotly)
library(dplyr)
library(tidyr)

# download the dataset file

  url<-"https://covid.ourworldindata.org/data/ecdc/total_cases.csv"

  dest<-"../Covid19_spread_rate/total_cases.csv"

  download.file(url,dest)

  
# read in the file
  
  mydata<-read.csv("total_cases.csv",stringsAsFactors=FALSE)

  
# prepare the data: 
    cnms<-colnames(mydata)
    cnms<-cnms[2:208]
  
#reshape the data
    mydata<-gather(mydata,cnms,key="Country",value="Cases")
  
# Remove unnecessary dots in the country names:
    mydata$Country<-gsub("\\."," ",mydata$Country)

# remove the NA
    mydata$Cases[is.na(mydata$Cases)]<-0

# convert the date variable from factor to Date
    mydata$date<-as.Date(mydata$date)
  
# Define UI for the application:
    ui <- fluidPage(

    # Application title
    titlePanel("Covid19 spreading rate comparison"),

    # Sidebar with a selection widget to define one or more countries to compare for user 
    sidebarLayout(
        
        sidebarPanel(
        
                helpText("Create a comparison graph of the Covid19 spreading rate all over the world"),
            
                selectInput("state",
                        
                            label= "Select a State:",
                        
                            choices = as.character(unique(mydata$Country[order(mydata$Country)])), 
                        
                            selected = "Israel",
                        
                            multiple = TRUE),
            
            dateRangeInput("date",
                           
                           h3("Date range"),
                           
                           start = min(mydata$date),
                           
                           end = max(mydata$date))
                
              ),
    
        # Show the generated plot
        
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Plot", plotlyOutput("plot1")),
                      tabPanel("About this application", verbatimTextOutput("About"),
                               h4("User Guide"),
                               p("Welcome to the Covid_19 spread exploration App. Here you can find as whatever you need to know to get started using your application."),
                               p("The main purpuse of this application is to let you explore and compare the Coronavirus spreading rate (calculated as the sum of confirmed cases to date) in countries all over the world."),
                               p("The data for this App comes from the",
                                a("OUR WORLD IN DATA",
                                  href = "https://ourworldindata.org/coronavirus-source-data"),
                               "website."),
                               strong("To use the application you follow the instructions:"),
                               br(),
                               br(),
                               tags$ol(
                                 tags$li("Use the 'Select a State box' to choose one or more countries to compare."),
                                 tags$li("Define 'Start' and 'End' date in the 'Date Range box'."),
                                 tags$li("In the 'Plot Panel' it shows you a scatter plot with a line for each country."),
                                 tags$li("Enjoy!")
                               )
                                )
                              
         
                        
                       
                  )
          )
        
    )     
    )


server <- function(input, output) {
    
    datainput <- reactive({
        
        selectcountry <- mydata %>% filter(Country %in% input$state) %>% 
            
            filter(date >= input$date[[1]] & date <= input$date[[2]]) %>%
        
            group_by(Country)
         })
    
    
    output$plot1 <- renderPlotly({
        
        plot1 <- plot_ly(data=datainput(),x = datainput()[[1]], y=datainput()[[3]], color = datainput()[[2]], type = "scatter",mode = "lines") 
            
            
          })
   
                
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

