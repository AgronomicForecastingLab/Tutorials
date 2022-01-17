#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("CPSC 541 - Quiz 1 "),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 2, 
                     h4("For each question, please download the data on your personal computer and write an R code to solve the problem . You can submit your quiz also by emailing it to hamzed@illinois.edu"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Question1", br(),
                         h3("Please download the data and fit two different models to this dataset . Then compare and contrast models based on their quality of fit."), 
                         downloadButton('downloadData', 'Download'),
                         plotOutput("plot", height = "700px")
                         ),
                tabPanel("Question2",br(),
                         h3("Please download the data and try to fit a multiple linear regression using MLE. "), 
                         downloadButton('downloadData2', 'Download'),
                          fluidRow(
                              column(6, plotOutput("plot2", height = "700px")),
                              column(6, plotOutput("plot3", height = "700px"))
                              )
                          )
               
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    x1 <- 141:300
    y <- (22)/(1+exp(-0.083*(x1-206)))
    y1 <- y + rnorm(length(x1), 0, 0.95)


    output$plot <- renderPlot({
        plot(x1, y1)  
    })
    
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('Q1-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
          dataq1 <- data.frame(x=x1, y=y1)
        write.csv(dataq1, con)
      }
    )
 #--------------------------- Q2   
    x2 <- runif(38, 60, 90)
    x22 <-  runif(38, -1.5, 5)
    y2 <- 5 + (-0.2*x2) + (2 * x22)
    y2 <- y2 + rnorm(length(x2), 0, 1.05)
        
    output$plot2 <- renderPlot({
        plot(x2, y2, xlab="x1", ylab="y")  
    })
    
    output$plot3 <- renderPlot({
        plot(x22, y2, xlab="x2", ylab="y")  
    })
    
    output$downloadData2 <- downloadHandler(
        filename = function() {
            paste('Q2-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
            dataq2 <- data.frame(x1=x2, x2=x22, y=y2)
            write.csv(dataq2, con)
        }
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
