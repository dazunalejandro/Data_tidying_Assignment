#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("Preprocessing.R")
list_choices <-  unique(movies_1016$genre)
vote_choices <- unique(movies_1016$votesFactor)


# Define UI for application that draws a histogram
ui <- navbarPage("Movie Industry Analysis",
                 
                 tabPanel("Income ",
                          fluidPage( 
                            sidebarLayout(# position = "right",
                              sidebarPanel(
                                checkboxGroupInput("checkGroup", 
                                                   h3("Select Genre"), 
                                                   choices =list_choices,
                                                   selected ="Comedy")
                              ),
                              mainPanel(
                                plotOutput(outputId = "value1")
                              )
                            )
                          ) # fluidPage
                 ) #  titlePa
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

