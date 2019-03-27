#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
source("Preprocessing.R")
list_choices <-  unique(movies_1016$genre)
vote_choices <- unique(movies_1016$votesFactor)

#theme = shinytheme("cyborg"),
# Define UI for application that draws a histogram
ui <- navbarPage(h3("Movie Industry Analysis"),
                 theme = shinytheme("flatly"),
                 tabPanel(h4("Income "),
                          fluidPage( 
                            sidebarLayout(
                              sidebarPanel(
                                checkboxGroupInput("checkGroup", 
                                                   h4("Select Genre"), 
                                                   choices =list_choices,
                                                   selected ="Comedy")
                              ),
                              mainPanel(
                                plotOutput(outputId = "value1")
                              
                            )
                          ) # fluidPage
                 )
            )#  titlePage
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$value1 <- renderPlot({
    movies_subset <- movies_1016 %>%
      filter(genre %in% input$checkGroup)
    
    
    
    ggplot(movies_subset) +
      ggtitle("Yearly Gross Income") +
      theme(plot.title = element_text(hjust = 0.5))+
      aes(x=year,y=grossPergenre,fill=genre,color=genre) +
      #facet_wrap(~genre) +
      #scale_y_continuous() +
      geom_line(size=1)+
      geom_point(size=2) +
      labs(x="Year",y="Gross Income") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)