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
                                div("Use the panel on the left to  to compare the yearly gross income of movies
                                           per genre", 
                                            style = "color:black",style = "font-family: 'times'; font-si20pt"),
                                plotOutput(outputId = "value1")
                              
                            )
                          ) # sidebarLayout
                 ) # fluidPage
            )# Panel 1
            tabPanel(h4("Critics  vs Viewers"),
                     fluidPage( 
                       sidebarLayout(# position = "right",
                         sidebarPanel(
                           checkboxGroupInput("checkGroup2", 
                                              h4("Select Popularity Level"), 
                                              choices =vote_choices,
                                              selected ="Popular"),
                           checkboxGroupInput("checkGroup3", 
                                              h4("Facet by three Genres"), 
                                              choices =list_choices,
                                              selected ="Comedy")
                         ),
                         mainPanel(
                           plotOutput(outputId = "value2")
                         )
                       )
                     )
            )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  
  output$value1 <- renderPlot({
    movies_subset <- movies_1016 %>%
      filter(genre %in% input$checkGroup)
    
    
    
    ggplot(movies_subset) +
      ggtitle("Yearly Gross Income") +
      aes(x=year,y=grossPergenre,fill=genre,color=genre) +
      labs(x="Year",y="Gross Income") +
      scale_colour_discrete(name="Genre") +
      scale_fill_discrete(name="Genre") +
      theme(plot.title = element_text(size=20,hjust = 0.5),
            axis.title=element_text(size=17),
            legend.text = element_text(size = 17),
            legend.title = element_text(size = 17)) +
      #facet_wrap(~genre) +
      #scale_y_continuous() +
      geom_line(size=1)+
      geom_point(size=2) 
  })
  
  
    

}


# Run the application 
shinyApp(ui = ui, server = server)