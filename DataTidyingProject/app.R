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

            ),# Panel 1
            tabPanel(h4("Critics vs Viewer"),
                     fluidPage( 
                       sidebarLayout(# position = "right",
                         sidebarPanel(
                           checkboxGroupInput("checkGroup2", 
                                              h4("Select Popularity Level"), 
                                              choices =vote_choices,
                                              selected ="Popular"),
                           checkboxGroupInput("checkGroup3", 
                                              h4("Facet by Three Genre"), 
                                              choices =list_choices,
                                              selected ="Comedy")
                         ),
                         mainPanel(
                           plotOutput(outputId = "value2")
                         )
                       )
                     )
            ),
            tabPanel("Origen ",
                     fluidPage( 
                       sidebarLayout(# position = "right",
                         sidebarPanel(
                           sliderInput("slider", label = h3("Slider Range"), min = 2010, 
                                       max = 2016, value = c(2014, 2015))
                         ),
                         mainPanel(
                           plotOutput(outputId = "range")
                         )
                       )
                     ) # fluidPage
            )#  titlePanel
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  ########################################
  ####################Panel 1 (Plot)
  ########################################
  
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
  
  ########################################
  ####################Panel 2 (Plot)
  ########################################
  
  
  output$value2 <- renderPlot({ 
    
    
    movies_subset <- movies_1016 %>%
      filter(votesFactor %in% input$checkGroup2) %>%
      filter(genre %in% input$checkGroup3)
    
    ggplot(movies_subset) +
      aes(x=score,fill=votesFactor,color=votesFactor) +
      scale_colour_discrete(name="Popularity Level")+
      facet_grid(~genre)+
      labs(x="Score",y="Density") +
      theme(plot.title = element_text(size=20,hjust = 0.5),
            axis.title=element_text(size=17),
            legend.text = element_text(size = 17),
            legend.title = element_text(size = 17)) +
      stat_density(geom = "line",size=0.5) 
     #geom_bar()
  })
  
  
  ########################################
  ####################Panel 3 (Plot)
  ########################################
  
  output$range <- renderPlot({ 
    movies_1016 <- movies_1016 %>%
      filter(year >= input$slider[1] & year <= input$slider[2]) %>%
      group_by(continent,rating) %>% tally() %>%
      mutate(freq = n/sum(n), cum = 1- cumsum(freq))
    
    
    movies_1016_breaks <- movies_1016$cum[movies_1016$cum>0]
    
    ggplot(movies_1016) +
      coord_polar("y", start=0) +
      geom_bar(aes(x="", y=freq, fill=rating), stat = "identity") +
      facet_wrap(~continent)+
      scale_y_continuous(labels = scales::percent,breaks = movies_1016_breaks) +
      theme(axis.line = element_blank(),
            axis.title=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    
    
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)