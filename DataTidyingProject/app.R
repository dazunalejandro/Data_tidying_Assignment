library(shiny)
library(shinythemes)
library(gridExtra)
library(ggpubr)
library(PerformanceAnalytics)
source("Preprocessing.R")
list_choices <-  unique(movies_1016$genre)
vote_choices <- unique(movies_1016$votesFactor)
cont_choices <- names(movies_1016)[c(1,6,11,13)]
year_choices <- unique(movies_1016$year)


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
                                              h4("Facet by three Genre"), 
                                              choices =list_choices,
                                              selected ="Comedy")
                         ),
                         mainPanel(
                           plotOutput(outputId = "value2")
                         )
                       )
                     )
            ),
            tabPanel(h4("Origen "),
                     fluidPage( 
                       sidebarLayout(# position = "right",
                          sidebarPanel(
                            sliderInput("slider", label = h4("Select Time Range"), min = 2005, 
                                     max = 2016, value = c(2014, 2015))
                            #selectInput('year', 'Select Year',year_choices)
                         ),
                         mainPanel(
                           plotOutput(outputId = "range")
                         )
                       )
                     ) # fluidPage
            ),#  titlePanel
            tabPanel(h4("Clustering "),
                     fluidPage( 
                       sidebarLayout(# position = "right",
                         sidebarPanel(
                           selectInput('xcol', 'Select First Variable', cont_choices),
                           selectInput('ycol', 'Select Second Variable', cont_choices,
                                       selected=cont_choices[[2]]),
                           numericInput('num_clusters', 'Select number of cluster', 3,
                                        min = 1, max = 9)
                         ),
                         mainPanel(
                           plotOutput('plot1')
                         )
                       )
                     )#  fluid Page
            )# TitlePanel
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
    
    #########
    ####America
    #########
    movies_subset <- movies_1016 %>%
      filter(year >= input$slider[1] & year <= input$slider[2]) %>%
      filter(continent=="America") %>%
      group_by(scoreFactor) %>% tally() %>%
      mutate(freq = n/sum(n), cum = 1- cumsum(freq))
    
  
    
    
    movies_1016_breaks1 <- movies_subset$cum[movies_subset$cum>0]
    
    first_plot <- ggplot(movies_subset) +
          coord_polar("y", start=0) +
          geom_bar(aes(x="", y=freq, fill=scoreFactor), stat = "identity") +
          scale_fill_discrete(name="Score Value")+
          scale_y_continuous(labels = scales::percent,breaks = movies_1016_breaks1) +
          theme(plot.title = element_text(size=20,hjust = 0.5),
                axis.line = element_blank(),
                axis.title=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                legend.position = "none") +
          #theme_minimal()+
          ggtitle("America")
    
    #########
    ####Europe
    #########
    movies_subset2 <- movies_1016 %>%
      filter(year >= input$slider[1] & year <= input$slider[2]) %>%
      filter(continent=="Europe") %>%
      group_by(scoreFactor) %>% tally() %>%
      mutate(freq = n/sum(n), cum = 1- cumsum(freq))
    
    
    
    
    movies_1016_breaks2 <- movies_subset2$cum[movies_subset2$cum>0]
    
    second_plot <- ggplot(movies_subset2) +
          coord_polar("y", start=0) +
          geom_bar(aes(x="", y=freq, fill=scoreFactor), stat = "identity") +
          scale_fill_discrete(name="Score Value")+
          scale_y_continuous(labels = scales::percent,breaks = movies_1016_breaks2) +
          theme(plot.title = element_text(size=20,hjust = 0.5),
                axis.line = element_blank(),
                axis.title=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                legend.position = "none") +
          #theme_minimal()+
          ggtitle("Europe")
    
    #########
    ####Asia
    #########
    
    movies_subset3 <- movies_1016 %>%
      filter(year >= input$slider[1] & year <= input$slider[2]) %>%
      filter(continent=="Asia") %>%
      group_by(scoreFactor) %>% tally() %>%
      mutate(freq = n/sum(n), cum = 1- cumsum(freq))
    
    
    
    
    movies_1016_breaks3 <- movies_subset3$cum[movies_subset3$cum>0]
    
    third_plot <- ggplot(movies_subset3) +
      coord_polar("y", start=0) +
      geom_bar(aes(x="", y=freq, fill=scoreFactor), stat = "identity") +
      scale_fill_discrete(name="Score Value") +
      scale_y_continuous(labels = scales::percent,breaks = movies_1016_breaks3) +
      theme(plot.title = element_text(size=20,hjust = 0.5),
            axis.line = element_blank(),
            axis.title=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.position = "none") +
      #theme_minimal()+
      ggtitle("Asia")
    
    #########
    ####Africa
    #########
    
    movies_subset4 <- movies_1016 %>%
      filter(year >= input$slider[1] & year <= input$slider[2]) %>%
      filter(continent=="Africa") %>%
      group_by(scoreFactor) %>% tally() %>%
      mutate(freq = n/sum(n), cum = 1- cumsum(freq))
    
    
    
    
    movies_1016_breaks4 <- movies_subset3$cum[movies_subset4$cum>0]
    
    fourth_plot <- ggplot(movies_subset4) +
      coord_polar("y", start=0) +
      geom_bar(aes(x="", y=freq, fill=scoreFactor), stat = "identity") +
      scale_fill_discrete(name="Score Value")+
      scale_y_continuous(labels = scales::percent,breaks = movies_1016_breaks4) +
      theme(plot.title = element_text(size=20,hjust = 0.5),
            axis.line = element_blank(),
            axis.title=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.position = "none") +
      #theme_minimal()+
      ggtitle("Africa")
    
    ggarrange(first_plot,second_plot,third_plot,fourth_plot, nrow = 2, ncol=2,
                 common.legend=TRUE, legend="bottom",
              font.label = list(size = 20))
    
    
  })
  
  ########################################
  ####################Panel 4 (Plot)
  ########################################
  
  selectedData <- reactive({
    movies_1016[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$num_clusters)
    
  })
  
  output$plot1 <- renderPlot({
    
    
    percentages <-  (clusters()$size/nrow(movies_1016))*100
    
    
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    #paste(round(100*m, 2), "%", sep="")
    
    #points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    text(clusters()$centers, y = NULL, labels=paste(round(percentages,2),"%",sep=""),cex = 1,col=0)
    #chart.Correlation(selectedData(), histogram=TRUE, pch=19)
})
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)


