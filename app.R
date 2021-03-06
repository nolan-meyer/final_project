library(shiny)
library(tidyverse)
library(plotly)

player_data <- read_csv("Data Sci - MSOC Data - Player Data.csv")
team_data <- read_csv("Data Sci - MSOC Data - Team Data.csv")

stats <- t(t(colnames(player_data[3:21])))



ui <- fluidPage(
  navbarPage(
    title = "Macalester Men's Soccer Database",
    tabPanel('Player Data', 
             DT::dataTableOutput('players')),
    tabPanel('Team Data', 
             DT::dataTableOutput('teams')),
    tabPanel('Player Time Series', 
             sidebarLayout(
               sidebarPanel(
                  varSelectInput(inputId = "variables",
                                 label = "Select Stat",
                                 data = player_data,
                                 selected = "G",
                                 multiple = FALSE),
                  textInput("player1", 
                            "Player 1", 
                            value = "Burke, Jake"),
                  textInput("player2", 
                            "Player 2", 
                            value = "Spurr, Charlie"),
                  textInput("player3", 
                            "Player 3", 
                            value = "Moore, Nick"),
                  sliderInput(inputId = "Season", 
                              label = "Seasons",
                              min = 2012, 
                              max = 2019, 
                              value = c(2012,2019),
                              sep = ""),
               submitButton(text = "Create plot")),
               mainPanel(
                  plotOutput(outputId = "timeplot"))
             )),
    tabPanel('Team Plots'),
    tabPanel('Scatter Plot',
             sidebarLayout(
               sidebarPanel(
                 varSelectInput(inputId = "xvar",
                                label = "X Axis Stat",
                                data = player_data,
                                selected = "G",
                                multiple = FALSE),
                 varSelectInput(inputId = "yvar",
                                label = "Y Axis Stat",
                                data = player_data,
                                selected = "A",
                                multiple = FALSE),
                 submitButton(text = "Create plot")),
               mainPanel(
                 plotlyOutput(outputId = "scatterplot")
               )
             ))
  ))
  

server <- function(input, output) {
  
  output$players <- DT::renderDataTable(
    DT::datatable(player_data, options = list(pageLength = 10)
  ))
  
  output$teams <- DT::renderDataTable(
    DT::datatable(team_data, options = list(pageLength = 10)
    ))
  
  output$timeplot <- renderPlot({
    player_data %>% 
      filter(Player %in% c(input$player1, input$player2, input$player3)) %>% 
      ggplot() +
      geom_point(aes(x = Season, 
                     y = !!input$variables, 
                     color = Player),
                alpha = 0.6, 
                size = 3) +
      geom_text(aes(x = Season,
                    y = !!input$variables,
                    label = Player), 
                hjust=0,
                vjust=0) +
      scale_x_continuous(limits = input$Season) +
      theme_minimal() 
    })
    
    output$scatterplot <- renderPlotly({
      p <- player_data %>% 
        ggplot(aes(x = !!input$xvar,
                   y = !!input$yvar,
                   group = Player,
                   label = Season)) +
        geom_jitter()
      ggplotly(p,
               tooltip = c("x", "y", "group", "label"))
        })
}

shinyApp(ui = ui, server = server)