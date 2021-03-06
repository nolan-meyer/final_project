library(shiny)
library(tidyverse)

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
    tabPanel('Plots', 
             sidebarLayout(
               sidebarPanel(
                  varSelectInput(inputId = "variables",
                                 label = "Select Stat",
                                 data = player_data,
                                 selected = "G",
                                 multiple = FALSE),
                  textInput("player1", 
                            "Player 1", 
                            value = "", 
                            placeholder = "Anwar, Omar"),
                  textInput("player2", 
                            "Player 2", 
                            value = "", 
                            placeholder = "Anwar, Omar"),
                  textInput("player3", 
                            "Player 3", 
                            value = "", 
                            placeholder = "Anwar, Omar"),
                  sliderInput(inputId = "Season", 
                              label = "Seasons",
                              min = 2012, 
                              max = 2019, 
                              value = c(2012,2019),
                              sep = ""),
               submitButton(text = "Create plot")),
               mainPanel(
                  plotOutput(outputId = "timeplot"))
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
      geom_line(aes(x = Season, 
                    y = !!input$variables, 
                    color = Player),
                alpha = 0.4, 
                size = 4) +
      scale_x_continuous(limits = input$Season) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)