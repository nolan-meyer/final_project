library(shiny)
library(tidyverse)

player_data <- read_csv("Data Sci - MSOC Data - Player Data.csv")
team_data <- read_csv("Data Sci - MSOC Data - Team Data.csv")


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
                  sliderInput(inputId = "Season", 
                              label = "Seasons",
                              min = 2012, 
                              max = 2019, 
                              value = c(2012,2019),
                              sep = ""),
                  textInput("Player", 
                            "Player", 
                            value = "", 
                            placeholder = "Anwar, Omar"),
                  varSelectInput(inputId = "variable",
                                  label = "Select Stat",
                                  data = player_data,
                                  selected = "G",
                                  multiple = FALSE),
               submitButton(text = "Create my plot!")),
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
      filter(Player == input$Player) %>% 
      ggplot() +
      geom_path(aes(x = Season, y = !!input$variable)) +
      scale_x_continuous(limits = input$Season) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)