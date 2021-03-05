library(shiny)
library(tidyverse)
player_data <- read_csv("Data Sci - MSOC Data - Player Data.csv")
team_data <- read_csv("Data Sci - MSOC Data - Team Data.csv")

ui <- fluidPage(
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
  submitButton(text = "Create my plot!"),
  plotOutput(outputId = "timeplot")
)

server <- function(input, output) {
  output$timeplot <- renderPlot({
    player_data %>% 
      filter(Player == input$Player) %>% 
      ggplot() +
      geom_line(aes(x = Season, y = G)) +
      scale_x_continuous(limits = input$Season) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)