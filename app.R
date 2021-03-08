library(shiny)
library(tidyverse)
library(plotly)

player_data <- read_csv("Data Sci - MSOC Data - Player Data.csv")
team_data <- read_csv("Data Sci - MSOC Data - Team Data.csv")

player_list <- as.data.frame(pull(player_data, Player), col.names = "Players:")
player_list <- unique(player_list)
player_list <- player_list %>% 
  rename("Players:" = "pull(player_data, Player)")

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
                                 selected = "Goals",
                                 multiple = FALSE),
                  selectInput(inputId = "player1", 
                            label = "Player 1", 
                            choices = player_list,
                            selected = "Moore, Nick"),
                  selectInput(inputId = "player2", 
                              label = "Player 2", 
                              choices = player_list,
                              selected = "Burke, Jake"),
                  selectInput(inputId = "player3", 
                              label = "Player 3", 
                              choices = player_list,
                              selected = "Spurr, Charlie"),
                  sliderInput(inputId = "Season", 
                              label = "Seasons",
                              min = 2012, 
                              max = 2019, 
                              value = c(2012,2019),
                              sep = ""),
               submitButton(text = "Create plot")),
               mainPanel(
                  plotOutput(outputId = "playerplot"))
             )),
    tabPanel('Team Time Series',
             sidebarLayout(
               sidebarPanel(
                 varSelectInput(inputId = "y_var1",
                                label = "Stat 1",
                                data = team_data,
                                selected = "Goals For",
                                multiple = FALSE),
                 varSelectInput(inputId = "y_var2",
                                label = "Stat 2",
                                data = team_data,
                                selected = "Goals Against",
                                multiple = FALSE),
                 varSelectInput(inputId = "y_var3",
                                label = "Stat 3",
                                data = team_data,
                                selected = "Wins",
                                multiple = FALSE),
                 sliderInput(inputId = "season", 
                             label = "Seasons",
                             min = 2012, 
                             max = 2019, 
                             value = c(2012,2019),
                             sep = ""),
                 submitButton(text = "Create plot")),
               mainPanel(
                 plotOutput(outputId = "teamplot")))
             )
             ,
    tabPanel('Scatter Plot',
             sidebarLayout(
               sidebarPanel(
                 varSelectInput(inputId = "xvar",
                                label = "X Axis Stat",
                                data = player_data,
                                selected = "Goals",
                                multiple = FALSE),
                 varSelectInput(inputId = "yvar",
                                label = "Y Axis Stat",
                                data = player_data,
                                selected = "Assists",
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
  
  output$playerplot <- renderPlot({
    player_data %>% 
      filter(Player %in% c(input$player1, input$player2, input$player3)) %>% 
      ggplot() +
      geom_line(aes(x = Season, 
                    y = !!input$variables, 
                    color = Player),
                alpha = 0.6, 
                size = 3) +
      scale_x_continuous(limits = input$Season) +
      theme_minimal() 
    })
    
  output$teamplot <- renderPlot({
    team_data %>% 
      ggplot(aes(x = Season)) +
      geom_line(aes(y = !!input$y_var1),
                color = "blue") +
      geom_line(aes(y = !!input$y_var2),
                color = "green") +
      geom_line(aes(y = !!input$y_var3),
                color = "red") +
      scale_x_continuous(limits = input$season) +
      labs(y = "") +
      theme_minimal()
  })
  
    output$scatterplot <- renderPlotly({
      p <- player_data %>% 
        ggplot(aes(x = !!input$xvar,
                   y = !!input$yvar,
                   color = factor(Season),
                   group = Player,
                   label = Season)) +
        geom_jitter() +
        labs( color = "Season")+
        theme_minimal()
      ggplotly(p,
               tooltip = c("x", "y", "group", "label"))
        })
}

shinyApp(ui = ui, server = server)