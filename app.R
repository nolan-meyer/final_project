library(shiny)
library(tidyverse)
library(plotly)
library(googlesheets4)
library(rsconnect)
library(wesanderson)

player_data <- read_csv("Data Sci - MSOC Data - Player Data.csv")
team_data <- read_csv("Data Sci - MSOC Data - Team Data.csv")

player_list <- as.data.frame(pull(player_data, Player), col.names = "Players:")
player_list <- unique(player_list)
player_list <- player_list %>% 
  rename("Players:" = "pull(player_data, Player)") %>% 
  arrange(`Players:`)

stats <- as.data.frame(t(t(colnames(player_data[3:21]))))
stats <- stats %>% 
  rename("Stats:" = "V1")



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
                                 label = "Select Stat:",
                                 data = player_data,
                                 selected = "Goals",
                                 multiple = FALSE),
                  selectInput(inputId = "player1", 
                            label = "Player 1:", 
                            choices = player_list,
                            selected = "Moore, Nick"),
                  selectInput(inputId = "player2", 
                              label = "Player 2:", 
                              choices = player_list,
                              selected = "Burke, Jake"),
                  selectInput(inputId = "player3", 
                              label = "Player 3:", 
                              choices = player_list,
                              selected = "Spurr, Charlie"),
                  sliderInput(inputId = "Season", 
                              label = "Seasons:",
                              min = 2012, 
                              max = 2019, 
                              value = c(2013,2019),
                              sep = ""),
               submitButton(text = "Create plot")),
               mainPanel(
                  plotlyOutput(outputId = "playerplot"))
             )),
    tabPanel('Team Time Series',
             sidebarLayout(
               sidebarPanel(
                 varSelectInput(inputId = "y_var1",
                                label = "Stat 1:",
                                data = team_data,
                                selected = "Goals For",
                                multiple = FALSE),
                 varSelectInput(inputId = "y_var2",
                                label = "Stat 2:",
                                data = team_data,
                                selected = "Goals Against",
                                multiple = FALSE),
                 varSelectInput(inputId = "y_var3",
                                label = "Stat 3:",
                                data = team_data,
                                selected = "Wins",
                                multiple = FALSE),
                 sliderInput(inputId = "season", 
                             label = "Seasons:",
                             min = 2012, 
                             max = 2019, 
                             value = c(2012,2019),
                             sep = ""),
                 submitButton(text = "Create plot")),
               mainPanel(
                 plotlyOutput(outputId = "teamplot")))
             )
             ,
    tabPanel('Player Scatter Plots',
             sidebarLayout(
               sidebarPanel(
                 varSelectInput(inputId = "xvar",
                                label = "X Axis Stat:",
                                data = player_data,
                                selected = "Goals",
                                multiple = FALSE),
                 varSelectInput(inputId = "yvar",
                                label = "Y Axis Stat:",
                                data = player_data,
                                selected = "Assists",
                                multiple = FALSE),
                 varSelectInput(inputId = "color",
                                label = "Color by:",
                                data = player_data,
                                selected = "Points Per 90",
                                multiple = FALSE),
                 sliderInput(inputId = "seasons", 
                             label = "Seasons:",
                             min = 2012, 
                             max = 2019, 
                             value = c(2012,2019),
                             sep = ""),
                 sliderInput(inputId = "minutes", 
                             label = "Minutes Played:",
                             min = 0, 
                             max = 2000, 
                             value = c(250,2000),
                             sep = ""),
                 sliderInput(inputId = "games", 
                             label = "Games Played:",
                             min = 0, 
                             max = 21, 
                             value = c(0,21),
                             sep = ""),
                 submitButton(text = "Create plot")),
               mainPanel(
                 plotlyOutput(outputId = "scatterplot")
               )
             ))
  ))
  

server <- function(input, output) {
  
  output$players <- DT::renderDataTable(
    DT::datatable(player_data, options = list(pageLength = 8)
  ))
  
  output$teams <- DT::renderDataTable(
    DT::datatable(team_data, options = list(pageLength = 8)
    ))
  
  output$playerplot <- renderPlotly({
    p1 <- player_data %>% 
      filter(Player %in% c(input$player1, input$player2, input$player3)) %>% 
      ggplot() +
      geom_line(aes(x = Season, 
                    y = !!input$variables, 
                    color = Player),
                alpha = 0.75, 
                size = 1.75) +
      geom_point(aes(x = Season, 
                     y = !!input$variables, 
                     color = Player,
                     group = Player),
                alpha = 0.9, 
                size = 3) +
      scale_x_continuous(limits = input$Season,
                         breaks = seq(min(input$Season), max(input$Season), 1)) +
      scale_color_manual(values = c("#D9514EFF", "#2A2B2DFF", "#2DA8D8FF")) +
      labs(title = "Individual statistics by season") +
      theme_minimal() +
      theme(panel.grid.major.x = element_line(color = "grey96", size = 0.2),
            panel.grid.major.y = element_line(color = "grey96", size = 0.2),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.text.x = element_text(colour = "black"),
            axis.text.y = element_text(colour = "black"),
            plot.title = element_text(size = 15, face = "bold"),
            panel.background = element_rect(fill = "ivory2"),
            plot.background = element_rect(fill = "ivory2"))
    
      ggplotly(p1,
               tooltip = c("x", "y", "group"))
    })
    
  output$teamplot <- renderPlotly({
    p2 <- team_data %>% 
      ggplot(aes(x = Season)) +
      geom_line(aes(y = !!input$y_var1),
                color = "#D9514EFF",
                alpha = 0.75, 
                size = 1.75) +
      geom_point(aes(y = !!input$y_var1),
                color = "#D9514EFF",
                alpha = 0.9, 
                size = 3) +
      geom_line(aes(y = !!input$y_var2),
                color = "#2A2B2DFF",
                alpha = 0.75, 
                size = 1.75) +
      geom_point(aes(y = !!input$y_var2),
                color = "#2A2B2DFF",
                alpha = 0.9, 
                size = 3) +
      geom_line(aes(y = !!input$y_var3),
                color = "#2DA8D8FF",
                alpha = 0.75, 
                size = 1.75) +
      geom_point(aes(y = !!input$y_var3),
                color = "#2DA8D8FF",
                alpha = 0.9, 
                size = 3) +
      scale_x_continuous(limits = input$season,
                         breaks = seq(min(input$season), max(input$season), 1)) +
      labs(title = "Team statistics by season", y = "") +
      theme_minimal() +
      theme(panel.grid.major.x = element_line(color = "grey96", size = 0.2),
            panel.grid.major.y = element_line(color = "grey96", size = 0.2),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.text.x = element_text(colour = "black"),
            axis.text.y = element_text(colour = "black"),
            plot.title = element_text(size = 15, face = "bold"),
            panel.background = element_rect(fill = "ivory2"),
            plot.background = element_rect(fill = "ivory2"))
    
    ggplotly(p2,
             tooltip = c("x", "y"))
  })
  
    
    output$scatterplot <- renderPlotly({
      p3 <- player_data %>% 
        filter(Season >= min(input$seasons),
               Season <= max(input$seasons),
               `Minutes Played` >= min(input$minutes),
               `Minutes Played` <= max(input$minutes),
               `Games Played` >= min(input$games),
               `Games Played` <= max(input$games)) %>% 
        ggplot(aes(x = !!input$xvar,
                   y = !!input$yvar,
                   color = !!input$color,
                   group = Player,
                   label = Season)) +
        geom_jitter() +
        theme_minimal() +
        labs(title = "Statistical Relationships") +
        scale_color_viridis_c(option = "A") +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              axis.text.x = element_text(colour = "black"),
              axis.text.y = element_text(colour = "black"),
              panel.border = element_blank(),
              panel.background = element_rect(fill = "snow2"),
              plot.background = element_rect(fill = "snow2"),
              plot.title = element_text(size = 15, face = "bold"))
      
      ggplotly(p3)
        })
}

shinyApp(ui = ui, server = server)