
# App for visualizing performance metrics over the previous season for a soccer team.

library(shiny)
library(DT)  # For interactive tables
library(readr)  # For reading CSV files
library(dplyr)
library(plotly)
library(shinythemes)

# Define available metrics globally or within a reactive expression
available_metrics <- c("distance", "decels", "accels", "hsr", "running_distance_2ms", "def_act", "touches", "obv", "actions_sum")

# Define the UI
ui <- fluidPage(
  titlePanel("Academy Soccer Team App"),
  theme = shinythemes::shinytheme("cyborg"),
  sidebarLayout(
    sidebarPanel(
      selectInput("teamSelect", "Select Team Name", choices = NULL),
      selectInput("player_select", "Select Players", choices = NULL, multiple = TRUE),
      sliderInput("date_range", "Select Date Range",
                  min = min(data$session_date),
                  max = max(data$session_date),
                  value = c(min(data$session_date), max(data$session_date)))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Team Overview',
                 selectInput("team_metric", "Select Metric to Plot", choices = available_metrics),
                 plotOutput("team_plot")),
        tabPanel("Team Match Information",
                 fluidRow(
                   column(4,
                          selectInput("selected_matches", "Select Match ID", choices = NULL, multiple = FALSE)
                   ),
                   column(4,
                          selectInput("x_metric", "Select X-axis Metric", choices = available_metrics)
                   ),
                   column(4,
                          selectInput("y_metric", "Select Y-axis Metric", choices = available_metrics)
                   )
                 ),
                 plotOutput("individual_game_scatter"),
                 tableOutput("gameDatatable")),
        tabPanel("High Level Player Overview",
                 selectInput("high_level_metric", "Select Metric for Player Plots", choices = available_metrics),
                 plotOutput("high_level_plot")),
        tabPanel("Win/Loss Distributions", 
                 selectInput("win_loss_metric", "Select Metric to Plot Distribution", choices = available_metrics),
                 plotOutput("win_loss_visualization"),
                 tableOutput("outcometable")),
        tabPanel("Metrics by Game Outcome",
                 selectInput("outcome_plot", "Select Metric", choices = available_metrics),
                 plotOutput("game_outcome_visualization")),
        tabPanel("Data Viewer", DTOutput("teamData"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Read the CSV file
  data <- reactive({
    read_csv("C:/Users/jadeg/OneDrive/Desktop/Soccer_data_app.csv")
  })
  
  # Update select input choices based on loaded data
  observeEvent(data(), {
    updateSelectInput(session, "teamSelect", choices = unique(data()$team_name))
  })
  
  # Update player select input based on team selection
  observeEvent(input$teamSelect, {
    selectedPlayers <- unique(data()[data()$team_name == input$teamSelect, "player_id"])
    updateSelectizeInput(session, "player_select", choices = selectedPlayers, options = list('plugins' = list('remove_button'), 'maxItems' = 5))
  })
  
  # Create team level chart for specific metric on team overview tab
  output$team_plot <- renderPlot({
    req(input$teamSelect, input$team_metric)  # Ensure both inputs are available
    
    team_df <- data()[data()$team_name == input$teamSelect, ]
    team_df <- team_df[team_df$session_date >= input$date_range[1] &
                         team_df$session_date <= input$date_range[2], ]
    
    # Create team plot based on selected metric
    p <- ggplot(team_df, aes(x = session_date, y = !!sym(input$team_metric), color = cat_min_played)) +
      geom_point(size = 3) +
      geom_smooth(method = "loess", se = TRUE, level = 0.25, color = "black") +
      labs(title = "Team Metric Trend Over Season with Average Line", x = "Session Date", y = input$team_metric, color = "Minutes Played")  +
      theme_bw() + 
      theme(plot.title = element_text(size = 17),
            axis.title.y = element_text(size = 15),
            axis.title.x = element_text(size = 15),
            axis.text = element_text(size = 13),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 13))
    
    # Calculate the average of the selected metric
    avg_metric <- mean(team_df[[input$team_metric]], na.rm = TRUE)
    
    # Add average line to the plot
    p + geom_hline(yintercept = avg_metric, linetype = "dashed", color = "#B30838", size = 1) +
      scale_color_gradient(low = "#013A81", high = "#F1CB00")
  })
  
  # Update match select input based on team selection
  observeEvent(input$teamSelect, {
    selectedmatches <- unique(data()[data()$team_name == input$teamSelect, "match_id"])
    updateSelectizeInput(session, "selected_matches", choices = selectedmatches)
  })
  
  # Render the scatter plot based on selected match IDs and chosen metrics
  output$individual_game_scatter <- renderPlot({
    req(input$teamSelect, input$selected_matches, input$x_metric, input$y_metric)
    
    # Filter data based on selected team and match IDs
    scatter_data <- data() %>%
      filter(team_name == input$teamSelect, match_id %in% input$selected_matches)
    
    # Create a scatter plot
    ggplot(scatter_data, aes(x = !!sym(input$x_metric), y = !!sym(input$y_metric), color = as.factor(player_id))) +
      geom_text(aes(label = player_id), size = 5.5, hjust = 0.4, vjust = -0.9) +
      geom_point(size = 5) +
      labs(title = paste("Scatter Plot for Match", input$selected_matches),
           x = input$x_metric, y = input$y_metric, color = "Player ID") +
      scale_x_continuous(expand = c(0.1, 0.1)) +
      scale_y_continuous(expand = c(0.1, 0.1)) +
      theme_bw() +
      theme(plot.title = element_text(size = 18),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 13),
            legend.title = element_text(size = 14))
  })
  
  # Create table for display in game tab
  output$gameDatatable <- renderTable({
    req(input$teamSelect, input$selected_matches)
    
    # Filter data based on selected team and match IDs
    game_table <- data() %>%
      filter(team_name == input$teamSelect, match_id %in% input$selected_matches)
    
    # Calculate averages for specific columns
    averages_data <- game_table %>%
      group_by(match_id) %>%
      summarise(
        `Activity Name` = first(activity_name),
        `Match Date` =  format(as.Date(unique(session_date)), "%Y-%m-%d"),
        `RSL Score` = sprintf("%.0f", first(rsl_score)),
        `Opponent Score` = sprintf("%.0f", first(opp_score)),
        `Avg Minutes Played` = sprintf("%.0f", mean(cat_min_played)),
        `Avg Distance Meters` = sprintf("%.1f", mean(distance)),
        `Avg Decels` = sprintf("%.0f", mean(decels)),
        `Avg Accels` = sprintf("%.0f", mean(accels)),
        `Avg HSR` = sprintf("%.1f", mean(hsr)),
        `Avg Running Distance 2ms` = sprintf("%.1f", mean(running_distance_2ms)),
        `Avg Defensive Actions` = sprintf("%.0f", mean(def_act)),
        `Avg Touches` = sprintf("%.0f", mean(touches)),
        `Avg On Ball Value` = sprintf("%.2f", mean(obv)),
        `Avg Actions Sum` = sprintf("%.0f", mean(actions_sum))
      ) %>%
      mutate(match_id = sprintf("%.0f", match_id))
  })
  
  # Create player level chart for a specific metric on player overview tab
  output$high_level_plot <- renderPlot({
    req(input$teamSelect, input$high_level_metric)  # Ensure both inputs are available
    
    filteredData <- data()[data()$team_name == input$teamSelect, ]
    if (!is.null(input$player_select)) {
      filteredData <- filteredData[filteredData$player_id %in% input$player_select, ]
    }
    filteredData <- filteredData[filteredData$session_date >= input$date_range[1] &
                                   filteredData$session_date <= input$date_range[2], ]
    
    # Create plot based on selected metric
    ggplot(filteredData, aes(x = session_date, y = !!sym(input$high_level_metric), color = as.factor(player_id))) +
      geom_point(size = 2.5) +
      geom_smooth(method = "auto", size = 1.5, se = TRUE, level = .2) +
      facet_wrap(~player_id) +
      labs(title = paste("'", input$high_level_metric, "' Metric Trend Over Season by Player ID"),
           x = "Session Date", y = input$high_level_metric, color = "Player ID") +
      theme_bw() +
      theme(plot.title = element_text(size = 16),
            axis.title.y = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            axis.text = element_text(size = 12),
            legend.position = "none",
            strip.text = element_text(size = 12))
  })
  
  # Create win/loss chart for a specific metric on win and loss tab
  output$win_loss_visualization <- renderPlot({
    req(input$teamSelect, input$win_loss_metric)  # Ensure both inputs are available
    
    wl_data <- data()[data()$team_name == input$teamSelect, ]
    
    # If a specific player is selected, filter the data for that player
    if (!is.null(input$player_select)) {
      wl_data <- wl_data[wl_data$player_id %in% input$player_select, ]
    }
    
    wl_data <- wl_data[wl_data$session_date >= input$date_range[1] &
                         wl_data$session_date <= input$date_range[2], ]
    
    # Define the order of categories and set colors manually
    order_categories <- c("Win", "Loss", "Tie")
    colors <- c("Win" = "#013A81", "Loss" = "#B30838", "Tie" = "#F1CB00")
    
    # Create plot based on selected metric
    ggplot(wl_data, aes(x = factor(Win_Loss, levels = order_categories), y = !!sym(input$win_loss_metric), fill = factor(Win_Loss, levels = order_categories))) +
      geom_boxplot() +
      scale_fill_manual(values = colors) +  # Set colors manually
      labs(title = paste("'", input$win_loss_metric, "'", "Distribution Over Specified Data Range"), x = "Game Outcome", y = input$win_loss_metric, fill = "Game Outcome") +
      theme_bw() +
      theme(plot.title = element_text(size = 16),
            axis.title.y = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            axis.text = element_text(size = 13),
            legend.text = element_text(size = 13),
            legend.title = element_text(size = 14))
  })
  
  # Create a summary table for win/loss/tie outcomes
  output$outcometable <- renderTable({
    req(input$teamSelect)  # Ensure the teamSelect input is available
    
    wl_table <- data()[data()$team_name == input$teamSelect, ]
    
    # If a specific player is selected, filter the data for that player
    if (!is.null(input$player_select)) {
      wl_table <- wl_table[wl_table$player_id %in% input$player_select, ]
    }
    
    wl_table <- wl_table[wl_table$session_date >= input$date_range[1] &
                           wl_table$session_date <= input$date_range[2], ]
    
    # Create a summary table
    summary_data <- data.frame(Win_Loss = c("Win", "Loss", "Tie"))
    
    # Use the selected win_loss_metric for summary statistics
    metric_column <- wl_table[[input$win_loss_metric]]
    
    # Create separate summaries for each outcome
    summary_data <- summary_data %>%
      rowwise() %>%
      mutate(
        Mean = mean(metric_column[wl_table$Win_Loss == Win_Loss]),
        Max = max(metric_column[wl_table$Win_Loss == Win_Loss]),
        Min = min(metric_column[wl_table$Win_Loss == Win_Loss]),
        Q1 = quantile(metric_column[wl_table$Win_Loss == Win_Loss], 0.25),
        Median = median(metric_column[wl_table$Win_Loss == Win_Loss]),
        Q3 = quantile(metric_column[wl_table$Win_Loss == Win_Loss], 0.75)
      )
    
    # Return the summary data frame
    summary_data
  })
  
  # Add output for Game Outcome tab
  output$game_outcome_visualization <- renderPlot({
    req(input$teamSelect, input$outcome_plot)  # Ensure both inputs are available
    
    acc_decel_data <- data()[data()$team_name == input$teamSelect, ]
    
    # If a specific player is selected, filter the data for that player
    if (!is.null(input$player_select)) {
      acc_decel_data <- acc_decel_data[acc_decel_data$player_id %in% input$player_select, ]
    }
    
    acc_decel_data <- acc_decel_data[acc_decel_data$session_date >= input$date_range[1] &
                                       acc_decel_data$session_date <= input$date_range[2], ]
    
    # Calculate the average of the selected metric based on Win/Loss
    avg_data <- aggregate(acc_decel_data[[input$outcome_plot]], 
                          by = list(session_date = acc_decel_data$session_date, Win_Loss = acc_decel_data$Win_Loss), 
                          FUN = mean, na.rm = TRUE)
    
    # Define the order of categories and set colors manually
    order_categories <- c("Win", "Loss", "Tie")
    colors <- c("Win" = "#013A81", "Loss" = "#B30838", "Tie" = "#F1CB00")
    
    # Create a bar plot based on the calculated averages
    ggplot(avg_data, aes(x = as.factor(session_date), y = x, fill = factor(Win_Loss, levels = order_categories))) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = sprintf("%.2f", x), y = x), vjust = -0.5, position = position_dodge(width = 0.9),
                size = 4, fontface = "bold") +  # Add labels
      scale_fill_manual(values = colors) +  # Set colors manually
      labs(title = "Average of Team or Player Selections by Game Outcome over Time", x = "Session Date", y = paste("Average of", input$outcome_plot), fill = "Game Outcome") +
      theme_bw() +
      theme(plot.title = element_text(size = 16),
            axis.title.y = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 11),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 13))
  })
  
  # Filter data based on selected team name and player IDs
  selectedData <- reactive({
    req(input$teamSelect)
    filteredData <- data()[data()$team_name == input$teamSelect, ]
    if (!is.null(input$player_select)) {
      filteredData <- filteredData[filteredData$player_id %in% input$player_select, ]
    }
    filteredData[filteredData$session_date >= input$date_range[1] &
                   filteredData$session_date <= input$date_range[2], ]
  })
  
  # Render the table based on the filtered data
  output$teamData <- renderDT({
    datatable(selectedData(), options = list(pageLength = 10))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

