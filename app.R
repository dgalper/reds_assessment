library(shiny)
library(tidyverse)
library(ggplot2)

# Read in predictions df
filled_predictions_df <- read_csv("filled_predictions.csv")

# Write function to get full names
convert_name <- function(name) {
  parts <- strsplit(name, ", ")[[1]]
  formatted_name <- paste(trimws(parts[2]), trimws(parts[1]))
  return(formatted_name)
}

# Add in clean full name
filled_predictions_df <- filled_predictions_df %>%
  mutate(CLEAN_NAME = sapply(PLAYER_NAME, convert_name))

# Define UI
ui <- fluidPage(
  titlePanel("Predicted 2024 Batter Pitch Mixes"),
  
  # Input: Dropdown for user to select a player name
  selectInput("player_name", "Select Player Name:", choices = unique(filled_predictions_df$CLEAN_NAME)),
  
  # Output: Pie chart
  plotOutput("pieChart")
)

# Define Server
server <- function(input, output) {
  
  # Reactive expression to filter data based on player name input
  player_data <- reactive({
    req(input$player_name)  # Ensure that input is available
    filled_predictions_df[filled_predictions_df$CLEAN_NAME == input$player_name, ]
  })
  
  # Render Pie Chart
  output$pieChart <- renderPlot({
    # Get data for the selected player
    selected_player <- player_data()
    
    # Ensure that data exists for the selected player
    if (nrow(selected_player) == 0) {
      return()
    }
    
    # Prepare data for pie chart
    # Prepare data for pie chart
    pitch_types <- c("Fastball", "Off-speed", "Breaking Ball")
    percentages <- c(selected_player$PITCH_TYPE_FB, selected_player$PITCH_TYPE_OS, selected_player$PITCH_TYPE_BB)
    
    # Add percentage labels to the chart
    labels <- paste0(round(100 * percentages, 1), "%")
    
    # Define colors for the pie chart and legend
    colors <- c("red", "skyblue", "green")
    
    # Create the pie chart using base R pie function
    pie(percentages, labels = labels, col = colors, 
        main = paste("Predicted 2024 Pitch Mix for", selected_player$CLEAN_NAME))
    
    # Add a legend to the pie chart
    legend("topright", legend = pitch_types, fill = colors, title = "Pitch Types")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
