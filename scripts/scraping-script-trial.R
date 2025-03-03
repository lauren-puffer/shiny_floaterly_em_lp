library(shiny)
library(rvest)
library(tidyverse)
library(leaflet)
library(bslib)
library(here)
library(janitor)
library(stringr)

# Input CSV with creek names and nodes for scraping
creek_data <- read.csv(here("data", "Floaterly Creek ID Spreadsheet - Sheet1.csv")) %>%
  clean_names() %>%
  rename(creek_node = url)

# Add a new column for the creek node (strip the part of the URL)
creek_data <- creek_data %>%
  mutate(creek_node = str_remove(creek_node, "https://rain.cosbpw.net/site/\\?site_id="))

# Function to scrape data based on creek node
scrape_creek_data <- function(creek_node) {
  # Construct the URL to scrape using the creek_node
  url <- paste0("https://rain.cosbpw.net/site/?site_id=", creek_node)
  
  # Read the HTML content of the page
  webpage <- read_html(url)
  
  # Scrape Flow Volume
  flow_volume_text <- webpage %>%
    html_nodes("body") %>%
    html_text() %>%
    str_split("\n") %>%
    unlist() %>%
    grep("Flow Volume", ., value = TRUE)
  
  # Extract the number after "Flow Volume"
  flow_volume <- NA
  if (length(flow_volume_text) > 0) {
    flow_volume <- webpage %>%
      html_nodes("div.h3") %>%
      html_text() %>%
      .[1]  # Get the first occurrence after "Flow Volume"
  }
  
  # Scrape Stage
  stage_text <- webpage %>%
    html_nodes("body") %>%
    html_text() %>%
    str_split("\n") %>%
    unlist() %>%
    grep("Stage", ., value = TRUE)
  
  # Extract the number after "Stage"
  stage <- NA
  if (length(stage_text) > 0) {
    stage <- webpage %>%
      html_nodes("div.h3") %>%
      html_text() %>%
      .[2]  # Get the second occurrence after "Flow Volume"
  }
  
  # Scrape weather condition and temperature (from another weather URL)
  weather_url <- "https://forecast.weather.gov/MapClick.php?lat=34.4262&lon=-119.8415"  # Modify with your actual coordinates
  
  # Read the HTML content of the weather page
  weather_page <- read_html(weather_url)
  
  # Scrape weather condition
  weather_condition <- weather_page %>%
    html_nodes(".myforecast-current") %>%
    html_text() %>%
    .[1]  # Get the first occurrence for the weather condition
  
  # Scrape temperature
  temperature <- weather_page %>%
    html_nodes(".myforecast-current-lrg") %>%
    html_text() %>%
    .[1]  # Get the first occurrence for the temperature
  
  # Scrape humidity
  weather_data <- weather_page %>%
    html_nodes("td") %>%
    html_text()
  
  # Find the index of the first occurrence of "Humidity"
  humidity_index <- grep("Humidity", weather_data)
  humidity_percentage <- gsub("[^0-9%]", "", weather_data[humidity_index + 1])
  
  # Find the index of the first occurrence of "Wind Speed"
  wind_speed_index <- grep("Wind Speed", weather_data)
  wind_speed <- gsub("\\s+", " ", weather_data[wind_speed_index + 1]) 
  
  # Return both Flow Volume, Stage, Weather Condition, Temperature, Humidity, and Wind Speed
  return(list(
    flow_volume = flow_volume, 
    stage = stage, 
    weather_condition = weather_condition, 
    temperature = temperature,
    humidity = humidity_percentage,
    wind_speed = wind_speed
  ))
}

# Define the UI
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "cerulean"),  
  titlePanel("Floaterly"),
  
  sidebarLayout(
    sidebarPanel(
      # Dynamically populate the dropdown based on creek names
      selectInput("creek", "Choose a body of water", choices = creek_data$common_name),
      actionButton("goButton", "Get Floatin"),
      
      # Help Button
      actionButton("help_button", "Help"),
      
      # Output area for help content
      uiOutput("help_content")
    ),
    
    mainPanel(
      # Output text to display scraped data
      verbatimTextOutput("creekData"),
      verbatimTextOutput("weatherData"),
      
      # Add map output to display the map
      leafletOutput("map_output", height = "600px")
    )
  )
)

# Server function
server <- function(input, output) {
  
  # Get the selected creek's node based on the selected creek name
  selected_creek_node <- reactive({
    selected_creek <- input$creek
    # Get the node value from the creek_data dataframe
    creek_node <- creek_data$creek_node[creek_data$common_name == selected_creek]
    return(creek_node)
  })
  
  # Reactive expression to fetch creek and weather data when the button is clicked
  creek_data_reactive <- eventReactive(input$goButton, {
    # Get the selected creek's node
    creek_node <- selected_creek_node()
    
    # Scrape the data (flow volume, stage, weather condition, and temperature)
    data <- scrape_creek_data(creek_node)
    
    # Return the scraped data as a formatted text
    formatted_creek_data <- paste(
      "Flow Volume: ", data$flow_volume, "\n",
      "Stage: ", data$stage
    )
    
    # Return the weather data as a formatted text
    formatted_weather_data <- paste(
      "Weather Condition: ", data$weather_condition, "\n",
      "Temperature: ", data$temperature, "\n",
      "Humidity: ", data$humidity, "\n",
      "Wind Speed: ", data$wind_speed
    )
    
    list(creek_data = formatted_creek_data, weather_data = formatted_weather_data)
  })
  
  # Render the creek data
  output$creekData <- renderText({
    creek_data_reactive()$creek_data
  })
  
  # Render the weather data
  output$weatherData <- renderText({
    creek_data_reactive()$weather_data
  })
  
  # Render the map with creek locations
  output$map_output <- renderLeaflet({
    leaflet(data = creek_data) %>%
      addTiles() %>%
      addMarkers(
        lng = ~long, lat = ~lat, 
        popup = ~paste("Lat: ", lat, "<br>Long: ", long),
        label = ~common_name,
        labelOptions = labelOptions(
          noHide = FALSE,
          direction = "top",
          style = list(
            "font-weight" = "bold",
            "font-size" = "14px",
            "color" = "black"
          )
        )
      ) %>%
      fitBounds(
        lng1 = min(creek_data$long), 
        lat1 = min(creek_data$lat),
        lng2 = max(creek_data$long),
        lat2 = max(creek_data$lat)
      )
  })
  
  # Observe Help Button click and display help content
  observeEvent(input$help_button, {
    output$help_content <- renderUI({
      tagList(
        tags$h3("How to Use Floaterly"),
        tags$p("Follow the steps below to get started:"),
        tags$ul(
          tags$li(
            tags$p("Step 1: Select a stream from the dropdown menu. The map shows available stream locations.")
          ),
          tags$li(
            tags$p("Step 2: Choose what data to include in your swim report (weather, flow, etc.).")
          ),
          tags$li(
            tags$p("Step 3: Click 'Get Floatin'' to generate your swim report.")
          )
        )
      )
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)





  
creek_data_reactive <- eventReactive(input$goButton, {
  # Get the selected creek's node
  creek_node <- selected_creek_node()
  
  # Scrape the data (flow volume, stage, weather condition, and temperature)
  data <- scrape_creek_data(creek_node)
  
  # Return the scraped data as a formatted text
  formatted_creek_data <- paste(
    "Flow Volume: ", data$flow_volume, "\n",
    "Stage: ", data$stage)
  
  # Return the formatted data
  list(creek_data = formatted_creek_data)
})
  


