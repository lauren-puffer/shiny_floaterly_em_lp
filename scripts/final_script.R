#Ella Moore & Lauren Puffer
#ESM 244 - Winter 2025

#Shiny App Layout

library(shiny)
library(here)
library(tidyverse)
library(magrittr)
library(xml2)
library(leaflet)
library(rvest)
library(stringr)
library(bslib)
library(janitor)

my_theme <- bs_theme(version = 4, bootswatch = "cerulean") %>%
  bs_theme_update(
    bg = 'rgb(255, 255, 255)', 
    fg = '#333333',  # Dark grey text
    primary = "#FFFFFF",  # Set primary button color to white
    secondary = "#FFFFFF", # Set secondary button color to white
    info = "#FFFFFF"
  ) %>%
  bs_add_rules("
    body {
      color: #333333 !important; /* Dark grey text */
    }

    /* Tab style customization */
    .nav-tabs {
      background-color: #f8f9fa;  /* Light background color for tabs */
      border-bottom: 2px solid #ccc; /* Optional: subtle border at the bottom */
    }

    .nav-tabs .nav-item.show .nav-link, .nav-tabs .nav-link.active {
      background-color: #e9ecef;  /* Active tab background */
      color: #333333 !important;  /* Active tab text color */
    }

    .nav-tabs .nav-link {
      color: #555555 !important;  /* Non-active tab text color */
      background-color: #fff !important;  /* Non-active tab background color */
      border: 1px solid #ddd;  /* Optional: subtle border around each tab */
    }

    /* Optional: Hover effect */
    .nav-tabs .nav-link:hover {
      background-color: #f1f1f1;
      color: #333333 !important;
    }
  ")





# Load creek data
creek_data <- read.csv(here("data", "Floaterly Creek ID Spreadsheet - Sheet1.csv")) |>
  clean_names() %>%
  rename(creek_node = url)

creek_data <- creek_data %>%
  mutate(creek_node = str_remove(creek_node, "https://rain.cosbpw.net/site/\\?site_id="))

# Function to scrape hydrologic data
scrape_creek_data <- function(creek_node) {
  url <- paste0("https://rain.cosbpw.net/site/?site_id=", creek_node)
  webpage <- read_html(url)
  
  # Scrape Flow Volume
  flow_volume_text <- webpage %>%
    html_nodes("body") %>%
    html_text() %>%
    str_split("\n") %>%
    unlist() %>%
    grep("Flow Volume", ., value = TRUE)
  
  flow_volume <- ifelse(length(flow_volume_text) > 0, str_extract(flow_volume_text[1], "\\d+"), NA)
  
  # Scrape Stage
  stage_text <- webpage %>%
    html_nodes("body") %>%
    html_text() %>%
    str_split("\n") %>%
    unlist() %>%
    grep("Stage", ., value = TRUE)
  
  stage <- ifelse(length(stage_text) > 0, str_extract(stage_text[1], "\\d+\\.\\d+"), NA)
  
  # Return data
  return(list(
    flow_volume = flow_volume,
    stage = stage
  ))
}


# UI
ui <- fluidPage(
  theme = my_theme,  
  
  sidebarLayout(
    sidebarPanel(
      tags$img(src = "Logo.png", height = "250px"),
      'Tell us about your float!',
      
      selectInput('creek', "Choose a body of water", choices = creek_data$common_name),
      actionButton("goButton", label = tags$img(src = "Get_Floatin.png", height = "100px")),
      
      actionButton("help_btn", label = tags$img(src = "Help.png", height = "100px")),
      
      checkboxGroupInput(
        inputId = 'hydrologic_data',
        label = "What data would you like in your swim report",
        choices = c('Weather', 'Water Conditions')
      ),
      
      checkboxGroupInput(
        inputId = 'recommendation_type',
        label = "What type of recommendation would you like?",
        choices = c('Safety Report', 'Local Recommendation')
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Swim Report", 
                 leafletOutput("map_output"), 
                 verbatimTextOutput("creekData"),
                 verbatimTextOutput("weatherData"),
                 verbatimTextOutput("localRecommendation")
        ),  
        tabPanel("Help", uiOutput("help_content")),  
        tabPanel("Gallery", uiOutput("gallery_content"))  
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Get the selected creek's node based on the selected creek name
  selected_creek_node <- reactive({
    selected_creek <- input$creek
    creek_node <- creek_data$creek_node[creek_data$common_name == selected_creek]
    return(creek_node)
  })
  
  # Static weather URL (you can modify this to be dynamic if needed)
  weather_url <- "https://forecast.weather.gov/MapClick.php?lat=34.4262&lon=-119.8415"  # Example URL
  
  # Function to scrape weather data
  scrape_weather_data <- function(weather_url) {
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
    
    # Return weather data
    return(list(
      weather_condition = weather_condition,
      temperature = temperature,
      humidity = humidity_percentage,
      wind_speed = wind_speed
    ))
  }
  
  # Reactive expression to fetch creek data when the button is clicked
  creek_data_reactive <- eventReactive(input$goButton, {
    # Get the selected creek's node
    creek_node <- selected_creek_node()
    
    # Scrape the creek data (flow volume, stage) based on the creek node
    creek_data <- scrape_creek_data(creek_node)
    
    # Return the creek data
    return(creek_data)
  })
  
  # Reactive expression to fetch weather data when the checkbox "Weather" is selected
  weather_data_reactive <- eventReactive(input$goButton, {
    # Scrape the weather data from the fixed URL
    weather_data <- scrape_weather_data(weather_url)
    
    # Return the weather data
    return(weather_data)
  })
  
  # Reactive expression for Local Recommendation based on selected creek
  local_recommendation_reactive <- reactive({
    selected_creek <- input$creek
    
    # Retrieve the corresponding local recommendation based on the selected creek
    local_rec <- creek_data$local_rec[creek_data$common_name == selected_creek]
    
    return(local_rec)
  })
  
  # Render the creek data (Flow Volume, Stage) based on checkbox input
  output$creekData <- renderText({
    creek_data <- creek_data_reactive()
    selected_data <- ""
    
    if ("Water Conditions" %in% input$hydrologic_data) {
      selected_data <- paste(selected_data,
                             "Flow Volume: ", creek_data$flow_volume, "\n",
                             "Stage: ", creek_data$stage, "\n")
    }
    
    return(selected_data)
  })
  
  # Render the weather data (Weather Condition, Temperature, etc.) based on checkbox input
  output$weatherData <- renderText({
    selected_data <- ""
    
    # Only render weather data if "Weather" is selected in the checkbox
    if ("Weather" %in% input$hydrologic_data) {
      weather_data <- weather_data_reactive()
      
      selected_data <- paste(selected_data,
                             "Weather Condition: ", weather_data$weather_condition, "\n",
                             "Temperature: ", weather_data$temperature, "\n",
                             "Humidity: ", weather_data$humidity, "\n",
                             "Wind Speed: ", weather_data$wind_speed, "\n")
    }
    
    return(selected_data)
  })
  
  # Render the Local Recommendation when "Local Recommendation" is selected and the Go button is clicked
  output$localRecommendation <- renderText({
    # Only render local recommendation if "Local Recommendation" is selected and "Go" button is clicked
    selected_data <- ""
    
    if ("Local Recommendation" %in% input$recommendation_type) {
      local_rec <- local_recommendation_reactive()
      selected_data <- paste("Local Recommendation: ", local_rec)
    }
    
    return(selected_data)
  })
  
  # Initially render the map in the "Swim Report" tab
  output$map_output <- renderLeaflet({
    req(creek_data)  
    leaflet(data = creek_data) %>%
      addTiles() %>%
      addMarkers(
        lng = ~long, lat = ~lat, 
        label = ~common_name  # Display name on hover
      ) %>%
      fitBounds(
        lng1 = min(creek_data$long), 
        lat1 = min(creek_data$lat),
        lng2 = max(creek_data$long),
        lat2 = max(creek_data$lat)
      )
  })
  
  
  # Render help content (always visible)
  output$help_content <- renderUI({
    tagList(
      h3("How to Floaterly"),
      p("Follow the steps below to get started:"),
      tags$ul(
        tags$li(
          p("Step 1: Select a stream from the dropdown menu. The map shows available stream locations.")
        ),
        tags$li(
          p("Step 2: Choose what data to include in your swim report (weather, flow, etc.).")
        ),
        tags$li(
          p("Step 3: Click 'Get Floatin'' to generate your swim report.")
        )
      )
    )
  })
  
  
  # Help button switches to Help tab
  observeEvent(input$help_btn, {
    updateTabsetPanel(session, "tabs", selected = "Help")
  })
}

# Run the app
shinyApp(ui = ui, server = server)


