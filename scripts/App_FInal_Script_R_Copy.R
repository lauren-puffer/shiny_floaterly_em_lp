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
library(shinythemes)


my_theme <- bs_theme(version = 4, bootswatch = "cerulean")


# Load creek data
creek_data <- read.csv(here("data", "Floaterly Creek ID Spreadsheet - Sheet1.csv")) |>
  clean_names() %>%
  drop_na()|>
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
  
  
  hydrologic_data <<- data.frame(
    flow_volume = flow_volume,
    stage = stage
  )
  
  #Changed scraping data to permanent temp data to make it run
  # Return data
  return(list(
  flow_volume = flow_volume,
  stage = stage
  
  ))
}


#Logistic regression model function. Logistic regression made in seperate script
predict_safety <- function(velocity_ft_s) {
  # Model coefficients
  intercept <- -18.1
  velocity_coef <- 8.35
  
  # Calculate the log-odds (linear predictor)
  log_odds <- intercept + velocity_coef * velocity_ft_s
  
  # Convert log-odds to probability using the logistic function
  prob <- 1 / (1 + exp(-log_odds))
  
  # Predict safety: if prob > 0.5, predict unsafe (1), else safe (0)
  if (prob > 0.5) {
    prediction <- 1
    message <- "According to our model the water is not safe to swim in."
  } else {
    prediction <- 0
    message <- "According to our model the water is safe to swim in."
  }
  
  # Return the prediction and message
  return(list(prediction = prediction, message = message, probability = prob))
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
                 verbatimTextOutput("localRecommendation"),
                 verbatimTextOutput("safetyReport")
        ),  
        tabPanel("How To Use Floaterly", uiOutput("help_content")),  
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
    
    
    
    
    
    #Added temporary code for scraping so it runs on my computer
    
    # Return weather data
   return(list(
    weather_condition = weather_condition,
   temperature = temperature,
   humidity = humidity_percentage,
  wind_speed = wind_speed))
    
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
  
  
  go_trigger <- eventReactive(input$goButton, {
    input$recommendation_type  # Returns the selected recommendation type when button is clicked
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
  
#run safety report
  output$safetyReport <- renderText({
    selected_data <- ""
    
    if ("Safety Report" %in% input$recommendation_type) {
      
      # Use hardcoded values from the hydrologic_data data frame
      flow_volume <- hydrologic_data$flow_volume
      stage <- hydrologic_data$stage
      
      # Ensure flow and stage are numeric, and check if they are NA or 0
      if (is.na(flow_volume) || is.na(stage) || flow_volume == 0 || stage == 0) {
        selected_data <- "There is not enough data to run our safety model."
      } else {
        velocity_input <- flow_volume / stage  # Compute velocity
        result <- predict_safety(velocity_input)  # Run safety prediction
        
        # Capture and display the safety report message
        selected_data <- paste("Safety Report:", result$message)
      }
    }
    
    return(selected_data)
  })


  # Initially render the map in the "Swim Report" tab
  output$map_output <- renderLeaflet({
    req(creek_data)  
    
    # Create a custom icon using the image from the www folder
    custom_icon <- makeIcon(
      iconUrl = "Icon.png",  # Specify the image file in the www folder
      iconWidth = 32,        # Adjust the size of the icon (width)
      iconHeight = 32,       # Adjust the size of the icon (height)
      iconAnchorX = 16,      # Anchor point for the icon (horizontal)
      iconAnchorY = 32,      # Anchor point for the icon (vertical)
      popupAnchorX = 0,      # Adjust the popup position (horizontal)
      popupAnchorY = -32     # Adjust the popup position (vertical)
    )
    
    leaflet(data = creek_data) %>%
      addTiles() %>%
      addMarkers(
        lng = ~long, lat = ~lat, 
        label = ~common_name,  # Display name on hover
        icon = custom_icon     # Use the custom icon for the markers
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
      h2("Floaterly Quick Tutorial"),  # Larger title
      p("Follow the steps below to get the most out of your Floaterly experience:"),
      
      # Step 1
      tags$div(
        tags$h3("Step 1: Select a Stream"),
        p("Choose one of Santa Barbara's beautiful local streams from the dropdown menu. 
         The map in the main panel shows all available stream locations.")
      ),
      
      # Step 2
      tags$div(
        tags$h3("Step 2: Customize Your Swim Report"),
        p("Use the checkboxes to select what kind of data you want included in your swim report.")
      ),
      
      # Weather Data Section
      tags$div(
        tags$h3("Weather Data"),
        p("By selecting the weather option, your report will include temperature, wind speed, humidity, and a forecast for the day. 
         This data is sourced from the National Weather Service web page for Santa Barbara Municipal Airport.")
      ),
      
      # Hydrologic Data Section
      tags$div(
        tags$h3("Hydrologic Data"),
        p("The stage and flow data represent the height of the water and the amount of water running through the stream. 
         This data is collected from the County of Santa Barbara data server.")
      ),
      
      # Safety Report Section
      tags$div(
        tags$h3("Safety Report"),
        p("The safety report is generated using a logistic regression model that characterizes the velocity of the stream 
         using flow and stage data scraped from the County of Santa Barbara data server."),
        p("Velocity is calculated assuming a square channel. The logistic regression model used was trained on USGS data for Mission Creek in Santa Barbara 
         from January 2024 to March 2024. This data range was selected for its large fluctuations in flow and stage."),
        p("Data was classified as safe or unsafe based on stage and velocity. All flows with a stage greater than 5 ft and more than 5.9 cfs were 
         considered unsafe. The stage limit was chosen based on an estimate of the average chest height of a person, and the velocity was selected 
         based on the International Scale of River Difficulty for flat water. This assumes that Class C rivers (velocity more than 4 mph) are unsafe for swimming. 
         This is a conservative estimate, reflected in the model.")
      ),
      
      # Local Recommendation Section
      tags$div(
        tags$h3("Local Recommendation"),
        p("The local recommendation is based on knowledge of popular swimming locations and the health and safety of the rivers. 
         This provides insights into which spots are safest for swimming based on current conditions.")
      ),
      
      # Step 3
      tags$div(
        tags$h3("Step 3: Generate Your Swim Report"),
        p("Click 'Get Floatin'' to generate your personalized swim report.")
      ),
      
      # Citation Section
      tags$div(
        tags$h3("Data Sources & Citations"),
        p("National Oceanic and Atmospheric Administration, N. (2025, March 2). National Weather Service. https://forecast.weather.gov/MapClick.php?lat=34.4262&lon=-119.8415 "),
        p("County of Santa Barbara. (n.d.). Home. County of Santa Barbara: Real Time Rainfall, River-Stream, and Reservoir Data. https://rain.cosbpw.net/ "),
        p("USGS. (n.d.). Mission C NR mission st nr santa barbara ca. USGS-Water Data for the Nation. https://waterdata.usgs.gov/monitoring-location/11119750/#dataTypeId=continuous-00065-0&period=P7D&showMedian=false "),
        p("Texas Parks and Wildlife. (n.d.). River safety. Texas Parks and Wildlife. https://tpwd.texas.gov/landwater/water/habitats/rivers/safety.phtml "),
        p("Additional Sources: 
          https://en.wikipedia.org/wiki/Santa_Maria_River_(California), 
https://en.wikipedia.org/wiki/Sisquoc_River, 
https://en.wikipedia.org/wiki/Santa_Ynez_River, 
https://en.wikipedia.org/wiki/Santa_Ynez_River, 
https://en.wikipedia.org/wiki/Santa_Ynez_River, 
https://en.wikipedia.org/wiki/Santa_Ynez_River, 
https://en.wikipedia.org/wiki/Carpinteria_Creek, 
https://sbparksandrec.santabarbaraca.gov/creeks/mission-creek, 
https://en.wikipedia.org/wiki/Sisquoc_River, 
https://en.wikipedia.org/wiki/Cuyama_River, 
")
      )
    )
  })
  
  
  
  output$gallery_content <- renderUI({
    tagList(
      h3("Get inspired by all of the beautiful rivers in Santa Barbara County"),
      
      # Image for Montecito Hot Springs
      div(
        tags$h4("Montecito Hot Springs"),
        tags$img(src = "Montecito_hot_springs.jpg", height = "400px", width = "auto"),
        p(a("Source", href = "https://www.findinghotsprings.com/mag/montecito-hot-springs", target = "_blank"))
      ),
      
      # Image for Santa Ynez River
      div(
        tags$h4("Santa Ynez River"),
        tags$img(src = "Santa_Ynez1.jpg", height = "400px", width = "auto"),
        p(a("Source", href = "https://en.wikipedia.org/wiki/Santa_Ynez_River", target = "_blank"))
      ),
      
      # Image for Santa Maria River
      div(
        tags$h4("Santa Maria River"),
        tags$img(src = "santamaria.jpg", height = "400px", width = "auto"),
        p(a("Source", href = "https://creeklands.org/projects/santa-maria-river-healthy-watershed-initiative/", target = "_blank"))
      ),
      
      # Image for Sisquoc River
      div(
        tags$h4("Sisquoc River"),
        tags$img(src = "Sisqoc.jpg", height = "400px", width = "auto"),
        p(a("Source", href = "https://askirtinthedirt.com/archives/6731", target = "_blank"))
      )
    )
  })
  
  
  
  # Help button switches to Help tab
  observeEvent(input$help_btn, {
    updateTabsetPanel(session, "tabs", selected = "How To Use Floaterly")
  })
}

# Run the app
shinyApp(ui = ui, server = server)




