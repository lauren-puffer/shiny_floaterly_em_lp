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


my_theme <- bs_theme(version = 4, bootswatch = "cerulean", font = "Fredoka One")  # You can change this to any font


# Load creek data
creek_data <- read.csv(here("data", "Floaterly Creek ID Spreadsheet - Sheet1.csv")) |>
  clean_names() %>%
  drop_na()|>
  rename(creek_node = url)

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
  
  # return values
  return(list(
    flow_volume = flow_volume, 
    stage = stage))
  
}



#hydrologic_data <<- data.frame(
#flow_volume = flow_volume,
#stage = stage)
#<<<<<<< HEAD:scripts/App_FInal_Script_R_Copy.R
#Changed scraping data to permanent temp data to make it run
# Return data
#=======
#>>>>>>> 4fc43b19321df90334a547e6f7cacd208e418ea7:scripts/App_Script_3.4.R


#Logistic regression model function. Logistic regression made in seperate script
predict_safety <- function(flow_volume, stage) {
  # Model coefficients
  intercept <- -18.1
  velocity_coef <- 8.35
  
  velocity_ft_s <- flow_volume / stage  
  
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
  return(list(prediction = prediction, message = message))
  
  #, probability = prob
}




# UI
ui <- fluidPage(
  theme = my_theme,  
  
  tags$head(
    tags$style(HTML("
      /* Hot Pink checkboxes */
      .checkbox input[type='checkbox'] {
        accent-color: #FF69B4;  /* Hot pink color */
      }

      /* Change the active tab background to Hot Pink and make the text black and bold */
      .nav-tabs .nav-item .active a {
        background-color: #FF69B4 !important;  /* Hot pink for active tab */
        color: black !important;  /* Black text for active tab */
        font-weight: bold !important;  /* Bold text for active tab */
      }
      
    "))
  ),
  
  
  sidebarLayout(
    sidebarPanel(
      tags$img(src = "Logo.png", height = "250px", style = "width: 100%; object-fit: contain;"),
      tags$h2("Tell us about your float!", 
              style = "font-size: 36px; font-weight: bold; font-family: 'Fredoka One', sans-serif")
  ,
  
      
  selectInput('creek', label = tags$b("Choose a body of water"), choices = creek_data$common_name),

      
      checkboxGroupInput(
        inputId = 'hydrologic_data',
        label = tags$b("What data would you like in your swim report?"),
        choices = c('Weather', 'Water Conditions')
      ),
      
      checkboxGroupInput(
        inputId = 'recommendation_type',
        label = tags$b("What type of recommendation would you like?"),
        choices = c('Safety Report', 'Local Recommendation')
      ), 
      tags$div(
        style = "display: flex; justify-content: space-evenly; align-items: center; width: 100%;",
        actionButton("goButton", label = tags$img(src = "Get_Floatin.png", height = "150px")),
        actionButton("help_btn", label = tags$img(src = "Help.png", height = "150px"))
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Swim Report", 
                 leafletOutput("map_output", height = "600px"), 
                 uiOutput("creekData"),
                 uiOutput("weatherData"),
                 uiOutput("localRecommendation"),
                 uiOutput("safetyReport")
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
  
  
  # Trigger event when Go button is pressed
  go_trigger <- eventReactive(input$goButton, {
    input$recommendation_type  # Returns the selected recommendation type when button is clicked
  })
  
  # Reactive expression for Local Recommendation based on selected creek
  local_recommendation_reactive <- reactive({
    req(input$creek)  # Ensure input is not NULL
    local_rec <- creek_data$local_rec[creek_data$common_name == input$creek]
    return(local_rec)
  })
  
  # Render the Local Recommendation only when "Local Recommendation" is checked and the Go button is pressed
  output$localRecommendation <- renderUI({
    req("Local Recommendation" %in% go_trigger())  # Ensures both the checkbox and button are used
    req(local_recommendation_reactive())  # Ensure data exists
    
    local_rec <- local_recommendation_reactive()
    
    # Ensure there is valid data before returning
    if (!is.null(local_rec) && local_rec != "") {
      return(HTML(paste("<h3><b>Local Recommendation:</b></h3>", 
                        local_rec)))
    } else {
      return("")  # Empty output if no recommendation is found
    }
  })
  
  
  # Render the creek data (Flow Volume, Stage) based on checkbox input
  output$creekData <- renderUI({
    creek_data <- creek_data_reactive()
    selected_data <- ""
    
    if ("Water Conditions" %in% input$hydrologic_data) {
      selected_data <- HTML(paste(
        "<h3><b>Water Conditions</b></h3>",  # Smaller title for Water Conditions
        "<b>Flow:</b> ", creek_data$flow_volume, "<br>",  # Bold Flow and Stage
        "<b>Stage:</b> ", creek_data$stage, "<br>"  # Bold Flow and Stage
      ))
    }
    
    return(selected_data)
  })
  
  # Render the weather data (Weather Condition, Temperature, etc.) based on checkbox input
  output$weatherData <- renderUI({
    selected_data <- ""
    
    # Only render weather data if "Weather" is selected in the checkbox
    if ("Weather" %in% input$hydrologic_data) {
      weather_data <- weather_data_reactive()
      
      selected_data <- HTML(paste(
        "<h3><b>Weather Report:</b></h3>", 
        "<b>Forecast:</b> ", weather_data$weather_condition, "<br>",
        "<b>Temperature:</b> ", weather_data$temperature, "°C<br>",
        "<b>Humidity:</b> ", weather_data$humidity, "%<br>",
        "<b>Wind Speed and Direction:</b> ", weather_data$wind_speed, " ", weather_data$wind_direction, "<br>"
      ))
    }
    return(selected_data)
  })
  
  
  # Trigger event when Go button is pressed
  go_trigger <- eventReactive(input$goButton, {
    input$recommendation_type  # Returns the selected recommendation type when button is clicked
  })
  
  # Render the Safety Report only when "Safety Report" is checked and the Go button is pressed
  output$safetyReport <- renderUI({
    req("Safety Report" %in% go_trigger())  # Ensure both the checkbox and button are used
    
    
    hydrologic_data <- creek_data_reactive()  # Get the latest hydrologic data
    
    # Ensure hydrologic_data is available
    req(hydrologic_data)
    
    # Extract necessary values
    flow_volume <- hydrologic_data$flow_volume
    stage <- hydrologic_data$stage
    
    # Ensure flow and stage are valid numeric values
    if (is.na(flow_volume) || is.na(stage) || flow_volume == 0 || stage == 0) {
      return("Safety Report", "Either flow or water height is 0 indicating that the stream is safe to swim in!")
    }
    
    # Compute velocity and predict safety
    #velocity_input <- flow_volume / stage  
    result <- predict_safety(flow_volume, stage)  
    
    # Return the safety report message
    return(HTML(paste("<h3><b>Safety Report:</b></h3>", result$message ,  "<br>",  # Adds a line break
                      "<p>To learn more about the logistic regression model used to evaluate safety, see the How To Use Floaterly tab"  # The additional message with line break
    )))
  })
  

  
  # Render the map with all the data initially
  output$map_output <- renderLeaflet({
    req(creek_data)  # Ensure creek_data is available
    
    # Create a custom icon using the image from the www folder
    custom_icon <- makeIcon(
      iconUrl = "Drop.png",  # Specify the image file in the www folder
      iconWidth = 32,        # Adjust the size of the icon (width)
      iconHeight = 32,       # Adjust the size of the icon (height)
      iconAnchorX = 16,      # Anchor point for the icon (horizontal)
      iconAnchorY = 32,      # Anchor point for the icon (vertical)
      popupAnchorX = 0,      # Adjust the popup position (horizontal)
      popupAnchorY = -32     # Adjust the popup position (vertical)
    )
    
    # Create the leaflet map with all the data
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
  

  
  # Reactive expression for the filtered creek data based on user selection
  filtered_creek_data <- reactive({
    req(input$goButton)  # Ensure this runs when the 'Go' button is pressed
    
    # Filter the data based on the selected creek
    creek_data %>% filter(common_name == input$creek)
  })
  
  
  # Render the map with only the selected creek when the 'Go' button is pressed
  observeEvent(input$goButton, {
    # Ensure filtered creek data is available

    req(filtered_creek_data())
    
    # Get the filtered creek data
    creek_data_filtered <- filtered_creek_data()
    
    # Define a new custom icon for use in leafletProxy
    custom_icon <- makeIcon(
      iconUrl = "Drop.png",  # Use the new icon specifically for the proxy update
      iconWidth = 32,
      iconHeight = 32,
      iconAnchorX = 16,
      iconAnchorY = 32,
      popupAnchorX = 0,
      popupAnchorY = -32
    )
    
    # Use leafletProxy to update the existing map
    leafletProxy("map_output", session) %>%
      clearMarkers() %>%  # Remove existing markers
      addMarkers(
        data = creek_data_filtered,
        lng = ~long, lat = ~lat, 
        label = ~common_name,  # Display name on hover
        icon= custom_icon# Use the custom icon for markers
      ) 
  })

  
  
  # Render help content (always visible)
  output$help_content <- renderUI({
    tagList(
      h2("Floaterly Quick Tutorial"),  # Larger title
      p("The purpouse of this app is to provide realtime recommendations regarding swimming safety and water conditions 
        for local streams in Santa Barbara County"),
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
        p("Use the checkboxes to select what kind of data or recommendations you want included in your swim report.")
      ),
      
      # Weather Data Section
      tags$div(
        tags$b("Weather Data"),
        p("By selecting the weather option, your report will include temperature, wind speed, humidity, and a forecast for the day. 
         This data is sourced from the National Weather Service web page for Santa Barbara Municipal Airport.")
      ),
      
      # Hydrologic Data Section
      tags$div(
        tags$b("Hydrologic Data"),
        p("The stage and flow data represent the height of the water and the amount of water running through the stream. 
         This data is collected from the County of Santa Barbara data server.")
      ),
      
      # Safety Report Section
      tags$div(
        tags$b("Safety Report"),
        p("The safety report is generated using a logistic regression model that characterizes the velocity of the stream 
         using flow and stage data scraped from the County of Santa Barbara data server."),
        p("Velocity is calculated assuming a square channel. The logistic regression model used was trained on USGS data for Mission Creek in Santa Barbara
         from January 2024 to March 2024. This data range was selected for its large fluctuations in flow and stage."),
        p("Data was classified as safe or unsafe based on stage and velocity. All flows with a stage greater than 5 ft and more than 5.9 cfs were 
         considered unsafe. The stage limit was chosen based on an estimate of the average chest height of a person, and the velocity was selected 
         based on the International Scale of River Difficulty for flat water (Texas Parks and Wildlife). This assumes that Class C rivers (velocity more than 4 mph) are unsafe for swimming. 
         This is a conservative estimate, reflected in the model.")
      ),
      
      # Local Recommendation Section
      tags$div(
        tags$b("Local Recommendation"),
        p("The local recommendation is based on knowledge of popular swimming locations and the health and safety of the rivers. 
         This provides insights into which spots are safest for swimming based on current conditions.")
      ),
      
      # Step 3
      tags$div(
        tags$h3("Step 3: Generate Your Swim Report"),
        p("Click the 'Get Floatin'' button to generate your personalized swim report.")
      ),
      
      # Citation Section
      tags$div(
        tags$h3("Data Sources & Citations"),
        p("National Oceanic and Atmospheric Administration, N. (2025, March 2). National Weather Service. 
     ", tags$a(href = "https://forecast.weather.gov/MapClick.php?lat=34.4262&lon=-119.8415", "Link")),
        p("County of Santa Barbara. (n.d.). Home. County of Santa Barbara: Real Time Rainfall, River-Stream, and Reservoir Data. 
     ", tags$a(href = "https://rain.cosbpw.net/", "Link")),
        p("USGS. (n.d.). Mission C NR mission st nr santa barbara ca. USGS-Water Data for the Nation. 
     ", tags$a(href = "https://waterdata.usgs.gov/monitoring-location/11119750/#dataTypeId=continuous-00065-0&period=P7D&showMedian=false", "Link")),
        p("Texas Parks and Wildlife. (n.d.). River safety. Texas Parks and Wildlife. 
     ", tags$a(href = "https://tpwd.texas.gov/landwater/water/habitats/rivers/safety.phtml", "Link")),
        p("Additional Sources:"),
        p(tags$a(href = "https://en.wikipedia.org/wiki/Santa_Maria_River_(California)", "Santa Maria River (California)")),
        p(tags$a(href = "https://en.wikipedia.org/wiki/Sisquoc_River", "Sisquoc River")),
        p(tags$a(href = "https://en.wikipedia.org/wiki/Santa_Ynez_River", "Santa Ynez River")),
        p(tags$a(href = "https://en.wikipedia.org/wiki/Santa_Ynez_River", "Santa Ynez River")),
        p(tags$a(href = "https://en.wikipedia.org/wiki/Santa_Ynez_River", "Santa Ynez River")),
        p(tags$a(href = "https://en.wikipedia.org/wiki/Santa_Ynez_River", "Santa Ynez River")),
        p(tags$a(href = "https://en.wikipedia.org/wiki/Carpinteria_Creek", "Carpinteria Creek")),
        p(tags$a(href = "https://sbparksandrec.santabarbaraca.gov/creeks/mission-creek", "Mission Creek")),
        p(tags$a(href = "https://en.wikipedia.org/wiki/Sisquoc_River", "Sisquoc River")),
        p(tags$a(href = "https://en.wikipedia.org/wiki/Cuyama_River", "Cuyama River"))
      )
      
    )
  })
  
  
  
  output$gallery_content <- renderUI({
    tagList(
      h3("Swimming Hole Hall of Fame"),
      
      # Image for Montecito Hot Springs
      div(
        tags$h4("Montecito Hot Springs"),
        tags$p("Montecito Hot Springs is a popular natural hot spring located in the Hills of Montecito. The hike to this location is 2.5 miles straight up into the hills. 
               This hike provides great views of the back yards of various rich homeowners. This is a very popular swimming spot especially 
               on the weekends. For the best experience go to the hot springs on a weekday morning!"),
        tags$img(src = "Montecito_hot_springs.jpg", height = "600px", width = "auto"),
        p(a("Source", href = "https://www.findinghotsprings.com/mag/montecito-hot-springs", target = "_blank"))
      ),
      
      
      # Image Red Rock
      div(
        tags$h4("Red Rock Swimming Hole"),
        tags$p("The red rock trail is a 1 mile out and back trail that will take you to one of the most popular swimming spots for Santa Barbara locals.
        There are many swimming holes along the trail but get here early to secure parking. There are toilets at the start of the hike. Make sure to bring plenty of water. 
      "),
        tags$img(src = "RedRock.jpg", height = "400px", width = "auto"),
        p(a("Source", href = "https://www.fs.usda.gov/recarea/lpnf/recreation/recarea/?recid=11093&actid=50", target = "_blank"))
      ),
      
      # Image for Sisquoc River
      div(
        tags$h4("Sisquoc River"),
        tags$p("The Sisquoc River is a beautiful pristine river that is perfect for dipping your toes during a hike in the Sierra Madre Mountains."
      ),
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



