#Ella Moore & Lauren Puffer
#ESM 244 - Winter 2025

#Shiny App Layout

#load packages  
library(shiny)
library(here)
library(tidyverse)
library(magrittr)
library(xml2)
library(leaflet)
library(readr)
library(httr)
library(rvest)
library(officer)
library(stringr)
library(bslib)
library(janitor)

##Add trial scraping data


# Input CSV with creek names and nodes for scraping
creek_data <- read.csv(here("scraping scripts", "Floaterly Creek ID Spreadsheet - Sheet1.csv")) %>%
  clean_names() |>
  rename(creek_node = url)

# Add a new column for the creek node (strip the part of the URL)
creek_data <- creek_data |>
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
  
  # Return both Flow Volume and Stage
  return(list(flow_volume = flow_volume, stage = stage))
}
  
  
#Make map for main panel

leaflet(data = creek_data) %>%
  addTiles() %>%
  addMarkers(
    lng = ~long, lat = ~lat, 
    popup = ~paste("Lat: ", lat, "<br>Long: ", long),  # Popup content
    label = ~common_name,  # Display the Common Name as a label
    labelOptions = labelOptions(
      noHide = FALSE,   # Keep label visible even when not hovering
      direction = "top",  # Position label above the marker
      style = list(
        "font-weight" = "bold",  # Bold text
        "font-size" = "14px",    # Font size
        "color" = "black"        # Label text color
      )
    )
  ) %>%
  fitBounds(
    lng1 = min(creek_data$long),  # Minimum longitude
    lat1 = min(creek_data$lat),   # Minimum latitude
    lng2 = max(creek_data$long),  # Maximum longitude
    lat2 = max(creek_data$lat)    # Maximum latitude
  )




##Scaffolding

#create a ui that has a dropdown menu for water body, 
#a text output for risk level, and a text output for weather and nitrate concentration data

ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "cerulean"),  
  titlePanel("Floaterly"),
  sidebarLayout(
    sidebarPanel(
      'Tell us about your float!',
      selectInput(
        selectInput("creek", "Choose a body of water", choices = creek_data$common_name),
        actionButton("goButton", "Get Floatin")
      ))
      
      ),
      checkboxGroupInput(
        inputId = 'hydrologic_data',
        label = "What do you want in your swim report",
        choices = c('Weather',
                    'Flows', 
                    'Swim Recomendation')
      ),
      actionButton("help_button", "Help"),
      actionButton("scrape_btn", "Get Floatin'"),
    mainPanel(
      verbatimTextOutput("creekData"),
      # Output to display the leaflet map
      leafletOutput("map_output")
    )
  )



server <- function(input, output, session) {
  
  weather_data_reactive <- eventReactive(input$scrape_btn, {
    # URL of the weather forecast
    url <- "https://forecast.weather.gov/MapClick.php?lat=34.4262&lon=-119.8415"
    
    # Read the HTML from the URL
    page <- read_html(url)
    
    #scrape all the 'td' elements which contain the weather data
    weather_data <- page |>
      html_nodes("td") |>
      html_text()
    
    #find the index of the first occurrence of "Humidity"
    humidity_index <- grep("Humidity", weather_data)
    humidity_percentage <- gsub("[^0-9%]", "", weather_data[humidity_index + 1])
    
    #find the index of the first occurrence of "Wind Speed"
    wind_speed_index <- grep("Wind Speed", weather_data)
    wind_speed <- gsub("\\s+", " ", weather_data[wind_speed_index + 1]) 
    
    #scrape for weather condition 
    weather_condition <- page |>
      html_nodes(".myforecast-current") |>
      html_text() |>
      .[1]  # Get the first instance for the weather condition
    
    temperature <- page |>
      html_nodes(".myforecast-current-lrg") |>
      html_text() |>
      .[1]  # Get the first instance for the temperature
    
    # Return scraped data as a list
    list(
      humidity = humidity_percentage,
      wind_speed = wind_speed,
      weather_condition = weather_condition,
      temperature = temperature
    )
  })
  
  # Display the scraped data when the button is pressed
  output$humidity <- renderText({
    weather <- weather_data_reactive()
    paste("Humidity:", weather$humidity)
  })
  
  output$wind_speed <- renderText({
    weather <- weather_data_reactive()
    paste("Wind Speed:", weather$wind_speed)
  })
  
  output$weather_condition <- renderText({
    weather <- weather_data_reactive()
    paste("Current Weather:", weather$weather_condition)
  })
  
  output$temperature <- renderText({
    weather <- weather_data_reactive()
    paste("Temperature:", weather$temperature)
  })
 
  

  #scrape data from a url and use html_node found in csv file
  
  #create interactive map showing the sites of each creek
  output$map_output <- renderLeaflet({
    leaflet(data = creek_data) |>
      addTiles() |>
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
  
  
  # Reactive function to filter water data
  water_select <- reactive({
    water_df <- hydrologic_data |>
      filter(water == input$water_body)
  })
  
  # Create a place for input weather data to be displayed in the app
  output$weather <- renderText({
    weather_df <- weather |>
      filter(water == input$water_body)
  })
  
  })
  
  # Use inputs of flow and stage for risk level output
  output$risk_level <- renderText({
    if (input$flow < 500 && input$stage < 5) {
      "Low Risk"
    } else if (input$flow >= 500 && input$stage < 5) {
      "Medium Risk"
    } else if (input$flow < 800 && input$stage >= 5) {
      "Medium Risk"
    } else {
      "High Risk"
    }
  })
  
  # Initially show a placeholder image
  output$imageUI <- renderUI({
    tags$img(src = "CoverImage.jpg", height = "300px")  # Change path and image as needed
  })
  
  # Change image when "Go" button is pressed
  observeEvent(input$Go, {
    output$imageUI <- renderUI({
      tags$img(src = "Error.jpg", height = "300px")  # Change path and image as needed
    })
  })
  
#Add content for when the user presses the help button
observeEvent(input$help_button, {
  output$help_content <- renderUI({
    tagList(
      h3("How to Floaterly"),
      p("Follow the steps below to get started:"),
      ul(
        li(
          p("Step 1: Click the 'Start' button to begin.Select a stream: Use the drop down menu to select a stream that you are considering swimming in. This list includes all streams in Santa Barbara county that have 
             hydrologic data that is updated automatically by USGS. Each stream may have multiple locations so use the map on the main panel to decide whihc location you are interested
             in."),
          img(src = "coverImage.jpg", height = "200px"), #Add screen shot of finished app
          p("Drop down menu for electing your stream of interest")
        ),
        li(
          p("Select your swim report: QUse the check boxes to decide what you want included in your swim report. You can include weather, stream flow, water height, and a recommendation if you 
            suffer from indecision."),
          img(src = "coverImage.jpg", height = "200px"), #Add screen shot of finished app
          p("Selection check boxes to specify what you want included in your swim report.")
        ),
        li(
          p("Go Button: Press this button when you are ready to see your swim report. Pressing this button should populate the swim report into the main pannel."),
          img(src = "coverImage.jpg", height = "200px"), #Add screen shot of finished app
          p("Press the Go button to check out your swim report.")
        )
      )
    )
  })
})

}

# combine into the app
shinyApp(ui = ui, server = server)



#scrape template
url <- "https://www.nationsonline.org/oneworld/country_code_list.htm"
hydro_data <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="CountryCode"]') %>%
  html_table()

