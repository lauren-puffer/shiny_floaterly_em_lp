#Ella Moore & Lauren Puffer
#ESM 244 - Winter 2025

#Shiny App Layout

#load packages  
library(shiny)
library(here)
library(tidyverse)
library(magrittr)
library(xml2)

##Scrape weather data

library(httr)
library(rvest)
library(officer)
library(stringr)
library(bslib)

# URL to scrape
url <- "https://forecast.weather.gov/MapClick.php?lat=34.4262&lon=-119.8415"


# Scrape the HTML content of the page
page <- read_html(url)

# Convert the HTML content to text
html_content <- as.character(page)

# Create a new Word document
doc <- read_docx()

# Add the scraped HTML content as a paragraph
doc <- doc %>%
  body_add_par(html_content, style = "Normal")

# Save the Word document
output_path <- "weather_html_content.docx"
print(doc, target = output_path)

# Confirm the output
cat("The HTML content has been saved to", output_path)




##Scrape for flow data
# URL to scrape
url <- "https://rain.cosbpw.net/site/?site_id=20&site=08f3908e-b09f-4bbe-85c9-702af65fa1e4"

# Read the HTML from the URL
webpage <- read_html(url)

# Find the text "Flow Volume" and extract the number in the next <h3> tag
flow_volume_text <- webpage %>%
  html_nodes("body") %>%    # Extract all body content
  html_text() %>%           # Convert to text
  str_split("\n") %>%       # Split by new line
  unlist() %>%              # Flatten to a vector of text
  grep("Flow Volume", ., value = TRUE)  # Find the line with "Flow Volume"

# If "Flow Volume" is found, extract the next <h3> number
if (length(flow_volume_text) > 0) {
  # Extract the number within the next <h3> element after "Flow Volume"
  number_after_h3 <- webpage %>%
    html_nodes("div.h3") %>%   # Target <div class="h3">
    html_text() %>%            # Get the text content of the <h3> element
    .[1]                       # Get the first occurrence
  
  cat("Flow Volume Number:", number_after_h3, "\n")
} else {
  cat("Flow Volume text not found.\n")
}


##Stage scrape
# URL to scrape
url <- "https://rain.cosbpw.net/site/?site_id=20&site=08f3908e-b09f-4bbe-85c9-702af65fa1e4"

# Read the HTML from the URL
webpage <- read_html(url)

# Find the text "Stage" and extract the number in the next <h3> tag
stage_text <- webpage %>%
  html_nodes("body") %>%    # Extract all body content
  html_text() %>%           # Convert to text
  str_split("\n") %>%       # Split by new line
  unlist() %>%              # Flatten to a vector of text
  grep("Stage", ., value = TRUE)  # Find the line with "Stage"

# If "Stage" is found, extract the next <h3> number
if (length(stage_text) > 0) {
  # Extract the number within the next <h3> element after "Stage"
  number_after_h3_stage <- webpage %>%
    html_nodes("div.h3") %>%   # Target <div class="h3">
    html_text() %>%            # Get the text content of the <h3> element
    .[2]                       # Get the second occurrence (next after "Flow Volume")
  
  cat("Stage Number:", number_after_h3_stage, "\n")
} else {
  cat("Stage text not found.\n")
}
  

hydrologic_data <- #data frame of useful scraped data




#National weather service scraping 
# URL of the weather forecast
url <- "https://forecast.weather.gov/MapClick.php?lat=34.4262&lon=-119.8415"

# Read the HTML from the URL
page <- read_html(url)

# Scrape all the 'td' elements which contain the weather data
weather_data <- page %>%
  html_nodes("td") %>%
  html_text()

# Find the index of the first occurrence of "Humidity"
humidity_index <- grep("Humidity", weather_data)

# Extract the percentage value (next element after "Humidity")
humidity_percentage <- gsub("[^0-9%]", "", weather_data[humidity_index + 1])

# Find the index of the first occurrence of "Wind Speed"
wind_speed_index <- grep("Wind Speed", weather_data)

# Extract the wind speed value (next element after "Wind Speed")
wind_speed <- weather_data[wind_speed_index + 1]

# Clean up the wind speed text (optional)
wind_speed <- gsub("\\s+", " ", wind_speed)  # Replace multiple spaces with a single space

# Scrape for weather condition (e.g., "Overcast") and temperature (e.g., "57°F")
weather_condition <- page %>%
  html_nodes(".myforecast-current") %>%
  html_text() %>%
  .[1]  # Get the first instance for the weather condition

temperature <- page %>%
  html_nodes(".myforecast-current-lrg") %>%
  html_text() %>%
  .[1]  # Get the first instance for the temperature

# Print the results
print(paste("The humidity is:", humidity_percentage))
print(paste("The wind speed is:", wind_speed))
print(paste("The current weather is:", weather_condition))
print(paste("The temperature is:", temperature))





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
        inputId = 'hydrologic_data',
        label = "Choose body of water",
        choices = c('Montecito Creek',
                    'Santa Ynez River at Lake Cachuma',
                    'Santa Ynez River at Lompoc',
                    'Husana River at Arroyo Grande',
                    'Zaca Creek at Buellton',
                    'Mission Creek',
                    'Los Laureles Creek',
                    'Carpinteria Creek')
      
      ),
      checkboxGroupInput(
        inputId = 'hydrologic_data',
        label = "Choose metric",
        choices = c('Weather',
                    'Nitrate concentration',
                    'Flows',
                    'Depth')
      ),
      actionButton("help", "Help"),
      actionButton("Go", "Get Floatin'"),
      

    ),
    mainPanel(
      uiOutput("imageUI"),  # Placeholder for dynamic image
      textOutput("Risk Level"),
      textOutput("Weather"),
      textOutput("Nitrate Concentration")
    )
  )
)


server <- function(input, output, session) {
  
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
  
  # Create a place for input nitrate concentration data to be displayed
  output$nitrate <- renderText({
    nitrate_df <- nitrate |>
      filter(water == input$water_body)
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
}

# combine into the app
shinyApp(ui = ui, server = server)




#scrape template
url <- "https://www.nationsonline.org/oneworld/country_code_list.htm"
hydro_data <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="CountryCode"]') %>%
  html_table()

