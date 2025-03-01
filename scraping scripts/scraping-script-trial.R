#Scraping using a CSV file with nodes 
# Lauren Puffer
#3/1/25
library(shiny)
library(rvest)
library(readr)  # For reading CSV files
library(stringr) # for altering observations in csv
library(here)
library(janitor)
library(tidyverse)

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

# Define the UI
ui <- fluidPage(
  titlePanel("Floaterly"),
  
  sidebarLayout(
    sidebarPanel(
      # Dynamically populate the dropdown based on creek names
      selectInput("creek", "Choose a body of water", choices = creek_data$common_name),
      actionButton("goButton", "Get Floatin")
    ),
    
    mainPanel(
      # Output text to display scraped data
      verbatimTextOutput("creekData")
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
  
  # Reactive expression to fetch creek data when the button is clicked
  creek_data_reactive <- eventReactive(input$goButton, {
    # Get the selected creek's node
    creek_node <- selected_creek_node()
    
    # Scrape the data (flow volume and stage)
    data <- scrape_creek_data(creek_node)
    
    # Return the scraped data as a formatted text
    formatted_data <- paste(
      "Flow Volume: ", data$flow_volume, "\n",
      "Stage: ", data$stage
    )
    
    return(formatted_data)
  })
  
  # Render the scraped data
  output$creekData <- renderText({
    creek_data_reactive()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

  