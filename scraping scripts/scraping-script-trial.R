#Scraping using a CSV file with nodes 
# Lauren Puffer
#3/1/25

library(shiny)
library(rvest)
library(readr)  # For reading CSV files

library(stringr) #for altering observations in csv


#Input CSV with creek names and nodes for scraping
#This needs to have just the nodes! not the entire URL
creek_data <- read.csv(here("scraping scripts","Floaterly Creek ID Spreadsheet - Sheet1.csv")) %>%
  clean_names()

creek_data |>
  mutate(creek_node = str_remove_all(url, "https://rain.cosbpw.net/site/?site_id="))


# Function to scrape data based on creek node
scrape_creek_data <- function(creek_node) {
  url <- paste0("https://rain.cosbpw.net/site/?site_id=", creek_node)  # Adjust base URL
  page <- read_html(url)
  data <- page %>%
    html_nodes(".creek-data") %>%
    html_text()
  return(data)
}


# Define the UI
ui <- fluidPage(
  titlePanel("Floaterly"),
  
  sidebarLayout(
    sidebarPanel(
      #Dynamically populate the dropdown based on creek names
      selectInput("creek", "Choose a body of water", choices = creek_data$common_name),
      actionButton("goButton", "Get Floatin")
    ),
    
    mainPanel(
      verbatimTextOutput("creekData")
    )
  )
)


server <- function(input, output) {
  
  # Get the selected creek's node based on the selected creek name
  selected_creek_node <- reactive({
    selected_creek <- input$creek
    # Get the node value (or full URL, depending on your CSV structure)
    creek_node <- creek_data$creek_node[creek_data$creek_name == selected_creek]
    return(creek_node)
  })
  
  # Reactive expression to fetch creek data when the button is clicked
  creek_data_reactive <- eventReactive(input$goButton, {
    # Get the selected creek's node
    creek_node <- selected_creek_node()
    
    # Scrape the data
    data <- scrape_creek_data(creek_node)
    
    return(data)
  })
  # Render the scraped data
  output$creekData <- renderText({
    creek_data_reactive()
  })
}

shinyApp(ui = ui, server = server)

  