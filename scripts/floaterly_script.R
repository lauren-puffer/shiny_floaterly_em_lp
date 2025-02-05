#Ella Moore & Lauren Puffer
#Shiny App Layout

#load packages  
library(shiny)
library(here)
library(tidyverse)

#create the user interface
ui<- fluidPage(
  titlePanel("Floaterly"),
  sidebarLayout(
    sidebarPanel('put my widgets here',
                 radioButtons(
                   inputId= 'hydrologic_data',
                   label = "Choose body of water",
                   choices = c('Lake','Creek','River!'='Water Body')
                 )),
    mainPanel('put my map here'),
  )
)

#create the server function
server <- function(input, output) {
  water_select <- recative({
    water_df <- warer |>
      filter(water == input$water_body)
  })
}

#combine into the app
shinyApp(ui=ui, server=server)

