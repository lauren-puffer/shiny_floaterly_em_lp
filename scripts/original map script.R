# Reactive expression to render the map
output$map_output <- renderLeaflet({
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
})



#OG scraping data for weather


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