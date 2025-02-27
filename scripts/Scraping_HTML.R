
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

