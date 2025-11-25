# PFF Data Scraping

library(tidyverse)
library(rvest)

# Set working directory
setwd("C:/Users/kingi/Documents/school/Data Analytics/assignment4")

# Create data folder
if(!dir.exists("data")) dir.create("data")

# Define the range of years
years <- 1996:2024

# Loop through each year
for (y in years) {
  # Build the URL for each season
  url <- paste0("https://www.pro-football-reference.com/years/", y, "/scrimmage.htm")
  
  cat("Scraping year:", y, "...\n")
  
  # Try to download and parse the table
  tryCatch({
    page <- read_html(url)
    
    # Extract the scrimmage table
    scrimmage <- page %>%
      html_element("table#scrimmage") %>%
      html_table()
    
    # Save as CSV inside 'data' folder
    file_path <- paste0("data/scrimmage_", y, ".csv")
    write_csv(scrimmage, file_path)
    
    cat("Saved:", file_path, "\n")
    
    Sys.sleep(2)
    
  }, error = function(e) {
    cat("Failed to scrape year", y, ":", e$message, "\n")
  })
}


