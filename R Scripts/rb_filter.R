# Raw Data combining and filtering

library(tidyverse)

# Set working directory 
setwd("C:/Users/kingi/Documents/school/Data Analytics/assignment4/data")

# List all scrimmage CSV files 
file_list <- list.files(pattern = "scrimmage_.*\\.csv$")

# Read all files and add Season column 
raw_list <- lapply(file_list, function(f) {
  
  # Extract season from filename
  season <- as.numeric(str_extract(f, "\\d+"))
  
  # Read CSV without header
  df <- read_csv(f, col_names = FALSE, show_col_types = FALSE)
  
  # Row 1 = category headers, Row 2 = variable names
  category_row <- as.character(df[1, ])
  var_row      <- as.character(df[2, ])
  
  # Combine to make names like Receiving_Yds, Rushing_Att, etc.
  new_names <- ifelse(category_row == "" | is.na(category_row),
                      var_row,
                      paste0(category_row, "_", var_row))

  new_names <- make.names(new_names, unique = TRUE)
  colnames(df) <- new_names
  
  # Drop the first two rows (category + header)
  df <- df[-c(1,2), ]
  
  # Add Season column
  df$Season <- season
  
  df
})

# Combine all years into one dataframe 
scrimmage_all <- bind_rows(raw_list)

# Convert key numeric columns
numeric_cols <- c("G", "GS", "Scrimmage_Touch", "Scrimmage_YScm",
                  "Receiving_Yds", "Receiving_TD", "Rushing_Yds", "Rushing_TD",
                  "Rk")

numeric_cols <- numeric_cols[numeric_cols %in% names(scrimmage_all)] # only existing columns
scrimmage_all[numeric_cols] <- lapply(scrimmage_all[numeric_cols], function(x) as.numeric(as.character(x)))

# Filter RBs with ≥50 touches
rb_data <- scrimmage_all %>%
  filter(Pos == "RB", Scrimmage_Touch >= 50) %>%
  mutate(
    touches_per_game = Scrimmage_Touch / G,
    yards_per_game = Scrimmage_YScm / G
  )

# Rename original Rk to season_rank 
if("Rk" %in% names(rb_data)){
  rb_data <- rb_data %>% rename(Season_Rank = Rk)
}

# Create total_rank for total yds across all seasons
rb_data <- rb_data %>%
  arrange(desc(Scrimmage_YScm)) %>%
  mutate(Total_Rank = row_number())

# Export cleaned dataset 
write_csv(rb_data, "rb_data.csv")
