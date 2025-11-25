# Preliminary Analysis

library(tidyverse)

# Load cleaned dataset
setwd("C:/Users/kingi/Documents/school/Data Analytics/assignment4")
rb_data <- read_csv("data/rb_data.csv", show_col_types = FALSE)

# Summary statistics 
summary_stats <- rb_data %>%
  select(Scrimmage_Touch, Scrimmage_YScm, Scrimmage_Y.Tch, touches_per_game, yards_per_game) %>%
  summary()

print(summary_stats)

# Histograms 

# Total touches (workload)
ggplot(rb_data, aes(x = Scrimmage_Touch)) +
  geom_histogram(binwidth = 25, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Total Touches", x = "Total Touches", y = "Count") +
  theme_minimal()

# Efficiency (yards per touch)
ggplot(rb_data, aes(x = Scrimmage_Y.Tch)) +
  geom_histogram(binwidth = 0.4, fill = "darkgreen", color = "black") +
  labs(title = "Distribution of Yards per Touch", x = "Yards per Touch", y = "Count") +
  theme_minimal()

# Yards per game
ggplot(rb_data, aes(x = yards_per_game)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") +
  labs(title = "Distribution of Yards per Game", x = "Yards per Game", y = "Count") +
  theme_minimal()

# Scatter plots 

# Efficiency vs. workload
ggplot(rb_data, aes(x = Scrimmage_Touch, y = Scrimmage_Y.Tch)) +
  geom_point(alpha = 0.5, color = "darkred") +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  labs(title = "RB Efficiency vs. Workload", x = "Total Touches", y = "Yards per Touch") +
  theme_minimal()

# Total yards vs. workload
ggplot(rb_data, aes(x = Scrimmage_Touch, y = Scrimmage_YScm)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  labs(title = "Total Yards vs. Workload", x = "Total Touches", y = "Total Yards") +
  theme_minimal()

# Correlations 
touch_vs_eff <- cor(as.numeric(rb_data$Scrimmage_Touch), as.numeric(rb_data$Scrimmage_Y.Tch))
touch_vs_ypg <- cor(as.numeric(rb_data$Scrimmage_Touch), as.numeric(rb_data$yards_per_game))

cat("Correlation between Total Touches and Yards per Touch:", round(touch_vs_eff, 3), "\n")
cat("Correlation between Total Touches and Yards per Game:", round(touch_vs_ypg, 3), "\n")

