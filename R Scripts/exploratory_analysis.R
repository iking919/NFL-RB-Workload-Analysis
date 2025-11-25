# Exploratory Analysis

library(tidyverse)
library(lubridate)
library(GGally)

# Load cleaned dataset 
setwd("C:/Users/kingi/Documents/school/Data Analytics/assignment4")
rb_data <- read_csv("data/rb_data.csv", show_col_types = FALSE)

# Data cleaning
# Ensure numeric columns are numeric
numeric_cols <- c("Scrimmage_Touch", "Scrimmage_YScm", "Scrimmage_Y.Tch",
                  "touches_per_game", "yards_per_game")
rb_data[numeric_cols] <- lapply(rb_data[numeric_cols], as.numeric)

# Create workload groups for exploratory comparison
median_touch <- median(rb_data$Scrimmage_Touch)
rb_data <- rb_data %>%
  mutate(
    workload_median_group = ifelse(Scrimmage_Touch > median_touch, "Above Median", "Below Median"),
    workload_fixed_group = ifelse(Scrimmage_Touch > 300, "High", "Low")
  )


# Get Average efficiency per season
season_summary <- rb_data %>%
  group_by(Season) %>%
  summarize(
    avg_yards_per_touch = mean(Scrimmage_Y.Tch),
    avg_yards_per_game = mean(yards_per_game),
    avg_total_touches = mean(Scrimmage_Touch),
    n_rb = n()
  )

# Line plots: Trends over seasons
ggplot(season_summary, aes(x = Season, y = avg_yards_per_touch)) +
  geom_line(color = "darkgreen") +
  geom_point() +
  labs(title = "Average Yards per Touch by Season",
       x = "Season",
       y = "Average Yards per Touch") +
  theme_minimal()

ggplot(season_summary, aes(x = Season, y = avg_total_touches)) +
  geom_line(color = "steelblue") +
  geom_point() +
  labs(title = "Average Total Touches by Season",
       x = "Season",
       y = "Average Total Touches") +
  theme_minimal()


# Workload group comparisons 

# Boxplots
ggplot(rb_data, aes(x = workload_median_group, y = Scrimmage_Y.Tch, fill = workload_median_group)) +
  geom_boxplot() +
  labs(title = "Efficiency by Workload Group (Median Threshold)",
       x = "Workload Group",
       y = "Yards per Touch") +
  scale_fill_manual(values = c("Above Median" = "red", "Below Median" = "green")) +
  theme_minimal()

ggplot(rb_data, aes(x = workload_fixed_group, y = Scrimmage_Y.Tch, fill = workload_fixed_group)) +
  geom_boxplot() +
  labs(title = "Efficiency by Workload Group (300 Touches)",
       x = "Workload Group",
       y = "Yards per Touch") +
  scale_fill_manual(values = c("High" = "red", "Low" = "green")) +
  theme_minimal()

#Density Plots
ggplot(rb_data, aes(x = Scrimmage_Y.Tch, fill = workload_median_group)) +
  geom_density(alpha = 0.4) +
  labs(title = "Density of Yards per Touch by Workload (Median)",
       x = "Yards per Touch", y = "Density") +
  theme_minimal()

ggplot(rb_data, aes(x = Scrimmage_Y.Tch, fill = workload_fixed_group)) +
  geom_density(alpha = 0.4) +
  labs(title = "Density of Yards per Touch by Workload (Fixed)",
       x = "Yards per Touch", y = "Density") +
  theme_minimal()

#Histograms
ggplot(rb_data, aes(x = Scrimmage_Touch, fill = workload_median_group)) +
  geom_histogram(binwidth = 25, alpha = 0.6, position = "identity") +
  labs(title = "Distribution of Total Touches by Workload Group (Median)",
       x = "Total Touches", y = "Count") +
  theme_minimal()

#Histograms
ggplot(rb_data, aes(x = Scrimmage_Touch, fill = workload_fixed_group)) +
  geom_histogram(binwidth = 25, alpha = 0.6, position = "identity") +
  labs(title = "Distribution of Total Touches by Workload Group (Fixed)",
       x = "Total Touches", y = "Count") +
  theme_minimal()


# Correlations and pairwise exploration
cor_matrix <- rb_data %>%
  select(Scrimmage_Touch, Scrimmage_Y.Tch, yards_per_game, Scrimmage_YScm) %>%
  cor(use = "complete.obs")

print(cor_matrix)

# Heatmap
ggpairs(rb_data %>% select(Scrimmage_Touch, Scrimmage_YScm, Scrimmage_Y.Tch, yards_per_game))

# Identify potential outliers or unusual seasons
high_touch <- rb_data %>% filter(Scrimmage_Touch > 400)
cat("RBs with >400 touches:\n")
print(high_touch %>% select(Player, Season, Scrimmage_Touch, Scrimmage_YScm, Scrimmage_Y.Tch))

high_touch <- rb_data %>% filter(Scrimmage_Y.Tch > 8.0)
cat("RBs with >8.0 yards/touch:\n")
print(high_touch %>% select(Player, Season, Scrimmage_Touch, Scrimmage_YScm, Scrimmage_Y.Tch))


