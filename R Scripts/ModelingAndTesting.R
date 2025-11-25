# Modeling And Testing

library(tidyverse)
library(broom)
library(caret)      
library(cluster)    
library(factoextra) 
library(ggplot2)

setwd("C:/Users/kingi/Documents/school/Data Analytics/assignment4")
dir.create("results", showWarnings = FALSE)
rb_data <- read_csv("data/rb_data.csv", show_col_types = FALSE)

# Ensure all necessary columns are numeric
numeric_cols <- c("Scrimmage_Touch", "Scrimmage_YScm", "Scrimmage_Y.Tch",
                  "touches_per_game", "yards_per_game", "Age")
for (col in numeric_cols) if (col %in% names(rb_data)) rb_data[[col]] <- as.numeric(rb_data[[col]])

# Recreate workload groups used in the exploratory section
median_touch <- median(rb_data$Scrimmage_Touch, na.rm = TRUE)
rb_data <- rb_data %>%
  mutate(
    workload_median_group = ifelse(Scrimmage_Touch > median_touch, "Above Median", "Below Median"),
    workload_fixed_group = ifelse(Scrimmage_Touch > 300, "High", "Low")
  )

# Save dataset for modeling
model_data <- rb_data 


# Hypothesis Testing: Welch's t-test 

# Split groups by median
median_high <- model_data %>% filter(workload_median_group == "Above Median") %>% pull(Scrimmage_Y.Tch)
median_low <- model_data %>% filter(workload_median_group == "Below Median") %>% pull(Scrimmage_Y.Tch)

t_median <- t.test(median_high, median_low, var.equal = FALSE)
print(tidy(t_median))

# 300 touch fixed threshold
fixed_high <- model_data %>% filter(workload_fixed_group == "High") %>% pull(Scrimmage_Y.Tch)
fixed_low  <- model_data %>% filter(workload_fixed_group == "Low")  %>% pull(Scrimmage_Y.Tch)

t_fixed <- t.test(fixed_high, fixed_low, var.equal = FALSE)
print(tidy(t_fixed))



# Predictive Modeling: Linear Regression 

# Model 1: Simple linear model
lin.mod1 <- lm(Scrimmage_Y.Tch ~ Scrimmage_Touch, model_data)
summary(lin.mod1)

# Scatter + regression line
ggplot(model_data, aes(x = Scrimmage_Touch, y = Scrimmage_Y.Tch)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  labs(title = "Model 1: Yards/Touch vs Total Touches",
       x = "Total Scrimmage Touches", y = "Yards per Touch")

# Residuals vs Fitted
ggplot(data.frame(fitted = fitted(lin.mod1), resid = resid(lin.mod1)),
       aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0) +
  labs(title = "Model 1: Residuals vs Fitted", x = "Fitted Values", y = "Residuals")


# Model 2: Multiple linear model 
lin.mod2 <- lm(Scrimmage_Y.Tch ~ Scrimmage_Touch + Age + yards_per_game + touches_per_game, model_data)
summary(lin.mod2)

# Scatter + regression line (main predictor = Scrimmage_Touch)
ggplot(model_data, aes(x = Scrimmage_Touch, y = Scrimmage_Y.Tch)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  labs(title = "Model 2: Multiple Linear Regression",
       x = "Total Scrimmage Touches", y = "Yards per Touch")

# Residuals vs Fitted
ggplot(data.frame(fitted = fitted(lin.mod2), resid = resid(lin.mod2)),
       aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0) +
  labs(title = "Model 2: Residuals vs Fitted", x = "Fitted Values", y = "Residuals")


# Model 3: Alternative model 
lin.mod3 <- lm(Scrimmage_Y.Tch ~ Age + yards_per_game, model_data)
summary(lin.mod3)

# Scatter plot (main predictor = Age)
ggplot(model_data, aes(x = Age, y = Scrimmage_Y.Tch)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  labs(title = "Model 3: Yards/Touch ~ Age + Yards/Game",
       x = "Age", y = "Yards per Touch")

# Residuals vs Fitted
ggplot(data.frame(fitted = fitted(lin.mod3), resid = resid(lin.mod3)),
       aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0) +
  labs(title = "Model 3: Residuals vs Fitted", x = "Fitted Values", y = "Residuals")


# K-means clustering 

# Features for clustering
cluster_feats <- c("Scrimmage_Touch", "Scrimmage_Y.Tch", "yards_per_game")
cluster_df <- model_data %>% select(all_of(cluster_feats)) %>% drop_na()
cluster_scaled <- scale(cluster_df)

# Range of k values to try
k.list <- 2:8

# Compute WCSS and silhouette widths for each k
wcss.list <- c()
sil.list  <- c()

for (k in k.list) {
  km <- kmeans(cluster_scaled, centers = k, nstart = 25)
  wcss.list <- c(wcss.list, km$tot.withinss)
  si <- silhouette(km$cluster, dist(cluster_scaled))
  sil.list <- c(sil.list, mean(si[, 3]))
  cat(sprintf("k = %d, WCSS = %.2f, Avg Silhouette = %.4f\n", k, km$tot.withinss, mean(si[,3])))
}

# Elbow plot
elbow_df <- data.frame(k = k.list, WCSS = wcss.list)
ggplot(elbow_df, aes(x = k, y = WCSS)) +
  geom_line() + geom_point() +
  labs(title = "Elbow Method for K-means", x = "Number of Clusters (k)", y = "Total Within-Cluster SS")

# Silhouette width plot
sil_df <- data.frame(k = k.list, Silhouette = sil.list)
ggplot(sil_df, aes(x = k, y = Silhouette)) +
  geom_line() + geom_point() +
  labs(title = "Average Silhouette Width by k", x = "Number of Clusters (k)", y = "Avg Silhouette Width")

# Select k with highest silhouette
best.k <- k.list[which.max(sil.list)]
cat(sprintf("Best k by silhouette width: %d\n", best.k))

# Fit final K-means model
set.seed(42)
kmeans_final <- kmeans(cluster_scaled, centers = best.k, nstart = 50)

# Attach cluster labels
clustered <- cluster_df %>%
  mutate(cluster = factor(kmeans_final$cluster))

# 2D scatter of two main features
ggplot(clustered, aes(x = Scrimmage_Touch, y = Scrimmage_Y.Tch, color = cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = paste("K-means Clusters (k =", best.k, ")"),
       x = "Scrimmage Touches", y = "Yards per Touch") +
  theme_minimal()

# Silhouette plot for final clusters
si.final <- silhouette(kmeans_final$cluster, dist(cluster_scaled))
fviz_silhouette(si.final)

