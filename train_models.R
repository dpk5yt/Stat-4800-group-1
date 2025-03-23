
library(dplyr)
library(readr)
library(mclust)  
library(ggplot2)

fg_data <- read_csv("fg_success_rates.csv")
yard_data <- read_csv("yard_distribution.csv")

# Logistic Regression for FG 
fg_data <- fg_data %>%
  mutate(success = ifelse(fg_success_rate > 0, 1, 0))

fg_model <- glm(success ~ kick_distance, data = fg_data, family = binomial)

saveRDS(fg_model, "fg_model.rds")


yard_data <- read_csv("yard_distribution.csv")

# Remove missing values 
yard_data <- na.omit(yard_data)

# Extract play-type-specific yardage
pass_yards <- yard_data %>% filter(play_type == "pass") %>% pull(mean_yards)
run_yards <- yard_data %>% filter(play_type == "run") %>% pull(mean_yards)

# Ensure numeric
pass_yards <- as.numeric(na.omit(pass_yards))
run_yards <- as.numeric(na.omit(run_yards))

# Add slight noise for singularity
if (length(unique(pass_yards)) == 1) {
  pass_yards <- pass_yards + rnorm(length(pass_yards), mean = 0, sd = 0.01)
}
if (length(unique(run_yards)) == 1) {
  run_yards <- run_yards + rnorm(length(run_yards), mean = 0, sd = 0.01)
}

if (length(pass_yards) > 10) {
  pass_gmm <- Mclust(pass_yards, G = 2)  # Two-component mixture
} else {
  pass_gmm <- Mclust(pass_yards)  # Let it auto-select G
}

if (length(run_yards) > 10) {
  run_gmm <- Mclust(run_yards, G = 2)
} else {
  run_gmm <- Mclust(run_yards)
}

# Save models
saveRDS(pass_gmm, "pass_gmm.rds")
saveRDS(run_gmm, "run_gmm.rds")


