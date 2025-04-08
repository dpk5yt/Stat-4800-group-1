
library(dplyr)
library(ggplot2)

source("get_EP.R")   

# Load the dataset
pbp_data <- readRDS("pbp2014-2024.rds")  # Update filename if different!

# Make get_EP work on vectors
get_EP <- Vectorize(get_EP)

# ---- Filter Only 2nd and 1 Plays ----

pbp_2nd_and_1 <- pbp_data %>%
  filter(down == 2, ydstogo == 1)

# ---- Separate Run and Shot Plays ----

# Define run plays
run_plays <- pbp_2nd_and_1 %>%
  filter(play_type == "run")

# Define shot plays (deep passes)
shot_plays <- pbp_2nd_and_1 %>%
  filter(play_type == "pass", air_yards >= 20 | pass_length == "deep")  

# ---- Process Run Plays ----

run_results <- run_plays %>%
  mutate(
    start_fp = yardline_100,
    new_fp = pmin(start_fp - yards_gained, 100),  # Clamp to 100
    new_fp = pmax(new_fp, 0),                     # Clamp to 0
    ep_after = get_EP(new_fp),
    yards_gained = yards_gained
  ) %>%
  select(start_fp, yards_gained, new_fp, ep_after)

# ---- Process Shot Plays ----

shot_results <- shot_plays %>%
  mutate(
    start_fp = yardline_100,
    new_fp = pmin(start_fp - yards_gained, 100),
    new_fp = pmax(new_fp, 0),
    ep_after = get_EP(new_fp),
    yards_gained = yards_gained
  ) %>%
  select(start_fp, yards_gained, new_fp, ep_after)

# ---- Summarize Results ----

summary_run <- run_results %>%
  summarize(mean_ep = mean(ep_after, na.rm = TRUE),
            success_rate = mean(yards_gained >= 1, na.rm = TRUE),
            turnover_rate = mean(new_fp <= 0, na.rm = TRUE))

summary_shot <- shot_results %>%
  summarize(mean_ep = mean(ep_after, na.rm = TRUE),
            success_rate = mean(yards_gained >= 1, na.rm = TRUE),
            turnover_rate = mean(new_fp <= 0, na.rm = TRUE))

print("Run on 2nd and 1 Summary (Real Data):")
print(summary_run)

print("Shot Play on 2nd and 1 Summary (Real Data):")
print(summary_shot)

# ---- Optional: Plot the Results ----

combined_results <- bind_rows(
  run_results %>% mutate(play_call = "run"),
  shot_results %>% mutate(play_call = "shot")
)

ggplot(combined_results, aes(x = ep_after, fill = play_call)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 50) +
  labs(title = "Expected Points After 2nd and 1 Decision (Real Data)",
       x = "Expected Points After Play",
       y = "Count") +
  theme_minimal()
