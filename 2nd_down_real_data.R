library(dplyr)
library(ggplot2)

# Load real play-by-play data
pbp <- readRDS("pbp2014-2024.rds")
source("get_EP.R")
get_EP <- Vectorize(get_EP)


# ---- Filter 2nd and 1 Plays ----
real_2nd_and_1 <- pbp %>%
  filter(down == 2,
         ydstogo == 1,
         play_type %in% c("run", "pass"),
         !is.na(yards_gained),
         !is.na(yardline_100))

# ---- Classify Play Call (Run vs Shot) ----
real_2nd_and_1 <- real_2nd_and_1 %>%
  mutate(
    play_call = case_when(
      play_type == "run" ~ "run",
      play_type == "pass" & (air_yards >= 20 | pass_length == "deep") ~ "shot",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(play_call))  # keep only run or shot plays

# ---- Calculate Derived Metrics ----
real_2nd_and_1_results <- real_2nd_and_1 %>%
  mutate(
    start_fp = yardline_100,
    new_fp = yardline_100 + yards_gained,
    new_fp = pmin(pmax(new_fp, 0), 100),  # clamp field position
    ep_after = get_EP(new_fp),
    success = yards_gained >= 1,
    turnover = (fumble_lost == 1 | interception == 1)
  ) %>%
  select(play_call, start_fp, yards_gained, new_fp, ep_after, success, turnover)

# ---- Summary Stats ----
real_summary <- real_2nd_and_1_results %>%
  group_by(play_call) %>%
  summarize(
    mean_ep = mean(ep_after, na.rm = TRUE),
    success_rate = mean(success, na.rm = TRUE),
    turnover_rate = mean(turnover, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )

print(real_summary)

# ---- EP Histogram Plot ----
ggplot(real_2nd_and_1_results, aes(x = ep_after, fill = play_call)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 50) +
  labs(title = "Expected Points After 2nd and 1 (Real Data)",
       x = "Expected Points",
       y = "Frequency") +
  theme_minimal()
