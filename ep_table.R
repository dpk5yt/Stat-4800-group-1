# Assuming you already loaded your pbp_data
pbp_data <- readRDS("pbp2014-2024.rds")  # or however you load it
# Filter for real offensive plays (runs or passes)
pbp_ep_model_data <- pbp_data %>%
  filter(!is.na(yardline_100),
         play_type %in% c("run", "pass")) %>%
  mutate(
    # Points scored on the play
    points = case_when(
      touchdown == 1 ~ 7,
      field_goal_result == "made" ~ 3,
      safety == 1 ~ -2,
      TRUE ~ 0
    )
  )

# Summarize average expected points by starting field position
ep_table <- pbp_ep_model_data %>%
  group_by(yardline_100) %>%
  summarize(mean_ep = mean(points, na.rm = TRUE)) %>%
  ungroup()

# Preview what the table looks like
print(ep_table)