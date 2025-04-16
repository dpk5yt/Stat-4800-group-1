library(dplyr)
library(ggplot2)

gmm_models <- readRDS("gmm_models_by_context.rds")
source("get_EP.R")

# Play Outcome Function
simulate_play_outcome <- function(play_type) {
  if (play_type == "run") {
    outcomes <- c("complete", "incomplete", "interception", "fumble")
    probs <- c(0.95, 0.0, 0.0, 0.05)
  } else if (play_type == "shot") {
    outcomes <- c("complete", "incomplete", "interception", "fumble")
    probs <- c(0.70, 0.20, 0.08, 0.02)
  } else {
    outcomes <- c("complete", "incomplete", "interception", "fumble")
    probs <- c(0.85, 0.10, 0.03, 0.02)
  }
  sample(outcomes, 1, prob = probs)
}

#  Sample Yards Gained From gmm
sample_yards_gained <- function(play_type, FP, YTG) {
  

  actual_play_type <- ifelse(play_type == "shot", "pass", play_type)
  
  field_zone <- case_when(
    FP <= 30 ~ "own_territory",
    FP <= 70 ~ "midfield",
    TRUE ~ "red_zone"
  )
  
  ytg_bucket <- case_when(
    YTG <= 3 ~ "short",
    YTG <= 7 ~ "medium",
    TRUE ~ "long"
  )
  
  key <- paste(actual_play_type, field_zone, ytg_bucket, sep = "_")
  
  if (!(key %in% names(gmm_models))) {
    warning(paste("No GMM found for", key, "- using fallback."))
    fallback_key <- paste(actual_play_type, "midfield", "medium", sep = "_")
    gmm <- gmm_models[[fallback_key]]
  } else {
    gmm <- gmm_models[[key]]
  }
  
  component <- sample(1:length(gmm$parameters$pro), 1, prob = gmm$parameters$pro)
  mean_val <- as.numeric(gmm$parameters$mean[component])
  var_val <- as.numeric(gmm$parameters$variance$sigmasq)[component]
  
  if (is.null(var_val) || length(var_val) == 0 || is.na(var_val) || var_val <= 0) {
    var_val <- 1
  }
  
  yards <- round(rnorm(1, mean = mean_val, sd = sqrt(var_val)))
  return(yards)
}


# Sim one play
simulate_single_play <- function(play_type, fp, ytg) {
  yards <- sample_yards_gained(play_type, fp, ytg)
  outcome <- simulate_play_outcome(play_type)
  turnover_flag <- FALSE
  if (outcome == "incomplete") {
    yards <- 0
  } else if (outcome == "interception") {
    yards <- -sample(5:30, 1)
    turnover_flag <- TRUE
  } else if (outcome == "fumble") {
    yards <- -sample(0:15, 1)
    turnover_flag <- TRUE
  }
  list(yards = yards, turnover = turnover_flag)
}

# Sim a singular 2nd and 1
simulate_2nd_and_1_play <- function(play_call, fp_start) {
  down <- 2
  ytg <- 1
  play <- simulate_single_play(play_call, fp_start, ytg)
  yards <- play$yards
  turnover_flag <- play$turnover
  if (play_call == "shot" && yards > 0) {
    yards <- yards + sample(5:15, 1)
  }
  fp_end <- min(max(fp_start + yards, 0), 100)  # clamp to [0, 100]
  ep_after <- get_EP(fp_end)
  success <- (yards >= ytg)
  tibble(
    play_call = play_call,
    start_fp = fp_start,
    yards_gained = yards,
    new_fp = fp_end,
    ep_after = ep_after,
    success = success,
    turnover = turnover_flag
  )
}

# batch Sim
simulate_2nd_and_1 <- function(n_sims = 10000) {
  all_plays <- list()
  set.seed(123)
  for (i in 1:n_sims) {
    fp_start <- sample(20:80, 1)
    run_result <- simulate_2nd_and_1_play("run", fp_start)
    shot_result <- simulate_2nd_and_1_play("shot", fp_start)
    all_plays[[length(all_plays) + 1]] <- run_result
    all_plays[[length(all_plays) + 1]] <- shot_result
  }
  bind_rows(all_plays)
}

# Run and summarize 
results <- simulate_2nd_and_1(10000)

summary_stats <- results %>%
  group_by(play_call) %>%
  summarize(
    mean_ep = mean(ep_after, na.rm = TRUE),
    success_rate = mean(success),
    turnover_rate = mean(turnover),
    .groups = "drop"
  )

print(summary_stats)

# Plot Distribution of EP
ggplot(results, aes(x = ep_after, fill = play_call)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 50) +
  labs(title = "Expected Points After 2nd and 1 (Simulated)",
       x = "Expected Points",
       y = "Frequency") +
  theme_minimal()
