

library(readr)
source("sample_yards_gained.R")

# trained FG model
fg_model <- readRDS("fg_model.rds")

# Logistic regression to estimate FG success
predict_fg_success <- function(FP) {
  kick_distance <- 100 - FP
  prob <- predict(fg_model, newdata = data.frame(kick_distance = kick_distance), type = "response")
  prob
}

# 4th down probabilities
predict_4th <- function(FP, YTG) {
  prob_row <- fourth_down_probs %>%
    filter(yardline_100 == FP, ydstogo == YTG) %>%
    select(decision, prob)
  
  if (nrow(prob_row) == 0) {
    go_prob <- 0.2
    fg_prob <- 0.3
    punt_prob <- 0.5
  } else {
    go_prob <- prob_row$prob[prob_row$decision == "go_for_it"]
    fg_prob <- prob_row$prob[prob_row$decision == "field_goal"]
    punt_prob <- prob_row$prob[prob_row$decision == "punt"]
  }
  
  total <- go_prob + fg_prob + punt_prob
  c(go_for_it = go_prob / total, fg = fg_prob / total, punt = punt_prob / total)
}

# Play dispatcher
run_play <- function(D, YTG, FP, play_type) {
  yards_gained <- sample_yards_gained(play_type, FP)
  new_FP <- FP + yards_gained
  forced_turnover <- runif(1) < 0.02
  is_touchdown <- (new_FP >= 100)
  
  if (new_FP <= 0) {
    list(D = D, YTG = YTG, FP = -5, exit_drive = 1)
  } else if (forced_turnover) {
    list(D = D, YTG = YTG, FP = FP, exit_drive = 1)
  } else if (is_touchdown) {
    list(D = D, YTG = YTG, FP = 105, exit_drive = 1)
  } else {
    new_YTG <- YTG - yards_gained
    if (new_YTG <= 0) {
      list(D = 1, YTG = 10, FP = new_FP, exit_drive = 0)
    } else {
      list(D = D + 1, YTG = new_YTG, FP = new_FP, exit_drive = 0)
    }
  }
}

# Down-specific 
down_one <- function(D, YTG, FP) {
  run_play(D, YTG, FP, "run")
}

down_two <- function(D, YTG, FP) {
  run_play(D, YTG, FP, "pass")
}

down_three <- function(D, YTG, FP) {
  run_play(D, YTG, FP, "pass")
}

down_four <- function(D, YTG, FP) {
  decision_probs <- predict_4th(FP, YTG)
  decision <- sample(c("go_for_it", "fg", "punt"), 1, prob = decision_probs)
  
  if (decision == "go_for_it") {
    yards_gained <- sample_yards_gained("pass", FP)
    new_FP <- FP + yards_gained
    
    if (yards_gained >= YTG) {
      list(D = 1, YTG = 10, FP = new_FP, exit_drive = 0)
    } else {
      list(D = D, YTG = YTG, FP = new_FP, exit_drive = 1)
    }
    
  } else if (decision == "fg") {
    p_fg <- predict_fg_success(FP)
    made_fg <- runif(1) < p_fg
    if (made_fg) {
      list(D = D, YTG = YTG, FP = 115, exit_drive = 1)
    } else {
      list(D = D, YTG = YTG, FP = FP, exit_drive = 1)
    }
    
  } else {
    punting_distance <- sample(30:50, 1)
    new_FP <- FP + punting_distance
    muffed_punt <- runif(1) < 0.05
    
    if (muffed_punt) {
      list(D = 1, YTG = 10, FP = new_FP, exit_drive = 0)
    } else {
      list(D = D, YTG = YTG, FP = new_FP, exit_drive = 1)
    }
  }
}
