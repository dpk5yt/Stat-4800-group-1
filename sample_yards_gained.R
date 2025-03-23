library(mclust)
library(dplyr)
library(readr)
library(tidyverse)

# trained models
pass_gmm <- readRDS("pass_gmm.rds")
run_gmm <- readRDS("run_gmm.rds")

# Simulate outcome layer (incompletion, interception, fumble)
simulate_play_outcome <- function(play_type) {
  if (play_type == "pass") {
    if (runif(1) < 0.3) return("incomplete")
    if (runif(1) < 0.025) return("interception")
    if (runif(1) < 0.015) return("fumble")
  } else if (play_type == "run") {
    if (runif(1) < 0.01) return("fumble")
  }
  "success"
}

# Sample yards gained 
sample_yards_gained <- function(play_type, FP) {
  outcome <- simulate_play_outcome(play_type)
  
  if (outcome == "incomplete") {
    return(0)
  } else if (outcome == "interception") {
    return(-sample(5:30, 1))
  } else if (outcome == "fumble") {
    return(-sample(0:15, 1))
  }
  
  if (play_type == "pass") {
    gmm <- pass_gmm
  } else {
    gmm <- run_gmm
  }
  
  component <- sample(1:length(gmm$parameters$pro), 1, prob = gmm$parameters$pro)
  mean_val <- gmm$parameters$mean[component]
  sd_val <- sqrt(gmm$parameters$variance$sigmasq[component])
  yards <- round(rnorm(1, mean = mean_val, sd = sd_val))
  
  # Adjust for red zone
  if (FP >= 80) {
    yards <- min(yards, 100 - FP)
  }
  
  yards
}