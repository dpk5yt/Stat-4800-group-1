
gmm_models <- readRDS("gmm_models_by_context.rds")

sample_yards_gained <- function(play_type, FP, YTG) {
  
  # 1. Bucket field position
  field_zone <- case_when(
    FP <= 30 ~ "own_territory",
    FP <= 70 ~ "midfield",
    TRUE ~ "red_zone"
  )
  
  # 2. Bucket yards to go
  ytg_bucket <- case_when(
    YTG <= 3 ~ "short",
    YTG <= 7 ~ "medium",
    TRUE ~ "long"
  )
  
  # 3. Build the key
  key <- paste(play_type, field_zone, ytg_bucket, sep = "_")
  
  # 4. Check if GMM exists
  if (!(key %in% names(gmm_models))) {
    warning(paste("No GMM found for key:", key, "- using fallback."))
    # fallback to generic midfield/medium model
    fallback_key <- paste(play_type, "midfield", "medium", sep = "_")
    gmm <- gmm_models[[fallback_key]]
  } else {
    gmm <- gmm_models[[key]]
  }
  
  # 5. Sample from GMM
  component <- sample(1:length(gmm$parameters$pro), 1, prob = gmm$parameters$pro)
  mean_val <- as.numeric(gmm$parameters$mean[component])
  var_val <- as.numeric(gmm$parameters$variance$sigmasq)[component]
  
  if (is.na(var_val) || var_val <= 0) var_val <- 1
  
  yards <- round(rnorm(1, mean = mean_val, sd = sqrt(var_val)))
  
  return(yards)
}
