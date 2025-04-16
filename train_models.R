library(dplyr)
library(mclust)

pbp <- readRDS("pbp2014-2024.rds")


#  Filter usable plays
pbp_clean <- pbp %>%
  filter(play_type %in% c("run", "pass"),
         !is.na(yards_gained),
         !is.na(yardline_100),
         !is.na(down),
         !is.na(ydstogo))

# Create contextual buckets
pbp_clean <- pbp_clean %>%
  mutate(
    field_zone = case_when(
      yardline_100 <= 30 ~ "own_territory",
      yardline_100 <= 70 ~ "midfield",
      TRUE ~ "red_zone"
    ),
    ytg_bucket = case_when(
      ydstogo <= 3 ~ "short",
      ydstogo <= 7 ~ "medium",
      TRUE ~ "long"
    )
  )

# Train GMMs by context
gmm_models <- list()

for (pt in c("run", "pass")) {
  for (fz in unique(pbp_clean$field_zone)) {
    for (ytg in unique(pbp_clean$ytg_bucket)) {
      
      subset_data <- pbp_clean %>%
        filter(play_type == pt, field_zone == fz, ytg_bucket == ytg)
      
      if (nrow(subset_data) >= 20) {
        yards <- subset_data$yards_gained
        if (length(unique(yards)) == 1) {
          yards <- yards + rnorm(length(yards), 0, 0.01)
        }
        gmm <- Mclust(yards, G = 2)
        key <- paste(pt, fz, ytg, sep = "_")
        gmm_models[[key]] <- gmm
      }
    }
  }
}


saveRDS(gmm_models, "gmm_models_by_context.rds")
