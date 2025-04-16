# Create a lookup version of get_EP
get_EP <- function(fp) {
  if (is.na(fp)) return(NA)
  if (fp <= 0) return(-2)   # Safety
  if (fp >= 100) return(7)  # Touchdown
  
  closest_fp <- round(fp)
  
  match <- ep_table %>% filter(yardline_100 == closest_fp)
  
  if (nrow(match) == 0) {
    return(NA)
  } else {
    return(match$mean_ep)
  }
}
