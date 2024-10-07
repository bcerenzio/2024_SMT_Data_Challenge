find_combinations <- function(time_remaining, prev_field_x, prev_field_y, distance) {
  final_results <- tibble(field_x2 = numeric(), field_y2 = numeric(),
                          time_remaining = numeric(),
                          distance = numeric())
  
  while(distance > 0){
    results <- tibble(x_dist = numeric(), y_dist = numeric())
    # Iterate over possible values of field_x
    for (x_dist in seq(-distance, distance, by = 0.05)) {
      y_dist <- sqrt(distance^2 - x_dist^2)
      
      results <- results %>% 
        add_row(x_dist = x_dist,
                y_dist = y_dist)
    }
    results1 <- results %>% 
      mutate(y_dist = y_dist*-1)
    results <- bind_rows(results, results1)
    results <- results %>% 
      mutate(field_x2 = round(prev_field_x + x_dist,1),
             field_y2 = round(prev_field_y + y_dist,1),
             distance = distance,
             time_remaining = time_remaining) %>% 
      select(field_x2, field_y2, distance, time_remaining)
    final_results <- final_results %>% 
      add_row(field_x2 = results$field_x2,
              field_y2 = results$field_y2,
              distance = results$distance,
              time_remaining = results$time_remaining)
    distance <- distance - 0.05
  }
  return(final_results)
  
}

save(find_combinations, file = "Optimization_Field_Model.RData")
