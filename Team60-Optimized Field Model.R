library(data.table) ### package for writing huge dataframes and reading huge dataframes
library(ff)
library(tidyverse)
library(ranger)
library(tictoc)


#### NOTE: Make simulation-esque code where it creates different
### x and y distances by decreasing the max_fielder_speed by 1 after every iteration
### until max_fielder_speed <= 0
find_combinations <- function(timestamp, prev_field_x, prev_field_y, distance, game_str, play_id, Day, rownumber) {
  #print(rownumber)
  #initialize dataframe that the function will return
  final_results <- tibble(field_x2 = numeric(), field_y2 = numeric(),
                          distance = numeric(),
                          timestamp = numeric(), game_str = character(),
                          play_id = numeric(), Day = character(), 
                          rownumber = numeric())
  # while loop creates every possible field_x and field_y combination (to the nearest half an inch) starting
  # from the max distance a fielder can travel (using his max_fielder_speed) going all the way down to 0
  while(distance > 0){
    #initialize dataframe for possible x and y distances
  results <- tibble(x_dist = numeric(), y_dist = numeric())
  # Iterate over possible values of field_x
  for (x_dist in seq(-distance, distance, by = 0.05)) {
    # pythagoreans theorem: b^2 = sqrt(c^2 - a^2) where b = y_dist, c = distance, a = x_dist
    y_dist <- sqrt(distance^2 - x_dist^2)
      #add x_dist and y_dist to results dataframe
        results <- results %>% 
          add_row(x_dist = x_dist,
                  y_dist = y_dist)
  }
  #include add negative possibities for y_dist
  results1 <- results %>% 
    mutate(y_dist = y_dist*-1)
  results <- bind_rows(results, results1)
  #add x_dist and y_dist values to previous field_x and field_y values to get the next
  #possible field_x and field_y combination. Also add other variables (timestamp, game_str, play_id, etc) for joining
  results <- results %>% 
    mutate(field_x2 = round(prev_field_x + x_dist,1),
           field_y2 = round(prev_field_y + y_dist,1),
           distance = distance,
           timestamp = timestamp,
           game_str = game_str,
           play_id = play_id, 
           Day = Day,
           rownumber = rownumber) %>% 
    select(field_x2, field_y2, distance, timestamp, game_str, play_id, Day, rownumber)
  #add rows from the results dataframe to the final dataframe
  final_results <- final_results %>% 
    add_row(field_x2 = results$field_x2,
            field_y2 = results$field_y2,
            distance = results$distance,
            timestamp = results$timestamp,
            game_str = results$game_str,
            play_id = results$play_id,
            Day = results$Day,
            rownumber = results$rownumber)
  #repeat the process with a slightly smaller max distance
  distance <- distance - 0.05
  }
  return(final_results)
}

final_dataset_2 <- final_dataset %>% 
  mutate(max_fielder_speed = ifelse(max_fielder_speed <= 1, 1,max_fielder_speed),
         dist_x = abs(dist_x),
         dist_y = abs(dist_y)) %>% 
    mutate(catch_probability = predict(catch_prob_model, final_dataset)$predictions[,2]) %>% 
  filter(timestamp > timestamp_at_contact, time_remaining %% 200 == 0) %>% 
  group_by(game_str, play_id) %>% 
  filter(any(catch_probability > 0.2) & any(catch_probability < 0.7)) %>% 
  ungroup() %>% 
  rename(field_x1 = field_x, field_y1 = field_y) %>%  #need to save optimal fielding positions as field_x and field_y
                                                      #for the catch probability model to run
                                                      # will revert back to original names at the end
  mutate(rownumber = row_number(), #to track progress on function
         field_x = NA,
         field_y = NA,
         catch_prob = NA)
  
tic()
for (i in 1:nrow(final_dataset_2)){
  if(i > 1){
    if (final_dataset_2[i-1,"primary_key"] != final_dataset_2[i,"primary_key"]){
      #once you come to a new play, apply field_x and field_y values to field_x1 and field_y1 on the first row
      final_dataset_2[i, "field_x"] <- final_dataset_2[i,"field_x1"]
      final_dataset_2[i, "field_y"] <- final_dataset_2[i,"field_y1"]
    }else if(final_dataset_2[i,"time_in_play"] < (400 + 800)){ #assuming contact occurs 0.4 seconds into the play
      #for the first 0.8 seconds after contact, the player is still accelerating and thus I made his max_distance/speed half of their top speed
      final_dataset_2[i,"prev_field_x"] <- final_dataset_2[i-1,"field_x"]
      final_dataset_2[i,"prev_field_y"] <- final_dataset_2[i-1, "field_y"]
      combinations <- find_combinations(pull(final_dataset_2[i,"timestamp"]),
                                        pull(final_dataset_2[i,"prev_field_x"]),
                                        pull(final_dataset_2[i,"prev_field_y"]),
                                        pull(round((final_dataset_2[i,"max_fielder_speed"]/5)*0.5,1)),
                                        pull(final_dataset_2[i, "game_str"]),
                                        pull(final_dataset_2[i, "play_id"]),
                                        pull(final_dataset_2[i,"Day"]),
                                        pull(final_dataset_2[i,"rownumber"]))
      df <- combinations %>% 
        left_join(final_dataset_2, by = c("timestamp", "game_str", "play_id", "Day", "rownumber")) %>% 
        mutate(field_x = coalesce(field_x, field_x2),
               field_y = coalesce(field_y, field_y2)) %>% 
        select(-field_x2, -field_y2) %>% 
        #calculating new dist_x, dist_y, and distance_from_ball for catch probability model
        mutate(dist_x = abs(final_ball_position_x - field_x),
               dist_y = abs(final_ball_position_y - field_y),
               distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2)
        )
      
      df <- df %>% 
        #filtering by distance first gets model moving in the right direction
        slice_min(distance_from_ball, n = 50)
      df <- df %>% 
        mutate(catch_prob1 = predict(catch_prob_model, df)$predictions[,2])
      df <- df %>% 
        slice_max(catch_prob1, n = 1) %>% 
        slice_min(abs(dist_y), n = 1) %>% #dist_y was deemed more important in the
        # catch probability model so I'm using that first
        # after total distance from ball
        slice_min(abs(dist_x),n = 1) %>%
        slice(1) %>%
        select(game_str, play_id, timestamp, Day,field_x, field_y, catch_prob1, rownumber) %>%
        rename(field_x2 = field_x, field_y2 = field_y)
      #rejoining best field_x and field_y combination back to the main dataset
      final_dataset_2 <- final_dataset_2 %>% 
        left_join(df, by = c("game_str", 'play_id', 'timestamp', 'Day', 'rownumber')) %>% 
        mutate(field_x = coalesce(field_x, field_x2),
               field_y = coalesce(field_y, field_y2),
               catch_prob = coalesce(catch_prob, catch_prob1)) %>% 
        select(-field_x2, -field_y2, -catch_prob1)
    } else if(final_dataset_2[i,"time_in_play"] < (400 + 2000) & final_dataset_2[i,"time_in_play"] > (400 + 800)){
      #if the play is between 0.8 and 1.6 seconds after contact, max_fielder_speed/distance is limited by a smaller factor as this is when players typically only
      # experience minor acceleration
      final_dataset_2[i,"prev_field_x"] <- final_dataset_2[i-1,"field_x"]
      final_dataset_2[i,"prev_field_y"] <- final_dataset_2[i-1, "field_y"]
      combinations <- find_combinations(pull(final_dataset_2[i,"timestamp"]),
                                        pull(final_dataset_2[i,"prev_field_x"]),
                                        pull(final_dataset_2[i,"prev_field_y"]),
                                        pull(round((final_dataset_2[i,"max_fielder_speed"]/5)*0.8,1)),
                                        pull(final_dataset_2[i, "game_str"]),
                                        pull(final_dataset_2[i, "play_id"]),
                                        pull(final_dataset_2[i,"Day"]),
                                        pull(final_dataset_2[i,"rownumber"]))
      df <- combinations %>% 
        left_join(final_dataset_2, by = c("timestamp", "game_str", "play_id", "Day", "rownumber")) %>% 
        mutate(field_x = coalesce(field_x, field_x2),
               field_y = coalesce(field_y, field_y2)) %>% 
        select(-field_x2, -field_y2) %>% 
        mutate(dist_x = abs(final_ball_position_x - field_x),
               dist_y = abs(final_ball_position_y - field_y),
               distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2)
        )
      
      df <- df %>% 
        slice_min(distance_from_ball, n = 50)
      df <- df %>% 
        mutate(catch_prob1 = predict(catch_prob_model, df)$predictions[,2])
      df <- df %>% 
        slice_max(catch_prob1, n = 1) %>% 
        slice_min(abs(dist_y), n = 1) %>% #dist_y was deemed more important in the
        # catch probability model so I'm using that first
        # after total distance from ball
        slice_min(abs(dist_x),n = 1) %>%
        slice(1) %>%
        select(game_str, play_id, timestamp, Day,field_x, field_y, catch_prob1, rownumber) %>%
        rename(field_x2 = field_x, field_y2 = field_y)
      
      final_dataset_2 <- final_dataset_2 %>% 
        left_join(df, by = c("game_str", 'play_id', 'timestamp', 'Day', 'rownumber')) %>% 
        mutate(field_x = coalesce(field_x, field_x2),
               field_y = coalesce(field_y, field_y2),
               catch_prob = coalesce(catch_prob, catch_prob1)) %>% 
        select(-field_x2, -field_y2, -catch_prob1)
    } else{
    final_dataset_2[i,"prev_field_x"] <- final_dataset_2[i-1,"field_x"]
    final_dataset_2[i,"prev_field_y"] <- final_dataset_2[i-1, "field_y"]
    combinations <- find_combinations(pull(final_dataset_2[i,"timestamp"]),
                                      pull(final_dataset_2[i,"prev_field_x"]),
                                      pull(final_dataset_2[i,"prev_field_y"]),
                                      pull(round(final_dataset_2[i,"max_fielder_speed"]/5,1)),
                                      pull(final_dataset_2[i, "game_str"]),
                                      pull(final_dataset_2[i, "play_id"]),
                                      pull(final_dataset_2[i,"Day"]),
                                      pull(final_dataset_2[i,"rownumber"]))
    df <- combinations %>% 
      left_join(final_dataset_2, by = c("timestamp", "game_str", "play_id", "Day", "rownumber")) %>% 
      mutate(field_x = coalesce(field_x, field_x2),
             field_y = coalesce(field_y, field_y2)) %>% 
      select(-field_x2, -field_y2) %>% 
      mutate(dist_x = abs(final_ball_position_x - field_x),
             dist_y = abs(final_ball_position_y - field_y),
             distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2)
             )
    
    df <- df %>% 
      slice_min(distance_from_ball, n = 50)
    df <- df %>% 
      mutate(catch_prob1 = predict(catch_prob_model, df)$predictions[,2])
    df <- df %>% 
     slice_max(catch_prob1, n = 1) %>% 
     slice_min(abs(dist_y), n = 1) %>% #dist_y was deemed more important in the
                                                  # catch probability model so I'm using that first
                                                  # after total distance from ball
     slice_min(abs(dist_x),n = 1) %>%
      slice(1) %>%
      select(game_str, play_id, timestamp, Day,field_x, field_y, catch_prob1, rownumber) %>%
      rename(field_x2 = field_x, field_y2 = field_y)
    
    final_dataset_2 <- final_dataset_2 %>% 
      left_join(df, by = c("game_str", 'play_id', 'timestamp', 'Day', 'rownumber')) %>% 
      mutate(field_x = coalesce(field_x, field_x2),
             field_y = coalesce(field_y, field_y2),
             catch_prob = coalesce(catch_prob, catch_prob1)) %>% 
      select(-field_x2, -field_y2, -catch_prob1)
    }
  }else{ #set the field_x1 and field_y1 in the first row at field_x and field_y values
    final_dataset_2[i, "field_x"] <- final_dataset_2[i,"field_x1"]
    final_dataset_2[i, "field_y"] <- final_dataset_2[i,"field_y1"]
  }
if(i %% 100 == 0){
  print(i) #prints row number every 100 rows to check on progress
}
}
toc() #12010.98

final_dataset_2 <- final_dataset_2 %>% 
  rename(opt_field_x = field_x, opt_field_y = field_y) %>% 
  rename(field_x = field_x1, field_y = field_y1)

final_dataset_2 <- final_dataset_2 %>% 
  mutate(diff_x = abs(field_x - opt_field_x),
         diff_y = abs(field_y - opt_field_y),
         diff_catch_prob = catch_prob - catch_probability)

summary(final_dataset_2$diff_catch_prob)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-0.5112  0.0196  0.1932  0.2727  0.4689  0.9972     383 

#summary statistics of diff_catch_prob at the end of play
final_dataset_2 %>% filter(time_remaining == 0) %>% reframe(Min. = min(diff_catch_prob, na.rm = T),
                                                            "1st Qu." = quantile(diff_catch_prob, 0.25, na.rm = T),
                                                            Median = median(diff_catch_prob, na.rm = T),
                                                            Mean = mean(diff_catch_prob, na.rm = T),
                                                            "3rd Qu." = quantile(diff_catch_prob,0.75, na.rm = T),
                                                            Max = max(diff_catch_prob, na.rm = T))
#  Min. `1st Qu.` Median  Mean `3rd Qu.`   Max
# -0.0299   0.00522  0.134 0.361     0.854 0.995

#Diff Catch Prob at all Data Points
final_dataset_2 %>% ggplot(aes(diff_catch_prob)) + 
  geom_density(fill = "blue") + 
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1, color = "gray") +
  theme_bw() +
  xlab("Optimized Catch Probability - Catch Probability") +
  ggtitle("Diff Catch Probability Density Plot") +
  scale_x_continuous(breaks = seq(-1, 1, 0.25))

#Diff Catch Prob at the end of Play
final_dataset_2 %>% filter(time_remaining == 0) %>% ggplot(aes(diff_catch_prob)) + 
  geom_density(fill = "blue") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray", linewidth = 1) +
  xlab("Optimized Catch Probability - Catch Probability")+
  ggtitle("Diff Catch Probability by End of Play") +
  theme_bw() +
  scale_x_continuous(breaks = seq(-1, 1, 0.25))
  
write_csv(final_dataset_2, file = "final_dataset_2.csv")
