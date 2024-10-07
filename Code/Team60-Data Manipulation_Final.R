library(tidyverse)


## will create a time in play column for each play in a given game
player_pos_all <- player_pos_all %>% group_by(play_id, Day, player_position, HomeTeam) %>% mutate(time_in_play = timestamp - first(timestamp)) %>% print(n = 25) %>% ungroup()

## will create a time in play column for ball position
ball_pos_all <- ball_pos_all %>% group_by(game_str, play_id, Day, HomeTeam) %>% mutate(time_in_play = timestamp - first(timestamp)) %>% print(n = 25) %>% ungroup()

all_tables <- ball_pos_all %>% 
  left_join(player_pos_all, by = c("game_str", 'play_id', 'timestamp', 'Season', 'HomeTeam', 'Day', 'time_in_play')) %>% 
  left_join(game_events_all, by = c('game_str', 'play_id', 'timestamp', 'Season', 'HomeTeam', 'Day'))

# joining all tables then filtering
game_events_flyballs <- all_tables %>% 
  group_by(game_str, play_id, Day) %>% 
  filter(any(event_code == 4), any(player_position.y %in% c(7,8,9))) %>% 
  ungroup()

#deletes all player position rows except for the outfielder who fields the ball
example <- game_events_flyballs %>% 
  group_by(game_str, play_id, Day, player_position.x) %>% 
  filter(any(player_position.x == player_position.y)) %>% 
  ungroup()

#determining pitch handedness
example <- example %>% 
  group_by(game_str, play_id, Day) %>% 
  mutate(pitcher_throws_right = ifelse(ball_position_x < 0 & event_code == 1, 1, 0)) %>%
  mutate(pitcher_throws_right = ifelse(is.na(event_code) | event_code != 1, first(pitcher_throws_right), pitcher_throws_right)) %>% 
  ungroup()

## creating separate table for each outfield position to sort through plays where two
## outfielders touch the ball

list_of_tables <- split(example, example$player_position.x)

for (level in unique(example$player_position.x)){
  assign(glue::glue("example_{level}"),list_of_tables[[as.character(level)]])
}


#adding pitch velo
pitch_velo <- function(df){
  df %>% 
    group_by(game_str, play_id, Day) %>% 
    mutate(pitch_velo = ifelse(event_code == 1, sqrt((
      ball_position_y - lead(ball_position_y, n = 2L))^2 + 
        (ball_position_x - lead(ball_position_x, n = 2L))^2 + 
        (ball_position_z - lead(ball_position_z, n = 2L))^2)/100*681.8,NA))  %>% 
    mutate(pitch_velo = ifelse(is.na(event_code) | event_code != 1, first(pitch_velo), pitch_velo)) %>% 
    ungroup()
}
#determining exit velo
exit_velo <- function(df){
  df %>% 
    group_by(game_str, play_id, Day) %>% 
    mutate(exit_velo = ifelse(event_code == 4, 
                              round(sqrt((lead(ball_position_x, n = 2L) - lead(ball_position_x))^2 + 
                                           (lead(ball_position_y, n = 2L) - lead(ball_position_y))^2 +
                                           (lead(ball_position_z, n = 2L) - lead(ball_position_z))^2)/
                                      50*681.8,2),NA)) %>%
    fill(exit_velo, .direction = c("downup")) %>% 
    ungroup()
}

#determining launch angle
LA <- function(df){
  df %>% 
    group_by(game_str, play_id, Day) %>% 
    mutate(LA = ifelse(event_code == 4, 
                       round(atan((lead(ball_position_z) - ball_position_z)/
                                    sqrt((lead(ball_position_x) - ball_position_x)^2 + 
                                           (lead(ball_position_y) - ball_position_y)^2))*(180/pi),2),NA)) %>%
    fill(LA, .direction = c("downup")) %>% 
    ungroup()
}

#determining how much distance the fielder covered during each timestamp
distance_covered <- function(df){
  df %>% 
    group_by(game_str, play_id, Day) %>% 
    mutate(distance_covered = ifelse(event_code == 4 | is.na(event_code), sqrt((field_x - lag(field_x))^2 + (field_y - lag(field_y))^2),0)) %>% 
    ungroup()
}


#determining whether a flyout was made
flyout <- function(df){
  df %>% 
    group_by(game_str, play_id, Day) %>% 
    mutate(flyout = ifelse(any(event_code %in% c(9, 10, 16)),FALSE, TRUE)) %>% 
    ungroup()
}

#adding column that details timestamp at contact
timestamp_at_contact <- function(df){
  df %>% 
    group_by(game_str, play_id, Day) %>% 
    mutate(timestamp_at_contact = ifelse(event_code == 4, timestamp, NA)) %>% 
    fill(timestamp_at_contact, .direction = "downup") %>% 
    ungroup()
}

#trying to gather ball_velo after contact
ball_velo <- function(df){
  df %>% 
    group_by(game_str, play_id, Day) %>% 
    mutate(ball_velo = ifelse(timestamp > timestamp_at_contact, 
                              round(sqrt((lead(ball_position_x) - ball_position_x)^2 + 
                                           (lead(ball_position_y) - ball_position_y)^2)/50*681.8,2),NA)) %>% 
    ungroup()
}

#fielder speed (mph)
player_speed <- function(df){
  df %>% 
    group_by(game_str, play_id, Day) %>% 
    mutate(fielder_speed = sqrt((lag(field_x, n = 5L) - field_x)^2 + (lag(field_y, n = 5L) - field_y)^2)/250*1000) %>% 
    ungroup()
}


delta_ball_x <- function(df){
  df %>% 
    group_by(game_str, play_id, Day) %>% 
    mutate(delta_ball_x = ifelse(timestamp_at_contact > timestamp, ball_position_x - lag(ball_position_x),NA)) %>% 
    ungroup()
}

delta_ball_z <- function(df){
  df %>% 
    group_by(game_str, play_id, Day) %>% 
    mutate(delta_ball_z = ifelse(timestamp_at_contact > timestamp, ball_position_z - lag(ball_position_z),NA)) %>% 
    ungroup()
}


# Calculating Pitch Movement
# Negative Hor_Mov = arm-side run, positive Hor_Mov = glove side run
pitch_movement <- function(df){
  df %>% 
    group_by(game_str, play_id, Day) %>%
    mutate(pitch = ifelse(timestamp < timestamp_at_contact, TRUE, FALSE)) %>%
    mutate(XHor_Mov = ifelse(pitch == TRUE & is.na(event_code), nth(delta_ball_x, 2),NA),
           z_velo = ifelse(pitch == TRUE & is.na(event_code), nth(delta_ball_z,2)/50*1000, NA)) %>% 
    ungroup() %>% 
    group_by(game_str, play_id, Day, pitch) %>% 
    mutate(XVert_Mov = ifelse(pitch == TRUE & is.na(event_code), last(((time_in_play/1000)^2)*-16) + last(z_velo*time_in_play/1000),NA))  %>%#effects due to gravity
    ungroup() %>%
    group_by(game_str, play_id, Day) %>% 
    mutate(sum_XHor_Mov = sum(XHor_Mov, na.rm = T)*12, #Multiplying by twelve converts to inches
           xVert_Mov_in = XVert_Mov*12,
           sum_Hor_Mov = sum(delta_ball_x, na.rm = T)*12,
           sum_Ver_Mov = sum(delta_ball_z, na.rm = T)*12,
           Hor_Mov = sum_Hor_Mov - sum_XHor_Mov,
           Ver_Mov = sum_Ver_Mov - xVert_Mov_in) %>%
    fill(Hor_Mov, .direction = "down") %>% 
    fill(Ver_Mov, .direction = "updown") %>% 
    mutate(Hor_Mov = ifelse(pitcher_throws_right == 0, Hor_Mov * -1, Hor_Mov)) %>% 
    ungroup()
}

#time remaining
time_remaining <- function(df){
  df %>% 
    group_by(game_str, play_id, Day) %>% 
    mutate(time_remaining = last(time_in_play) - time_in_play) %>% 
    ungroup()
} 

# first move (both horizontally and vertically)

first_move <- function(df){
  df %>% 
    group_by(game_str, play_id, Day, pitch) %>% 
    mutate(first_move_x = case_when(pitch == FALSE & first(field_x) < nth(field_x,2) ~ "Left", 
                                    pitch == FALSE & first(field_x) > nth(field_x,2) ~ "Right",
                                    .default = NA),
           first_move_y = case_when(pitch == FALSE & first(field_y) > nth(field_y, 2) ~ "Back", 
                                    pitch == FALSE & first(field_y) < nth(field_y, 2) ~ "Forward",
                                    .default = NA)) %>% 
    ungroup() %>% 
    group_by(game_str, play_id, Day) %>% 
    fill(first_move_x, .direction = "up") %>% 
    fill(first_move_y, .direction = "up") %>% 
    ungroup()
}

# the direction the ball will land in relation to the fielder
### NOTE: SAVE FOR THE END
# ball_direction <- function(df) {
#   df %>% 
#     group_by(game_str, play_id, Day) %>% 
#     mutate(ball_direction_x = ifelse(last(ball_position_x) > first(field_x), "Left", "Right"),
#            ball_direction_y = ifelse(last(ball_position_y) > first(field_y), "Back", "Forward"))
#   
# }

#removes grounders (batted balls with LA < 10 per statcast)
remove_grounders <- function(df) {
  df %>% 
    group_by(game_str, play_id, Day) %>% 
    filter(any(LA > 10)) %>% 
    ungroup()
}

#trying to find who made a play on the ball first
first_attempt <- function(df) {
  df %>% 
    group_by(game_str, play_id, Day) %>% 
    mutate(fielded_first = case_when(player_position.y == 7 ~ 7,
                                     player_position.y == 8 ~ 8,
                                     player_position.y == 9 ~ 9)) %>% 
    fill(fielded_first, .direction = "updown") %>% 
    mutate(fielded_first = first(fielded_first)) %>% 
    ungroup()
}

# figuring out when the ball bounced (if it did)
ball_bounced_time <- function(df){
  df %>% 
    group_by(game_str, play_id, Day) %>% 
    mutate(timestamp_ball_bounced = ifelse(event_code %in% c(9,10,16), timestamp,NA)) %>% 
    fill(timestamp_ball_bounced, .direction = "downup") %>%
    mutate(timestamp_ball_bounced = ifelse(is.na(timestamp_ball_bounced), last(timestamp),first(timestamp_ball_bounced))) %>% 
    ungroup()
}



data_frames <- list(example_7, example_8, example_9)

# Apply the function to each data frame in the list and assign the results back
for (i in seq_along(data_frames)) {
  data_frames[[i]] <- pitch_velo(data_frames[[i]])
  data_frames[[i]] <- exit_velo(data_frames[[i]])
  data_frames[[i]] <- LA(data_frames[[i]])
  data_frames[[i]] <- distance_covered(data_frames[[i]])
  data_frames[[i]] <- flyout(data_frames[[i]])
  data_frames[[i]] <- timestamp_at_contact(data_frames[[i]])
  data_frames[[i]] <- ball_velo(data_frames[[i]])
  data_frames[[i]] <- player_speed(data_frames[[i]])
  data_frames[[i]] <- delta_ball_x(data_frames[[i]])
  data_frames[[i]] <- delta_ball_z(data_frames[[i]])
  data_frames[[i]] <- pitch_movement(data_frames[[i]])
  data_frames[[i]] <- time_remaining(data_frames[[i]])
  data_frames[[i]] <- first_move(data_frames[[i]])
  data_frames[[i]] <- remove_grounders(data_frames[[i]])
  data_frames[[i]] <- first_attempt(data_frames[[i]])
  data_frames[[i]] <- ball_bounced_time(data_frames[[i]])
}

# Assign the processed data frames back to their original names
example_7 <- data_frames[[1]]
example_8 <- data_frames[[2]]
example_9 <- data_frames[[3]]

#creating outfield tables to extract player ids
left_fielders <- game_info_all %>% select(game_str, play_per_game, top_bottom, left_field)
center_fielders <- game_info_all %>% select(game_str, play_per_game, top_bottom, center_field)
right_fielders <- game_info_all %>% select(game_str, play_per_game, top_bottom, right_field)

#joining player_ids onto respective tables
example_7 <- example_7 %>% 
  left_join(left_fielders, by = c("game_str", 'play_per_game'))
example_7 <- example_7 %>% 
  group_by(game_str, play_id, Day) %>% 
  fill(left_field, .direction = "updown") %>%
  fill(top_bottom, .direction = "updown") %>% 
  ungroup()


example_8 <- example_8 %>% 
  left_join(center_fielders, by = c("game_str", 'play_per_game'))
example_8 <- example_8 %>% 
  group_by(game_str, play_id, Day) %>% 
  fill(center_field, .direction = "updown") %>%
  fill(top_bottom, .direction = "updown") %>% 
  ungroup()

example_9 <- example_9 %>% 
  left_join(right_fielders, by = c("game_str", 'play_per_game'))
example_9 <- example_9 %>% 
  group_by(game_str, play_id, Day) %>% 
  fill(right_field, .direction = "updown") %>%
  fill(top_bottom, .direction = "updown") %>% 
  ungroup()


#combining all data together
final_dataset <- bind_rows(example_7, example_8, example_9) %>% 
  arrange(game_str, play_id, Day, timestamp)

#filtering out all occurrences where two fielders are tracked in the dataset
final_dataset <- final_dataset %>% 
  group_by(game_str, play_id, Day) %>% 
  filter(player_position.x == fielded_first) %>% 
  ungroup()

# Adding Level Column
final_dataset <- final_dataset %>% 
  mutate(Level = case_when(HomeTeam == "Home1A" ~ "A",
                           HomeTeam == "Home2A" ~ "A+",
                           HomeTeam == "Home3A" ~ "AA",
                           HomeTeam == "Home4A" ~ 'AAA'))

# Identifying Pitches
final_dataset <- final_dataset %>% 
  group_by(game_str, play_id, Day) %>% 
  mutate(pitch_type = case_when(
    pitch_velo < 65 ~ "Eephus",
    pitch_velo > 88 & abs(Hor_Mov) < Ver_Mov & Hor_Mov < 0 ~ "4-Seam",
    pitch_velo > 88 & Ver_Mov < abs(Hor_Mov) & Hor_Mov < 0 ~ "Sinker",
     Hor_Mov < -10 & Ver_Mov < 12 | pitch_velo < 88 & Hor_Mov < -10  ~ "Changeup",
     Hor_Mov > -10 & Hor_Mov < 0 & Ver_Mov < 12 | 
      pitch_velo < 88 & Hor_Mov > -10 & Hor_Mov < 0 ~ "Splitter", 
     Hor_Mov >= 0 & Hor_Mov < 10 & Ver_Mov < 10 & Ver_Mov > -5.5 ~ "Slider",
      Hor_Mov >= 0 & Hor_Mov < 10 & Ver_Mov > 10 ~ "Cutter",
      Hor_Mov >= 10 & Ver_Mov >= -5  & abs(Hor_Mov) > abs(Ver_Mov) ~ "Sweeper",
     Hor_Mov >= 7 & Ver_Mov <= -5 ~ "Slurve",
     Hor_Mov >= 0 & Ver_Mov < -5 & abs(Ver_Mov) > abs(Hor_Mov)  ~ "Curveball"
    )) %>% 
   mutate(pitch_type = factor(pitch_type, levels = c("4-Seam", "Sinker", "Cutter",
                                                     "Splitter", 'Changeup',
                                                     "Slider", 'Sweeper',
                                                     'Curveball', 'Slurve',
                                                     'Eephus')))%>% 
  ungroup()

## eliminating rows after ball bounced
final_dataset <- final_dataset %>% 
  group_by(game_str, play_id, Day) %>% 
  filter(timestamp <= timestamp_ball_bounced) %>% 
  ungroup()


final_dataset <- final_dataset %>% 
  group_by(game_str, play_id, Day) %>% 
  mutate(ball_direction_x = ifelse(final_ball_position_x > first(field_x), "Left", "Right"),
         ball_direction_y = ifelse(final_ball_position_y > first(field_y), "Back", "Forward")) %>% 
  ungroup()

#calculating fielder speed in ft/s
# final_dataset1 <- final_dataset1 %>% 
#   mutate(fielder_speed_ft_s = fielder_speed*1.467)





#removing rows after catch was made
final_dataset <- final_dataset %>% 
  group_by(game_str, play_id, Day) %>% 
  mutate(timestamp_at_catch = ifelse(event_code == 2, timestamp,NA)) %>% 
  fill(timestamp_at_catch, .direction = "updown") %>% 
  mutate(timestamp_at_catch = first(timestamp_at_catch)) %>% 
  mutate(timestamp_at_catch = ifelse(is.na(timestamp_at_catch), last(timestamp), timestamp_at_catch)) %>% 
  ungroup()

final_dataset <- final_dataset %>% 
  filter(timestamp <= timestamp_at_catch)

#fixing time_remaining column:
final_dataset <- final_dataset %>% 
  group_by(game_str, play_id, Day) %>% 
  mutate(time_remaining = last(time_in_play) - time_in_play) %>% 
  ungroup()

#max speed
final_dataset <- final_dataset %>% 
  group_by(game_str, play_id, Day) %>% 
  mutate(fielder_speed = ifelse(time_remaining ==0, NA, fielder_speed),
         max_fielder_speed = max(fielder_speed, na.rm = TRUE)) %>% 
           ungroup()

#calculating spray angle
final_dataset <- final_dataset %>% 
  group_by(game_str, play_id, Day) %>% 
  mutate(spray_angle = ifelse(event_code == 4, asin((last(ball_position_x) - ball_position_x)/
                                                      (sqrt((last(ball_position_x)-ball_position_x)^2 + 
                                                              (last(ball_position_y) - ball_position_y)^2)))*180/pi,
                              NA)) %>% 
  fill(spray_angle, .direction = "downup") %>% 
  ungroup()

#creating a dataframe with pitcherids
pitchers <- game_info_all %>% 
  select(game_str, play_per_game, pitcher)

#joining pitchers to the data
final_dataset <- final_dataset %>% 
  left_join(pitchers, by = c("game_str", 'play_id' = 'play_per_game'))



# adding ball final position to see how far the 
# final landing spot of the ball is from the fielder at any
# given time
final_dataset <- final_dataset %>% 
  group_by(game_str, play_id, Day) %>% 
  mutate(final_ball_position_x = last(ball_position_x),
         final_ball_position_y = last(ball_position_y)) %>% 
  filter(final_ball_position_y >= 120) %>% 
  ungroup()

#calculating the distance the fielder is from the final ball destination
final_dataset <- final_dataset %>% 
  mutate(dist_x = final_ball_position_x - field_x,
         dist_y = final_ball_position_y - field_y,
         distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))

#prev fielder_position and distance from ball
final_dataset <- final_dataset %>% 
  group_by(game_str, play_id, Day) %>% 
  mutate(prev_field_x = lag(field_x),
         prev_field_y = lag(field_y),
         prev_distance = lag(distance_from_ball),
         prev_dist_x = lag(dist_x),
         prev_dist_y = lag(dist_y)) %>% 
  ungroup()

#rewriting flyout function
final_dataset <- final_dataset %>% 
  group_by(game_str, play_id, Day) %>% 
  mutate(flyout = ifelse(any(event_code %in% c(9, 10, 16)),FALSE, TRUE)) %>% 
  ## correcting for any possible data errors
  filter(!(last(distance_from_ball) > 8 & flyout == TRUE)) %>% # I realize 8 feet, seems excessive but I want to account for any
                                                                           # data error in player tracking since that could be less precise
                                                                           # than ball tracking
  ungroup()

#adding exit_velo_y & exit_velo_x for ball_position_model
final_dataset <- final_dataset %>% 
  group_by(game_str, play_id, Day) %>% 
  mutate(exit_velo_y = exit_velo*cos((spray_angle*(pi/180))),
         exit_velo_x = exit_velo*sin((spray_angle*(pi/180)))) %>% 
  ungroup()

### adding primary key & rownumber
final_dataset <- final_dataset %>% 
  mutate(rownumber = row_number()) %>%
  group_by(game_str, play_id, Day) %>% 
  mutate(primary_key = first(rownumber)) %>% 
  ungroup() #### roughly 100 rows per play

#making a csv. For the models we make later, they will be too big to load
#everything in at once
write_csv(final_dataset, file = "final_dataset.csv")

#separating tables by league_level
# list_of_tables <- split(example, example$HomeTeam)
# 
# for (level in unique(example$HomeTeam)){
#   assign(glue::glue("example_{level}"),list_of_tables[[as.character(level)]])
# }
# 
# 
# 
