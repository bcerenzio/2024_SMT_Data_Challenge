library(tidyverse)
library(ranger)

#Creating Fielder ID and Position Columns
final_dataset_1 <- final_dataset %>% 
  mutate(fielder_id = case_when(
    !is.na(left_field) ~ left_field,
    !is.na(center_field) ~ center_field,
    !is.na(right_field) ~ right_field,
    .default = NA #if all 3 are NA and fielder ID isn't captured
  ),
        fielder_position = case_when(
          player_position.x == 7 ~ "LF",
          player_position.x == 8 ~ "CF",
          player_position.x == 9 ~ "RF"
        ))

#### Filtering Data at Point of Contact as well as Home Team Fielding ####
oaa_data <- final_dataset_1 %>% 
  filter(event_code == 4, top_bottom == "top")

### Finding Catch Probability at the point of contact ####
load('Catch_Prob_Model.RData')

oaa_data <- oaa_data %>% 
  mutate(catch_probability = predict(catch_prob_model, oaa_data)$predictions[,2],
         OAA = flyout - catch_probability) %>% 
  mutate(OAA = OAA - mean(OAA)) #making the average OAA 0

oaa_data %>% ggplot(aes(catch_probability)) + 
  geom_density() +
  scale_x_continuous(breaks = seq(0,1,0.1))



oaa_data <- oaa_data %>% 
  group_by(fielder_id, fielder_position, Level, Season) %>% 
  reframe(fielder_id, fielder_position, OAA = sum(OAA), Level, Season,
          plays = n(), OAA_Per_Play = sum(OAA)/n()) %>% 
  distinct() %>% 
  filter(!is.na(fielder_id))


oaa_data <- oaa_data %>% 
  mutate(oaa_percentile = round(percent_rank(OAA),4)*100,
         oaa_per_play_percentile = round(percent_rank(OAA_Per_Play),4)*100) %>%
  mutate(OAA = round(OAA,2),
         OAA_Per_Play = round(OAA_Per_Play, 2))

oaa_data <- oaa_data %>% arrange(desc(oaa_percentile))

write_csv(oaa_data, "oaa_data.csv")

### Route Statistic ###

route_data <- final_dataset_1 %>% 
  filter(top_bottom == "top", timestamp >= timestamp_at_contact)

route_data <- route_data %>% 
  mutate(catch_probability = predict(catch_prob_model, route_data)$predictions[,2]) %>% 
  group_by(primary_key) %>% 
  mutate(diff_catch_prob = last(catch_probability) - first(catch_probability)) %>% 
  ungroup()

route_data <- route_data %>% 
  filter(event_code == 4) %>% 
  mutate(diff_catch_prob = diff_catch_prob - mean(diff_catch_prob)) #making average route score 0

route_data %>% ggplot(aes(diff_catch_prob)) + 
  geom_density()

route_data <- route_data %>% 
  group_by(fielder_id, fielder_position, Level, Season) %>% 
  reframe(fielder_id, fielder_position, route_score = sum(diff_catch_prob), Level, Season,
          plays = n(), Route_Score_Per_Play = sum(diff_catch_prob)/n()) %>% 
  distinct() %>% 
  filter(!is.na(fielder_id))


route_data <- route_data %>% 
  mutate(route_percentile = round(percent_rank(route_score),4)*100,
         route_score_per_play_percentile = round(percent_rank(Route_Score_Per_Play),4)*100) %>%
  mutate(route_score = round(route_score,2),
         Route_Score_Per_Play = round(Route_Score_Per_Play, 2))

route_data <- route_data %>% arrange(desc(route_percentile))

write_csv(route_data, "route_data.csv")
