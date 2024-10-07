library(ranger)
library(tidymodels)
library(tidyverse)
set.seed(101)

final_dataset <- read_csv('final_dataset.csv')

location_training_AAA <- read_csv("location_training_AAA1.csv", show_col_types = FALSE)
location_training_AA <- read_csv("location_training_AA1.csv", show_col_types = FALSE)
location_training_Aplus <- read_csv("location_training_Aplus1.csv", show_col_types = FALSE)
location_training_A <- read_csv("location_training_A1.csv", show_col_types = FALSE)
location_test_AAA <- read_csv("location_test_AAA1.csv", show_col_types = FALSE)
location_test_AA <- read_csv("location_test_AA1.csv", show_col_types = FALSE)
location_test_Aplus <- read_csv("location_test_Aplus1.csv", show_col_types = FALSE)
location_test_A <- read_csv("location_test_A1.csv", show_col_types = FALSE)

### Note: When creating these models, I assumed that each levels corresponded to the AAA, AA, A+, and A levels
### in the minor league system, however, I have been told that this is not the case. Instead of changing these model
### and data names, here is a key of which level corresponds to which from the orignal data:

# AAA -> 4A/Home4A
# AA -> 3A/Home3A
# Aplus -> 2A/Home2A
# A -> 1A/Home1A
#

#### Tuning for AAA####
dat_AAA <- final_dataset %>%
  reframe(game_str, play_id, timestamp, timestamp_at_contact,ball_position_x, ball_position_y, 
          max_fielder_speed, time_remaining, time_in_play, field_y, field_x, distance_from_ball, 
          final_ball_position_x, final_ball_position_y, Level = Level, pitch_type, prev_field_x, prev_field_y,
          prev_distance, Season, player_position.x, flyout, dist_x, prev_dist_x,dist_y, prev_dist_y, primary_key,
          exit_velo,LA, spray_angle) %>% 
  filter(timestamp >= timestamp_at_contact, Level == 'AAA') %>% 
  drop_na()

write_csv(dat_AAA, file = "dat_AAA.csv")

field_split_AAA <- group_initial_split(dat_AAA, primary_key,prop = (nrow(dat_AAA)-1000)/nrow(dat_AAA), strata = player_position.x)

location_training_AAA <- training(field_split_AAA)

write_csv(location_training_AAA, file = "location_training_AAA1.csv")

location_test_AAA <- testing(field_split_AAA)
location_test_AAA <- location_test_AAA %>% 
  group_by(primary_key) %>% 
  mutate(field_x1 = NA,
         dist_x1 = NA,
         diff_weights = seq(1,5,length.out = n())) %>% 
  ungroup()

write_csv(location_test_AAA, file = 'location_test_AAA1.csv')

find_weight_AAAx <- function(par){
  if(par[1] > 1){par[1] <- 1} ### optimize function was spawning errors that at a certain point splitweights were outside the bounds [0,1] for reasons I can't explain
  if(par[1] < 0){par[1] <- 0.0001}### this is an extra level of precaution
  if(par[2] > 1){par[2] <- 1}
  if(par[2] < 0){par[2] <- 0.0001}
  if(par[3] > 1){par[3] <- 1}
  if(par[3] < 0){par[3] <- 0.0001}
  if(par[4] > 1){par[4] <- 1}
  if(par[4] < 0){par[4] <- 0.0001}
  if(par[5] > 1){par[5] <- 1}
  if(par[5] < 0){par[5] <- 0.0001}
  if(par[6] > 1){par[6] <- 1}
  if(par[6] < 0){par[6] <- 0.0001}
  if(par[7] > 1){par[7] <- 1}
  if(par[7] < 0){par[7] <- 0.0001}
  model2 <- ranger(field_x ~ prev_field_x + 
                     prev_distance + time_remaining + 
                     final_ball_position_y + 
                     time_in_play +
                     prev_field_y +
                     prev_dist_x +
                     final_ball_position_x, 
                   data = location_training_AAA,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_AAAx,
                   split.select.weights = c(par[1],  par[2], par[3], par[4],
                                            par[5], par[6], par[7], 0),
                   num.trees = 300,
                   always.split.variables = c("final_ball_position_x"), 
                   num.threads = 0,
                   min.node.size = 10,
                   mtry = 5,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_AAA)){
    if (i > 1){
      if (location_test_AAA[i,"primary_key"] != location_test_AAA[i-1,'primary_key']){
        preds_field_position_x <- predict(model2, location_test_AAA[i,])$predictions
        location_test_AAA[i,"field_x1"] <- preds_field_position_x
        location_test_AAA[i,'dist_x1'] <- location_test_AAA[i,'final_ball_position_x'] - location_test_AAA[i, 'field_x1']
        location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x1'])^2 + (location_test_AAA[i,'dist_y'])^2)
      }else{
        location_test_AAA[i, "prev_field_x"] <- location_test_AAA[i-1, "field_x1"]
        location_test_AAA[i,"prev_distance"] <- location_test_AAA[i-1, "distance_from_ball"]
        location_test_AAA[i,"prev_dist_x"] <- location_test_AAA[i-1, "dist_x1"]
        preds_field_position_x <- predict(model2, location_test_AAA[i,])$predictions
        location_test_AAA[i,"field_x1"] <- preds_field_position_x
        location_test_AAA[i,'dist_x1'] <- location_test_AAA[i,'final_ball_position_x'] - location_test_AAA[i, 'field_x1']
        location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x1'])^2 + (location_test_AAA[i,'dist_y'])^2)
      }
    }else{
      preds_field_position_x <- predict(model2, location_test_AAA[i,])$predictions
      location_test_AAA[i,"field_x1"] <- preds_field_position_x
      location_test_AAA[i,'dist_x1'] <- location_test_AAA[i,'final_ball_position_x'] - location_test_AAA[i, 'field_x1']
      location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x1'])^2 + (location_test_AAA[i,'dist_y'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
   rmse <- sqrt(mean((location_test_AAA$field_x - location_test_AAA$field_x1)^2))
  print(par)
  print(rmse)
  return(rmse)
}

(optimized_weight_AAAx <- optim(par = c(rep(0.5,7)), find_weight_AAAx, hessian = TRUE, method = "L-BFGS-B", lower = 0.47, upper = 0.53, control = list(maxit = 5, trace = 6, factr = 0.01/.Machine$double.eps, pgtol = 1e-4, ndeps = rep(0.001,7), REPORT = 1))) #lower set to 0.0001 rather than 0 so mtry has enough variables to choose from 

# par: c(0.497953, 0.501888, 0.498008, 0.501949, 0.498038, 0.502137, 0.498037) 
# rmse  = 4.51789

split_weight_AAAx <- c(optimized_weight_AAAx$par,0)



find_randsplit_AAAx <- function(randsplit){
  randsplit <- ceiling(randsplit)
  model2 <- ranger(field_x ~ prev_field_x +
                     prev_distance + time_remaining + 
                     final_ball_position_y + 
                     time_in_play +
                     prev_field_y +
                     prev_dist_x +
                     final_ball_position_x, 
                   data = location_training_AAA,
                   splitrule = "extratrees",
                   num.random.splits = randsplit,
                   always.split.variables = c("final_ball_position_x"),
                   split.select.weights = split_weight_AAAx,
                   mtry = 5,
                   min.node.size = 10,
                   num.threads = 0,
                   num.trees = 300, #setting ntree to 300 to hopefully allow the model to develop
                   # quicker while tuning
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_AAA)){
    if (i > 1){
      if (location_test_AAA[i,"primary_key"] != location_test_AAA[i-1,'primary_key']){
        preds_field_position_x <- predict(model2, location_test_AAA[i,])$predictions
        location_test_AAA[i,"field_x1"] <- preds_field_position_x
        location_test_AAA[i,'dist_x1'] <- location_test_AAA[i,'final_ball_position_x'] - location_test_AAA[i, 'field_x1']
        location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x1'])^2 + (location_test_AAA[i,'dist_y'])^2)
      }else{
        location_test_AAA[i, "prev_field_x"] <- location_test_AAA[i-1, "field_x1"]
        location_test_AAA[i,"prev_distance"] <- location_test_AAA[i-1, "distance_from_ball"]
        location_test_AAA[i,"prev_dist_x"] <- location_test_AAA[i-1, "dist_x1"]
        preds_field_position_x <- predict(model2, location_test_AAA[i,])$predictions
        location_test_AAA[i,"field_x1"] <- preds_field_position_x
        location_test_AAA[i,'dist_x1'] <- location_test_AAA[i,'final_ball_position_x'] - location_test_AAA[i, 'field_x1']
        location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x1'])^2 + (location_test_AAA[i,'dist_y'])^2)
      }
    }else{
      preds_field_position_x <- predict(model2, location_test_AAA[i,])$predictions
      location_test_AAA[i,"field_x1"] <- preds_field_position_x
      location_test_AAA[i,'dist_x1'] <- location_test_AAA[i,'final_ball_position_x'] - location_test_AAA[i, 'field_x1']
      location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x1'])^2 + (location_test_AAA[i,'dist_y'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  rmse <- sqrt(mean((location_test_AAA$field_x - location_test_AAA$field_x1)^2))
  print(randsplit)
  print(rmse)
  return(rmse)
}

rs_AAAx <- tibble(
  randsplit = 2:20,
  rmse = map_dbl(2:20, find_randsplit_AAAx)
)

rs_AAAx <- rs_AAAx %>%
  slice_min(rmse, n = 1)

print(rs_AAAx)

optimized_rs_AAAx <- rs_AAAx$randsplit # optimizes at 6, rmse = 4.75


find_minnodesize_AAAx <- function(node){
  node <- floor(node)
  model2 <- ranger(field_x ~ prev_field_x + 
                     prev_distance + time_remaining + 
                     final_ball_position_y + 
                     time_in_play +
                     prev_field_y +
                     prev_dist_x +
                     final_ball_position_x, 
                   data = location_training_AAA,
                   num.trees = 300,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_AAAx,
                   split.select.weights = split_weight_AAAx,
                   always.split.variables = c("final_ball_position_x"),
                   num.threads = 0,
                   min.node.size = node,
                   mtry = 5,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_AAA)){
    if (i > 1){
      if (location_test_AAA[i,"primary_key"] != location_test_AAA[i-1,'primary_key']){
        preds_field_position_x <- predict(model2, location_test_AAA[i,])$predictions
        location_test_AAA[i,"field_x1"] <- preds_field_position_x
        location_test_AAA[i,'dist_x1'] <- location_test_AAA[i,'final_ball_position_x'] - location_test_AAA[i, 'field_x1']
        location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x1'])^2 + (location_test_AAA[i,'dist_y'])^2)
      }else{
        location_test_AAA[i, "prev_field_x"] <- location_test_AAA[i-1, "field_x1"]
        location_test_AAA[i,"prev_distance"] <- location_test_AAA[i-1, "distance_from_ball"]
        location_test_AAA[i,"prev_dist_x"] <- location_test_AAA[i-1, "dist_x1"]
        preds_field_position_x <- predict(model2, location_test_AAA[i,])$predictions
        location_test_AAA[i,"field_x1"] <- preds_field_position_x
        location_test_AAA[i,'dist_x1'] <- location_test_AAA[i,'final_ball_position_x'] - location_test_AAA[i, 'field_x1']
        location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x1'])^2 + (location_test_AAA[i,'dist_y'])^2)
      }
    }else{
      preds_field_position_x <- predict(model2, location_test_AAA[i,])$predictions
      location_test_AAA[i,"field_x1"] <- preds_field_position_x
      location_test_AAA[i,'dist_x1'] <- location_test_AAA[i,'final_ball_position_x'] - location_test_AAA[i, 'field_x1']
      location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x1'])^2 + (location_test_AAA[i,'dist_y'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  rmse <- sqrt(mean((location_test_AAA$field_x - location_test_AAA$field_x1)^2))
  print(node)
  print(rmse)
  return(rmse)
}

(optimized_minnodesize_AAAx <- optimize(find_minnodesize_AAAx, c(6,25), tol = 1)) #optimized at 21, rmse = 5.00
optimized_node_AAAx <- floor(optimized_minnodesize_AAAx$minimum) 


find_minbucket_AAAx <- function(bucket){
  bucket <- floor(bucket)
  model2 <- ranger(field_x ~ prev_field_x + 
                     prev_distance + time_remaining + 
                     final_ball_position_y + 
                     time_in_play +
                     prev_field_y +
                     prev_dist_x +
                     final_ball_position_x, 
                   data = location_training_AAA,
                   num.trees = 300,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_AAAx,
                   min.node.size = optimized_node_AAAx,
                   split.select.weights = split_weight_AAAx,
                  always.split.variables = c("final_ball_position_x"),
                   mtry = 5,
                   min.bucket = bucket,
                   num.threads = 0,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_AAA)){
    if (i > 1){
      if (location_test_AAA[i,"primary_key"] != location_test_AAA[i-1,'primary_key']){
        preds_field_position_x <- predict(model2, location_test_AAA[i,])$predictions
        location_test_AAA[i,"field_x1"] <- preds_field_position_x
        location_test_AAA[i,'dist_x1'] <- location_test_AAA[i,'final_ball_position_x'] - location_test_AAA[i, 'field_x1']
        location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x1'])^2 + (location_test_AAA[i,'dist_y'])^2)
      }else{
        location_test_AAA[i, "prev_field_x"] <- location_test_AAA[i-1, "field_x1"]
        location_test_AAA[i,"prev_distance"] <- location_test_AAA[i-1, "distance_from_ball"]
        location_test_AAA[i,"prev_dist_x"] <- location_test_AAA[i-1, "dist_x1"]
        preds_field_position_x <- predict(model2, location_test_AAA[i,])$predictions
        location_test_AAA[i,"field_x1"] <- preds_field_position_x
        location_test_AAA[i,'dist_x1'] <- location_test_AAA[i,'final_ball_position_x'] - location_test_AAA[i, 'field_x1']
        location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x1'])^2 + (location_test_AAA[i,'dist_y'])^2)
      }
    }else{
      preds_field_position_x <- predict(model2, location_test_AAA[i,])$predictions
      location_test_AAA[i,"field_x1"] <- preds_field_position_x
      location_test_AAA[i,'dist_x1'] <- location_test_AAA[i,'final_ball_position_x'] - location_test_AAA[i, 'field_x1']
      location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x1'])^2 + (location_test_AAA[i,'dist_y'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  rmse <- sqrt(mean((location_test_AAA$field_x - location_test_AAA$field_x1)^2))
  print(bucket)
  print(rmse)
  return(rmse)
}

(optimized_minbucket_AAAx <- optimize(find_minbucket_AAAx, c(1,floor(optimized_node_AAAx/2)+1), tol = 1)) # optimized at 7
optimized_minbucket_AAAx <- floor(optimized_minbucket_AAAx$minimum) # rmse = 5.04


find_ntree_AAAx <- function(ntree){
  model2 <- ranger(field_x ~ prev_field_x + 
                     prev_distance + time_remaining  + 
                     final_ball_position_y + 
                      time_in_play +
                     prev_field_y +
                     prev_dist_x +
                     final_ball_position_x, 
                   data = location_training_AAA,
                   num.trees = ntree,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_AAAx,
                   min.node.size = optimized_node_AAAx,
                   split.select.weights = split_weight_AAAx,
                   always.split.variables = c("final_ball_position_x"),
                   mtry = 5,
                   min.bucket = optimized_minbucket_AAAx,
                   num.threads = 0,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_AAA)){
    if (i > 1){
      if (location_test_AAA[i,"primary_key"] != location_test_AAA[i-1,'primary_key']){
        preds_field_position_x <- predict(model2, location_test_AAA[i,])$predictions
        location_test_AAA[i,"field_x1"] <- preds_field_position_x
        location_test_AAA[i,'dist_x1'] <- location_test_AAA[i,'final_ball_position_x'] - location_test_AAA[i, 'field_x1']
        location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x1'])^2 + (location_test_AAA[i,'dist_y'])^2)
      }else{
        location_test_AAA[i, "prev_field_x"] <- location_test_AAA[i-1, "field_x1"]
        location_test_AAA[i,"prev_distance"] <- location_test_AAA[i-1, "distance_from_ball"]
        location_test_AAA[i,"prev_dist_x"] <- location_test_AAA[i-1, "dist_x1"]
        preds_field_position_x <- predict(model2, location_test_AAA[i,])$predictions
        location_test_AAA[i,"field_x1"] <- preds_field_position_x
        location_test_AAA[i,'dist_x1'] <- location_test_AAA[i,'final_ball_position_x'] - location_test_AAA[i, 'field_x1']
        location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x1'])^2 + (location_test_AAA[i,'dist_y'])^2)
      }
    }else{
      preds_field_position_x <- predict(model2, location_test_AAA[i,])$predictions
      location_test_AAA[i,"field_x1"] <- preds_field_position_x
      location_test_AAA[i,'dist_x1'] <- location_test_AAA[i,'final_ball_position_x'] - location_test_AAA[i, 'field_x1']
      location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x1'])^2 + (location_test_AAA[i,'dist_y'])^2)
    }
    if(i %% 250 == 0){
      print(paste("Row ",i))
    }
  }
  rmse <- sqrt(mean((location_test_AAA$field_x - location_test_AAA$field_x1)^2))
  print(ntree)
  print(rmse)
  return(rmse)
}

tree_perf_AAAx <- tibble(
  ntree = 1:6*100,
  rmse_value = map_dbl(1:6*100, find_ntree_AAAx) 
)




tree_perf_AAAx %>% ggplot(aes(ntree,rmse_value)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  ylab("RMSE") +
  scale_x_continuous(breaks = seq(0,600,200)) #optimizes at 400


field_model_AAAx <- ranger(field_x ~ prev_field_x + 
                             prev_distance + time_remaining  + final_ball_position_y + 
                             time_in_play +
                             prev_field_y +
                             prev_dist_x +
                             final_ball_position_x, 
                           data = location_training_AAA,
                           num.trees = 200, #using 200 trees to save memory and had similar rmse to 400 trees
                           splitrule = "extratrees",
                           split.select.weights = split_weight_AAAx,
                           always.split.variables = c("final_ball_position_x"),
                           num.random.splits = optimized_rs_AAAx, 
                           min.node.size = optimized_node_AAAx, 
                           mtry = 5,
                           min.bucket = optimized_minbucket_AAAx, 
                           importance = "permutation",
                           scale.permutation.importance = TRUE,
                           seed = 101)

for (i in 1:nrow(location_test_AAA)){
  if (i > 1){
    if (location_test_AAA[i,"primary_key"] != location_test_AAA[i-1,'primary_key']){
      preds_field_position_x <- predict(field_model_AAAx, location_test_AAA[i,])$predictions
      location_test_AAA[i,"field_x1"] <- preds_field_position_x
      location_test_AAA[i,'dist_x1'] <- location_test_AAA[i,'final_ball_position_x'] - location_test_AAA[i, 'field_x1']
      location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x1'])^2 + (location_test_AAA[i,'dist_y'])^2)
    }else{
      location_test_AAA[i, "prev_field_x"] <- location_test_AAA[i-1, "field_x1"]
      location_test_AAA[i,"prev_distance"] <- location_test_AAA[i-1, "distance_from_ball"]
      location_test_AAA[i,"prev_dist_x"] <- location_test_AAA[i-1, "dist_x1"]
      preds_field_position_x <- predict(field_model_AAAx, location_test_AAA[i,])$predictions
      location_test_AAA[i,"field_x1"] <- preds_field_position_x
      location_test_AAA[i,'dist_x1'] <- location_test_AAA[i,'final_ball_position_x'] - location_test_AAA[i, 'field_x1']
      location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x1'])^2 + (location_test_AAA[i,'dist_y'])^2)
    }
  }else{
    preds_field_position_x <- predict(field_model_AAAx, location_test_AAA[i,])$predictions
    location_test_AAA[i,"field_x1"] <- preds_field_position_x
    location_test_AAA[i,'dist_x1'] <- location_test_AAA[i,'final_ball_position_x'] - location_test_AAA[i, 'field_x1']
    location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x1'])^2 + (location_test_AAA[i,'dist_y'])^2)
  }
  if(i %% 250 == 0){
    print(i)
  }
}

location_test_AAA <- location_test_AAA  %>% 
  mutate(diff_x = abs(field_x - field_x1))

rmse(location_test_AAA, field_x, field_x1) #4.99

location_test_AAA %>% ggplot(aes(y = diff_x, x = time_in_play,group = primary_key, color = time_remaining)) +
  geom_path()+
  scale_color_continuous(low = 'red', high = 'green',name = "Time Remaining (ms)") +
  labs(x = "Time in Play (ms)", y = "Difference in Predicted Field X vs Actual Field X (ft)",
       title = "Difference in Field X coordinates (ft) vs \n Time in Play (ms) (4A Level)") +
  theme_bw()

location_test_AAA <- read_csv("location_test_AAA1.csv", show_col_types = FALSE)

#### Final Field Model AAA X ####

field_model_AAAx <- ranger(field_x ~ prev_field_x +  
                           prev_distance + time_remaining  + final_ball_position_y + 
                           time_in_play +
                           prev_field_y +
                           prev_dist_x +
                          final_ball_position_x, 
                         data = dat_AAA,
                         num.trees = 200, #using 200 trees to save memory
                         splitrule = "extratrees",
                         split.select.weights = split_weight_AAAx,
                         always.split.variables = c("final_ball_position_x"),
                         num.random.splits = optimized_rs_AAAx, #6
                         min.node.size = optimized_node_AAAx, #21
                         mtry = 5, 
                         min.bucket = optimized_minbucket_AAAx, #7
                         importance = "permutation",
                         scale.permutation.importance = TRUE,
                         seed = 101)


for (i in 1:nrow(location_test_AAA)){
  if (i > 1){
    if (location_test_AAA[i,"primary_key"] != location_test_AAA[i-1,'primary_key']){
      preds_field_position_x <- predict(field_model_AAAx, location_test_AAA[i,])$predictions
      location_test_AAA[i,"field_x1"] <- preds_field_position_x
      location_test_AAA[i,'dist_x1'] <- location_test_AAA[i,'final_ball_position_x'] - location_test_AAA[i, 'field_x1']
      location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x1'])^2 + (location_test_AAA[i,'dist_y'])^2)
    }else{
      location_test_AAA[i, "prev_field_x"] <- location_test_AAA[i-1, "field_x1"]
      location_test_AAA[i,"prev_distance"] <- location_test_AAA[i-1, "distance_from_ball"]
      location_test_AAA[i,"prev_dist_x"] <- location_test_AAA[i-1, "dist_x1"]
      preds_field_position_x <- predict(field_model_AAAx, location_test_AAA[i,])$predictions
      location_test_AAA[i,"field_x1"] <- preds_field_position_x
      location_test_AAA[i,'dist_x1'] <- location_test_AAA[i,'final_ball_position_x'] - location_test_AAA[i, 'field_x1']
      location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x1'])^2 + (location_test_AAA[i,'dist_y'])^2)
      # 
      # location_test_AAA <- location_test_AAA %>%
      #   mutate(dist_x = final_ball_position_x - field_x1,
      #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
    }
  }else{
    preds_field_position_x <- predict(field_model_AAAx, location_test_AAA[i,])$predictions
    location_test_AAA[i,"field_x1"] <- preds_field_position_x
    location_test_AAA[i,'dist_x1'] <- location_test_AAA[i,'final_ball_position_x'] - location_test_AAA[i, 'field_x1']
    location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x1'])^2 + (location_test_AAA[i,'dist_y'])^2)
  }
  if(i %% 250 == 0){
    print(i)
  }
}


location_test_AAA <- location_test_AAA  %>% 
  mutate(diff_x = abs(field_x - field_x1))

rmse(location_test_AAA, field_x, field_x1) # 4.20

importance_AAAx <- tibble(feature = field_model_AAAx$forest$independent.variable.names, importance = pull(as.data.frame(field_model_AAAx$variable.importance)),
                          split_weight = split_weight_AAAx) %>% 
  arrange(desc(importance))

print(importance_AAAx)

#### Tuning for AA ####
dat_AA <- final_dataset %>%
  reframe(game_str, play_id, timestamp, timestamp_at_contact,ball_position_x, ball_position_y, 
          max_fielder_speed, time_remaining, time_in_play, field_y, field_x, distance_from_ball, 
          final_ball_position_x, final_ball_position_y, Level = Level, pitch_type, prev_field_x, prev_field_y,
          prev_distance, Season, player_position.x, flyout, dist_x, prev_dist_x,dist_y, prev_dist_y, primary_key,
          exit_velo,LA, spray_angle) %>% 
  filter(timestamp >= timestamp_at_contact, Level == 'AA') %>% 
  drop_na()

write_csv(dat_AA, file = "dat_AA.csv")

field_split_AA <- group_initial_split(dat_AA, primary_key,prop = (nrow(dat_AA) - 1000)/nrow(dat_AA), strata = player_position.x)

location_training_AA <- training(field_split_AA)

write_csv(location_training_AA, file = "location_training_AA1.csv")

location_test_AA <- testing(field_split_AA)
location_test_AA <- location_test_AA %>% 
  mutate(field_x1 = NA,
         dist_x1 = NA)

write_csv(location_test_AA, file = 'location_test_AA1.csv')

find_randsplit_AAx <- function(randsplit){
  randsplit <- ceiling(randsplit)
  model2 <- ranger(field_x ~  prev_field_x + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_y + 
                     time_in_play +
                     prev_field_y +
                     prev_dist_x +
                     final_ball_position_x, 
                   data = location_training_AA,
                   split.select.weights = c(rep(0.5,7),0),
                   num.threads = 0,
                   always.split.variables = c("final_ball_position_x"),
                   splitrule = "extratrees",
                   num.random.splits = randsplit,
                   mtry = 5,
                   num.trees = 300, 
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_AA)){
    if (i > 1){
      if (location_test_AA[i,"primary_key"] != location_test_AA[i-1,'primary_key']){
        preds_field_position_x <- predict(model2, location_test_AA[i,])$predictions
        location_test_AA[i,"field_x1"] <- preds_field_position_x
        location_test_AA[i,'dist_x1'] <- location_test_AA[i,'final_ball_position_x'] - location_test_AA[i, 'field_x1']
        location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x1'])^2 + (location_test_AA[i,'dist_y'])^2)
      }else{
        location_test_AA[i, "prev_field_x"] <- location_test_AA[i-1, "field_x1"]
        location_test_AA[i,"prev_distance"] <- location_test_AA[i-1, "distance_from_ball"]
        location_test_AA[i,"prev_dist_x"] <- location_test_AA[i-1, "dist_x1"]
        preds_field_position_x <- predict(model2, location_test_AA[i,])$predictions
        location_test_AA[i,"field_x1"] <- preds_field_position_x
        location_test_AA[i,'dist_x1'] <- location_test_AA[i,'final_ball_position_x'] - location_test_AA[i, 'field_x1']
        location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x1'])^2 + (location_test_AA[i,'dist_y'])^2)
      }
    }else{
      preds_field_position_x <- predict(model2, location_test_AA[i,])$predictions
      location_test_AA[i,"field_x1"] <- preds_field_position_x
      location_test_AA[i,'dist_x1'] <- location_test_AA[i,'final_ball_position_x'] - location_test_AA[i, 'field_x1']
      location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x1'])^2 + (location_test_AA[i,'dist_y'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  rmse <- sqrt(mean((location_test_AA$field_x - location_test_AA$field_x1)^2))
  print(randsplit)
  print(rmse)
  return(rmse)
}

rs_AAx <- tibble(
  randsplit = 2:20,
  rmse = map_dbl(2:20, find_randsplit_AAx)
)

rs_AAx <- rs_AAx %>% 
  slice_min(rmse, n = 1) 

print(rs_AAx)

optimized_rs_AAx <- rs_AAx$randsplit #optimizes at 11, rmse = 6.52



find_weight_AAx <- function(par){
  if(par[1] > 1){par[1] <- 1} ### optimize function was spawning errors that at a certain point splitweights were outside the bounds [0,1] for reasons I can't explain
  if(par[1] < 0){par[1] <- 0.0001}### this is an extra level of precaution
  if(par[2] > 1){par[2] <- 1}
  if(par[2] < 0){par[2] <- 0.0001}
  if(par[3] > 1){par[3] <- 1}
  if(par[3] < 0){par[3] <- 0.0001}
  if(par[4] > 1){par[4] <- 1}
  if(par[4] < 0){par[4] <- 0.0001}
  if(par[5] > 1){par[5] <- 1}
  if(par[5] < 0){par[5] <- 0.0001}
  if(par[6] > 1){par[6] <- 1}
  if(par[6] < 0){par[6] <- 0.0001}
  if(par[7] > 1){par[7] <- 1}
  if(par[7] < 0){par[7] <- 0.0001}
  model2 <- ranger(field_x ~ prev_field_x + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_y + 
                     time_in_play +
                     prev_field_y +
                     prev_dist_x +
                     final_ball_position_x, 
                   data = location_training_AA,
                   split.select.weights = c(par[1],  par[2], par[3], par[4],
                                            par[5], par[6], par[7],0),
                   num.trees = 300,
                   always.split.variables = c("final_ball_position_x"), 
                   num.threads = 0,
                   splitrule = 'extratrees',
                   num.random.splits = optimized_rs_AAx,
                   mtry = 5,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_AA)){
    if (i > 1){
      if (location_test_AA[i,"primary_key"] != location_test_AA[i-1,'primary_key']){
        preds_field_position_x <- predict(model2, location_test_AA[i,])$predictions
        location_test_AA[i,"field_x1"] <- preds_field_position_x
        location_test_AA[i,'dist_x1'] <- location_test_AA[i,'final_ball_position_x'] - location_test_AA[i, 'field_x1']
        location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x1'])^2 + (location_test_AA[i,'dist_y'])^2)
      }else{
        location_test_AA[i, "prev_field_x"] <- location_test_AA[i-1, "field_x1"]
        location_test_AA[i,"prev_distance"] <- location_test_AA[i-1, "distance_from_ball"]
        location_test_AA[i,"prev_dist_x"] <- location_test_AA[i-1, "dist_x1"]
        preds_field_position_x <- predict(model2, location_test_AA[i,])$predictions
        location_test_AA[i,"field_x1"] <- preds_field_position_x
        location_test_AA[i,'dist_x1'] <- location_test_AA[i,'final_ball_position_x'] - location_test_AA[i, 'field_x1']
        location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x1'])^2 + (location_test_AA[i,'dist_y'])^2)
      }
    }else{
      preds_field_position_x <- predict(model2, location_test_AA[i,])$predictions
      location_test_AA[i,"field_x1"] <- preds_field_position_x
      location_test_AA[i,'dist_x1'] <- location_test_AA[i,'final_ball_position_x'] - location_test_AA[i, 'field_x1']
      location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x1'])^2 + (location_test_AA[i,'dist_y'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  #preds <- predict(model2, location_test_AA)$predictions
  # location_test_AA <- location_test_AA %>% 
  #  mutate(diffs = (field_x - field_x1)*diff_weights)
  rmse <- sqrt(mean((location_test_AA$field_x - location_test_AA$field_x1)^2))
  print(par)
  print(rmse)
  return(rmse)
}

(optimized_weight_AAx <- optim(par = c(rep(0.5,7)), find_weight_AAx, hessian = TRUE, method = "L-BFGS-B", lower = 0.4, upper = 0.6, control = list(maxit = 5, trace = 6, factr = 0.01/.Machine$double.eps,pgtol = 1e-4, ndeps = rep(0.01,7), REPORT = 1))) #lower set to 0.0001 rather than 0 so mtry has enough variables to choose from 

#par: 0.5 0.5 0.5 0.5 0.5 0.5 0.5

#rmse = 6.524618

split_weight_AAx <- c(optimized_weight_AAx$par,1)


find_minnodesize_AAx <- function(node){
  node <- floor(node)
  model2 <- ranger(field_x ~ prev_field_x + 
                     prev_distance + time_remaining + 
                     final_ball_position_y + 
                     time_in_play +
                     prev_field_y +
                     prev_dist_x +
                     final_ball_position_x, 
                   data = location_training_AA,
                   split.select.weights = split_weight_AAx,
                   num.threads = 0,
                   always.split.variables = c("final_ball_position_x"),
                   num.trees = 300,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_AAx,
                   min.node.size = node,
                   mtry = 5,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_AA)){
    if (i > 1){
      if (location_test_AA[i,"primary_key"] != location_test_AA[i-1,'primary_key']){
        preds_field_position_x <- predict(model2, location_test_AA[i,])$predictions
        location_test_AA[i,"field_x1"] <- preds_field_position_x
        location_test_AA[i,'dist_x1'] <- location_test_AA[i,'final_ball_position_x'] - location_test_AA[i, 'field_x1']
        location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x1'])^2 + (location_test_AA[i,'dist_y'])^2)
      }else{
        location_test_AA[i, "prev_field_x"] <- location_test_AA[i-1, "field_x1"]
        location_test_AA[i,"prev_distance"] <- location_test_AA[i-1, "distance_from_ball"]
        location_test_AA[i,"prev_dist_x"] <- location_test_AA[i-1, "dist_x1"]
        preds_field_position_x <- predict(model2, location_test_AA[i,])$predictions
        location_test_AA[i,"field_x1"] <- preds_field_position_x
        location_test_AA[i,'dist_x1'] <- location_test_AA[i,'final_ball_position_x'] - location_test_AA[i, 'field_x1']
        location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x1'])^2 + (location_test_AA[i,'dist_y'])^2)
      }
    }else{
      preds_field_position_x <- predict(model2, location_test_AA[i,])$predictions
      location_test_AA[i,"field_x1"] <- preds_field_position_x
      location_test_AA[i,'dist_x1'] <- location_test_AA[i,'final_ball_position_x'] - location_test_AA[i, 'field_x1']
      location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x1'])^2 + (location_test_AA[i,'dist_y'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  rmse <- sqrt(mean((location_test_AA$field_x - location_test_AA$field_x1)^2))
  print(node)
  print(rmse)
  return(rmse)
}

(optimized_minnodesize_AAx <- optimize(find_minnodesize_AAx, c(6,25), tol = 1))
optimized_node_AAx <- floor(optimized_minnodesize_AAx$minimum) #optimized at 13, rmse = 6.83


find_minbucket_AAx <- function(bucket){
  bucket <- floor(bucket)
  model2 <- ranger(field_x ~ prev_field_x +  
                     prev_distance + time_remaining + 
                     final_ball_position_y + 
                     time_in_play +
                     prev_field_y +
                     prev_dist_x +
                     final_ball_position_x, 
                   data = location_training_AA,
                   split.select.weights = split_weight_AAx,
                   num.threads = 0,
                   always.split.variables = c("final_ball_position_x"),
                   num.trees = 300,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_AAx,
                   min.node.size = optimized_node_AAx,
                   mtry = 5,
                   min.bucket = bucket,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_AA)){
    if (i > 1){
      if (location_test_AA[i,"primary_key"] != location_test_AA[i-1,'primary_key']){
        preds_field_position_x <- predict(model2, location_test_AA[i,])$predictions
        location_test_AA[i,"field_x1"] <- preds_field_position_x
        location_test_AA[i,'dist_x1'] <- location_test_AA[i,'final_ball_position_x'] - location_test_AA[i, 'field_x1']
        location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x1'])^2 + (location_test_AA[i,'dist_y'])^2)
      }else{
        location_test_AA[i, "prev_field_x"] <- location_test_AA[i-1, "field_x1"]
        location_test_AA[i,"prev_distance"] <- location_test_AA[i-1, "distance_from_ball"]
        location_test_AA[i,"prev_dist_x"] <- location_test_AA[i-1, "dist_x1"]
        preds_field_position_x <- predict(model2, location_test_AA[i,])$predictions
        location_test_AA[i,"field_x1"] <- preds_field_position_x
        location_test_AA[i,'dist_x1'] <- location_test_AA[i,'final_ball_position_x'] - location_test_AA[i, 'field_x1']
        location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x1'])^2 + (location_test_AA[i,'dist_y'])^2)
      }
    }else{
      preds_field_position_x <- predict(model2, location_test_AA[i,])$predictions
      location_test_AA[i,"field_x1"] <- preds_field_position_x
      location_test_AA[i,'dist_x1'] <- location_test_AA[i,'final_ball_position_x'] - location_test_AA[i, 'field_x1']
      location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x1'])^2 + (location_test_AA[i,'dist_y'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  rmse <- sqrt(mean((location_test_AA$field_x - location_test_AA$field_x1)^2))
  print(bucket)
  print(rmse)
  return(rmse)
}

(optimized_minbucket_AAx <- optimize(find_minbucket_AAx, c(1,floor(optimized_node_AAx/2)+1), tol = 1)) #optimizes at 2, rmse = 6.81
optimized_minbucket_AAx <- floor(optimized_minbucket_AAx$minimum)




find_ntree_AAx <- function(ntree){
  location_test_AA <- read_csv("location_test_AA1.csv", show_col_types = FALSE)
  model2 <- ranger(field_x ~ prev_field_x + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_y + 
                     time_in_play +
                     prev_field_y +
                     prev_dist_x +
                     final_ball_position_x, 
                   data = location_training_AA,
                   split.select.weights = split_weight_AAx,
                   num.threads = 0,
                   always.split.variables = c("final_ball_position_x"),
                   num.trees = ntree,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_AAx,
                   min.node.size = optimized_node_AAx,
                   mtry = 5,
                   min.bucket = optimized_minbucket_AAx,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_AA)){
    if (i > 1){
      if (location_test_AA[i,"primary_key"] != location_test_AA[i-1,'primary_key']){
        preds_field_position_x <- predict(model2, location_test_AA[i,])$predictions
        location_test_AA[i,"field_x1"] <- preds_field_position_x
        location_test_AA[i,'dist_x1'] <- location_test_AA[i,'final_ball_position_x'] - location_test_AA[i, 'field_x1']
        location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x1'])^2 + (location_test_AA[i,'dist_y'])^2)
      }else{
        location_test_AA[i, "prev_field_x"] <- location_test_AA[i-1, "field_x1"]
        location_test_AA[i,"prev_distance"] <- location_test_AA[i-1, "distance_from_ball"]
        location_test_AA[i,"prev_dist_x"] <- location_test_AA[i-1, "dist_x1"]
        preds_field_position_x <- predict(model2, location_test_AA[i,])$predictions
        location_test_AA[i,"field_x1"] <- preds_field_position_x
        location_test_AA[i,'dist_x1'] <- location_test_AA[i,'final_ball_position_x'] - location_test_AA[i, 'field_x1']
        location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x1'])^2 + (location_test_AA[i,'dist_y'])^2)
      }
    }else{
      preds_field_position_x <- predict(model2, location_test_AA[i,])$predictions
      location_test_AA[i,"field_x1"] <- preds_field_position_x
      location_test_AA[i,'dist_x1'] <- location_test_AA[i,'final_ball_position_x'] - location_test_AA[i, 'field_x1']
      location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x1'])^2 + (location_test_AA[i,'dist_y'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  rmse <- sqrt(mean((location_test_AA$field_x - location_test_AA$field_x1)^2))
  print(ntree)
  print(rmse)
  return(rmse)
}

tree_perf_AAx <- tibble(
  ntree = 1:6*100,
  rmse_value = map_dbl(1:6*100, find_ntree_AAx)
)




tree_perf_AAx %>% ggplot(aes(ntree,rmse_value)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  ylab("RMSE") +
  scale_x_continuous(breaks = seq(0,1000,200)) #optimizes at 300


field_model_AAx <- ranger(field_x ~ prev_field_x +  
                            prev_distance + time_remaining + 
                            final_ball_position_y + 
                            time_in_play +
                            prev_field_y +
                            prev_dist_x +
                            final_ball_position_x, 
                          data = location_training_AA,
                          split.select.weights = split_weight_AAx,
                          num.threads = 0,
                          always.split.variables = c("final_ball_position_x"),
                          num.trees = 300,
                          splitrule = "extratrees",
                          num.random.splits = optimized_rs_AAx,
                          min.node.size = optimized_node_AAx,
                          mtry = 5,
                          min.bucket = optimized_minbucket_AAx,
                          seed = 101)

for (i in 1:nrow(location_test_AA)){
  if (i > 1){
    if (location_test_AA[i,"primary_key"] != location_test_AA[i-1,'primary_key']){
      preds_field_position_x <- predict(field_model_AAx, location_test_AA[i,])$predictions
      location_test_AA[i,"field_x1"] <- preds_field_position_x
      location_test_AA[i,'dist_x1'] <- location_test_AA[i,'final_ball_position_x'] - location_test_AA[i, 'field_x1']
      location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x1'])^2 + (location_test_AA[i,'dist_y'])^2)
    }else{
      location_test_AA[i, "prev_field_x"] <- location_test_AA[i-1, "field_x1"]
      location_test_AA[i,"prev_distance"] <- location_test_AA[i-1, "distance_from_ball"]
      location_test_AA[i,"prev_dist_x"] <- location_test_AA[i-1, "dist_x1"]
      preds_field_position_x <- predict(field_model_AAx, location_test_AA[i,])$predictions
      location_test_AA[i,"field_x1"] <- preds_field_position_x
      location_test_AA[i,'dist_x1'] <- location_test_AA[i,'final_ball_position_x'] - location_test_AA[i, 'field_x1']
      location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x1'])^2 + (location_test_AA[i,'dist_y'])^2)
    }
  }else{
    preds_field_position_x <- predict(field_model_AAx, location_test_AA[i,])$predictions
    location_test_AA[i,"field_x1"] <- preds_field_position_x
    location_test_AA[i,'dist_x1'] <- location_test_AA[i,'final_ball_position_x'] - location_test_AA[i, 'field_x1']
    location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x1'])^2 + (location_test_AA[i,'dist_y'])^2)
  }
  if(i %% 250 == 0){
    print(i)
  }
}

location_test_AA <- location_test_AA  %>% 
  mutate(diff_x = abs(field_x - field_x1))

rmse(location_test_AA, field_x, field_x1) # 6.82

location_test_AA %>% ggplot(aes(y = diff_x, x = time_in_play,group = primary_key, color = time_remaining)) +
  geom_path()

location_test_AA <- read_csv("location_test_AA1.csv", show_col_types = FALSE)


#### Final Field Model AA X ####

field_model_AAx <- ranger(field_x ~ prev_field_x + 
                            prev_distance + time_remaining + 
                            final_ball_position_y + 
                            time_in_play +
                            prev_field_y +
                            prev_dist_x +
                            final_ball_position_x, 
                           data = dat_AA,
                           num.trees = 300,
                          split.select.weights = split_weight_AAx,
                          num.threads = 0,
                          always.split.variables = c("final_ball_position_x"),
                           splitrule = "extratrees",
                           num.random.splits = optimized_rs_AAx, #11
                           min.node.size = optimized_node_AAx, #13
                           mtry = 5, 
                           min.bucket = optimized_minbucket_AAx, #2
                           importance = "permutation",
                          scale.permutation.importance = TRUE,
                           seed = 101)
for (i in 1:nrow(location_test_AA)){
  if (i > 1){
    if (location_test_AA[i,"primary_key"] != location_test_AA[i-1,'primary_key']){
      preds_field_position_x <- predict(field_model_AAx, location_test_AA[i,])$predictions
      location_test_AA[i,"field_x1"] <- preds_field_position_x
      location_test_AA[i,'dist_x1'] <- location_test_AA[i,'final_ball_position_x'] - location_test_AA[i, 'field_x1']
      location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x1'])^2 + (location_test_AA[i,'dist_y'])^2)
    }else{
      location_test_AA[i, "prev_field_x"] <- location_test_AA[i-1, "field_x1"]
      location_test_AA[i,"prev_distance"] <- location_test_AA[i-1, "distance_from_ball"]
      location_test_AA[i,"prev_dist_x"] <- location_test_AA[i-1, "dist_x1"]
      preds_field_position_x <- predict(field_model_AAx, location_test_AA[i,])$predictions
      location_test_AA[i,"field_x1"] <- preds_field_position_x
      location_test_AA[i,'dist_x1'] <- location_test_AA[i,'final_ball_position_x'] - location_test_AA[i, 'field_x1']
      location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x1'])^2 + (location_test_AA[i,'dist_y'])^2)
    }
  }else{
    preds_field_position_x <- predict(field_model_AAx, location_test_AA[i,])$predictions
    location_test_AA[i,"field_x1"] <- preds_field_position_x
    location_test_AA[i,'dist_x1'] <- location_test_AA[i,'final_ball_position_x'] - location_test_AA[i, 'field_x1']
    location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x1'])^2 + (location_test_AA[i,'dist_y'])^2)
  }
  if(i %% 250 == 0){
    print(i)
  }
}

location_test_AA <- location_test_AA  %>% 
  mutate(diff_x = abs(field_x - field_x1))

rmse(location_test_AA, field_x, field_x1) # 4.06

location_test_AA %>% ggplot(aes(y = diff_x, x = time_in_play,group = primary_key, color = time_remaining)) +
  geom_path()

importance_AAx <- tibble(feature = field_model_AAx$forest$independent.variable.names, importance = pull(as.data.frame(field_model_AAx$variable.importance)),
                         split_weight = split_weight_AAx) %>% 
  arrange(desc(importance))

print(importance_AAx)

#### Tuning For A+ ####
dat_Aplus <- final_dataset %>%
  reframe(game_str, play_id, timestamp, timestamp_at_contact,ball_position_x, ball_position_y, 
          max_fielder_speed, time_remaining, time_in_play, field_y, field_x, distance_from_ball, 
          final_ball_position_x, final_ball_position_y, Level = Level, pitch_type, prev_field_x, prev_field_y,
          prev_distance, Season, player_position.x, flyout, dist_x, prev_dist_x,dist_y, prev_dist_y, primary_key,
          exit_velo,LA, spray_angle) %>% 
  filter(timestamp >= timestamp_at_contact, Level == 'A+') %>% 
  drop_na()

write_csv(dat_Aplus, file = "dat_Aplus.csv")

field_split_Aplus <- group_initial_split(dat_Aplus, primary_key,prop = (nrow(dat_Aplus)-800)/nrow(dat_Aplus), strata = player_position.x)

location_training_Aplus <- training(field_split_Aplus)

write_csv(location_training_Aplus, file = "location_training_Aplus1.csv")

location_test_Aplus <- testing(field_split_Aplus)
location_test_Aplus <- location_test_Aplus %>% 
  mutate(field_x1 = NA,
         dist_x1 = NA)

write_csv(location_test_Aplus, file = 'location_test_Aplus1.csv')

find_randsplit_Aplusx <- function(randsplit){
  randsplit <- ceiling(randsplit)
  model2 <- ranger(field_x ~ prev_field_x + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_y + 
                     time_in_play +
                     prev_field_y +
                     prev_dist_x +
                     final_ball_position_x, 
                   data = location_training_Aplus,
                   split.select.weights = c(rep(0.5,7),0),
                   num.threads = 0,
                    always.split.variables = c("final_ball_position_x"),
                   splitrule = "extratrees",
                   num.random.splits = randsplit,
                   min.node.size = 10,
                   mtry = 5,
                   num.trees = 300, 
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_Aplus)){
    if (i > 1){
      if (location_test_Aplus[i,"primary_key"] != location_test_Aplus[i-1,'primary_key']){
        preds_field_position_x <- predict(model2, location_test_Aplus[i,])$predictions
        location_test_Aplus[i,"field_x1"] <- preds_field_position_x
        location_test_Aplus[i,'dist_x1'] <- location_test_Aplus[i,'final_ball_position_x'] - location_test_Aplus[i, 'field_x1']
        location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x1'])^2 + (location_test_Aplus[i,'dist_y'])^2)
      }else{
        location_test_Aplus[i, "prev_field_x"] <- location_test_Aplus[i-1, "field_x1"]
        location_test_Aplus[i,"prev_distance"] <- location_test_Aplus[i-1, "distance_from_ball"]
        location_test_Aplus[i,"prev_dist_x"] <- location_test_Aplus[i-1, "dist_x1"]
        preds_field_position_x <- predict(model2, location_test_Aplus[i,])$predictions
        location_test_Aplus[i,"field_x1"] <- preds_field_position_x
        location_test_Aplus[i,'dist_x1'] <- location_test_Aplus[i,'final_ball_position_x'] - location_test_Aplus[i, 'field_x1']
        location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x1'])^2 + (location_test_Aplus[i,'dist_y'])^2)
      }
    }else{
      preds_field_position_x <- predict(model2, location_test_Aplus[i,])$predictions
      location_test_Aplus[i,"field_x1"] <- preds_field_position_x
      location_test_Aplus[i,'dist_x1'] <- location_test_Aplus[i,'final_ball_position_x'] - location_test_Aplus[i, 'field_x1']
      location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x1'])^2 + (location_test_Aplus[i,'dist_y'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  rmse <- sqrt(mean((location_test_Aplus$field_x - location_test_Aplus$field_x1)^2))
  print(randsplit)
  print(rmse)
  return(rmse)
}

rs_Aplusx <- tibble(
  randsplit = 2:20,
  rmse = map_dbl(2:20, find_randsplit_Aplusx)
)

rs_Aplusx <- rs_Aplusx %>% 
  slice_min(rmse, n = 1) 

print(rs_Aplusx)

optimized_rs_Aplusx <- rs_Aplusx$randsplit #optimizes at 4, rmse = 6.7



find_weight_Aplusx <- function(par){
  if(par[1] > 1){par[1] <- 1} ### optimize function was spawning errors that at a certain point splitweights were outside the bounds [0,1] for reasons I can't explain
  if(par[1] < 0){par[1] <- 0.1}### this is an extra level of precaution
  if(par[2] > 1){par[2] <- 1}
  if(par[2] < 0){par[2] <- 0.1}
  if(par[3] > 1){par[3] <- 1}
  if(par[3] < 0){par[3] <- 0.1}
  if(par[4] > 1){par[4] <- 1}
  if(par[4] < 0){par[4] <- 0.1}
  if(par[5] > 1){par[5] <- 1}
  if(par[5] < 0){par[5] <- 0.1}
  if(par[6] > 1){par[6] <- 1}
  if(par[6] < 0){par[6] <- 0.1}
  if(par[7] > 1){par[7] <- 1}
  if(par[7] < 0){par[7] <- 0.1}
  model2 <- ranger(field_x ~ prev_field_x + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_y + 
                     time_in_play +
                     prev_field_y +
                     prev_dist_x +
                     final_ball_position_x, 
                   data = location_training_Aplus,
                   split.select.weights = c(par[1],  par[2], par[3], par[4],
                                            par[5], par[6], par[7], 0),
                    num.trees = 300,
                   always.split.variables = c("final_ball_position_x"), 
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_Aplusx,
                   num.threads = 0,
                   min.node.size = 10,
                   mtry = 5,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_Aplus)){
    if (i > 1){
      if (location_test_Aplus[i,"primary_key"] != location_test_Aplus[i-1,'primary_key']){
        preds_field_position_x <- predict(model2, location_test_Aplus[i,])$predictions
        location_test_Aplus[i,"field_x1"] <- preds_field_position_x
        location_test_Aplus[i,'dist_x1'] <- location_test_Aplus[i,'final_ball_position_x'] - location_test_Aplus[i, 'field_x1']
        location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x1'])^2 + (location_test_Aplus[i,'dist_y'])^2)
      }else{
        location_test_Aplus[i, "prev_field_x"] <- location_test_Aplus[i-1, "field_x1"]
        location_test_Aplus[i,"prev_distance"] <- location_test_Aplus[i-1, "distance_from_ball"]
        location_test_Aplus[i,"prev_dist_x"] <- location_test_Aplus[i-1, "dist_x1"]
        preds_field_position_x <- predict(model2, location_test_Aplus[i,])$predictions
        location_test_Aplus[i,"field_x1"] <- preds_field_position_x
        location_test_Aplus[i,'dist_x1'] <- location_test_Aplus[i,'final_ball_position_x'] - location_test_Aplus[i, 'field_x1']
        location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x1'])^2 + (location_test_Aplus[i,'dist_y'])^2)
      }
    }else{
      preds_field_position_x <- predict(model2, location_test_Aplus[i,])$predictions
      location_test_Aplus[i,"field_x1"] <- preds_field_position_x
      location_test_Aplus[i,'dist_x1'] <- location_test_Aplus[i,'final_ball_position_x'] - location_test_Aplus[i, 'field_x1']
      location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x1'])^2 + (location_test_Aplus[i,'dist_y'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  #preds <- predict(model2, location_test_Aplus)$predictions
  # location_test_Aplus <- location_test_Aplus %>% 
  #  mutate(diffs = (field_x - field_x1)*diff_weights)
  rmse <- sqrt(mean((location_test_Aplus$field_x - location_test_Aplus$field_x1)^2))
  print(par)
  print(rmse)
  return(rmse)
}
#(optimized_weight_AAAx <- optimize(find_weight_AAAx, c(0,0.5), tol = 0.02))
### Note: Should takes a few hours to run
(optimized_weight_Aplusx <- optim(par = c(rep(0.5,7)), find_weight_Aplusx, hessian = TRUE, method = "L-BFGS-B", lower = 0.4, upper = 0.6, control = list(maxit = 5, trace = 6, factr = 0.01/.Machine$double.eps,pgtol = 1e-4, ndeps = rep(0.01,7), REPORT = 1))) #lower set to 0.1 rather than 0 so mtry has enough variables to choose from 

# par: c(0.4, 0.4, 0.4, 0.4, 0.599939, 0.4, 0.6) 
# rmse = 5.86242

split_weight_Aplusx <- c(optimized_weight_Aplusx$par,0)

find_minnodesize_Aplusx <- function(node){
  node <- floor(node)
  model2 <- ranger(field_x ~ prev_field_x +  
                     prev_distance + time_remaining + 
                     final_ball_position_y + 
                     time_in_play +
                     prev_field_y +
                     prev_dist_x +
                     final_ball_position_x, 
                   data = location_training_Aplus,
                   split.select.weights = split_weight_Aplusx,
                   num.threads = 0,
                   always.split.variables = c("final_ball_position_x"),
                   num.trees = 300,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_Aplusx,
                   min.node.size = node,
                   mtry = 5,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_Aplus)){
    if (i > 1){
      if (location_test_Aplus[i,"primary_key"] != location_test_Aplus[i-1,'primary_key']){
        preds_field_position_x <- predict(model2, location_test_Aplus[i,])$predictions
        location_test_Aplus[i,"field_x1"] <- preds_field_position_x
        location_test_Aplus[i,'dist_x1'] <- location_test_Aplus[i,'final_ball_position_x'] - location_test_Aplus[i, 'field_x1']
        location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x1'])^2 + (location_test_Aplus[i,'dist_y'])^2)
      }else{
        location_test_Aplus[i, "prev_field_x"] <- location_test_Aplus[i-1, "field_x1"]
        location_test_Aplus[i,"prev_distance"] <- location_test_Aplus[i-1, "distance_from_ball"]
        location_test_Aplus[i,"prev_dist_x"] <- location_test_Aplus[i-1, "dist_x1"]
        preds_field_position_x <- predict(model2, location_test_Aplus[i,])$predictions
        location_test_Aplus[i,"field_x1"] <- preds_field_position_x
        location_test_Aplus[i,'dist_x1'] <- location_test_Aplus[i,'final_ball_position_x'] - location_test_Aplus[i, 'field_x1']
        location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x1'])^2 + (location_test_Aplus[i,'dist_y'])^2)
        # 
        # location_test_Aplus <- location_test_Aplus %>%
        #   mutate(dist_x = final_ball_position_x - field_x1,
        #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
      }
    }else{
      preds_field_position_x <- predict(model2, location_test_Aplus[i,])$predictions
      location_test_Aplus[i,"field_x1"] <- preds_field_position_x
      location_test_Aplus[i,'dist_x1'] <- location_test_Aplus[i,'final_ball_position_x'] - location_test_Aplus[i, 'field_x1']
      location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x1'])^2 + (location_test_Aplus[i,'dist_y'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }

  rmse <- sqrt(mean((location_test_Aplus$field_x - location_test_Aplus$field_x1)^2))
  print(node)
  print(rmse)
  return(rmse)
}

(optimized_minnodesize_Aplusx <- optimize(find_minnodesize_Aplusx, c(11,25), tol = 1))
optimized_node_Aplusx <- floor(optimized_minnodesize_Aplusx$minimum) # optimized at 15, rmse = 5.47

find_minbucket_Aplusx <- function(bucket){
  bucket <- floor(bucket)
  model2 <- ranger(field_x ~ prev_field_x +  
                     prev_distance + time_remaining + 
                     final_ball_position_y + 
                     time_in_play +
                     prev_field_y +
                     prev_dist_x +
                     final_ball_position_x, 
                   data = location_training_Aplus,
                   split.select.weights = split_weight_Aplusx,
                   num.threads = 0,
                   always.split.variables = c("final_ball_position_x"),
                   num.trees = 300,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_Aplusx,
                   min.node.size = optimized_node_Aplusx,
                   mtry = 5,
                   min.bucket = bucket,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_Aplus)){
    if (i > 1){
      if (location_test_Aplus[i,"primary_key"] != location_test_Aplus[i-1,'primary_key']){
        preds_field_position_x <- predict(model2, location_test_Aplus[i,])$predictions
        location_test_Aplus[i,"field_x1"] <- preds_field_position_x
        location_test_Aplus[i,'dist_x1'] <- location_test_Aplus[i,'final_ball_position_x'] - location_test_Aplus[i, 'field_x1']
        location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x1'])^2 + (location_test_Aplus[i,'dist_y'])^2)
      }else{
        location_test_Aplus[i, "prev_field_x"] <- location_test_Aplus[i-1, "field_x1"]
        location_test_Aplus[i,"prev_distance"] <- location_test_Aplus[i-1, "distance_from_ball"]
        location_test_Aplus[i,"prev_dist_x"] <- location_test_Aplus[i-1, "dist_x1"]
        preds_field_position_x <- predict(model2, location_test_Aplus[i,])$predictions
        location_test_Aplus[i,"field_x1"] <- preds_field_position_x
        location_test_Aplus[i,'dist_x1'] <- location_test_Aplus[i,'final_ball_position_x'] - location_test_Aplus[i, 'field_x1']
        location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x1'])^2 + (location_test_Aplus[i,'dist_y'])^2)
        # 
        # location_test_Aplus <- location_test_Aplus %>%
        #   mutate(dist_x = final_ball_position_x - field_x1,
        #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
      }
    }else{
      preds_field_position_x <- predict(model2, location_test_Aplus[i,])$predictions
      location_test_Aplus[i,"field_x1"] <- preds_field_position_x
      location_test_Aplus[i,'dist_x1'] <- location_test_Aplus[i,'final_ball_position_x'] - location_test_Aplus[i, 'field_x1']
      location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x1'])^2 + (location_test_Aplus[i,'dist_y'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  rmse <- sqrt(mean((location_test_Aplus$field_x - location_test_Aplus$field_x1)^2))
  print(bucket)
  print(rmse)
  return(rmse)
}

(optimized_minbucket_Aplusx <- optimize(find_minbucket_Aplusx, c(1,floor(optimized_node_Aplusx/2)+1), tol = 1)) # optimizes at 1, rmse = 5.47
optimized_minbucket_Aplusx <- floor(optimized_minbucket_Aplusx$minimum)


find_ntree_Aplusx <- function(ntree){
  model2 <- ranger(field_x ~ prev_field_x +
                     prev_distance + time_remaining + 
                     final_ball_position_y + 
                     time_in_play +
                     prev_field_y +
                     prev_dist_x +
                     final_ball_position_x, 
                   data = location_training_Aplus,
                   split.select.weights = split_weight_Aplusx,
                   num.threads = 0,
                   always.split.variables = c("final_ball_position_x"),
                   num.trees = ntree,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_Aplusx,
                   min.node.size = optimized_node_Aplusx,
                   mtry = 5,
                   min.bucket = optimized_minbucket_Aplusx,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_Aplus)){
    if (i > 1){
      if (location_test_Aplus[i,"primary_key"] != location_test_Aplus[i-1,'primary_key']){
        preds_field_position_x <- predict(model2, location_test_Aplus[i,])$predictions
        location_test_Aplus[i,"field_x1"] <- preds_field_position_x
        location_test_Aplus[i,'dist_x1'] <- location_test_Aplus[i,'final_ball_position_x'] - location_test_Aplus[i, 'field_x1']
        location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x1'])^2 + (location_test_Aplus[i,'dist_y'])^2)
      }else{
        location_test_Aplus[i, "prev_field_x"] <- location_test_Aplus[i-1, "field_x1"]
        location_test_Aplus[i,"prev_distance"] <- location_test_Aplus[i-1, "distance_from_ball"]
        location_test_Aplus[i,"prev_dist_x"] <- location_test_Aplus[i-1, "dist_x1"]
        preds_field_position_x <- predict(model2, location_test_Aplus[i,])$predictions
        location_test_Aplus[i,"field_x1"] <- preds_field_position_x
        location_test_Aplus[i,'dist_x1'] <- location_test_Aplus[i,'final_ball_position_x'] - location_test_Aplus[i, 'field_x1']
        location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x1'])^2 + (location_test_Aplus[i,'dist_y'])^2)
        # 
        # location_test_Aplus <- location_test_Aplus %>%
        #   mutate(dist_x = final_ball_position_x - field_x1,
        #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
      }
    }else{
      preds_field_position_x <- predict(model2, location_test_Aplus[i,])$predictions
      location_test_Aplus[i,"field_x1"] <- preds_field_position_x
      location_test_Aplus[i,'dist_x1'] <- location_test_Aplus[i,'final_ball_position_x'] - location_test_Aplus[i, 'field_x1']
      location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x1'])^2 + (location_test_Aplus[i,'dist_y'])^2)
    }
    if(i %% 250 == 0){
      print(paste("Row: ",i))
    }
  }
  #preds <- predict(model2, location_test_Aplus)$predictions
  # location_test_Aplus <- location_test_Aplus %>% 
  #  mutate(diffs = (field_x - field_x1)*diff_weights)
  rmse <- sqrt(mean((location_test_Aplus$field_x - location_test_Aplus$field_x1)^2))
  print(ntree)
  print(rmse)
  return(rmse)
}

tree_perf_Aplusx <- tibble(
  ntree = 1:6*100,
  rmse_value = map_dbl(1:6*100, find_ntree_Aplusx)
)




tree_perf_Aplusx %>% ggplot(aes(ntree,rmse_value)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  ylab("RMSE") +
  scale_x_continuous(breaks = seq(0,600,200)) #optimizes at 400


field_model_Aplusx <- ranger(field_x ~ prev_field_x +  
                               prev_distance + time_remaining + 
                               final_ball_position_y + 
                               time_in_play +
                               prev_field_y +
                               prev_dist_x +
                               final_ball_position_x, 
                             data = location_training_Aplus,
                             split.select.weights = split_weight_Aplusx,
                             num.threads = 0,
                             always.split.variables = c("final_ball_position_x"),
                          num.trees = 400,
                          splitrule = "extratrees",
                          num.random.splits = optimized_rs_Aplusx,
                          min.node.size = optimized_node_Aplusx,
                          mtry = 5,
                          min.bucket = optimized_minbucket_Aplusx,
                          importance = "permutation",
                          scale.permutation.importance = TRUE,
                          seed = 101)



for (i in 1:nrow(location_test_Aplus)){
  if (i > 1){
    if (location_test_Aplus[i,"primary_key"] != location_test_Aplus[i-1,'primary_key']){
      preds_field_position_x <- predict(field_model_Aplusx, location_test_Aplus[i,])$predictions
      location_test_Aplus[i,"field_x1"] <- preds_field_position_x
      location_test_Aplus[i,'dist_x1'] <- location_test_Aplus[i,'final_ball_position_x'] - location_test_Aplus[i, 'field_x1']
      location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x1'])^2 + (location_test_Aplus[i,'dist_y'])^2)
    }else{
      location_test_Aplus[i, "prev_field_x"] <- location_test_Aplus[i-1, "field_x1"]
      location_test_Aplus[i,"prev_distance"] <- location_test_Aplus[i-1, "distance_from_ball"]
      location_test_Aplus[i,"prev_dist_x"] <- location_test_Aplus[i-1, "dist_x1"]
      preds_field_position_x <- predict(field_model_Aplusx, location_test_Aplus[i,])$predictions
      location_test_Aplus[i,"field_x1"] <- preds_field_position_x
      location_test_Aplus[i,'dist_x1'] <- location_test_Aplus[i,'final_ball_position_x'] - location_test_Aplus[i, 'field_x1']
      location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x1'])^2 + (location_test_Aplus[i,'dist_y'])^2)
      # 
      # location_test_Aplus <- location_test_Aplus %>%
      #   mutate(dist_x = final_ball_position_x - field_x1,
      #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
    }
  }else{
    preds_field_position_x <- predict(field_model_Aplusx, location_test_Aplus[i,])$predictions
    location_test_Aplus[i,"field_x1"] <- preds_field_position_x
    location_test_Aplus[i,'dist_x1'] <- location_test_Aplus[i,'final_ball_position_x'] - location_test_Aplus[i, 'field_x1']
    location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x1'])^2 + (location_test_Aplus[i,'dist_y'])^2)
  }
  if(i %% 250 == 0){
    print(i)
  }
}

location_test_Aplus <- location_test_Aplus  %>% 
  mutate(#pred_x = preds2,
    diff_x = abs(field_x - field_x1))

rmse(location_test_Aplus, field_x, field_x1) # 5.42

location_test_Aplus %>% ggplot(aes(y = diff_x, x = time_in_play,group = primary_key, color = time_remaining)) +
  geom_path()

location_test_Aplus <- read_csv("location_test_Aplus1.csv", show_col_types = FALSE)

#### Final Field Model Aplus X ####

field_model_Aplusx <- ranger(field_x ~ prev_field_x + 
                               prev_distance + time_remaining + 
                               final_ball_position_y + 
                               time_in_play +
                               prev_field_y +
                               prev_dist_x +
                               final_ball_position_x, 
                             data = dat_Aplus,
                             split.select.weights = split_weight_Aplusx,
                             num.threads = 0,
                             always.split.variables = c("final_ball_position_x"),
                          num.trees = 400,
                          splitrule = "extratrees",
                          num.random.splits = optimized_rs_Aplusx, #12
                          min.node.size = optimized_node_Aplusx, #11
                          mtry = 5, 
                          min.bucket = optimized_minbucket_Aplusx, #1
                          importance = "permutation",
                          scale.permutation.importance = TRUE,
                          seed = 101)

for (i in 1:nrow(location_test_Aplus)){
  if (i > 1){
    if (location_test_Aplus[i,"primary_key"] != location_test_Aplus[i-1,'primary_key']){
      preds_field_position_x <- predict(field_model_Aplusx, location_test_Aplus[i,])$predictions
      location_test_Aplus[i,"field_x1"] <- preds_field_position_x
      location_test_Aplus[i,'dist_x1'] <- location_test_Aplus[i,'final_ball_position_x'] - location_test_Aplus[i, 'field_x1']
      location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x1'])^2 + (location_test_Aplus[i,'dist_y'])^2)
    }else{
      location_test_Aplus[i, "prev_field_x"] <- location_test_Aplus[i-1, "field_x1"]
      location_test_Aplus[i,"prev_distance"] <- location_test_Aplus[i-1, "distance_from_ball"]
      location_test_Aplus[i,"prev_dist_x"] <- location_test_Aplus[i-1, "dist_x1"]
      preds_field_position_x <- predict(field_model_Aplusx, location_test_Aplus[i,])$predictions
      location_test_Aplus[i,"field_x1"] <- preds_field_position_x
      location_test_Aplus[i,'dist_x1'] <- location_test_Aplus[i,'final_ball_position_x'] - location_test_Aplus[i, 'field_x1']
      location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x1'])^2 + (location_test_Aplus[i,'dist_y'])^2)
      # 
      # location_test_Aplus <- location_test_Aplus %>%
      #   mutate(dist_x = final_ball_position_x - field_x1,
      #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
    }
  }else{
    preds_field_position_x <- predict(field_model_Aplusx, location_test_Aplus[i,])$predictions
    location_test_Aplus[i,"field_x1"] <- preds_field_position_x
    location_test_Aplus[i,'dist_x1'] <- location_test_Aplus[i,'final_ball_position_x'] - location_test_Aplus[i, 'field_x1']
    location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x1'])^2 + (location_test_Aplus[i,'dist_y'])^2)
  }
  if(i %% 250 == 0){
    print(i)
  }
}

location_test_Aplus <- location_test_Aplus  %>% 
  mutate(
    diff_x = abs(field_x - field_x1))

rmse(location_test_Aplus, field_x, field_x1) # 3.01

location_test_Aplus %>% ggplot(aes(y = diff_x, x = time_in_play,group = primary_key, color = time_remaining)) +
  geom_path()

importance_Aplusx <- tibble(feature = field_model_Aplusx$forest$independent.variable.names, importance = pull(as.data.frame(field_model_Aplusx$variable.importance)),
                            split_weight = split_weight_Aplusx) %>% 
  arrange(desc(importance))

print(importance_Aplusx)

#### Tuning for A ####

dat_A <- final_dataset %>%
  reframe(game_str, play_id, timestamp, timestamp_at_contact,ball_position_x, ball_position_y, 
          max_fielder_speed, time_remaining, time_in_play, field_y, field_x, distance_from_ball, 
          final_ball_position_x, final_ball_position_y, Level = Level, pitch_type, prev_field_x, prev_field_y,
          prev_distance, Season, player_position.x, flyout, dist_x, prev_dist_x,dist_y, prev_dist_y, primary_key,
          exit_velo,LA, spray_angle) %>% 
  filter(timestamp >= timestamp_at_contact, Level == 'A') %>% 
  drop_na()

write_csv(dat_A, file = "dat_A.csv")

field_split_A <- group_initial_split(dat_A, primary_key,prop = (nrow(dat_A) - 1000)/nrow(dat_A), strata = player_position.x)

location_training_A <- training(field_split_A)

write_csv(location_training_A, file = "location_training_A1.csv")

location_test_A <- testing(field_split_A)
location_test_A <- location_test_A %>% 
  mutate(field_x1 = NA,
         dist_x1 = NA)

write_csv(location_test_A, file = 'location_test_A1.csv')


find_randsplit_Ax <- function(randsplit){
  randsplit <- ceiling(randsplit)
  model2 <- ranger(field_x ~ prev_field_x + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_y + 
                     time_in_play +
                     prev_field_y +
                     prev_dist_x +
                     final_ball_position_x, 
                   data = location_training_A,
                   split.select.weights = c(rep(0.5,7),0),
                   splitrule = "extratrees",
                   num.random.splits = randsplit,
                   mtry = 6,
                   always.split.variables = c("final_ball_position_x"),
                   num.trees = 300, 
                   min.node.size = 15,
                   num.threads = 0,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_A)){
    if (i > 1){
      if (location_test_A[i,"primary_key"] != location_test_A[i-1,'primary_key']){
        preds_field_position_x <- predict(model2, location_test_A[i,])$predictions
        location_test_A[i,"field_x1"] <- preds_field_position_x
        location_test_A[i,'dist_x1'] <- location_test_A[i,'final_ball_position_x'] - location_test_A[i, 'field_x1']
        location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x1'])^2 + (location_test_A[i,'dist_y'])^2)
      }else{
        location_test_A[i, "prev_field_x"] <- location_test_A[i-1, "field_x1"]
        location_test_A[i,"prev_distance"] <- location_test_A[i-1, "distance_from_ball"]
        location_test_A[i,"prev_dist_x"] <- location_test_A[i-1, "dist_x1"]
        preds_field_position_x <- predict(model2, location_test_A[i,])$predictions
        location_test_A[i,"field_x1"] <- preds_field_position_x
        location_test_A[i,'dist_x1'] <- location_test_A[i,'final_ball_position_x'] - location_test_A[i, 'field_x1']
        location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x1'])^2 + (location_test_A[i,'dist_y'])^2)
        # 
        # location_test_A <- location_test_A %>%
        #   mutate(dist_x = final_ball_position_x - field_x1,
        #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
      }
    }else{
      preds_field_position_x <- predict(model2, location_test_A[i,])$predictions
      location_test_A[i,"field_x1"] <- preds_field_position_x
      location_test_A[i,'dist_x1'] <- location_test_A[i,'final_ball_position_x'] - location_test_A[i, 'field_x1']
      location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x1'])^2 + (location_test_A[i,'dist_y'])^2)
    }
    if(i %% 200 == 0){
      print(i)
    }
  }
  #preds <- predict(model2, location_test_A)$predictions
  # location_test_A <- location_test_A %>% 
  #  mutate(diffs = (field_x - field_x1)*diff_weights)
  rmse <- sqrt(mean((location_test_A$field_x - location_test_A$field_x1)^2))
  print(randsplit)
  print(rmse)
  return(rmse)
}
optimize(find_randsplit_Ax, c(0,50), tol = 1)#optimizes at 15, rmse = 7.54


optimized_rs_Ax <- 15 


find_weight_Ax <- function(par){
  if(par[1] > 1){par[1] <- 1} ### optimize function was spawning errors that at a certain point splitweights were outside the bounds [0,1] for reasons I can't explain
  if(par[1] < 0){par[1] <- 0.1}### this is an extra level of precaution
  if(par[2] > 1){par[2] <- 1}
  if(par[2] < 0){par[2] <- 0.1}
  if(par[3] > 1){par[3] <- 1}
  if(par[3] < 0){par[3] <- 0.1}
  if(par[4] > 1){par[4] <- 1}
  if(par[4] < 0){par[4] <- 0.1}
  if(par[5] > 1){par[5] <- 1}
  if(par[5] < 0){par[5] <- 0.1}
  if(par[6] > 1){par[6] <- 1}
  if(par[6] < 0){par[6] <- 0.1}
  if(par[7] > 1){par[7] <- 1}
  if(par[7] < 0){par[7] <- 0.1}
   # if(par[8] > 1){par[8] <- 1}
   # if(par[8] < 0){par[8] <- 0.1}
  model2 <- ranger(field_x ~ prev_field_x + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_y + 
                     time_in_play +
                     prev_field_y +
                     prev_dist_x +
                     final_ball_position_x, 
                   data = location_training_A,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_Ax,
                   split.select.weights = c(par[1],  par[2], par[3], par[4],
                                            par[5], par[6], par[7], 0),
                   num.trees = 300,
                   always.split.variables = c("final_ball_position_x"), #8.7
                   num.threads = 0,
                   mtry = 6,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_A)){
    if (i > 1){
      if (location_test_A[i,"primary_key"] != location_test_A[i-1,'primary_key']){
        preds_field_position_x <- predict(model2, location_test_A[i,])$predictions
        location_test_A[i,"field_x1"] <- preds_field_position_x
        location_test_A[i,'dist_x1'] <- location_test_A[i,'final_ball_position_x'] - location_test_A[i, 'field_x1']
        location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x1'])^2 + (location_test_A[i,'dist_y'])^2)
      }else{
        location_test_A[i, "prev_field_x"] <- location_test_A[i-1, "field_x1"]
        location_test_A[i,"prev_distance"] <- location_test_A[i-1, "distance_from_ball"]
        location_test_A[i,"prev_dist_x"] <- location_test_A[i-1, "dist_x1"]
        preds_field_position_x <- predict(model2, location_test_A[i,])$predictions
        location_test_A[i,"field_x1"] <- preds_field_position_x
        location_test_A[i,'dist_x1'] <- location_test_A[i,'final_ball_position_x'] - location_test_A[i, 'field_x1']
        location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x1'])^2 + (location_test_A[i,'dist_y'])^2)
        # 
        # location_test_A <- location_test_A %>%
        #   mutate(dist_x = final_ball_position_x - field_x1,
        #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
      }
    }else{
      preds_field_position_x <- predict(model2, location_test_A[i,])$predictions
      location_test_A[i,"field_x1"] <- preds_field_position_x
      location_test_A[i,'dist_x1'] <- location_test_A[i,'final_ball_position_x'] - location_test_A[i, 'field_x1']
      location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x1'])^2 + (location_test_A[i,'dist_y'])^2)
    }
    if(i %% 200 == 0){
      print(i)
    }
  }
  #preds <- predict(model2, location_test_A)$predictions
  # location_test_A <- location_test_A %>% 
  #  mutate(diffs = (field_x - field_x1)*diff_weights)
  rmse <- sqrt(mean((location_test_A$field_x - location_test_A$field_x1)^2))
  print(par)
  print(rmse)
  return(rmse)
}
### Note: Should takes a few hours to run
(optimized_weight_Ax <- optim(par = c(rep(0.5,5), 0.45, 0.5), find_weight_Ax, hessian = TRUE, method = "L-BFGS-B", lower = 0.4, upper = 0.6, control = list(maxit = 5, trace = 6, factr = 0.01/.Machine$double.eps, ndeps = rep(0.001,7), REPORT = 1)))  

#par: c(0.5003595, 0.5003595, 0.5003595, 0.4996405, 0.4996405, 0.4498202, 0.4996405)

#rmse = 6.174415

split_weight_Ax <- c(optimized_weight_Ax$par,0)


find_minnodesize_Ax <- function(node){
  node <- floor(node)
  model2 <- ranger(field_x ~ prev_field_x + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_y + 
                     time_in_play +
                     prev_field_y +
                     prev_dist_x +
                     final_ball_position_x, 
                   data = location_training_A,
                   split.select.weights = split_weight_Ax,
                   always.split.variables = c("final_ball_position_x"),
                   num.trees = 300,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_Ax,
                   mtry = 6,
                   min.node.size = node,
                   num.threads = 0,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_A)){
    if (i > 1){
      if (location_test_A[i,"primary_key"] != location_test_A[i-1,'primary_key']){
        preds_field_position_x <- predict(model2, location_test_A[i,])$predictions
        location_test_A[i,"field_x1"] <- preds_field_position_x
        location_test_A[i,'dist_x1'] <- location_test_A[i,'final_ball_position_x'] - location_test_A[i, 'field_x1']
        location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x1'])^2 + (location_test_A[i,'dist_y'])^2)
      }else{
        location_test_A[i, "prev_field_x"] <- location_test_A[i-1, "field_x1"]
        location_test_A[i,"prev_distance"] <- location_test_A[i-1, "distance_from_ball"]
        location_test_A[i,"prev_dist_x"] <- location_test_A[i-1, "dist_x1"]
        preds_field_position_x <- predict(model2, location_test_A[i,])$predictions
        location_test_A[i,"field_x1"] <- preds_field_position_x
        location_test_A[i,'dist_x1'] <- location_test_A[i,'final_ball_position_x'] - location_test_A[i, 'field_x1']
        location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x1'])^2 + (location_test_A[i,'dist_y'])^2)
      }
    }else{
      preds_field_position_x <- predict(model2, location_test_A[i,])$predictions
      location_test_A[i,"field_x1"] <- preds_field_position_x
      location_test_A[i,'dist_x1'] <- location_test_A[i,'final_ball_position_x'] - location_test_A[i, 'field_x1']
      location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x1'])^2 + (location_test_A[i,'dist_y'])^2)
    }
    if(i %% 200 == 0){
      print(i)
    }
  }
  rmse <- sqrt(mean((location_test_A$field_x - location_test_A$field_x1)^2))
  print(node)
  print(rmse)
  return(rmse)
}

(optimized_minnodesize_Ax <- optimize(find_minnodesize_Ax, c(15,25), tol = 1))
optimized_node_Ax <- floor(optimized_minnodesize_Ax$minimum) #optimized at 17, rmse = 6.70

find_minbucket_Ax <- function(bucket){
  bucket <- floor(bucket)
  model2 <- ranger(field_x ~ prev_field_x + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_y + 
                     time_in_play +
                     prev_field_y +
                     prev_dist_x +
                     final_ball_position_x, 
                   data = location_training_A,
                   num.trees = 300,
                   split.select.weights = split_weight_Ax,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_Ax,
                   always.split.variables = c("final_ball_position_x"),
                   num.threads = 0,
                   min.node.size = optimized_node_Ax,
                   mtry = 6,
                   min.bucket = bucket,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_A)){
    if (i > 1){
      if (location_test_A[i,"primary_key"] != location_test_A[i-1,'primary_key']){
        preds_field_position_x <- predict(model2, location_test_A[i,])$predictions
        location_test_A[i,"field_x1"] <- preds_field_position_x
        location_test_A[i,'dist_x1'] <- location_test_A[i,'final_ball_position_x'] - location_test_A[i, 'field_x1']
        location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x1'])^2 + (location_test_A[i,'dist_y'])^2)
      }else{
        location_test_A[i, "prev_field_x"] <- location_test_A[i-1, "field_x1"]
        location_test_A[i,"prev_distance"] <- location_test_A[i-1, "distance_from_ball"]
        location_test_A[i,"prev_dist_x"] <- location_test_A[i-1, "dist_x1"]
        preds_field_position_x <- predict(model2, location_test_A[i,])$predictions
        location_test_A[i,"field_x1"] <- preds_field_position_x
        location_test_A[i,'dist_x1'] <- location_test_A[i,'final_ball_position_x'] - location_test_A[i, 'field_x1']
        location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x1'])^2 + (location_test_A[i,'dist_y'])^2)
        # 
        # location_test_A <- location_test_A %>%
        #   mutate(dist_x = final_ball_position_x - field_x1,
        #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
      }
    }else{
      preds_field_position_x <- predict(model2, location_test_A[i,])$predictions
      location_test_A[i,"field_x1"] <- preds_field_position_x
      location_test_A[i,'dist_x1'] <- location_test_A[i,'final_ball_position_x'] - location_test_A[i, 'field_x1']
      location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x1'])^2 + (location_test_A[i,'dist_y'])^2)
    }
    if(i %% 200 == 0){
      print(i)
    }
  }
  #preds <- predict(model2, location_test_A)$predictions
  # location_test_A <- location_test_A %>% 
  #  mutate(diffs = (field_x - field_x1)*diff_weights)
  rmse <- sqrt(mean((location_test_A$field_x - location_test_A$field_x1)^2))
  print(bucket)
  print(rmse)
  return(rmse)
}

(optimized_minbucket_Ax <- optimize(find_minbucket_Ax, c(1,floor(optimized_node_Ax/2)+1), tol = 1)) # optimized at 3
optimized_minbucket_Ax <- floor(optimized_minbucket_Ax$minimum) #rmse = 6.75


find_ntree_Ax <- function(ntree){
  model2 <- ranger(field_x ~ prev_field_x + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_y + 
                     time_in_play +
                     prev_field_y +
                     prev_dist_x +
                     final_ball_position_x, 
                   data = location_training_A,
                   split.select.weights = split_weight_Ax,
                   num.trees = ntree,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_Ax,
                   min.node.size = optimized_node_Ax,
                   always.split.variables = c("final_ball_position_x"),
                   mtry = 6,
                   min.bucket = optimized_minbucket_Ax,
                   num.threads = 0,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_A)){
    if (i > 1){
      if (location_test_A[i,"primary_key"] != location_test_A[i-1,'primary_key']){
        preds_field_position_x <- predict(model2, location_test_A[i,])$predictions
        location_test_A[i,"field_x1"] <- preds_field_position_x
        location_test_A[i,'dist_x1'] <- location_test_A[i,'final_ball_position_x'] - location_test_A[i, 'field_x1']
        location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x1'])^2 + (location_test_A[i,'dist_y'])^2)
      }else{
        location_test_A[i, "prev_field_x"] <- location_test_A[i-1, "field_x1"]
        location_test_A[i,"prev_distance"] <- location_test_A[i-1, "distance_from_ball"]
        location_test_A[i,"prev_dist_x"] <- location_test_A[i-1, "dist_x1"]
        preds_field_position_x <- predict(model2, location_test_A[i,])$predictions
        location_test_A[i,"field_x1"] <- preds_field_position_x
        location_test_A[i,'dist_x1'] <- location_test_A[i,'final_ball_position_x'] - location_test_A[i, 'field_x1']
        location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x1'])^2 + (location_test_A[i,'dist_y'])^2)
        # 
        # location_test_A <- location_test_A %>%
        #   mutate(dist_x = final_ball_position_x - field_x1,
        #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
      }
    }else{
      preds_field_position_x <- predict(model2, location_test_A[i,])$predictions
      location_test_A[i,"field_x1"] <- preds_field_position_x
      location_test_A[i,'dist_x1'] <- location_test_A[i,'final_ball_position_x'] - location_test_A[i, 'field_x1']
      location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x1'])^2 + (location_test_A[i,'dist_y'])^2)
    }
    if(i %% 200 == 0){
      print(i)
    }
  }
  #preds <- predict(model2, location_test_A)$predictions
  # location_test_A <- location_test_A %>% 
  #  mutate(diffs = (field_x - field_x1)*diff_weights)
  rmse <- sqrt(mean((location_test_A$field_x - location_test_A$field_x1)^2))
  print(ntree)
  print(rmse)
  return(rmse)
}

tree_perf_Ax <- tibble(
  ntree = 1:6*100,
  rmse_value = map_dbl(1:6*100, find_ntree_Ax)
)



tree_perf_Ax %>% ggplot(aes(ntree,rmse_value)) +
  geom_line() +
  #geom_smooth(se = FALSE) +
  ylab("RMSE") +
  scale_x_continuous(breaks = seq(0,600,200)) #minimizes at 300


field_model_Ax <- ranger(field_x ~ prev_field_x + #max_fielder_speed + 
                           prev_distance + time_remaining + 
                           final_ball_position_y + 
                           time_in_play +
                           prev_field_y +
                           prev_dist_x +
                           final_ball_position_x, 
                         data = location_training_A,
                         split.select.weights = split_weight_Ax,
                         num.trees = 300,
                         splitrule = "extratrees",
                         always.split.variables = c("final_ball_position_x"),
                         num.random.splits = optimized_rs_Ax,
                         min.node.size = optimized_node_Ax,
                         mtry = 6,
                         min.bucket = optimized_minbucket_Ax,
                         num.threads = 0,
                         importance = "permutation",
                         scale.permutation.importance = TRUE,
                         seed = 101)



for (i in 1:nrow(location_test_A)){
  if (i > 1){
    if (location_test_A[i,"primary_key"] != location_test_A[i-1,'primary_key']){
      preds_field_position_x <- predict(field_model_Ax, location_test_A[i,])$predictions
      location_test_A[i,"field_x1"] <- preds_field_position_x
      location_test_A[i,'dist_x1'] <- location_test_A[i,'final_ball_position_x'] - location_test_A[i, 'field_x1']
      location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x1'])^2 + (location_test_A[i,'dist_y'])^2)
    }else{
      location_test_A[i, "prev_field_x"] <- location_test_A[i-1, "field_x1"]
      location_test_A[i,"prev_distance"] <- location_test_A[i-1, "distance_from_ball"]
      location_test_A[i,"prev_dist_x"] <- location_test_A[i-1, "dist_x1"]
      preds_field_position_x <- predict(field_model_Ax, location_test_A[i,])$predictions
      location_test_A[i,"field_x1"] <- preds_field_position_x
      location_test_A[i,'dist_x1'] <- location_test_A[i,'final_ball_position_x'] - location_test_A[i, 'field_x1']
      location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x1'])^2 + (location_test_A[i,'dist_y'])^2)
      # 
      # location_test_A <- location_test_A %>%
      #   mutate(dist_x = final_ball_position_x - field_x1,
      #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
    }
  }else{
    preds_field_position_x <- predict(field_model_Ax, location_test_A[i,])$predictions
    location_test_A[i,"field_x1"] <- preds_field_position_x
    location_test_A[i,'dist_x1'] <- location_test_A[i,'final_ball_position_x'] - location_test_A[i, 'field_x1']
    location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x1'])^2 + (location_test_A[i,'dist_y'])^2)
  }
  if(i %% 250 == 0){
    print(i)
  }
}

location_test_A <- location_test_A %>% 
  mutate(#pred_x = preds2,
    diff_x = abs(field_x - field_x1))

rmse(location_test_A, field_x, field_x1) # 6.76

location_test_A %>% ggplot(aes(y = diff_x, x = time_in_play,group = primary_key, color = time_remaining)) +
  geom_path()

location_test_A <- read_csv("location_test_A1.csv", show_col_types = FALSE)

#### Final Field Model A X ####

field_model_Ax <- ranger(field_x ~ prev_field_x + #max_fielder_speed + 
                           prev_distance + time_remaining + 
                           final_ball_position_y + 
                           time_in_play +
                           prev_field_y +
                           prev_dist_x +
                           final_ball_position_x, 
                         data = dat_A,
                         split.select.weights = split_weight_Ax,
                         num.trees = 300, 
                         splitrule = "extratrees",
                         always.split.variables = c("final_ball_position_x"),
                         num.random.splits = optimized_rs_Ax, #15
                         min.node.size = optimized_node_Ax, #17
                         mtry = 6,
                         min.bucket = optimized_minbucket_Ax, #3
                         num.threads = 0,
                         importance = "permutation",
                         scale.permutation.importance = TRUE,
                         seed = 101)


for (i in 1:nrow(location_test_A)){
  if (i > 1){
    if (location_test_A[i,"primary_key"] != location_test_A[i-1,'primary_key']){
      preds_field_position_x <- predict(field_model_Ax, location_test_A[i,])$predictions
      location_test_A[i,"field_x1"] <- preds_field_position_x
      location_test_A[i,'dist_x1'] <- location_test_A[i,'final_ball_position_x'] - location_test_A[i, 'field_x1']
      location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x1'])^2 + (location_test_A[i,'dist_y'])^2)
    }else{
      location_test_A[i, "prev_field_x"] <- location_test_A[i-1, "field_x1"]
      location_test_A[i,"prev_distance"] <- location_test_A[i-1, "distance_from_ball"]
      location_test_A[i,"prev_dist_x"] <- location_test_A[i-1, "dist_x1"]
      preds_field_position_x <- predict(field_model_Ax, location_test_A[i,])$predictions
      location_test_A[i,"field_x1"] <- preds_field_position_x
      location_test_A[i,'dist_x1'] <- location_test_A[i,'final_ball_position_x'] - location_test_A[i, 'field_x1']
      location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x1'])^2 + (location_test_A[i,'dist_y'])^2)
      # 
      # location_test_A <- location_test_A %>%
      #   mutate(dist_x = final_ball_position_x - field_x1,
      #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
    }
  }else{
    preds_field_position_x <- predict(field_model_Ax, location_test_A[i,])$predictions
    location_test_A[i,"field_x1"] <- preds_field_position_x
    location_test_A[i,'dist_x1'] <- location_test_A[i,'final_ball_position_x'] - location_test_A[i, 'field_x1']
    location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x1'])^2 + (location_test_A[i,'dist_y'])^2)
  }
  if(i %% 250 == 0){
    print(i)
  }
}

location_test_A <- location_test_A %>% 
  mutate(#pred_x = preds2,
    diff_x = abs(field_x - field_x1))

rmse(location_test_A, field_x, field_x1) # 4.66

location_test_A %>% ggplot(aes(y = diff_x, x = time_in_play,group = primary_key, color = time_remaining)) +
  geom_path()


importance_Ax <- tibble(feature = field_model_Ax$forest$independent.variable.names, importance = pull(as.data.frame(field_model_Ax$variable.importance))) %>% 
  arrange(desc(importance))

print(importance_Ax)
#### Save Models ####
save(field_model_AAAx, field_model_AAx, field_model_Aplusx, field_model_Ax, file = "Field_Model_X1.RData")

write_rds(field_model_AAAx, file = "Field_Model_AAAx.RDS", compress = 'xz', compression = 9L)
write_rds(field_model_AAx, file = "Field_Model_AAx.RDS", compress = 'xz', compression = 9L)
write_rds(field_model_Aplusx, file = "Field_Model_Aplusx.RDS", compress = 'xz', compression = 9L)
write_rds(field_model_Ax, file = "Field_Model_Ax.RDS", compress = 'xz', compression = 9L)
