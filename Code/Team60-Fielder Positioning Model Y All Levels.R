library(ranger)
library(tidymodels)
library(tidyverse)
set.seed(101)

final_dataset <- read_csv("final_dataset.csv", show_col_types = FALSE)
dat_AAA <- read_csv("dat_AAA.csv", show_col_types = FALSE)
dat_AA <- read_csv("dat_AA.csv", show_col_types = FALSE)
dat_Aplus <- read_csv("dat_Aplus.csv", show_col_types = FALSE)
dat_A <- read_csv("dat_A.csv", show_col_types = FALSE)
location_training_AAA <- read_csv("location_training_AAA1.csv", show_col_types = FALSE)
location_training_AA <- read_csv("location_training_AA1.csv", show_col_types = FALSE)
location_training_Aplus <- read_csv("location_training_Aplus1.csv", show_col_types = FALSE)
location_training_A <- read_csv("location_training_A1.csv", show_col_types = FALSE)
location_test_AAA <- read_csv("location_test_AAA1.csv", show_col_types = FALSE)
location_test_AA <- read_csv("location_test_AA1.csv", show_col_types = FALSE)
location_test_Aplus <- read_csv("location_test_Aplus1.csv", show_col_types = FALSE)
location_test_A <- read_csv("location_test_A1.csv", show_col_types = FALSE)
#### Note: Using Train and Test Data from field_x Model Script ####

### Note: When creating these models, I assumed that each levels corresponded to the AAA, AA, A+, and A levels
### in the minor league system, however, I have been told that this is not the case. Instead of changing these model
### and data names, here is a key of which level corresponds to which from the orignal data:

# AAA -> 4A/Home4A
# AA -> 3A/Home3A
# Aplus -> 2A/Home2A
# A -> 1A/Home1A


#### Tuning for AAA####
find_weight_AAAy <- function(par){
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
  #weight <- round(weight, 2)
  model2 <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_x + 
                     time_in_play +
                     prev_field_x +
                     prev_dist_y +
                     final_ball_position_y, 
                   data = location_training_AAA,
                   always.split.variables = c("final_ball_position_y"),
                   num.threads = 0,
                   num.trees = 300,
                   mtry = 5,
                   seed = 101,
                   split.select.weights = c(par[1],  par[2], par[3], par[4],
                                            par[5], par[6], par[7],0))
  print("Forest Complete")
  for (i in 1:nrow(location_test_AAA)){
    if (i > 1){
      if (location_test_AAA[i,"primary_key"] != location_test_AAA[i-1,'primary_key']){
        preds_field_position_y <- predict(model2, location_test_AAA[i,])$predictions
        location_test_AAA[i,"field_y1"] <- preds_field_position_y
        location_test_AAA[i,'dist_y1'] <- location_test_AAA[i,'final_ball_position_y'] - location_test_AAA[i, 'field_y1']
        location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x'])^2 + (location_test_AAA[i,'dist_y1'])^2)
      }else{
        location_test_AAA[i, "prev_field_y"] <- location_test_AAA[i-1, "field_y1"]
        location_test_AAA[i,"prev_distance"] <- location_test_AAA[i-1, "distance_from_ball"]
        location_test_AAA[i,"prev_dist_y"] <- location_test_AAA[i-1, "dist_y1"]
        preds_field_position_y <- predict(model2, location_test_AAA[i,])$predictions
        location_test_AAA[i,"field_y1"] <- preds_field_position_y
        location_test_AAA[i,'dist_y1'] <- location_test_AAA[i,'final_ball_position_y'] - location_test_AAA[i, 'field_y1']
        location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x'])^2 + (location_test_AAA[i,'dist_y1'])^2)
        # 
        # location_test_AAA <- location_test_AAA %>%
        #   mutate(dist_x = final_ball_position_x - field_y1,
        #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
      }
    }else{
      preds_field_position_y <- predict(model2, location_test_AAA[i,])$predictions
      location_test_AAA[i,"field_y1"] <- preds_field_position_y
      location_test_AAA[i,'dist_y1'] <- location_test_AAA[i,'final_ball_position_y'] - location_test_AAA[i, 'field_y1']
      location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x'])^2 + (location_test_AAA[i,'dist_y1'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  #preds <- predict(model2, location_test_AAA)$predictions
  # location_test_AAA <- location_test_AAA %>% 
  #  mutate(diffs = (field_x - field_y1)*diff_weights)
  rmse <- sqrt(mean((location_test_AAA$field_y - location_test_AAA$field_y1)^2))
  print(par)
  print(rmse)
  return(rmse)
}
(optimized_weight_AAAy <- optim(par = c(rep(0.5,6),0.501), find_weight_AAAy, hessian = TRUE, method = "L-BFGS-B", lower = 0.45, upper = 0.55, control = list(maxit = 5, trace = 6, factr = 0.01/.Machine$double.eps, pgtol = 1e-4, ndeps = rep(0.001,7), REPORT = 1))) #lower set to 0.0001 rather than 0 so mtry has enough variables to choose from 

# par: c(0.500, 0.500, 0.500, 0.500, 0.500, 0.500, 0.501)
#rmse = 3.892446

split_weight_AAAy <- c(optimized_weight_AAAy$par,0)

#Using extratrees splitrule because it does a better job of predicting more unusual
# field_y positions (really deep or really shallow)

find_randsplit_AAAy <- function(randsplit){
  randsplit <- ceiling(randsplit)
  model2 <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_x + 
                     time_in_play +
                     prev_field_x +
                     prev_dist_y +
                     final_ball_position_y, 
                   data = location_training_AAA,
                   split.select.weights = split_weight_AAAy,
                   always.split.variables = c("final_ball_position_y"),
                   num.threads = 0,
                   splitrule = "extratrees",
                   num.random.splits = randsplit,
                   num.trees = 300,
                   min.node.size = 10,
                   mtry = 5,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_AAA)){
    if (i > 1){
      if (location_test_AAA[i,"primary_key"] != location_test_AAA[i-1,'primary_key']){
        preds_field_position_y <- predict(model2, location_test_AAA[i,])$predictions
        location_test_AAA[i,"field_y1"] <- preds_field_position_y
        location_test_AAA[i,'dist_y1'] <- location_test_AAA[i,'final_ball_position_y'] - location_test_AAA[i, 'field_y1']
        location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x'])^2 + (location_test_AAA[i,'dist_y1'])^2)
      }else{
        location_test_AAA[i, "prev_field_y"] <- location_test_AAA[i-1, "field_y1"]
        location_test_AAA[i,"prev_distance"] <- location_test_AAA[i-1, "distance_from_ball"]
        location_test_AAA[i,"prev_dist_y"] <- location_test_AAA[i-1, "dist_y1"]
        preds_field_position_y <- predict(model2, location_test_AAA[i,])$predictions
        location_test_AAA[i,"field_y1"] <- preds_field_position_y
        location_test_AAA[i,'dist_y1'] <- location_test_AAA[i,'final_ball_position_y'] - location_test_AAA[i, 'field_y1']
        location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x'])^2 + (location_test_AAA[i,'dist_y1'])^2)
        # 
        # location_test_AAA <- location_test_AAA %>%
        #   mutate(dist_x = final_ball_position_x - field_y1,
        #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
      }
    }else{
      preds_field_position_y <- predict(model2, location_test_AAA[i,])$predictions
      location_test_AAA[i,"field_y1"] <- preds_field_position_y
      location_test_AAA[i,'dist_y1'] <- location_test_AAA[i,'final_ball_position_y'] - location_test_AAA[i, 'field_y1']
      location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x'])^2 + (location_test_AAA[i,'dist_y1'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  #preds <- predict(model2, location_test_AAA)$predictions
  # location_test_AAA <- location_test_AAA %>% 
  #  mutate(diffs = (field_x - field_y1)*diff_weights)
  rmse <- sqrt(mean((location_test_AAA$field_y - location_test_AAA$field_y1)^2))
  print(randsplit)
  print(rmse)
  return(rmse)
}
rs_AAAy <- tibble(
  randsplit = 2:20,
  rmse = map_dbl(2:20, find_randsplit_AAAy)
)

rs_AAAy <- rs_AAAy %>% 
  slice_min(rmse, n = 1) 

print(rs_AAAy)

optimized_rs_AAAy <- rs_AAAy$randsplit #minimizes at 19, rmse = 4.01

find_minnodesize_AAAy <- function(node){
  node <- floor(node)
  model2 <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_x + 
                     time_in_play +
                     prev_field_x +
                     prev_dist_y +
                     final_ball_position_y, 
                   data = location_training_AAA,
                   split.select.weights = split_weight_AAAy,
                   always.split.variables = c("final_ball_position_y"),
                   num.threads = 0,
                   num.trees = 300,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_AAAy,
                   mtry = 5,
                   min.node.size = node,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_AAA)){
    if (i > 1){
      if (location_test_AAA[i,"primary_key"] != location_test_AAA[i-1,'primary_key']){
        preds_field_position_y <- predict(model2, location_test_AAA[i,])$predictions
        location_test_AAA[i,"field_y1"] <- preds_field_position_y
        location_test_AAA[i,'dist_y1'] <- location_test_AAA[i,'final_ball_position_y'] - location_test_AAA[i, 'field_y1']
        location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x'])^2 + (location_test_AAA[i,'dist_y1'])^2)
      }else{
        location_test_AAA[i, "prev_field_y"] <- location_test_AAA[i-1, "field_y1"]
        location_test_AAA[i,"prev_distance"] <- location_test_AAA[i-1, "distance_from_ball"]
        location_test_AAA[i,"prev_dist_y"] <- location_test_AAA[i-1, "dist_y1"]
        preds_field_position_y <- predict(model2, location_test_AAA[i,])$predictions
        location_test_AAA[i,"field_y1"] <- preds_field_position_y
        location_test_AAA[i,'dist_y1'] <- location_test_AAA[i,'final_ball_position_y'] - location_test_AAA[i, 'field_y1']
        location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x'])^2 + (location_test_AAA[i,'dist_y1'])^2)
        # 
        # location_test_AAA <- location_test_AAA %>%
        #   mutate(dist_x = final_ball_position_x - field_y1,
        #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
      }
    }else{
      preds_field_position_y <- predict(model2, location_test_AAA[i,])$predictions
      location_test_AAA[i,"field_y1"] <- preds_field_position_y
      location_test_AAA[i,'dist_y1'] <- location_test_AAA[i,'final_ball_position_y'] - location_test_AAA[i, 'field_y1']
      location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x'])^2 + (location_test_AAA[i,'dist_y1'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  #preds <- predict(model2, location_test_AAA)$predictions
  # location_test_AAA <- location_test_AAA %>% 
  #  mutate(diffs = (field_x - field_y1)*diff_weights)
  rmse <- sqrt(mean((location_test_AAA$field_y - location_test_AAA$field_y1)^2))
  print(node)
  print(rmse)
  return(rmse)
}

(optimized_minnodesize_AAAy <- optimize(find_minnodesize_AAAy, c(15,25), tol = 1))
optimized_node_AAAy <- floor(optimized_minnodesize_AAAy$minimum) #optimized at 23, rmse = 4.19

find_minbucket_AAAy <- function(bucket){
  bucket <- floor(bucket)
  model2 <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_x + 
                     time_in_play +
                     prev_field_x +
                     prev_dist_y +
                     final_ball_position_y, 
                   data = location_training_AAA,
                   split.select.weights = split_weight_AAAy,
                   always.split.variables = c("final_ball_position_y"),
                   num.threads = 0,
                   num.trees = 300,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_AAAy,
                   min.node.size = optimized_node_AAAy,
                   mtry = 5,
                   min.bucket = bucket,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_AAA)){
    if (i > 1){
      if (location_test_AAA[i,"primary_key"] != location_test_AAA[i-1,'primary_key']){
        preds_field_position_y <- predict(model2, location_test_AAA[i,])$predictions
        location_test_AAA[i,"field_y1"] <- preds_field_position_y
        location_test_AAA[i,'dist_y1'] <- location_test_AAA[i,'final_ball_position_y'] - location_test_AAA[i, 'field_y1']
        location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x'])^2 + (location_test_AAA[i,'dist_y1'])^2)
      }else{
        location_test_AAA[i, "prev_field_y"] <- location_test_AAA[i-1, "field_y1"]
        location_test_AAA[i,"prev_distance"] <- location_test_AAA[i-1, "distance_from_ball"]
        location_test_AAA[i,"prev_dist_y"] <- location_test_AAA[i-1, "dist_y1"]
        preds_field_position_y <- predict(model2, location_test_AAA[i,])$predictions
        location_test_AAA[i,"field_y1"] <- preds_field_position_y
        location_test_AAA[i,'dist_y1'] <- location_test_AAA[i,'final_ball_position_y'] - location_test_AAA[i, 'field_y1']
        location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x'])^2 + (location_test_AAA[i,'dist_y1'])^2)
        # 
        # location_test_AAA <- location_test_AAA %>%
        #   mutate(dist_x = final_ball_position_x - field_y1,
        #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
      }
    }else{
      preds_field_position_y <- predict(model2, location_test_AAA[i,])$predictions
      location_test_AAA[i,"field_y1"] <- preds_field_position_y
      location_test_AAA[i,'dist_y1'] <- location_test_AAA[i,'final_ball_position_y'] - location_test_AAA[i, 'field_y1']
      location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x'])^2 + (location_test_AAA[i,'dist_y1'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  #preds <- predict(model2, location_test_AAA)$predictions
  # location_test_AAA <- location_test_AAA %>% 
  #  mutate(diffs = (field_x - field_y1)*diff_weights)
  rmse <- sqrt(mean((location_test_AAA$field_y - location_test_AAA$field_y1)^2))
  print(bucket)
  print(rmse)
  return(rmse)
}

(optimized_minbucket_AAAy <- optimize(find_minbucket_AAAy, c(1,floor(optimized_node_AAAy/2)+1), tol = 1)) # optimized at 6
optimized_minbucket_AAAy <- floor(optimized_minbucket_AAAy$minimum) #rmse = 4.19


find_ntree_AAAy <- function(ntree){
  model2 <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_x + 
                     time_in_play +
                     prev_field_x +
                     prev_dist_y +
                     final_ball_position_y, 
                   data = location_training_AAA,
                   split.select.weights = split_weight_AAAy,
                   always.split.variables = c("final_ball_position_y"),
                   num.threads = 0,
                   num.trees = ntree,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_AAAy,
                   min.node.size = optimized_node_AAAy,
                   mtry = 5,
                   min.bucket = optimized_minbucket_AAAy,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_AAA)){
    if (i > 1){
      if (location_test_AAA[i,"primary_key"] != location_test_AAA[i-1,'primary_key']){
        preds_field_position_y <- predict(model2, location_test_AAA[i,])$predictions
        location_test_AAA[i,"field_y1"] <- preds_field_position_y
        location_test_AAA[i,'dist_y1'] <- location_test_AAA[i,'final_ball_position_y'] - location_test_AAA[i, 'field_y1']
        location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x'])^2 + (location_test_AAA[i,'dist_y1'])^2)
      }else{
        location_test_AAA[i, "prev_field_y"] <- location_test_AAA[i-1, "field_y1"]
        location_test_AAA[i,"prev_distance"] <- location_test_AAA[i-1, "distance_from_ball"]
        location_test_AAA[i,"prev_dist_y"] <- location_test_AAA[i-1, "dist_y1"]
        preds_field_position_y <- predict(model2, location_test_AAA[i,])$predictions
        location_test_AAA[i,"field_y1"] <- preds_field_position_y
        location_test_AAA[i,'dist_y1'] <- location_test_AAA[i,'final_ball_position_y'] - location_test_AAA[i, 'field_y1']
        location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x'])^2 + (location_test_AAA[i,'dist_y1'])^2)
        # 
        # location_test_AAA <- location_test_AAA %>%
        #   mutate(dist_x = final_ball_position_x - field_y1,
        #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
      }
    }else{
      preds_field_position_y <- predict(model2, location_test_AAA[i,])$predictions
      location_test_AAA[i,"field_y1"] <- preds_field_position_y
      location_test_AAA[i,'dist_y1'] <- location_test_AAA[i,'final_ball_position_y'] - location_test_AAA[i, 'field_y1']
      location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x'])^2 + (location_test_AAA[i,'dist_y1'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  #preds <- predict(model2, location_test_AAA)$predictions
  # location_test_AAA <- location_test_AAA %>% 
  #  mutate(diffs = (field_x - field_y1)*diff_weights)
  rmse <- sqrt(mean((location_test_AAA$field_y - location_test_AAA$field_y1)^2))
  print(ntree)
  print(rmse)
  return(rmse)
}

tree_perf_AAAy <- tibble(
  ntree = 1:6*100,
  rmse_value = map_dbl(1:6*100, find_ntree_AAAy)
)


tree_perf_AAAy %>% ggplot(aes(ntree,rmse_value)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  ylab("RMSE") +
  scale_x_continuous(breaks = seq(0,600,200)) #optimizes at 500, but would be too much memory. Trying 300


field_model_AAAy <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                             prev_distance + time_remaining + 
                             final_ball_position_x + 
                             time_in_play +
                             prev_field_x +
                             prev_dist_y +
                             final_ball_position_y, 
                           data = location_training_AAA,
                           num.trees = 300,
                           split.select.weights = split_weight_AAAy,
                           always.split.variables = c("final_ball_position_y"),
                           num.threads = 0,
                           splitrule = "extratrees",
                           num.random.splits = optimized_rs_AAAy,
                           min.node.size = optimized_node_AAAy,
                           mtry = 5,
                           min.bucket = optimized_minbucket_AAAy,
                           importance = "permutation",
                           scale.permutation.importance = TRUE,
                           seed = 101)



for (i in 1:nrow(location_test_AAA)){
  if (i > 1){
    if (location_test_AAA[i,"primary_key"] != location_test_AAA[i-1,'primary_key']){
      preds_field_position_y <- predict(field_model_AAAy, location_test_AAA[i,])$predictions
      location_test_AAA[i,"field_y1"] <- preds_field_position_y
      location_test_AAA[i,'dist_y1'] <- location_test_AAA[i,'final_ball_position_y'] - location_test_AAA[i, 'field_y1']
      location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x'])^2 + (location_test_AAA[i,'dist_y1'])^2)
    }else{
      location_test_AAA[i, "prev_field_y"] <- location_test_AAA[i-1, "field_y1"]
      location_test_AAA[i,"prev_distance"] <- location_test_AAA[i-1, "distance_from_ball"]
      location_test_AAA[i,"prev_dist_y"] <- location_test_AAA[i-1, "dist_y1"]
      preds_field_position_y <- predict(field_model_AAAy, location_test_AAA[i,])$predictions
      location_test_AAA[i,"field_y1"] <- preds_field_position_y
      location_test_AAA[i,'dist_y1'] <- location_test_AAA[i,'final_ball_position_y'] - location_test_AAA[i, 'field_y1']
      location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x'])^2 + (location_test_AAA[i,'dist_y1'])^2)
      # 
      # location_test_AAA <- location_test_AAA %>%
      #   mutate(dist_x = final_ball_position_x - field_x1,
      #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
    }
  }else{
    preds_field_position_y <- predict(field_model_AAAy, location_test_AAA[i,])$predictions
    location_test_AAA[i,"field_y1"] <- preds_field_position_y
    location_test_AAA[i,'dist_y1'] <- location_test_AAA[i,'final_ball_position_y'] - location_test_AAA[i, 'field_y1']
    location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x'])^2 + (location_test_AAA[i,'dist_y1'])^2)
  }
  if(i %% 750 == 0){
    print(i)
  }
}


location_test_AAA <- location_test_AAA  %>% 
  mutate(diff_y = abs(field_y - field_y1))

rmse(location_test_AAA, field_y, field_y1) #4.19

location_test_AAA %>% ggplot(aes(y = diff_y, x = time_in_play,group = primary_key, color = time_remaining)) +
  geom_path()

location_test_AAA <- read_csv("location_test_AAA1.csv", show_col_types = FALSE)



#### Final Field Model AAA Y ####

field_model_AAAy <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                             prev_distance + time_remaining + 
                             final_ball_position_x + 
                             time_in_play +
                             prev_field_x +
                             prev_dist_y +
                             final_ball_position_y, 
                           data = dat_AAA,
                           split.select.weights = split_weight_AAAy,
                           always.split.variables = c("final_ball_position_y"),
                           num.threads = 0,
                           num.trees = 300,
                           splitrule = "extratrees",
                           num.random.splits = optimized_rs_AAAy, #19
                           min.node.size = optimized_node_AAAy, #23
                           mtry = 5, 
                           min.bucket = optimized_minbucket_AAAy, #6
                           importance = "permutation",
                          scale.permutation.importance = TRUE,
                           seed = 101)

for (i in 1:nrow(location_test_AAA)){
  if (i > 1){
    if (location_test_AAA[i,"primary_key"] != location_test_AAA[i-1,'primary_key']){
      preds_field_position_y <- predict(field_model_AAAy, location_test_AAA[i,])$predictions
      location_test_AAA[i,"field_y1"] <- preds_field_position_y
      location_test_AAA[i,'dist_y1'] <- location_test_AAA[i,'final_ball_position_y'] - location_test_AAA[i, 'field_y1']
      location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x'])^2 + (location_test_AAA[i,'dist_y1'])^2)
    }else{
      location_test_AAA[i, "prev_field_y"] <- location_test_AAA[i-1, "field_y1"]
      location_test_AAA[i,"prev_distance"] <- location_test_AAA[i-1, "distance_from_ball"]
      location_test_AAA[i,"prev_dist_y"] <- location_test_AAA[i-1, "dist_y1"]
      preds_field_position_y <- predict(field_model_AAAy, location_test_AAA[i,])$predictions
      location_test_AAA[i,"field_y1"] <- preds_field_position_y
      location_test_AAA[i,'dist_y1'] <- location_test_AAA[i,'final_ball_position_y'] - location_test_AAA[i, 'field_y1']
      location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x'])^2 + (location_test_AAA[i,'dist_y1'])^2)
      # 
      # location_test_AAA <- location_test_AAA %>%
      #   mutate(dist_x = final_ball_position_x - field_x1,
      #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
    }
  }else{
    preds_field_position_y <- predict(field_model_AAAy, location_test_AAA[i,])$predictions
    location_test_AAA[i,"field_y1"] <- preds_field_position_y
    location_test_AAA[i,'dist_y1'] <- location_test_AAA[i,'final_ball_position_y'] - location_test_AAA[i, 'field_y1']
    location_test_AAA[i,'distance_from_ball'] <- sqrt((location_test_AAA[i,'dist_x'])^2 + (location_test_AAA[i,'dist_y1'])^2)
  }
  if(i %% 250 == 0){
    print(i)
  }
}

location_test_AAA <- location_test_AAA  %>% 
  mutate(diff_y = abs(field_y - field_y1))

rmse(location_test_AAA, field_y, field_y1) #3.52

location_test_AAA %>% ggplot(aes(y = diff_y, x = time_in_play,group = primary_key, color = time_remaining)) +
  geom_path()

importance_AAAy <- tibble(feature = field_model_AAAy$forest$independent.variable.names, importance = pull(as.data.frame(field_model_AAAy$variable.importance)),
                          split_weight = split_weight_AAAy) %>% 
  arrange(desc(importance))

print(importance_AAAy)

#### Tuning for AA ####

find_randsplit_AAy <- function(randsplit){
  randsplit <- ceiling(randsplit)
  model2 <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_x + 
                     time_in_play +
                     prev_field_x +
                     prev_dist_y +
                     final_ball_position_y, 
                   data = location_training_AA,
                   splitrule = "extratrees",
                   num.random.splits = randsplit,
                   split.select.weights = c(rep(0.5,7),0),
                   always.split.variables = c("final_ball_position_y"),
                   num.threads = 0,
                   num.trees = 300, 
                   mtry = 5,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_AA)){
    if (i > 1){
      if (location_test_AA[i,"primary_key"] != location_test_AA[i-1,'primary_key']){
        preds_field_position_y <- predict(model2, location_test_AA[i,])$predictions
        location_test_AA[i,"field_y1"] <- preds_field_position_y
        location_test_AA[i,'dist_y1'] <- location_test_AA[i,'final_ball_position_y'] - location_test_AA[i, 'field_y1']
        location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x'])^2 + (location_test_AA[i,'dist_y1'])^2)
      }else{
        location_test_AA[i, "prev_field_y"] <- location_test_AA[i-1, "field_y1"]
        location_test_AA[i,"prev_distance"] <- location_test_AA[i-1, "distance_from_ball"]
        location_test_AA[i,"prev_dist_y"] <- location_test_AA[i-1, "dist_y1"]
        preds_field_position_y <- predict(model2, location_test_AA[i,])$predictions
        location_test_AA[i,"field_y1"] <- preds_field_position_y
        location_test_AA[i,'dist_y1'] <- location_test_AA[i,'final_ball_position_y'] - location_test_AA[i, 'field_y1']
        location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x'])^2 + (location_test_AA[i,'dist_y1'])^2)
        # 
        # location_test_AA <- location_test_AA %>%
        #   mutate(dist_x = final_ball_position_x - field_y1,
        #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
      }
    }else{
      preds_field_position_y <- predict(model2, location_test_AA[i,])$predictions
      location_test_AA[i,"field_y1"] <- preds_field_position_y
      location_test_AA[i,'dist_y1'] <- location_test_AA[i,'final_ball_position_y'] - location_test_AA[i, 'field_y1']
      location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x'])^2 + (location_test_AA[i,'dist_y1'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  #preds <- predict(model2, location_test_AA)$predictions
  # location_test_AA <- location_test_AA %>% 
  #  mutate(diffs = (field_x - field_y1)*diff_weights)
  rmse <- sqrt(mean((location_test_AA$field_y - location_test_AA$field_y1)^2))
  print(randsplit)
  print(rmse)
  return(rmse)
}

rs_AAy <- tibble(
  randsplit = 2:20,
  rmse = map_dbl(2:20, find_randsplit_AAy)
)

rs_AAy <- rs_AAy %>% 
  slice_min(rmse, n = 1) 

print(rs_AAy)

optimized_rs_AAy <- rs_AAy$randsplit #minimizes at 2, rmse = 5.52

find_weight_AAy <- function(par){
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
  #weight <- round(weight, 2)
  model2 <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_x + 
                     time_in_play +
                     prev_field_x +
                     prev_dist_y +
                     final_ball_position_y, 
                   data = location_training_AA,
                   splitrule = 'extratrees',
                   num.random.splits = optimized_rs_AAy,
                   always.split.variables = c("final_ball_position_y"),
                   num.threads = 0,
                   num.trees = 300,
                   mtry = 5,
                   seed = 101,
                   split.select.weights = c(par[1],  par[2], par[3], par[4],
                                            par[5], par[6], par[7],0))
  print("Forest Complete")
  for (i in 1:nrow(location_test_AA)){
    if (i > 1){
      if (location_test_AA[i,"primary_key"] != location_test_AA[i-1,'primary_key']){
        preds_field_position_y <- predict(model2, location_test_AA[i,])$predictions
        location_test_AA[i,"field_y1"] <- preds_field_position_y
        location_test_AA[i,'dist_y1'] <- location_test_AA[i,'final_ball_position_y'] - location_test_AA[i, 'field_y1']
        location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x'])^2 + (location_test_AA[i,'dist_y1'])^2)
      }else{
        location_test_AA[i, "prev_field_y"] <- location_test_AA[i-1, "field_y1"]
        location_test_AA[i,"prev_distance"] <- location_test_AA[i-1, "distance_from_ball"]
        location_test_AA[i,"prev_dist_y"] <- location_test_AA[i-1, "dist_y1"]
        preds_field_position_y <- predict(model2, location_test_AA[i,])$predictions
        location_test_AA[i,"field_y1"] <- preds_field_position_y
        location_test_AA[i,'dist_y1'] <- location_test_AA[i,'final_ball_position_y'] - location_test_AA[i, 'field_y1']
        location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x'])^2 + (location_test_AA[i,'dist_y1'])^2)
        # 
        # location_test_AA <- location_test_AA %>%
        #   mutate(dist_x = final_ball_position_x - field_y1,
        #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
      }
    }else{
      preds_field_position_y <- predict(model2, location_test_AA[i,])$predictions
      location_test_AA[i,"field_y1"] <- preds_field_position_y
      location_test_AA[i,'dist_y1'] <- location_test_AA[i,'final_ball_position_y'] - location_test_AA[i, 'field_y1']
      location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x'])^2 + (location_test_AA[i,'dist_y1'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  #preds <- predict(model2, location_test_AA)$predictions
  # location_test_AA <- location_test_AA %>% 
  #  mutate(diffs = (field_x - field_y1)*diff_weights)
  rmse <- sqrt(mean((location_test_AA$field_y - location_test_AA$field_y1)^2))
  print(par)
  print(rmse)
  return(rmse)
}
(optimized_weight_AAy <- optim(par = c(0.5, 0.51, rep(0.5,5)), find_weight_AAy, hessian = TRUE, method = "L-BFGS-B", lower = 0.4, upper = 0.6, control = list(maxit = 5, trace = 6, factr = 0.01/.Machine$double.eps, pgtol = 1e-4, ndeps = rep(0.01,7), REPORT = 1))) #lower set to 0.0001 rather than 0 so mtry has enough variables to choose from 

#par: c(0.5000162, 0.5100142, 0.4999847, 0.4999836, 0.4999833, 0.5000192, 0.4999833)
#rmse = 4.937771

split_weight_AAy <- c(optimized_weight_AAy$par,0)

find_minnodesize_AAy <- function(node){
  node <- floor(node)
  model2 <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_x + 
                     time_in_play +
                     prev_field_x +
                     prev_dist_y +
                     final_ball_position_y, 
                   data = location_training_AA,
                   split.select.weights = split_weight_AAy,
                   always.split.variables = c("final_ball_position_y"),
                   num.threads = 0,
                   num.trees = 300,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_AAy,
                   min.node.size = node,
                   mtry = 5,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_AA)){
    if (i > 1){
      if (location_test_AA[i,"primary_key"] != location_test_AA[i-1,'primary_key']){
        preds_field_position_y <- predict(model2, location_test_AA[i,])$predictions
        location_test_AA[i,"field_y1"] <- preds_field_position_y
        location_test_AA[i,'dist_y1'] <- location_test_AA[i,'final_ball_position_y'] - location_test_AA[i, 'field_y1']
        location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x'])^2 + (location_test_AA[i,'dist_y1'])^2)
      }else{
        location_test_AA[i, "prev_field_y"] <- location_test_AA[i-1, "field_y1"]
        location_test_AA[i,"prev_distance"] <- location_test_AA[i-1, "distance_from_ball"]
        location_test_AA[i,"prev_dist_y"] <- location_test_AA[i-1, "dist_y1"]
        preds_field_position_y <- predict(model2, location_test_AA[i,])$predictions
        location_test_AA[i,"field_y1"] <- preds_field_position_y
        location_test_AA[i,'dist_y1'] <- location_test_AA[i,'final_ball_position_y'] - location_test_AA[i, 'field_y1']
        location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x'])^2 + (location_test_AA[i,'dist_y1'])^2)
        # 
        # location_test_AA <- location_test_AA %>%
        #   mutate(dist_x = final_ball_position_x - field_y1,
        #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
      }
    }else{
      preds_field_position_y <- predict(model2, location_test_AA[i,])$predictions
      location_test_AA[i,"field_y1"] <- preds_field_position_y
      location_test_AA[i,'dist_y1'] <- location_test_AA[i,'final_ball_position_y'] - location_test_AA[i, 'field_y1']
      location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x'])^2 + (location_test_AA[i,'dist_y1'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  #preds <- predict(model2, location_test_AA)$predictions
  # location_test_AA <- location_test_AA %>% 
  #  mutate(diffs = (field_x - field_y1)*diff_weights)
  rmse <- sqrt(mean((location_test_AA$field_y - location_test_AA$field_y1)^2))
  print(node)
  print(rmse)
  return(rmse)
}

(optimized_minnodesize_AAy <- optimize(find_minnodesize_AAy, c(5,25), tol = 1))
optimized_node_AAy <- floor(optimized_minnodesize_AAy$minimum) #optimized at 9, rmse = 5.17

find_minbucket_AAy <- function(bucket){
  bucket <- floor(bucket)
  model2 <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_x + 
                     time_in_play +
                     prev_field_x +
                     prev_dist_y +
                     final_ball_position_y, 
                   data = location_training_AA,
                   split.select.weights = split_weight_AAy,
                   always.split.variables = c("final_ball_position_y"),
                   num.threads = 0,
                   num.trees = 300,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_AAy,
                   min.node.size = optimized_node_AAy,
                   mtry = 5,
                   min.bucket = bucket,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_AA)){
    if (i > 1){
      if (location_test_AA[i,"primary_key"] != location_test_AA[i-1,'primary_key']){
        preds_field_position_y <- predict(model2, location_test_AA[i,])$predictions
        location_test_AA[i,"field_y1"] <- preds_field_position_y
        location_test_AA[i,'dist_y1'] <- location_test_AA[i,'final_ball_position_y'] - location_test_AA[i, 'field_y1']
        location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x'])^2 + (location_test_AA[i,'dist_y1'])^2)
      }else{
        location_test_AA[i, "prev_field_y"] <- location_test_AA[i-1, "field_y1"]
        location_test_AA[i,"prev_distance"] <- location_test_AA[i-1, "distance_from_ball"]
        location_test_AA[i,"prev_dist_y"] <- location_test_AA[i-1, "dist_y1"]
        preds_field_position_y <- predict(model2, location_test_AA[i,])$predictions
        location_test_AA[i,"field_y1"] <- preds_field_position_y
        location_test_AA[i,'dist_y1'] <- location_test_AA[i,'final_ball_position_y'] - location_test_AA[i, 'field_y1']
        location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x'])^2 + (location_test_AA[i,'dist_y1'])^2)
        # 
        # location_test_AA <- location_test_AA %>%
        #   mutate(dist_x = final_ball_position_x - field_y1,
        #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
      }
    }else{
      preds_field_position_y <- predict(model2, location_test_AA[i,])$predictions
      location_test_AA[i,"field_y1"] <- preds_field_position_y
      location_test_AA[i,'dist_y1'] <- location_test_AA[i,'final_ball_position_y'] - location_test_AA[i, 'field_y1']
      location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x'])^2 + (location_test_AA[i,'dist_y1'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  #preds <- predict(model2, location_test_AA)$predictions
  # location_test_AA <- location_test_AA %>% 
  #  mutate(diffs = (field_x - field_y1)*diff_weights)
  rmse <- sqrt(mean((location_test_AA$field_y - location_test_AA$field_y1)^2))
  print(bucket)
  print(rmse)
  return(rmse)
}

(optimized_minbucket_AAy <- optimize(find_minbucket_AAy, c(1,floor(optimized_node_AAy/2)+1), tol = 1)) # optimized at 
optimized_minbucket_AAy <- floor(optimized_minbucket_AAy$minimum) #rmse = 5.11


find_ntree_AAy <- function(ntree){
  model2 <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_x + 
                     time_in_play +
                     prev_field_x +
                     prev_dist_y +
                     final_ball_position_y, 
                   data = location_training_AA,
                   split.select.weights = split_weight_AAy,
                   always.split.variables = c("final_ball_position_y"),
                   num.threads = 0,
                   num.trees = ntree,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_AAy,
                   min.node.size = optimized_node_AAy,
                   mtry = 5,
                   min.bucket = optimized_minbucket_AAy,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_AA)){
    if (i > 1){
      if (location_test_AA[i,"primary_key"] != location_test_AA[i-1,'primary_key']){
        preds_field_position_y <- predict(model2, location_test_AA[i,])$predictions
        location_test_AA[i,"field_y1"] <- preds_field_position_y
        location_test_AA[i,'dist_y1'] <- location_test_AA[i,'final_ball_position_y'] - location_test_AA[i, 'field_y1']
        location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x'])^2 + (location_test_AA[i,'dist_y1'])^2)
      }else{
        location_test_AA[i, "prev_field_y"] <- location_test_AA[i-1, "field_y1"]
        location_test_AA[i,"prev_distance"] <- location_test_AA[i-1, "distance_from_ball"]
        location_test_AA[i,"prev_dist_y"] <- location_test_AA[i-1, "dist_y1"]
        preds_field_position_y <- predict(model2, location_test_AA[i,])$predictions
        location_test_AA[i,"field_y1"] <- preds_field_position_y
        location_test_AA[i,'dist_y1'] <- location_test_AA[i,'final_ball_position_y'] - location_test_AA[i, 'field_y1']
        location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x'])^2 + (location_test_AA[i,'dist_y1'])^2)
        # 
        # location_test_AA <- location_test_AA %>%
        #   mutate(dist_x = final_ball_position_x - field_y1,
        #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
      }
    }else{
      preds_field_position_y <- predict(model2, location_test_AA[i,])$predictions
      location_test_AA[i,"field_y1"] <- preds_field_position_y
      location_test_AA[i,'dist_y1'] <- location_test_AA[i,'final_ball_position_y'] - location_test_AA[i, 'field_y1']
      location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x'])^2 + (location_test_AA[i,'dist_y1'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  #preds <- predict(model2, location_test_AA)$predictions
  # location_test_AA <- location_test_AA %>% 
  #  mutate(diffs = (field_x - field_y1)*diff_weights)
  rmse <- sqrt(mean((location_test_AA$field_y - location_test_AA$field_y1)^2))
  print(ntree)
  print(rmse)
  return(rmse)
}

tree_perf_AAy <- tibble(
  ntree = 1:6*100,
  rmse_value = map_dbl(1:6*100, find_ntree_AAy)
)

tree_perf_AAy %>% ggplot(aes(ntree,rmse_value)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  ylab("RMSE") +
  scale_x_continuous(breaks = seq(0,1000,200)) #optimizes at 300


field_model_AAy <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                            prev_distance + time_remaining + 
                            final_ball_position_x + 
                            time_in_play +
                            prev_field_x +
                            prev_dist_y +
                            final_ball_position_y, 
                          data = location_training_AA,
                          split.select.weights = split_weight_AAy,
                          always.split.variables = c("final_ball_position_y"),
                          num.threads = 0,
                          num.trees = 300,
                          splitrule = "extratrees",
                          num.random.splits = optimized_rs_AAy,
                          min.node.size = optimized_node_AAy,
                          mtry = 5,
                          min.bucket = optimized_minbucket_AAy,
                          importance = "permutation",
                          seed = 101)

for (i in 1:nrow(location_test_AA)){
  if (i > 1){
    if (location_test_AA[i,"primary_key"] != location_test_AA[i-1,'primary_key']){
      preds_field_position_y <- predict(field_model_AAy, location_test_AA[i,])$predictions
      location_test_AA[i,"field_y1"] <- preds_field_position_y
      location_test_AA[i,'dist_y1'] <- location_test_AA[i,'final_ball_position_y'] - location_test_AA[i, 'field_y1']
      location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x'])^2 + (location_test_AA[i,'dist_y1'])^2)
    }else{
      location_test_AA[i, "prev_field_y"] <- location_test_AA[i-1, "field_y1"]
      location_test_AA[i,"prev_distance"] <- location_test_AA[i-1, "distance_from_ball"]
      location_test_AA[i,"prev_dist_y"] <- location_test_AA[i-1, "dist_y1"]
      preds_field_position_y <- predict(field_model_AAy, location_test_AA[i,])$predictions
      location_test_AA[i,"field_y1"] <- preds_field_position_y
      location_test_AA[i,'dist_y1'] <- location_test_AA[i,'final_ball_position_y'] - location_test_AA[i, 'field_y1']
      location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x'])^2 + (location_test_AA[i,'dist_y1'])^2)
      # 
      # location_test_AA <- location_test_AA %>%
      #   mutate(dist_x = final_ball_position_x - field_x1,
      #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
    }
  }else{
    preds_field_position_y <- predict(field_model_AAy, location_test_AA[i,])$predictions
    location_test_AA[i,"field_y1"] <- preds_field_position_y
    location_test_AA[i,'dist_y1'] <- location_test_AA[i,'final_ball_position_y'] - location_test_AA[i, 'field_y1']
    location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x'])^2 + (location_test_AA[i,'dist_y1'])^2)
  }
  if(i %% 250 == 0){
    print(i)
  }
}

location_test_AA <- location_test_AA  %>% 
  mutate(diff_y = abs(field_y - field_y1))

rmse(location_test_AA, field_y, field_y1) #5.11

location_test_AA %>% ggplot(aes(y = diff_y, x = time_in_play,group = primary_key, color = time_remaining)) +
  geom_path()

location_test_AA <- read_csv("location_test_AA1.csv", show_col_types = FALSE)

#### Final Field Model AA Y ####

field_model_AAy <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                            prev_distance + time_remaining + 
                            final_ball_position_x + 
                            time_in_play +
                            prev_field_x +
                            prev_dist_y +
                            final_ball_position_y, 
                          data = dat_AA,
                          split.select.weights = split_weight_AAy,
                          always.split.variables = c("final_ball_position_y"),
                          num.threads = 0,
                          num.trees = 300,
                          splitrule = "extratrees",
                          num.random.splits = optimized_rs_AAy, #4
                          min.node.size = optimized_node_AAy, #11
                          mtry = 5,
                          min.bucket = optimized_minbucket_AAy, #2
                          importance = "permutation",
                          scale.permutation.importance = TRUE,
                          seed = 101)

for (i in 1:nrow(location_test_AA)){
  if (i > 1){
    if (location_test_AA[i,"primary_key"] != location_test_AA[i-1,'primary_key']){
      preds_field_position_y <- predict(field_model_AAy, location_test_AA[i,])$predictions
      location_test_AA[i,"field_y1"] <- preds_field_position_y
      location_test_AA[i,'dist_y1'] <- location_test_AA[i,'final_ball_position_y'] - location_test_AA[i, 'field_y1']
      location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x'])^2 + (location_test_AA[i,'dist_y1'])^2)
    }else{
      location_test_AA[i, "prev_field_y"] <- location_test_AA[i-1, "field_y1"]
      location_test_AA[i,"prev_distance"] <- location_test_AA[i-1, "distance_from_ball"]
      location_test_AA[i,"prev_dist_y"] <- location_test_AA[i-1, "dist_y1"]
      preds_field_position_y <- predict(field_model_AAy, location_test_AA[i,])$predictions
      location_test_AA[i,"field_y1"] <- preds_field_position_y
      location_test_AA[i,'dist_y1'] <- location_test_AA[i,'final_ball_position_y'] - location_test_AA[i, 'field_y1']
      location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x'])^2 + (location_test_AA[i,'dist_y1'])^2)
      # 
      # location_test_AA <- location_test_AA %>%
      #   mutate(dist_x = final_ball_position_x - field_x1,
      #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
    }
  }else{
    preds_field_position_y <- predict(field_model_AAy, location_test_AA[i,])$predictions
    location_test_AA[i,"field_y1"] <- preds_field_position_y
    location_test_AA[i,'dist_y1'] <- location_test_AA[i,'final_ball_position_y'] - location_test_AA[i, 'field_y1']
    location_test_AA[i,'distance_from_ball'] <- sqrt((location_test_AA[i,'dist_x'])^2 + (location_test_AA[i,'dist_y1'])^2)
  }
  if(i %% 250 == 0){
    print(i)
  }
}

location_test_AA <- location_test_AA  %>% 
  mutate(diff_y = abs(field_y - field_y1))

rmse(location_test_AA, field_y, field_y1) #2.08

location_test_AA %>% ggplot(aes(y = diff_y, x = time_in_play,group = primary_key, color = time_remaining)) +
  geom_path()


importance_AAy <- tibble(feature = field_model_AAy$forest$independent.variable.names, importance = pull(as.data.frame(field_model_AAy$variable.importance))) %>% 
  arrange(desc(importance))

print(importance_AAy)

#### Tuning For A+ ####
find_randsplit_Aplusy <- function(randsplit){
  randsplit <- ceiling(randsplit)
  model2 <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_x + 
                     time_in_play +
                     prev_field_x +
                     prev_dist_y +
                     final_ball_position_y, 
                   data = location_training_Aplus,
                   splitrule = 'extratrees',
                   num.random.splits = randsplit,
                   always.split.variables = c("final_ball_position_y"),
                   num.threads = 0,
                   num.trees = 300,
                   mtry = 5,
                   min.node.size = 10,
                   seed = 101,
                   split.select.weights = c(rep(0.5,7),0))
  print("Forest Complete")
  for (i in 1:nrow(location_test_Aplus)){
    if (i > 1){
      if (location_test_Aplus[i,"primary_key"] != location_test_Aplus[i-1,'primary_key']){
        preds_field_position_y <- predict(model2, location_test_Aplus[i,])$predictions
        location_test_Aplus[i,"field_y1"] <- preds_field_position_y
        location_test_Aplus[i,'dist_y1'] <- location_test_Aplus[i,'final_ball_position_y'] - location_test_Aplus[i, 'field_y1']
        location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x'])^2 + (location_test_Aplus[i,'dist_y1'])^2)
      }else{
        location_test_Aplus[i, "prev_field_y"] <- location_test_Aplus[i-1, "field_y1"]
        location_test_Aplus[i,"prev_distance"] <- location_test_Aplus[i-1, "distance_from_ball"]
        location_test_Aplus[i,"prev_dist_y"] <- location_test_Aplus[i-1, "dist_y1"]
        preds_field_position_y <- predict(model2, location_test_Aplus[i,])$predictions
        location_test_Aplus[i,"field_y1"] <- preds_field_position_y
        location_test_Aplus[i,'dist_y1'] <- location_test_Aplus[i,'final_ball_position_y'] - location_test_Aplus[i, 'field_y1']
        location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x'])^2 + (location_test_Aplus[i,'dist_y1'])^2)
        # 
        # location_test_Aplus <- location_test_Aplus %>%
        #   mutate(dist_x = final_ball_position_x - field_y1,
        #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
      }
    }else{
      preds_field_position_y <- predict(model2, location_test_Aplus[i,])$predictions
      location_test_Aplus[i,"field_y1"] <- preds_field_position_y
      location_test_Aplus[i,'dist_y1'] <- location_test_Aplus[i,'final_ball_position_y'] - location_test_Aplus[i, 'field_y1']
      location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x'])^2 + (location_test_Aplus[i,'dist_y1'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  #preds <- predict(model2, location_test_AA)$predictions
  # location_test_AA <- location_test_AA %>% 
  #  mutate(diffs = (field_x - field_y1)*diff_weights)
  rmse <- sqrt(mean((location_test_Aplus$field_y - location_test_Aplus$field_y1)^2))
  print(randsplit)
  print(rmse)
  return(rmse)
}

rs_Aplusy <- tibble(
  randsplit = 2:20,
  rmse = map_dbl(2:20, find_randsplit_Aplusy)
)

rs_Aplusy <- rs_Aplusy %>%
  slice_min(rmse, n = 1)

print(rs_Aplusy)

optimized_rs_Aplusy <- rs_Aplusy$randsplit # optimized at 9, rmse = 6.69

find_weight_Aplusy <- function(par){
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
  #weight <- round(weight, 2)
  model2 <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_x + 
                     time_in_play +
                     prev_field_x +
                     prev_dist_y +
                     final_ball_position_y, 
                   data = location_training_Aplus,
                   splitrule = 'extratrees',
                   num.random.splits = optimized_rs_Aplusy,
                   always.split.variables = c("final_ball_position_y"),
                   num.threads = 0,
                   num.trees = 300,
                   mtry = 5,
                   min.node.size = 10,
                   seed = 101,
                   split.select.weights = c(par[1],  par[2], par[3], par[4],
                                            par[5], par[6], par[7],0))
  print("Forest Complete")
  for (i in 1:nrow(location_test_Aplus)){
    if (i > 1){
      if (location_test_Aplus[i,"primary_key"] != location_test_Aplus[i-1,'primary_key']){
        preds_field_position_y <- predict(model2, location_test_Aplus[i,])$predictions
        location_test_Aplus[i,"field_y1"] <- preds_field_position_y
        location_test_Aplus[i,'dist_y1'] <- location_test_Aplus[i,'final_ball_position_y'] - location_test_Aplus[i, 'field_y1']
        location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x'])^2 + (location_test_Aplus[i,'dist_y1'])^2)
      }else{
        location_test_Aplus[i, "prev_field_y"] <- location_test_Aplus[i-1, "field_y1"]
        location_test_Aplus[i,"prev_distance"] <- location_test_Aplus[i-1, "distance_from_ball"]
        location_test_Aplus[i,"prev_dist_y"] <- location_test_Aplus[i-1, "dist_y1"]
        preds_field_position_y <- predict(model2, location_test_Aplus[i,])$predictions
        location_test_Aplus[i,"field_y1"] <- preds_field_position_y
        location_test_Aplus[i,'dist_y1'] <- location_test_Aplus[i,'final_ball_position_y'] - location_test_Aplus[i, 'field_y1']
        location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x'])^2 + (location_test_Aplus[i,'dist_y1'])^2)
        # 
        # location_test_Aplus <- location_test_Aplus %>%
        #   mutate(dist_x = final_ball_position_x - field_y1,
        #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
      }
    }else{
      preds_field_position_y <- predict(model2, location_test_Aplus[i,])$predictions
      location_test_Aplus[i,"field_y1"] <- preds_field_position_y
      location_test_Aplus[i,'dist_y1'] <- location_test_Aplus[i,'final_ball_position_y'] - location_test_Aplus[i, 'field_y1']
      location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x'])^2 + (location_test_Aplus[i,'dist_y1'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  #preds <- predict(model2, location_test_Aplus)$predictions
  # location_test_Aplus <- location_test_Aplus %>% 
  #  mutate(diffs = (field_x - field_y1)*diff_weights)
  rmse <- sqrt(mean((location_test_Aplus$field_y - location_test_Aplus$field_y1)^2))
  print(par)
  print(rmse)
  return(rmse)
}
(optimized_weight_Aplusy <- optim(par = c(rep(0.5,7)), find_weight_Aplusy, hessian = TRUE, method = "L-BFGS-B", lower = 0.47, upper = 0.53, control = list(maxit = 5, trace = 6, factr = 0.01/.Machine$double.eps, pgtol = 1e-4, ndeps = rep(0.001,7), REPORT = 1))) #lower set to 0.0001 rather than 0 so mtry has enough variables to choose from 

# par = c(0.47, 0.47, 0.47, 0.470422, 0.53, 0.53, 0.529206) 
# rmse =  6.36763

split_weight_Aplusy <- c(optimized_weight_Aplusy$par,0)

find_minnodesize_Aplusy <- function(node){
  node <- floor(node)
  model2 <- ranger(field_y ~ prev_field_y + 
                     prev_distance + time_remaining + 
                     final_ball_position_x + 
                     time_in_play +
                     prev_field_x +
                     prev_dist_y +
                     final_ball_position_y, 
                   data = location_training_Aplus,
                   num.trees = 300,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_Aplusy,
                   always.split.variables = c("final_ball_position_y"),
                   num.threads = 0,
                   split.select.weights = split_weight_Aplusy,
                   mtry = 5,
                   min.node.size = node,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_Aplus)){
    if (i > 1){
      if (location_test_Aplus[i,"primary_key"] != location_test_Aplus[i-1,'primary_key']){
        preds_field_position_y <- predict(model2, location_test_Aplus[i,])$predictions
        location_test_Aplus[i,"field_y1"] <- preds_field_position_y
        location_test_Aplus[i,'dist_y1'] <- location_test_Aplus[i,'final_ball_position_y'] - location_test_Aplus[i, 'field_y1']
        location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x'])^2 + (location_test_Aplus[i,'dist_y1'])^2)
      }else{
        location_test_Aplus[i, "prev_field_y"] <- location_test_Aplus[i-1, "field_y1"]
        location_test_Aplus[i,"prev_distance"] <- location_test_Aplus[i-1, "distance_from_ball"]
        location_test_Aplus[i,"prev_dist_y"] <- location_test_Aplus[i-1, "dist_y1"]
        preds_field_position_y <- predict(model2, location_test_Aplus[i,])$predictions
        location_test_Aplus[i,"field_y1"] <- preds_field_position_y
        location_test_Aplus[i,'dist_y1'] <- location_test_Aplus[i,'final_ball_position_y'] - location_test_Aplus[i, 'field_y1']
        location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x'])^2 + (location_test_Aplus[i,'dist_y1'])^2)
      }
    }else{
      preds_field_position_y <- predict(model2, location_test_Aplus[i,])$predictions
      location_test_Aplus[i,"field_y1"] <- preds_field_position_y
      location_test_Aplus[i,'dist_y1'] <- location_test_Aplus[i,'final_ball_position_y'] - location_test_Aplus[i, 'field_y1']
      location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x'])^2 + (location_test_Aplus[i,'dist_y1'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  rmse <- sqrt(mean((location_test_Aplus$field_y - location_test_Aplus$field_y1)^2))
  print(node)
  print(rmse)
  return(rmse)
}

(optimized_minnodesize_Aplusy <- optimize(find_minnodesize_Aplusy, c(11,25), tol = 1))
optimized_node_Aplusy <- floor(optimized_minnodesize_Aplusy$minimum) #optimized at 15, rmse = 7.33

find_minbucket_Aplusy <- function(bucket){
  bucket <- floor(bucket)
  model2 <- ranger(field_y ~ prev_field_y + 
                     prev_distance + time_remaining + 
                     final_ball_position_x + 
                     time_in_play +
                     prev_field_x +
                     prev_dist_y +
                     final_ball_position_y, 
                   data = location_training_Aplus,
                   always.split.variables = c("final_ball_position_y"),
                   num.threads = 0,
                   split.select.weights = split_weight_Aplusy,
                   num.trees = 300,
                    splitrule = "extratrees",
                    num.random.splits = optimized_rs_Aplusy,
                   min.node.size = optimized_node_Aplusy,
                   mtry = 5,
                   min.bucket = bucket,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_Aplus)){
    if (i > 1){
      if (location_test_Aplus[i,"primary_key"] != location_test_Aplus[i-1,'primary_key']){
        preds_field_position_y <- predict(model2, location_test_Aplus[i,])$predictions
        location_test_Aplus[i,"field_y1"] <- preds_field_position_y
        location_test_Aplus[i,'dist_y1'] <- location_test_Aplus[i,'final_ball_position_y'] - location_test_Aplus[i, 'field_y1']
        location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x'])^2 + (location_test_Aplus[i,'dist_y1'])^2)
      }else{
        location_test_Aplus[i, "prev_field_y"] <- location_test_Aplus[i-1, "field_y1"]
        location_test_Aplus[i,"prev_distance"] <- location_test_Aplus[i-1, "distance_from_ball"]
        location_test_Aplus[i,"prev_dist_y"] <- location_test_Aplus[i-1, "dist_y1"]
        preds_field_position_y <- predict(model2, location_test_Aplus[i,])$predictions
        location_test_Aplus[i,"field_y1"] <- preds_field_position_y
        location_test_Aplus[i,'dist_y1'] <- location_test_Aplus[i,'final_ball_position_y'] - location_test_Aplus[i, 'field_y1']
        location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x'])^2 + (location_test_Aplus[i,'dist_y1'])^2)
      }
    }else{
      preds_field_position_y <- predict(model2, location_test_Aplus[i,])$predictions
      location_test_Aplus[i,"field_y1"] <- preds_field_position_y
      location_test_Aplus[i,'dist_y1'] <- location_test_Aplus[i,'final_ball_position_y'] - location_test_Aplus[i, 'field_y1']
      location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x'])^2 + (location_test_Aplus[i,'dist_y1'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  rmse <- sqrt(mean((location_test_Aplus$field_y - location_test_Aplus$field_y1)^2))
  print(bucket)
  print(rmse)
  return(rmse)
}

(optimized_minbucket_Aplusy <- optimize(find_minbucket_Aplusy, c(1,floor(optimized_node_Aplusy/2)+1), tol = 1)) # optimized at 4
optimized_minbucket_Aplusy <- floor(optimized_minbucket_Aplusy$minimum) #rmse = 7.06


find_ntree_Aplusy <- function(ntree){
  model2 <- ranger(field_y ~ prev_field_y +
                     prev_distance + time_remaining + 
                     final_ball_position_x + 
                     time_in_play +
                     prev_field_x +
                     prev_dist_y +
                     final_ball_position_y, 
                   data = location_training_Aplus,
                   always.split.variables = c("final_ball_position_y"),
                   num.threads = 0,
                   split.select.weights = split_weight_Aplusy,
                   num.trees = ntree,
                    splitrule = "extratrees",
                    num.random.splits = optimized_rs_Aplusy,
                   min.node.size = optimized_node_Aplusy,
                   mtry = 5,
                   min.bucket = optimized_minbucket_Aplusy,
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_Aplus)){
    if (i > 1){
      if (location_test_Aplus[i,"primary_key"] != location_test_Aplus[i-1,'primary_key']){
        preds_field_position_y <- predict(model2, location_test_Aplus[i,])$predictions
        location_test_Aplus[i,"field_y1"] <- preds_field_position_y
        location_test_Aplus[i,'dist_y1'] <- location_test_Aplus[i,'final_ball_position_y'] - location_test_Aplus[i, 'field_y1']
        location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x'])^2 + (location_test_Aplus[i,'dist_y1'])^2)
      }else{
        location_test_Aplus[i, "prev_field_y"] <- location_test_Aplus[i-1, "field_y1"]
        location_test_Aplus[i,"prev_distance"] <- location_test_Aplus[i-1, "distance_from_ball"]
        location_test_Aplus[i,"prev_dist_y"] <- location_test_Aplus[i-1, "dist_y1"]
        preds_field_position_y <- predict(model2, location_test_Aplus[i,])$predictions
        location_test_Aplus[i,"field_y1"] <- preds_field_position_y
        location_test_Aplus[i,'dist_y1'] <- location_test_Aplus[i,'final_ball_position_y'] - location_test_Aplus[i, 'field_y1']
        location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x'])^2 + (location_test_Aplus[i,'dist_y1'])^2)
      }
    }else{
      preds_field_position_y <- predict(model2, location_test_Aplus[i,])$predictions
      location_test_Aplus[i,"field_y1"] <- preds_field_position_y
      location_test_Aplus[i,'dist_y1'] <- location_test_Aplus[i,'final_ball_position_y'] - location_test_Aplus[i, 'field_y1']
      location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x'])^2 + (location_test_Aplus[i,'dist_y1'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  rmse <- sqrt(mean((location_test_Aplus$field_y - location_test_Aplus$field_y1)^2))
  print(ntree)
  print(rmse)
  return(rmse)
}

tree_perf_Aplusy <- tibble(
  ntree = 1:6*100,
  rmse_value = map_dbl(1:6*100, find_ntree_Aplusy)
)

tree_perf_Aplusy %>% ggplot(aes(ntree,rmse_value)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  ylab("RMSE") +
  scale_x_continuous(breaks = seq(0,600,200)) #optimizes at 300


field_model_Aplusy <- ranger(field_y ~ prev_field_y +  
                               prev_distance + time_remaining + 
                               final_ball_position_x + 
                               time_in_play +
                               prev_field_x +
                               prev_dist_y +
                               final_ball_position_y, 
                             data = location_training_Aplus,
                             always.split.variables = c("final_ball_position_y"),
                             num.threads = 0,
                             split.select.weights = split_weight_Aplusy,
                             num.trees = 300,
                              splitrule = "extratrees",
                              num.random.splits = optimized_rs_Aplusy,
                             min.node.size = optimized_node_Aplusy,
                             mtry = 5,
                             min.bucket = optimized_minbucket_Aplusy,
                             importance = "permutation",
                             seed = 101)

for (i in 1:nrow(location_test_Aplus)){
  if (i > 1){
    if (location_test_Aplus[i,"primary_key"] != location_test_Aplus[i-1,'primary_key']){
      preds_field_position_y <- predict(field_model_Aplusy, location_test_Aplus[i,])$predictions
      location_test_Aplus[i,"field_y1"] <- preds_field_position_y
      location_test_Aplus[i,'dist_y1'] <- location_test_Aplus[i,'final_ball_position_y'] - location_test_Aplus[i, 'field_y1']
      location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x'])^2 + (location_test_Aplus[i,'dist_y1'])^2)
    }else{
      location_test_Aplus[i, "prev_field_y"] <- location_test_Aplus[i-1, "field_y1"]
      location_test_Aplus[i,"prev_distance"] <- location_test_Aplus[i-1, "distance_from_ball"]
      location_test_Aplus[i,"prev_dist_y"] <- location_test_Aplus[i-1, "dist_y1"]
      preds_field_position_y <- predict(field_model_Aplusy, location_test_Aplus[i,])$predictions
      location_test_Aplus[i,"field_y1"] <- preds_field_position_y
      location_test_Aplus[i,'dist_y1'] <- location_test_Aplus[i,'final_ball_position_y'] - location_test_Aplus[i, 'field_y1']
      location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x'])^2 + (location_test_Aplus[i,'dist_y1'])^2)
      # 
      # location_test_Aplus <- location_test_Aplus %>%
      #   mutate(dist_x = final_ball_position_x - field_x1,
      #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
    }
  }else{
    preds_field_position_y <- predict(field_model_Aplusy, location_test_Aplus[i,])$predictions
    location_test_Aplus[i,"field_y1"] <- preds_field_position_y
    location_test_Aplus[i,'dist_y1'] <- location_test_Aplus[i,'final_ball_position_y'] - location_test_Aplus[i, 'field_y1']
    location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x'])^2 + (location_test_Aplus[i,'dist_y1'])^2)
  }
  if(i %% 250 == 0){
    print(i)
  }
}

location_test_Aplus <- location_test_Aplus %>% 
  mutate(diff_y = abs(field_y - field_y1))

rmse(location_test_Aplus, field_y, field_y1) #7.06

location_test_Aplus %>% ggplot(aes(y = diff_y, x = time_in_play,group = primary_key, color = time_remaining)) +
  geom_path()

location_test_Aplus <- read_csv("location_test_Aplus1.csv", show_col_types = FALSE)


#### Final Field Model A+ Y ####

field_model_Aplusy <- ranger(field_y ~ prev_field_y +  
                               prev_distance + time_remaining + 
                               final_ball_position_x + 
                               time_in_play +
                               prev_field_x +
                               prev_dist_y +
                               final_ball_position_y, 
                             data = dat_Aplus,
                             num.trees = 300,
                             always.split.variables = c("final_ball_position_y"),
                             num.threads = 0,
                             split.select.weights = split_weight_Aplusy,
                              splitrule = "extratrees",
                              num.random.splits = optimized_rs_Aplusy, #9
                             min.node.size = optimized_node_Aplusy, #15
                             mtry = 5, 
                             min.bucket = optimized_minbucket_Aplusy, #4
                             importance = "permutation",
                             scale.permutation.importance = TRUE,
                             seed = 101)

for (i in 1:nrow(location_test_Aplus)){
  if (i > 1){
    if (location_test_Aplus[i,"primary_key"] != location_test_Aplus[i-1,'primary_key']){
      preds_field_position_y <- predict(field_model_Aplusy, location_test_Aplus[i,])$predictions
      location_test_Aplus[i,"field_y1"] <- preds_field_position_y
      location_test_Aplus[i,'dist_y1'] <- location_test_Aplus[i,'final_ball_position_y'] - location_test_Aplus[i, 'field_y1']
      location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x'])^2 + (location_test_Aplus[i,'dist_y1'])^2)
    }else{
      location_test_Aplus[i, "prev_field_y"] <- location_test_Aplus[i-1, "field_y1"]
      location_test_Aplus[i,"prev_distance"] <- location_test_Aplus[i-1, "distance_from_ball"]
      location_test_Aplus[i,"prev_dist_y"] <- location_test_Aplus[i-1, "dist_y1"]
      preds_field_position_y <- predict(field_model_Aplusy, location_test_Aplus[i,])$predictions
      location_test_Aplus[i,"field_y1"] <- preds_field_position_y
      location_test_Aplus[i,'dist_y1'] <- location_test_Aplus[i,'final_ball_position_y'] - location_test_Aplus[i, 'field_y1']
      location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x'])^2 + (location_test_Aplus[i,'dist_y1'])^2)
      # 
      # location_test_Aplus <- location_test_Aplus %>%
      #   mutate(dist_x = final_ball_position_x - field_x1,
      #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
    }
  }else{
    preds_field_position_y <- predict(field_model_Aplusy, location_test_Aplus[i,])$predictions
    location_test_Aplus[i,"field_y1"] <- preds_field_position_y
    location_test_Aplus[i,'dist_y1'] <- location_test_Aplus[i,'final_ball_position_y'] - location_test_Aplus[i, 'field_y1']
    location_test_Aplus[i,'distance_from_ball'] <- sqrt((location_test_Aplus[i,'dist_x'])^2 + (location_test_Aplus[i,'dist_y1'])^2)
  }
  if(i %% 250 == 0){
    print(i)
  }
}

location_test_Aplus <- location_test_Aplus %>% 
  mutate(diff_y = abs(field_y - field_y1))

rmse(location_test_Aplus, field_y, field_y1) #6.45

location_test_Aplus %>% ggplot(aes(y = diff_y, x = time_in_play,group = primary_key, color = time_remaining)) +
  geom_path()

importance_Aplusy <- tibble(feature = field_model_Aplusy$forest$independent.variable.names, importance = pull(as.data.frame(field_model_Aplusy$variable.importance))) %>% 
  arrange(desc(importance))

print(importance_Aplusy)

#### Tuning for A ####
find_randsplit_Ay <- function(randsplit){
  randsplit <- ceiling(randsplit)
  model2 <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_x + 
                     time_in_play +
                     prev_field_x +
                     prev_dist_y +
                     final_ball_position_y, 
                   data = location_training_A,
                   splitrule = "extratrees",
                   num.random.splits = randsplit,
                   always.split.variables = c("final_ball_position_y"),
                   split.select.weights = c(rep(0.5,7),0),
                   min.node.size = 10,
                   num.threads = 0,
                   mtry = 5,
                   num.trees = 300, 
                   seed = 101)
  print("Forest Complete")
  for (i in 1:nrow(location_test_A)){
    if (i > 1){
      if (location_test_A[i,"primary_key"] != location_test_A[i-1,'primary_key']){
        preds_field_position_y <- predict(model2, location_test_A[i,])$predictions
        location_test_A[i,"field_y1"] <- preds_field_position_y
        location_test_A[i,'dist_y1'] <- location_test_A[i,'final_ball_position_y'] - location_test_A[i, 'field_y1']
        location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x'])^2 + (location_test_A[i,'dist_y1'])^2)
      }else{
        location_test_A[i, "prev_field_y"] <- location_test_A[i-1, "field_y1"]
        location_test_A[i,"prev_distance"] <- location_test_A[i-1, "distance_from_ball"]
        location_test_A[i,"prev_dist_y"] <- location_test_A[i-1, "dist_y1"]
        preds_field_position_y <- predict(model2, location_test_A[i,])$predictions
        location_test_A[i,"field_y1"] <- preds_field_position_y
        location_test_A[i,'dist_y1'] <- location_test_A[i,'final_ball_position_y'] - location_test_A[i, 'field_y1']
        location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x'])^2 + (location_test_A[i,'dist_y1'])^2)
        # 
        # location_test_A <- location_test_A %>%
        #   mutate(dist_x = final_ball_position_x - field_y1,
        #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
      }
    }else{
      preds_field_position_y <- predict(model2, location_test_A[i,])$predictions
      location_test_A[i,"field_y1"] <- preds_field_position_y
      location_test_A[i,'dist_y1'] <- location_test_A[i,'final_ball_position_y'] - location_test_A[i, 'field_y1']
      location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x'])^2 + (location_test_A[i,'dist_y1'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  #preds <- predict(model2, location_test_A)$predictions
  # location_test_A <- location_test_A %>% 
  #  mutate(diffs = (field_x - field_y1)*diff_weights)
  rmse <- sqrt(mean((location_test_A$field_y - location_test_A$field_y1)^2))
  print(randsplit)
  print(rmse)
  return(rmse)
}

rs_Ay <- tibble(
  randsplit = 2:20,
  rmse = map_dbl(2:20, find_randsplit_Ay)
)

rs_Ay <- rs_Ay %>%
  slice_min(rmse, n = 1)

print(rs_Ay)

optimized_rs_Ay <- rs_Ay$randsplit #minimizes at 2, rmse = 5.39

find_weight_Ay <- function(par){
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
  model2 <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_x + 
                     time_in_play +
                     prev_field_x +
                     prev_dist_y +
                     final_ball_position_y, 
                   data = location_training_A,
                   splitrule = 'extratrees',
                   num.random.splits = optimized_rs_Ay,
                   always.split.variables = c("final_ball_position_y"),
                   num.threads = 0,
                   num.trees = 300,
                   mtry = 5,
                   min.node.size = 10,
                   seed = 101,
                   split.select.weights = c(par[1],  par[2], par[3], par[4],
                                            par[5], par[6], par[7],0))
  print("Forest Complete")
  for (i in 1:nrow(location_test_A)){
    if (i > 1){
      if (location_test_A[i,"primary_key"] != location_test_A[i-1,'primary_key']){
        preds_field_position_y <- predict(model2, location_test_A[i,])$predictions
        location_test_A[i,"field_y1"] <- preds_field_position_y
        location_test_A[i,'dist_y1'] <- location_test_A[i,'final_ball_position_y'] - location_test_A[i, 'field_y1']
        location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x'])^2 + (location_test_A[i,'dist_y1'])^2)
      }else{
        location_test_A[i, "prev_field_y"] <- location_test_A[i-1, "field_y1"]
        location_test_A[i,"prev_distance"] <- location_test_A[i-1, "distance_from_ball"]
        location_test_A[i,"prev_dist_y"] <- location_test_A[i-1, "dist_y1"]
        preds_field_position_y <- predict(model2, location_test_A[i,])$predictions
        location_test_A[i,"field_y1"] <- preds_field_position_y
        location_test_A[i,'dist_y1'] <- location_test_A[i,'final_ball_position_y'] - location_test_A[i, 'field_y1']
        location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x'])^2 + (location_test_A[i,'dist_y1'])^2)
        # 
        # location_test_A <- location_test_A %>%
        #   mutate(dist_x = final_ball_position_x - field_y1,
        #          distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
      }
    }else{
      preds_field_position_y <- predict(model2, location_test_A[i,])$predictions
      location_test_A[i,"field_y1"] <- preds_field_position_y
      location_test_A[i,'dist_y1'] <- location_test_A[i,'final_ball_position_y'] - location_test_A[i, 'field_y1']
      location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x'])^2 + (location_test_A[i,'dist_y1'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  rmse <- sqrt(mean((location_test_A$field_y - location_test_A$field_y1)^2))
  print(par)
  print(rmse)
  return(rmse)
}
(optimized_weight_Ay <- optim(par = c(rep(0.5,5), 0.499, 0.5), find_weight_Ay, hessian = TRUE, method = "L-BFGS-B", lower = 0.47, upper = 0.53, control = list(maxit = 5, trace = 6, factr = 0.01/.Machine$double.eps, pgtol = 1e-4, ndeps = rep(0.001,7), REPORT = 1))) #lower set to 0.0001 rather than 0 so mtry has enough variables to choose from 

#par: c(0.500, 0.500, 0.500, 0.500, 0.500, 0.499, 0.500)
# rmse = 5.171023

split_weight_Ay <- c(optimized_weight_Ay$par,0)

find_minnodesize_Ay <- function(node){
  node <- floor(node)
  model2 <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_x + 
                     time_in_play +
                     prev_field_x +
                     prev_dist_y +
                     final_ball_position_y, 
                   data = location_training_A,
                   splitrule = 'extratrees',
                   num.random.splits = optimized_rs_Ay,
                   always.split.variables = c("final_ball_position_y"),
                   num.threads = 0,
                   num.trees = 300,
                   mtry = 5,
                   min.node.size = node,
                   seed = 101,
                   split.select.weights = split_weight_Ay)
  print("Forest Complete")
  for (i in 1:nrow(location_test_A)){
    if (i > 1){
      if (location_test_A[i,"primary_key"] != location_test_A[i-1,'primary_key']){
        preds_field_position_y <- predict(model2, location_test_A[i,])$predictions
        location_test_A[i,"field_y1"] <- preds_field_position_y
        location_test_A[i,'dist_y1'] <- location_test_A[i,'final_ball_position_y'] - location_test_A[i, 'field_y1']
        location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x'])^2 + (location_test_A[i,'dist_y1'])^2)
      }else{
        location_test_A[i, "prev_field_y"] <- location_test_A[i-1, "field_y1"]
        location_test_A[i,"prev_distance"] <- location_test_A[i-1, "distance_from_ball"]
        location_test_A[i,"prev_dist_y"] <- location_test_A[i-1, "dist_y1"]
        preds_field_position_y <- predict(model2, location_test_A[i,])$predictions
        location_test_A[i,"field_y1"] <- preds_field_position_y
        location_test_A[i,'dist_y1'] <- location_test_A[i,'final_ball_position_y'] - location_test_A[i, 'field_y1']
        location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x'])^2 + (location_test_A[i,'dist_y1'])^2)
      }
    }else{
      preds_field_position_y <- predict(model2, location_test_A[i,])$predictions
      location_test_A[i,"field_y1"] <- preds_field_position_y
      location_test_A[i,'dist_y1'] <- location_test_A[i,'final_ball_position_y'] - location_test_A[i, 'field_y1']
      location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x'])^2 + (location_test_A[i,'dist_y1'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  rmse <- sqrt(mean((location_test_A$field_y - location_test_A$field_y1)^2))
  print(node)
  print(rmse)
  return(rmse)
}

(optimized_minnodesize_Ay <- optimize(find_minnodesize_Ay, c(15,25), tol = 1))
optimized_node_Ay <- floor(optimized_minnodesize_Ay$minimum) #optimized at 17, rmse = 5.30

find_minbucket_Ay <- function(bucket){
  bucket <- floor(bucket)
  model2 <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_x + 
                     time_in_play +
                     prev_field_x +
                     prev_dist_y +
                     final_ball_position_y, 
                   data = location_training_A,
                   splitrule = 'extratrees',
                   num.random.splits = optimized_rs_Ay,
                   always.split.variables = c("final_ball_position_y"),
                   num.threads = 0,
                   num.trees = 300,
                   mtry = 5,
                   min.node.size = optimized_node_Ay,
                   seed = 101,
                   split.select.weights = split_weight_Ay,
                   min.bucket = bucket)
  print("Forest Complete")
  for (i in 1:nrow(location_test_A)){
    if (i > 1){
      if (location_test_A[i,"primary_key"] != location_test_A[i-1,'primary_key']){
        preds_field_position_y <- predict(model2, location_test_A[i,])$predictions
        location_test_A[i,"field_y1"] <- preds_field_position_y
        location_test_A[i,'dist_y1'] <- location_test_A[i,'final_ball_position_y'] - location_test_A[i, 'field_y1']
        location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x'])^2 + (location_test_A[i,'dist_y1'])^2)
      }else{
        location_test_A[i, "prev_field_y"] <- location_test_A[i-1, "field_y1"]
        location_test_A[i,"prev_distance"] <- location_test_A[i-1, "distance_from_ball"]
        location_test_A[i,"prev_dist_y"] <- location_test_A[i-1, "dist_y1"]
        preds_field_position_y <- predict(model2, location_test_A[i,])$predictions
        location_test_A[i,"field_y1"] <- preds_field_position_y
        location_test_A[i,'dist_y1'] <- location_test_A[i,'final_ball_position_y'] - location_test_A[i, 'field_y1']
        location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x'])^2 + (location_test_A[i,'dist_y1'])^2)
      }
    }else{
      preds_field_position_y <- predict(model2, location_test_A[i,])$predictions
      location_test_A[i,"field_y1"] <- preds_field_position_y
      location_test_A[i,'dist_y1'] <- location_test_A[i,'final_ball_position_y'] - location_test_A[i, 'field_y1']
      location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x'])^2 + (location_test_A[i,'dist_y1'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  rmse <- sqrt(mean((location_test_A$field_y - location_test_A$field_y1)^2))
  print(bucket)
  print(rmse)
  return(rmse)
}

(optimized_minbucket_Ay <- optimize(find_minbucket_Ay, c(1,floor(optimized_node_Ay/2)+1), tol = 1)) # optimized at 5
optimized_minbucket_Ay <- floor(optimized_minbucket_Ay$minimum) #rmse = 5.11


find_ntree_Ay <- function(ntree){
  model2 <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                     prev_distance + time_remaining + 
                     final_ball_position_x + 
                     time_in_play +
                     prev_field_x +
                     prev_dist_y +
                     final_ball_position_y, 
                   data = location_training_A,
                   splitrule = 'extratrees',
                   num.random.splits = optimized_rs_Ay,
                   always.split.variables = c("final_ball_position_y"),
                   num.threads = 0,
                   num.trees = ntree,
                   mtry = 5,
                   min.node.size = optimized_node_Ay,
                   seed = 101,
                   split.select.weights = split_weight_Ay,
                   min.bucket = optimized_minbucket_Ay)
  print("Forest Complete")
  for (i in 1:nrow(location_test_A)){
    if (i > 1){
      if (location_test_A[i,"primary_key"] != location_test_A[i-1,'primary_key']){
        preds_field_position_y <- predict(model2, location_test_A[i,])$predictions
        location_test_A[i,"field_y1"] <- preds_field_position_y
        location_test_A[i,'dist_y1'] <- location_test_A[i,'final_ball_position_y'] - location_test_A[i, 'field_y1']
        location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x'])^2 + (location_test_A[i,'dist_y1'])^2)
      }else{
        location_test_A[i, "prev_field_y"] <- location_test_A[i-1, "field_y1"]
        location_test_A[i,"prev_distance"] <- location_test_A[i-1, "distance_from_ball"]
        location_test_A[i,"prev_dist_y"] <- location_test_A[i-1, "dist_y1"]
        preds_field_position_y <- predict(model2, location_test_A[i,])$predictions
        location_test_A[i,"field_y1"] <- preds_field_position_y
        location_test_A[i,'dist_y1'] <- location_test_A[i,'final_ball_position_y'] - location_test_A[i, 'field_y1']
        location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x'])^2 + (location_test_A[i,'dist_y1'])^2)
      }
    }else{
      preds_field_position_y <- predict(model2, location_test_A[i,])$predictions
      location_test_A[i,"field_y1"] <- preds_field_position_y
      location_test_A[i,'dist_y1'] <- location_test_A[i,'final_ball_position_y'] - location_test_A[i, 'field_y1']
      location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x'])^2 + (location_test_A[i,'dist_y1'])^2)
    }
    if(i %% 250 == 0){
      print(i)
    }
  }
  rmse <- sqrt(mean((location_test_A$field_y - location_test_A$field_y1)^2))
  print(ntree)
  print(rmse)
  return(rmse)
}

tree_perf_Ay <- tibble(
  ntree = 1:6*100,
  rmse_value = map_dbl(1:6*100, find_ntree_Ay) 
)

tree_perf_Ay %>% ggplot(aes(ntree,rmse_value)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  ylab("RMSE") +
  scale_x_continuous(breaks = seq(0,600,200)) # don't want to use 100 trees for chance of overfitting
                                              # using 200 trees which is the best next value

field_model_Ay <- ranger(field_y ~ prev_field_y + #max_fielder_speed + 
                           prev_distance + time_remaining + 
                           final_ball_position_x + 
                           time_in_play +
                           prev_field_x +
                           prev_dist_y +
                           final_ball_position_y, 
                         data = location_training_A,
                         splitrule = 'extratrees',
                         num.random.splits = optimized_rs_Ay,
                         always.split.variables = c("final_ball_position_y"),
                         num.threads = 0,
                         num.trees = 200,
                         mtry = 5,
                         min.node.size = optimized_node_Ay,
                         seed = 101,
                         split.select.weights = split_weight_Ay,
                         min.bucket = optimized_minbucket_Ay,
                         importance = "permutation",
                         scale.permutation.importance = TRUE)



for (i in 1:nrow(location_test_A)){
  if (i > 1){
    if (location_test_A[i,"primary_key"] != location_test_A[i-1,'primary_key']){
      preds_field_position_y <- predict(field_model_Ay, location_test_A[i,])$predictions
      location_test_A[i,"field_y1"] <- preds_field_position_y
      location_test_A[i,'dist_y1'] <- location_test_A[i,'final_ball_position_y'] - location_test_A[i, 'field_y1']
      location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x'])^2 + (location_test_A[i,'dist_y1'])^2)
    }else{
      location_test_A[i, "prev_field_y"] <- location_test_A[i-1, "field_y1"]
      location_test_A[i,"prev_distance"] <- location_test_A[i-1, "distance_from_ball"]
      location_test_A[i,"prev_dist_y"] <- location_test_A[i-1, "dist_y1"]
      preds_field_position_y <- predict(field_model_Ay, location_test_A[i,])$predictions
      location_test_A[i,"field_y1"] <- preds_field_position_y
      location_test_A[i,'dist_y1'] <- location_test_A[i,'final_ball_position_y'] - location_test_A[i, 'field_y1']
      location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x'])^2 + (location_test_A[i,'dist_y1'])^2)
    }
  }else{
    preds_field_position_y <- predict(field_model_Ay, location_test_A[i,])$predictions
    location_test_A[i,"field_y1"] <- preds_field_position_y
    location_test_A[i,'dist_y1'] <- location_test_A[i,'final_ball_position_y'] - location_test_A[i, 'field_y1']
    location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x'])^2 + (location_test_A[i,'dist_y1'])^2)
  }
  if(i %% 250 == 0){
    print(i)
  }
}


location_test_A <- location_test_A %>% 
  mutate(diff_y = abs(field_y - field_y1))

rmse(location_test_A, field_y, field_y1) #4.95

location_test_A %>% ggplot(aes(y = diff_y, x = time_in_play,group = primary_key, color = time_remaining)) +
  geom_path()

location_test_A <- read_csv("location_test_A1.csv", show_col_types = FALSE)


#### Final Field Model A Y ####

field_model_Ay <- ranger(field_y ~ prev_field_y +  
                           prev_distance + time_remaining + 
                           final_ball_position_x + 
                           time_in_play +
                           prev_field_x +
                           prev_dist_y +
                           final_ball_position_y, 
                         data = dat_A,
                         splitrule = 'extratrees',
                         num.random.splits = optimized_rs_Ay,
                         always.split.variables = c("final_ball_position_y"),
                         num.threads = 0,
                         num.trees = 200,
                         mtry = 5,
                         min.node.size = optimized_node_Ay,
                         seed = 101,
                         split.select.weights = split_weight_Ay,
                         min.bucket = optimized_minbucket_Ay,
                         importance = "permutation",
                         scale.permutation.importance = TRUE)

for (i in 1:nrow(location_test_A)){
  if (i > 1){
    if (location_test_A[i,"primary_key"] != location_test_A[i-1,'primary_key']){
      preds_field_position_y <- predict(field_model_Ay, location_test_A[i,])$predictions
      location_test_A[i,"field_y1"] <- preds_field_position_y
      location_test_A[i,'dist_y1'] <- location_test_A[i,'final_ball_position_y'] - location_test_A[i, 'field_y1']
      location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x'])^2 + (location_test_A[i,'dist_y1'])^2)
    }else{
      location_test_A[i, "prev_field_y"] <- location_test_A[i-1, "field_y1"]
      location_test_A[i,"prev_distance"] <- location_test_A[i-1, "distance_from_ball"]
      location_test_A[i,"prev_dist_y"] <- location_test_A[i-1, "dist_y1"]
      preds_field_position_y <- predict(field_model_Ay, location_test_A[i,])$predictions
      location_test_A[i,"field_y1"] <- preds_field_position_y
      location_test_A[i,'dist_y1'] <- location_test_A[i,'final_ball_position_y'] - location_test_A[i, 'field_y1']
      location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x'])^2 + (location_test_A[i,'dist_y1'])^2)
    }
  }else{
    preds_field_position_y <- predict(field_model_Ay, location_test_A[i,])$predictions
    location_test_A[i,"field_y1"] <- preds_field_position_y
    location_test_A[i,'dist_y1'] <- location_test_A[i,'final_ball_position_y'] - location_test_A[i, 'field_y1']
    location_test_A[i,'distance_from_ball'] <- sqrt((location_test_A[i,'dist_x'])^2 + (location_test_A[i,'dist_y1'])^2)
  }
  if(i %% 250 == 0){
    print(i)
  }
}

location_test_A <- location_test_A %>% 
  mutate(diff_y = abs(field_y - field_y1))

rmse(location_test_A, field_y, field_y1) #3.82

location_test_A %>% ggplot(aes(y = diff_y, x = time_in_play,group = primary_key, color = time_remaining)) +
  geom_path()

importance_Ay <- tibble(feature = field_model_Ay$forest$independent.variable.names, importance = pull(as.data.frame(field_model_Ay$variable.importance))) %>% 
  arrange(desc(importance))

print(importance_Ay)
#### Save Models ####
save(field_model_AAAy, field_model_AAy, field_model_Aplusy, field_model_Ay, file = "Field_Model_Y1.RData")

write_rds(field_model_AAAy, file = "Field_Model_AAAy.RDS", compress = 'xz', compression = 9L)
write_rds(field_model_AAy, file = "Field_Model_AAy.RDS", compress = 'xz', compression = 9L)
write_rds(field_model_Aplusy, file = "Field_Model_Aplusy.RDS", compress = 'xz', compression = 9L)
write_rds(field_model_Ay, file = "Field_Model_Ay.RDS", compress = 'xz', compression = 9L)

