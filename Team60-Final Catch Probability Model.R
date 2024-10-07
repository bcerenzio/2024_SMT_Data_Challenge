library(ranger)
library(tidymodels)
library(tidyverse)
library(parallel)
set.seed(101)

final_dataset <- read_csv('final_dataset.csv')

catch_prob_data <- final_dataset %>%
  reframe(primary_key, timestamp, ball_position_x, ball_position_y, 
          max_fielder_speed = round(max_fielder_speed,1), time_remaining, field_y = round(field_y,1), 
          field_x = round(field_x,1), distance_from_ball = round(distance_from_ball,1),
          dist_x = abs(round(dist_x,1)), dist_y = abs(round(dist_y,1)),
          final_ball_position_x = round(final_ball_position_x,1), final_ball_position_y = round(final_ball_position_y,1), Level,
         Season, player_position.x, flyout, timestamp_at_contact, time_in_play) %>%
  filter(timestamp >= timestamp_at_contact) %>% 
  drop_na()

write_csv(catch_prob_data, file = "catch_prob_data.csv")

catch_split <- group_initial_split(catch_prob_data, primary_key,prop = 0.8, strata = flyout)

train_catch_prob <- training(catch_split)

write_csv(train_catch_prob, file = 'train_catch_prob.csv')

test_catch_prob <- testing(catch_split)

write_csv(test_catch_prob, file = 'test_catch_prob.csv')


# there is a major disproportionate representation of flyouts vs hits, so I'm gonna use the
# class.weights argument to make sure they are represented equally

table(catch_prob_data$flyout)
#TRUE: 100408
#FALSE: 46534

true_weight <- nrow(catch_prob_data)/(2*100408)
false_weight <- nrow(catch_prob_data)/(2*46534)

find_splitrule_c <- function(splitrule){
  model1 <- ranger(as.factor(flyout) ~ max_fielder_speed + 
                     time_remaining + 
                     distance_from_ball + final_ball_position_y + final_ball_position_x + 
                      dist_x + dist_y, 
                   data = train_catch_prob, 
                   importance = 'impurity',
                   class.weights = c("FALSE" = false_weight, "TRUE" = true_weight),
                   probability = TRUE, 
                   splitrule = splitrule,
                   seed = 101,
                   num.threads = 0,
                   num.trees = 300)
  preds <- predict(model1, test_catch_prob)
  brier_score <- mean((test_catch_prob$flyout - preds$predictions[,2])^2)
  print(splitrule)
  print(brier_score)
  return(brier_score)
}

splitrule_c <- tibble(
  splitrule = c("gini", "extratrees", 'hellinger'),
) 

splitrule_c <- splitrule_c %>% 
  mutate(breir_score = map_dbl(splitrule_c$splitrule, find_splitrule_c)) 
#performed best with hellinger brier = 0.051

find_randsplits_c <- function(randsplit){
  randsplit <- ceiling(randsplit)
  model1 <- ranger(as.factor(flyout) ~ max_fielder_speed + 
                     time_remaining +
                     distance_from_ball + final_ball_position_y + final_ball_position_x + 
                     dist_x + dist_y, 
                   data = train_catch_prob, 
                   importance = 'impurity',
                   class.weights = c("FALSE" = false_weight, "TRUE" = true_weight),
                   probability = TRUE, 
                   splitrule = "extratrees",
                   num.random.splits = randsplit,
                   seed = 101,
                   num.threads = 0,
                   num.trees = 300)
  preds <- predict(model1, test_catch_prob)
  brier_score <- mean((test_catch_prob$flyout - preds$predictions[,2])^2)
  print(randsplit)
  print(brier_score)
  return(brier_score)
}
rs_c <- tibble(
  rs = 2:50,
  brier = map_dbl(2:50, find_randsplits_c)
)

rs_c <- rs_c %>% slice_min(brier, n = 1)

print(rs_c)

optimized_rs_c <- rs_c$rs #optimizes at 4, brier = 0.501, better than hellinger

find_mtry_c <- function(mtry){
  model1 <- ranger(as.factor(flyout) ~ max_fielder_speed + time_remaining +
                     distance_from_ball + final_ball_position_y + final_ball_position_x + 
                     dist_x + dist_y,
                   data = train_catch_prob,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_c,
                   probability = TRUE,
                   class.weights = c("FALSE" = false_weight, "TRUE" = true_weight),
                   mtry = mtry,
                   seed = 101,
                   num.threads = 0,
                   num.trees = 300)
  preds <- predict(model1, test_catch_prob)
  brier_score <- mean((test_catch_prob$flyout - preds$predictions[,2])^2)
  print(mtry)
  print(brier_score)
  return(brier_score)
}

mtry_c <- tibble(
  mtry = 1:7,
  brier= map_dbl(1:7,find_mtry_c)
)

mtry_c <- mtry_c %>% 
  slice_min(brier, n = 1)

print(mtry_c)

optimized_mtry_c <- mtry_c$mtry # minimizes at 6, brier = 0.0468

find_minnodesize_c <- function(node){
  node <- floor(node)
  model1 <- ranger(as.factor(flyout) ~ max_fielder_speed + time_remaining +
                     distance_from_ball + final_ball_position_y + final_ball_position_x + 
                     dist_x + dist_y,
                   data = train_catch_prob,
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_c,
                   probability = TRUE,
                   class.weights = c("FALSE" = false_weight, "TRUE" = true_weight),
                   min.node.size = node,
                   mtry = optimized_mtry_c,
                   num.threads = 0,
                   seed = 101,
                   num.trees = 300)
  preds <- predict(model1, test_catch_prob)
  brier_score <- mean((test_catch_prob$flyout - preds$predictions[,2])^2)
  print(node)
  print(brier_score)
  return(brier_score)
}

(optimized_minnodesize_c <- optimize(find_minnodesize_c, c(10,200), tol = 1))
optimized_node_c <- floor(optimized_minnodesize_c$minimum) #optimized at 107, brier = 0.0458


find_minbucket_c <- function(bucket){
  bucket <- floor(bucket)
  model1 <- ranger(as.factor(flyout) ~ max_fielder_speed + time_remaining +
                     distance_from_ball + final_ball_position_y + final_ball_position_x + 
                     dist_x + dist_y, 
                   data = train_catch_prob, 
                   importance = 'impurity',
                   probability = TRUE, mtry = optimized_mtry_c,
                   class.weights = c("FALSE" = false_weight, "TRUE" = true_weight),
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_c,
                   min.node.size = optimized_node_c,
                   min.bucket = bucket,
                   num.threads = 0,
                   seed = 101,
                   num.trees = 300)
  preds <- predict(model1, test_catch_prob)
  brier_score <- mean((test_catch_prob$flyout - preds$predictions[,2])^2)
  print(bucket)
  print(brier_score)
  return(brier_score)
}

(optimized_minbucket_c <- optimize(find_minbucket_c, c(1,floor(optimized_node_c/2)+1), tol = 1)) # optimized at 8, brier = 0.0457     
optimized_bucket_c <- floor(optimized_minbucket_c$minimum)                  

find_ntree_c <- function(ntree){
  model1 <- ranger(as.factor(flyout) ~ max_fielder_speed + time_remaining + 
                     distance_from_ball + final_ball_position_y + final_ball_position_x + 
                      dist_x + dist_y, 
                   data = train_catch_prob, 
                   probability = TRUE, mtry = optimized_mtry_c,
                   min.node.size = optimized_node_c,
                   min.bucket = optimized_bucket_c,
                   class.weights = c("FALSE" = false_weight, "TRUE" = true_weight),
                   splitrule = "extratrees",
                   num.random.splits = optimized_rs_c,
                   num.trees = ntree,
                   num.threads = 0,
                   verbose = TRUE,
                   seed = 101)
  preds <- predict(model1, test_catch_prob)
  brier_score <- mean((test_catch_prob$flyout - preds$predictions[,2])^2)
  print(brier_score)
  print(ntree) 
  return(brier_score)
}

tree_perf_c <- tibble(
  ntree = 1:10*100,
  brier_score = map_dbl(1:10*100, find_ntree_c)
)

tree_perf_c %>% ggplot(aes(ntree,brier_score)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  scale_x_continuous(breaks = seq(0,1000,by = 200)) #optimizes at 400


model_c <- ranger(as.factor(flyout) ~ max_fielder_speed + time_remaining + 
                    distance_from_ball + final_ball_position_y + final_ball_position_x + 
                    dist_x + dist_y, 
                 data = train_catch_prob, 
                 importance = 'impurity',
                 probability = TRUE, mtry = optimized_mtry_c, 
                 min.node.size = optimized_node_c, 
                 min.bucket = optimized_bucket_c,
                 splitrule = "extratrees",
                 num.random.splits = optimized_rs_c,
                 num.threads = 0,
                 num.trees = 400,
                 class.weights = c("FALSE" = false_weight, "TRUE" = true_weight),
                 verbose = TRUE,
                 seed = 101)
preds_test <- predict(model_c, test_catch_prob)


test_catch_prob <- test_catch_prob %>% 
  mutate(catch_probability = preds_test$predictions[,2],
         diff = flyout - catch_probability,
         brier = (flyout - catch_probability)^2) #breier score = 0.0457

test_catch_prob %>% ggplot(aes(catch_probability)) +
  geom_density(fill = 'blue') +
  xlab("Catch Probability") +
  ggtitle("Catch Probability Density Plot") + 
  theme_bw()

test_catch_prob %>% ggplot(aes(brier)) +
  geom_density(fill = "blue") +
  scale_x_continuous(breaks = seq(0,1,0.1)) +
  xlab("Brier Score") +
  ggtitle("Catch Probability Brier Score Density Plot") +
  theme_bw()

test_catch_prob <- test_catch_prob %>% 
  mutate(time_remaining_s = time_remaining/1000,
         catch_probability_pct = round(catch_probability*100, 2))

test_catch_prob %>% ggplot(aes(catch_probability_pct, distance_from_ball,color = time_remaining)) +
  geom_jitter(alpha = 0.3) +
  scale_color_continuous(low = 'red', high = 'green', name = 'Time \nRemaining (ms)') +
  theme_bw() +
  ylab("Distance From Final Ball Position (Ft)") +
  xlab("Catch Probability") +
  ggtitle("Distance From Final Ball Position (ft) vs \nCatch Probability")


test_catch_prob %>% ggplot(aes(time_remaining_s, distance_from_ball,color = catch_probability_pct)) +
  geom_jitter(alpha = 0.15) +
  scale_color_continuous(low = 'red', high = 'green', name = 'Catch \n Probability') +
  theme_bw() +
  ylab("Distance From Final Ball Position (Ft)") +
  xlab("Time Remaining (s)") +
  ggtitle("Distance From Final Ball Position (ft) vs \n Time Remaining (s)") +
  scale_x_reverse()+
  scale_y_continuous(position = "right") +
  theme(legend.position = "left")

#final model
catch_prob_model <- ranger(as.factor(flyout) ~ max_fielder_speed + time_remaining + 
                             distance_from_ball + final_ball_position_y + final_ball_position_x + 
                             dist_x + dist_y, 
                           data = catch_prob_data, 
                           importance = 'impurity',
                           probability = TRUE, mtry = optimized_mtry_c, 
                           min.node.size = optimized_node_c, 
                           min.bucket = optimized_bucket_c,
                           splitrule = "extratrees",
                           num.random.splits = optimized_rs_c,
                           num.trees = 400,
                           class.weights = c("FALSE" = false_weight, "TRUE" = true_weight),
                           verbose = TRUE,
                           num.threads = 0,
                           seed = 101)

save(catch_prob_model, file = "Catch_Prob_Model.RData")


