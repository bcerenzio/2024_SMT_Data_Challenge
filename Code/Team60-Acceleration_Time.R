library(tidyverse)

## 90ft sprint data from baseball savant
running_splits <- read_csv("running_splits.csv")


running_splits <- running_splits %>% 
  slice_sample(n = 20)

running_splits <- running_splits %>% 
   pivot_longer(cols = starts_with("seconds_since_hit_"), names_to = "feet", values_to = "time") %>% 
   mutate(feet = as.numeric(str_sub(feet, start = -2L, end = -1L)))

running_splits <- running_splits %>% 
  mutate(speed = ifelse(feet == 0, 0, (feet-lag(feet))/(time - lag(time)))) %>% 
  mutate(accel = ifelse(feet == 0, 0, (speed - lag(speed))/(time - lag(time))))

running_splits %>% ggplot(aes(feet, accel, color = as.factor(player_id), group = player_id)) +
  geom_path() +
  geom_point() +
  scale_x_continuous(breaks = seq(5,90,10),
                     limits = c(5,90)) +
  ylab("Acceleration (ft/s^2)") +
  xlab("Feet") + 
  ggtitle("Acceleration Down the First Base Line") +
  theme_bw()+
  theme(legend.text = element_blank(),
        legend.box = element_blank(),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.ticks = element_blank(),
        legend.background = element_blank(),
        legend.ticks.length = element_blank())


# #### note: two major points: 10 ft for major acceleration, up until 35 feet till they reach true full speed

 running_splits <- read_csv("running_splits.csv")
# ## mean time at 10 ft
 mean(running_splits$seconds_since_hit_010) #0.85 seconds
# 
# ### mean time at 35 ft
 mean(running_splits$seconds_since_hit_035) ## 2 seconds


# running_splits <- running_splits %>% select(-`last_name, first_name`, -name_abbrev, 
#                                             -team_id, -position_name, -bat_side, -age, -player_id)
# 
# running_splits <- running_splits %>% t()
# 
# running_splits <- as.data.frame(running_splits)
# 
# running_splits <- running_splits %>% 
#   mutate(ft_s_v1 = ifelse(V1 == 0, 0, 5/(V1 - lag(V1))),
#          accel_v1 = ifelse(V1 == 0, 0, (ft_s_v1 - lag(ft_s_v1))/(V1 - lag(V1))),
#          ft_s_v2 = ifelse(V2 == 0, 0, 5/(V2 - lag(V2))),
#          accel_v2 = ifelse(V2 == 0, 0, (ft_s_v2 - lag(ft_s_v2))/(V2 - lag(V2))),
#          ft_s_v3 = ifelse(V3 == 0, 0, 5/(V3 - lag(V3))),
#          accel_v3 = ifelse(V3 == 0, 0, (ft_s_v3 - lag(ft_s_v3))/(V3 - lag(V3))),
#          ft_s_v4 = ifelse(V4 == 0, 0, 5/(V4 - lag(V4))),
#          accel_v4 = ifelse(V4 == 0, 0, (ft_s_v4 - lag(ft_s_v4))/(V4 - lag(V4))),
#          ft_s_v5 = ifelse(V5 == 0, 0, 5/(V5 - lag(V5))),
#          accel_v5 = ifelse(V5 == 0, 0, (ft_s_v5 - lag(ft_s_v5))/(V5 - lag(V5))))
# 
# 
# ggplot(data = data.frame(
#   x = seq(0,90,5),
#   y = running_splits$ft_s_v1
# ), aes(x,y)) +
#   geom_path() +
#   geom_path(data = data.frame(
#     x = seq(0,90,5),
#     y = running_splits$ft_s_v2
#   ), aes(x,y)) + 
#   geom_path(data = data.frame(
#     x = seq(0,90,5),
#     y = running_splits$ft_s_v3
#   ), aes(x,y)) +
#   geom_path(data = data.frame(
#     x = seq(0,90,5),
#     y = running_splits$ft_s_v4
#   ), aes(x,y)) +
#   geom_path(data = data.frame(
#     x = seq(0,90,5),
#     y = running_splits$ft_s_v5
#   ), aes(x,y)) +
#   scale_x_continuous(breaks = seq(0,90,5)) +
#   theme_classic() +
#   xlab("Distance (ft)") +
#   ylab("Speed (ft/s)") +
#   ggtitle("Player Speed Down the Baseline (ft/s)")
# 
# 
# ggplot(data = data.frame(
#   x = seq(0,90,5),
#   y = running_splits$accel_v1
# ), aes(x,y)) +
#   geom_path() +
#   geom_path(data = data.frame(
#     x = seq(0,90,5),
#     y = running_splits$accel_v2),
#     aes(x,y), color = "blue") +
#   geom_path(data = data.frame(
#     x = seq(0,90,5),
#     y = running_splits$accel_v3),
#     aes(x,y), color = "red") +
#   geom_path(data = data.frame(
#     x = seq(0,90,5),
#     y = running_splits$accel_v4),
#     aes(x,y), color = "orange") +
#   geom_path(data = data.frame(
#     x = seq(0,90,5),
#     y = running_splits$accel_v5),
#     aes(x,y), color = "green") +
#   scale_x_continuous(breaks = seq(0,90,5)) +
#   geom_vline(xintercept = 35, linetype = 'dashed') +
#   geom_vline(xintercept = 10, linetype = 'dashed') +
#   geom_hline(yintercept = 0)
# 
# 
# #### note: two major points: 10 ft for major acceleration, up until 35 feet till they reach true full speed
# 
# running_splits <- read_csv("running_splits.csv")
# ## mean time at 5 ft
# mean(running_splits$seconds_since_hit_010) #0.85 seconds
# 
# ### mean time at 35 ft
# mean(running_splits$seconds_since_hit_035) ## 2 seconds
