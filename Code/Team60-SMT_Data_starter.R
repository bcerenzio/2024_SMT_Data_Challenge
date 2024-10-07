# Welcome to the 2024 SMT Data Challenge! Here are some functions to help you get
# started. After you unzip the dataset, copy the name of the directory you saved 
# it to into the 'data_directory` field below. After making sure you have the 
# `arrow` package installed, you may call this file at the top of your work file(s)
# by calling `source("SMT_Data_starter.R"). Then, you may apply functions and 
# operations to the table names below as you would any other table and load them 
# into your working environment by calling `collect()`. For an example of this 
# process, un-comment and run the lines below the starter code. 
# 
# WARNING: The data subsets are large, especially `player_pos`. Reading the 
#   entire subset at once without filtering may incur performance issues on your 
#   machine or even crash your R session. It is recommended that you filter 
#   data subsets wisely before calling `collect()`.

data_directory <- '2024_SMT_Data_Challenge'

###############################################################################
################## STARTER CODE: DO NOT MODIFY ################################
###############################################################################

library(arrow)
library(tidyverse)

game_info <- arrow::open_csv_dataset(paste0(data_directory,"/game_info"), 
                                     partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"), 
                                     hive_style = F, 
                                     unify_schemas = T, 
                                     na = c("", "NA", "NULL", NA, "\\N"))

ball_pos <- arrow::open_csv_dataset(paste0(data_directory,"/ball_pos"), 
                                    partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"), 
                                    hive_style = F, 
                                    unify_schemas = T, 
                                    na = c("", "NA", "NULL", NA, "\\N"))

game_events <- arrow::open_csv_dataset(paste0(data_directory,"/game_events"), 
                                       partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"), 
                                       hive_style = F, 
                                       unify_schemas = T, 
                                       na = c("", "NA", "NULL", NA, "\\N"))

player_pos <- arrow::open_csv_dataset(paste0(data_directory,"/player_pos"), 
                                      partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"), 
                                      hive_style = F, 
                                      unify_schemas = T, 
                                      na = c("", "NA", "NULL", NA, "\\N"))

team_info <- arrow::open_csv_dataset(paste0(data_directory,"/team_info.csv"), 
                                     hive_style = F, 
                                     unify_schemas = T, 
                                     na = c("", "NA", "NULL", NA, "\\N"))

###############################################################################
########################## END STARTER CODE ###################################
###############################################################################

# game_info_demo <- game_info |>
#    filter(Day == "day_059", 
#           inning == 3) |> 
#    collect()
# 
# 
# ball_pos_demo <- ball_pos |>
#   filter(Day == "day_059") |> 
#   collect()

ball_pos_all <- ball_pos %>% 
  collect()

game_info_all <- collect(game_info)

game_events_all <- collect(game_events)

#do player pos in sections
# player_pos_all <- collect(player_pos)


team_info_all <- collect(team_info)

#day_numbers <- game_info_all %>% select(Day) %>% unique()

# extract_player_pos_data <- function(day){
#   player_pos %>%
#            filter(Day == day) %>%
#            collect()
# }
# 
# player_pos_all <- map_df(day_numbers, extract_player_pos_data)

#### Player POS Data ####
player_pos_1 <- player_pos %>% 
  filter(Day %in% c("day_001","day_002","day_003","day_004","day_005","day_006","day_007","day_010"),
                    player_position %in% c(7,8,9)) %>% 
  collect()

player_pos_2 <- player_pos %>% 
  filter(Day %in% c("day_011","day_012","day_013","day_014","day_015","day_018","day_019","day_020","day_021"),
         player_position %in% c(7,8,9)) %>% 
  collect()

player_pos_3 <- player_pos %>% 
  filter(Day %in% c("day_023","day_024", "day_025", "day_031", "day_032", "day_033", "day_034", "day_035"),
         player_position %in% c(7,8,9)) %>% 
  collect()

player_pos_4 <- player_pos %>% 
  filter(Day %in% c("day_036", "day_037", "day_038","day_048", "day_049","day_050","day_026",
                    "day_039","day_041.5", "day_041", "day_046", "day_047.5", "day_047", "day_016", "day_017",
                    "day_028.5"),
         player_position %in% c(7,8,9)) %>% 
  collect()

player_pos_5 <- player_pos %>% 
  filter(Day %in% c("day_028", "day_029", "day_030", "day_040", "day_043", "day_044", "day_045",  
                     "day_008", "day_009", "day_027", "day_042", "day_009.5", "day_017.5", "day_022", "day_044.5",
                     "day_054", "day_055", "day_056", "day_059", "day_060", "day_065", "day_066", "day_067"),
         player_position %in% c(7,8,9)) %>% 
  collect()

player_pos_6 <- player_pos %>% 
  filter(Day %in% c("day_074", "day_075", "day_076", "day_077", "day_091", "day_092", "day_093", "day_094",  
          "day_095", "day_096", "day_103", "day_104", "day_105", "day_106", "day_107", "day_108",  
          "day_109", "day_112", "day_113", "day_114", "day_116", "day_117", "day_118", "day_128",  
          "day_129", "day_130", "day_131", "day_135", "day_136", "day_137", "day_043.5", "day_057"),
         player_position %in% c(7,8,9)) %>% 
  collect()

player_pos_7 <- player_pos %>% 
  filter(Day %in% c("day_058", "day_061", "day_062", "day_063", "day_071", "day_072.5", "day_072", "day_073",  
                     "day_081", "day_082", "day_083", "day_089", "day_090", "day_092.5", "day_097", "day_098",  
                     "day_109.5", "day_110", "day_111", "day_115", "day_119", "day_120", "day_121", "day_122",  
                     "day_123", "day_132", "day_133.5", "day_133", "day_134.5", "day_134", "day_051", "day_052"),
         player_position %in% c(7,8,9)) %>% 
  collect()

player_pos_8 <- player_pos %>% 
  filter(Day %in% c("day_053", "day_064", "day_078", "day_079", "day_084", "day_085", "day_086", "day_087",  
                     "day_099", "day_100", "day_101", "day_124", "day_125", "day_126", "day_138", "day_139",  
                     "day_140", "day_068", "day_069", "day_070", "day_080.5", "day_080", "day_088", "day_102",  
                     "day_127", "day_141", "day_142", "day_143"),
         player_position %in% c(7,8,9)) %>% 
  collect()

player_pos_1_half <- bind_rows(player_pos_1, player_pos_2, player_pos_3, player_pos_4)
player_pos_2_half <- bind_rows(player_pos_5, player_pos_6, player_pos_7, player_pos_8)

player_pos_all <- bind_rows(player_pos_1_half, player_pos_2_half)

