library(plotly)
library(tidyverse)


#### Play 1 Code ####
play3890 <- final_dataset_2 %>% filter(primary_key == 3890)

play3890 <- play3890 %>% 
  add_row(field_x = rep(first(play3890$field_x),3),
          field_y = rep(first(play3890$field_y),3),
          opt_field_x = rep(first(play3890$field_x),3),
          opt_field_y = rep(first(play3890$field_y),3),
          time_in_play = seq(0,400, length.out = 3),
          ball_position_x = rep(0,3),
          ball_position_y = seq(54.5,0,length.out = 3)
          ) %>% 
  arrange(time_in_play)


play3890_line <- play3890 %>% accumulate_by(~time_in_play/1000)

play1 <- p + 
  geom_path(data = play3890_line, aes(field_x, field_y, frame = time_in_play_s), color = "lightblue") +
  geom_path(data = play3890_line, aes(opt_field_x, opt_field_y, frame = time_in_play_s), color = 'lightgreen')

play1 <- ggplotly(play1) %>%
  layout(
    dragmode = "pan",
    modebar = list(
      remove = c("lasso", 'select')
    )
  ) %>% 
  add_trace(
    data = play3890,
    x = ~ball_position_x,
    y = ~ball_position_y,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'white'),
    frame = ~time_in_play/1000
  ) %>%
  add_trace(
    data = play3890,
    x = ~field_x,
    y = ~field_y,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'lightblue', opacity = 0.5),
    frame = ~time_in_play/1000
  ) %>%
  add_trace(
    data = play3890,
    x = ~opt_field_x,
    y = ~opt_field_y,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'lightgreen', opacity = 0.5),
    frame = ~time_in_play/1000
  ) %>% 
  add_trace(
    text = ~paste("Catch Prob: ", round(catch_probability * 100, 1), "%"),
    x = 150,
    y = 30,
    type = 'scatter',
    mode = "text",
    showlegend = FALSE,
    textfont = list(color = 'lightblue', size = 12, family = 'serif'),
    frame = ~time_in_play/1000
  ) %>% 
  add_trace(
    text = ~paste("Catch Prob: ", round(catch_prob * 100, 1), "%"),
    x = -150,
    y = 30,
    type = 'scatter',
    mode = "text",
    showlegend = FALSE,
    textfont = list(color = 'lightgreen', size = 12, family = 'serif'),
    frame = ~time_in_play/1000
  ) %>% 
  animation_opts(frame = 400, redraw = FALSE) %>% 
  animation_slider(currentvalue = list(prefix = "Time in Play (s):", font = list(color = "white")))

play1


#### Play 2 Code ####
play155703 <- final_dataset_2 %>% filter(primary_key == 155703)

play155703 <- play155703 %>% 
  add_row(field_x = rep(first(play155703$field_x),3),
          field_y = rep(first(play155703$field_y),3),
          opt_field_x = rep(first(play155703$field_x),3),
          opt_field_y = rep(first(play155703$field_y),3),
          time_in_play = seq(0,400, length.out = 3),
          ball_position_x = rep(0,3),
          ball_position_y = seq(54.5,0,length.out = 3)
  ) %>% 
  arrange(time_in_play)


play155703_line <- play155703 %>% accumulate_by(~time_in_play/1000)

play2 <- p + 
  geom_path(data = play155703_line, aes(field_x, field_y, frame = time_in_play_s), color = "lightblue") +
  geom_path(data = play155703_line, aes(opt_field_x, opt_field_y, frame = time_in_play_s), color = 'lightgreen')

play2 <- ggplotly(play2) %>%
  layout(
    dragmode = "pan",
    modebar = list(
      remove = c("lasso", 'select')
    )
  ) %>% 
  add_trace(
    data = play155703,
    x = ~ball_position_x,
    y = ~ball_position_y,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'white'),
    frame = ~time_in_play/1000
  ) %>%
  add_trace(
    data = play155703,
    x = ~field_x,
    y = ~field_y,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'lightblue', opacity = 0.5),
    frame = ~time_in_play/1000
  ) %>%
  add_trace(
    data = play155703,
    x = ~opt_field_x,
    y = ~opt_field_y,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'lightgreen', opacity = 0.5),
    frame = ~time_in_play/1000
  ) %>% 
  add_trace(
    text = ~paste("Catch Prob: ", round(catch_probability * 100, 1), "%"),
    x = 150,
    y = 30,
    type = 'scatter',
    mode = "text",
    showlegend = FALSE,
    textfont = list(color = 'lightblue', size = 12, family = 'serif'),
    frame = ~time_in_play/1000
  ) %>% 
  add_trace(
    text = ~paste("Catch Prob: ", round(catch_prob * 100, 1), "%"),
    x = -150,
    y = 30,
    type = 'scatter',
    mode = "text",
    showlegend = FALSE,
    textfont = list(color = 'lightgreen', size = 12, family = 'serif'),
    frame = ~time_in_play/1000
  ) %>% 
  animation_opts(frame = 400, redraw = FALSE) %>% 
  animation_slider(currentvalue = list(prefix = "Time in Play (s):", font = list(color = "white")))

play2


#### Play 3 Code ####
play135786 <- final_dataset_2 %>% filter(primary_key == 135786)

play135786 <- play135786 %>% 
  add_row(field_x = rep(first(play135786$field_x),3),
          field_y = rep(first(play135786$field_y),3),
          opt_field_x = rep(first(play135786$field_x),3),
          opt_field_y = rep(first(play135786$field_y),3),
          time_in_play = seq(0,400, length.out = 3),
          ball_position_x = rep(0,3),
          ball_position_y = seq(54.5,0,length.out = 3)
  ) %>% 
  arrange(time_in_play)


play135786_line <- play135786 %>% accumulate_by(~time_in_play/1000)

play3 <- p + 
  geom_path(data = play135786_line, aes(field_x, field_y, frame = time_in_play_s), color = "lightblue") +
  geom_path(data = play135786_line, aes(opt_field_x, opt_field_y, frame = time_in_play_s), color = 'lightgreen')

play3 <- ggplotly(play3) %>%
  layout(
    dragmode = "pan",
    modebar = list(
      remove = c("lasso", 'select')
    )
  ) %>% 
  add_trace(
    data = play135786,
    x = ~ball_position_x,
    y = ~ball_position_y,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'white'),
    frame = ~time_in_play/1000
  ) %>%
  add_trace(
    data = play135786,
    x = ~field_x,
    y = ~field_y,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'lightblue', opacity = 0.5),
    frame = ~time_in_play/1000
  ) %>%
  add_trace(
    data = play135786,
    x = ~opt_field_x,
    y = ~opt_field_y,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'lightgreen', opacity = 0.5),
    frame = ~time_in_play/1000
  ) %>% 
  add_trace(
    text = ~paste("Catch Prob: ", round(catch_probability * 100, 1), "%"),
    x = 150,
    y = 30,
    type = 'scatter',
    mode = "text",
    showlegend = FALSE,
    textfont = list(color = 'lightblue', size = 12, family = 'serif'),
    frame = ~time_in_play/1000
  ) %>% 
  add_trace(
    text = ~paste("Catch Prob: ", round(catch_prob * 100, 1), "%"),
    x = -150,
    y = 30,
    type = 'scatter',
    mode = "text",
    showlegend = FALSE,
    textfont = list(color = 'lightgreen', size = 12, family = 'serif'),
    frame = ~time_in_play/1000
  ) %>% 
  animation_opts(frame = 400, redraw = FALSE) %>% 
  animation_slider(currentvalue = list(prefix = "Time in Play (s):", font = list(color = "white")))

play3

#### Play 4 Code ####
play55731 <- final_dataset_2 %>% filter(primary_key == 55731)

play55731 <- play55731 %>% 
  add_row(field_x = rep(first(play55731$field_x),3),
          field_y = rep(first(play55731$field_y),3),
          opt_field_x = rep(first(play55731$field_x),3),
          opt_field_y = rep(first(play55731$field_y),3),
          time_in_play = seq(0,400, length.out = 3),
          ball_position_x = rep(0,3),
          ball_position_y = seq(54.5,0,length.out = 3)
  ) %>% 
  arrange(time_in_play)


play55731_line <- play55731 %>% accumulate_by(~time_in_play/1000)

play4 <- p + 
  geom_path(data = play55731_line, aes(field_x, field_y, frame = time_in_play_s), color = "lightblue") +
  geom_path(data = play55731_line, aes(opt_field_x, opt_field_y, frame = time_in_play_s), color = 'lightgreen')

play4 <- ggplotly(play4) %>%
  layout(
    dragmode = "pan",
    modebar = list(
      remove = c("lasso", 'select')
    )
  ) %>% 
  add_trace(
    data = play55731,
    x = ~ball_position_x,
    y = ~ball_position_y,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'white'),
    frame = ~time_in_play/1000
  ) %>%
  add_trace(
    data = play55731,
    x = ~field_x,
    y = ~field_y,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'lightblue', opacity = 0.5),
    frame = ~time_in_play/1000
  ) %>%
  add_trace(
    data = play55731,
    x = ~opt_field_x,
    y = ~opt_field_y,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'lightgreen', opacity = 0.5),
    frame = ~time_in_play/1000
  ) %>% 
  add_trace(
    text = ~paste("Catch Prob: ", round(catch_probability * 100, 1), "%"),
    x = 150,
    y = 30,
    type = 'scatter',
    mode = "text",
    showlegend = FALSE,
    textfont = list(color = 'lightblue', size = 12, family = 'serif'),
    frame = ~time_in_play/1000
  ) %>% 
  add_trace(
    text = ~paste("Catch Prob: ", round(catch_prob * 100, 1), "%"),
    x = -150,
    y = 30,
    type = 'scatter',
    mode = "text",
    showlegend = FALSE,
    textfont = list(color = 'lightgreen', size = 12, family = 'serif'),
    frame = ~time_in_play/1000
  ) %>% 
  animation_opts(frame = 400, redraw = FALSE) %>% 
  animation_slider(currentvalue = list(prefix = "Time in Play (s):", font = list(color = "white")))

play4

#### Play 5 Code ####
play61373 <- final_dataset_2 %>% filter(primary_key == 61373)

play61373 <- play61373 %>% 
  add_row(field_x = rep(first(play61373$field_x),3),
          field_y = rep(first(play61373$field_y),3),
          opt_field_x = rep(first(play61373$field_x),3),
          opt_field_y = rep(first(play61373$field_y),3),
          time_in_play = seq(0,400, length.out = 3),
          ball_position_x = rep(0,3),
          ball_position_y = seq(54.5,0,length.out = 3)
  ) %>% 
  arrange(time_in_play)


play61373_line <- play61373 %>% accumulate_by(~time_in_play/1000)

play5 <- p + 
  geom_path(data = play61373_line, aes(field_x, field_y, frame = time_in_play_s), color = "lightblue") +
  geom_path(data = play61373_line, aes(opt_field_x, opt_field_y, frame = time_in_play_s), color = 'lightgreen')

play5 <- ggplotly(play5) %>%
  layout(
    dragmode = "pan",
    modebar = list(
      remove = c("lasso", 'select')
    )
  ) %>% 
  add_trace(
    data = play61373,
    x = ~ball_position_x,
    y = ~ball_position_y,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'white'),
    frame = ~time_in_play/1000
  ) %>%
  add_trace(
    data = play61373,
    x = ~field_x,
    y = ~field_y,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'lightblue', opacity = 0.5),
    frame = ~time_in_play/1000
  ) %>%
  add_trace(
    data = play61373,
    x = ~opt_field_x,
    y = ~opt_field_y,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'lightgreen', opacity = 0.5),
    frame = ~time_in_play/1000
  ) %>% 
  add_trace(
    text = ~paste("Catch Prob: ", round(catch_probability * 100, 1), "%"),
    x = 150,
    y = 30,
    type = 'scatter',
    mode = "text",
    showlegend = FALSE,
    textfont = list(color = 'lightblue', size = 12, family = 'serif'),
    frame = ~time_in_play/1000
  ) %>% 
  add_trace(
    text = ~paste("Catch Prob: ", round(catch_prob * 100, 1), "%"),
    x = -150,
    y = 30,
    type = 'scatter',
    mode = "text",
    showlegend = FALSE,
    textfont = list(color = 'lightgreen', size = 12, family = 'serif'),
    frame = ~time_in_play/1000
  ) %>% 
  animation_opts(frame = 400, redraw = FALSE) %>% 
  animation_slider(currentvalue = list(prefix = "Time in Play (s):", font = list(color = "white")))

play5

#### Play 6 Code ####
play137763 <- final_dataset_2 %>% filter(primary_key == 137763)

play137763 <- play137763 %>% 
  add_row(field_x = rep(first(play137763$field_x),3),
          field_y = rep(first(play137763$field_y),3),
          opt_field_x = rep(first(play137763$field_x),3),
          opt_field_y = rep(first(play137763$field_y),3),
          time_in_play = seq(0,400, length.out = 3),
          ball_position_x = rep(0,3),
          ball_position_y = seq(54.5,0,length.out = 3)
  ) %>% 
  arrange(time_in_play)


play137763_line <- play137763 %>% accumulate_by(~time_in_play/1000)

play6 <- p + 
  geom_path(data = play137763_line, aes(field_x, field_y, frame = time_in_play_s), color = "lightblue") +
  geom_path(data = play137763_line, aes(opt_field_x, opt_field_y, frame = time_in_play_s), color = 'lightgreen')

play6 <- ggplotly(play6) %>%
  layout(
    dragmode = "pan",
    modebar = list(
      remove = c("lasso", 'select')
    )
  ) %>% 
  add_trace(
    data = play137763,
    x = ~ball_position_x,
    y = ~ball_position_y,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'white'),
    frame = ~time_in_play/1000
  ) %>%
  add_trace(
    data = play137763,
    x = ~field_x,
    y = ~field_y,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'lightblue', opacity = 0.5),
    frame = ~time_in_play/1000
  ) %>%
  add_trace(
    data = play137763,
    x = ~opt_field_x,
    y = ~opt_field_y,
    type = 'scatter',
    mode = 'markers',
    marker = list(color = 'lightgreen', opacity = 0.5),
    frame = ~time_in_play/1000
  ) %>% 
  add_trace(
    text = ~paste("Catch Prob: ", round(catch_probability * 100, 1), "%"),
    x = 150,
    y = 30,
    type = 'scatter',
    mode = "text",
    showlegend = FALSE,
    textfont = list(color = 'lightblue', size = 12, family = 'serif'),
    frame = ~time_in_play/1000
  ) %>% 
  add_trace(
    text = ~paste("Catch Prob: ", round(catch_prob * 100, 1), "%"),
    x = -150,
    y = 30,
    type = 'scatter',
    mode = "text",
    showlegend = FALSE,
    textfont = list(color = 'lightgreen', size = 12, family = 'serif'),
    frame = ~time_in_play/1000
  ) %>% 
  animation_opts(frame = 400, redraw = FALSE) %>% 
  animation_slider(currentvalue = list(prefix = "Time in Play (s):", font = list(color = "white")))

play6

#### Save Graphs ####
save(play1, play2, play3, play4, play5, play6, file = "Optimization_Example_Plots.RData")
