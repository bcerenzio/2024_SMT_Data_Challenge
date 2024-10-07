all_pitches <- final_dataset %>% filter(!is.na(pitch_type)) %>% filter(pitch_type != "Eephus")

all_pitches <- all_pitches %>% 
  group_by(primary_key) %>% 
  filter(row_number() == 1)

all_pitches <- all_pitches %>% 
  group_by(pitch_type) %>% 
  reframe(Hor_Mov = mean(Hor_Mov, na.rm = T),
          Ver_Mov = mean(Ver_Mov, na.rm = T),
          pitches = n())

all_pitches$pitch_type <- factor(all_pitches$pitch_type, levels = c("4-Seam", 'Sinker', 'Cutter','Splitter', 'Changeup',
                                                                    'Slider', 'Slurve','Sweeper', 'Curveball',' Eephus'))



all_pitches %>% 
  ggplot(aes(Hor_Mov, Ver_Mov, color = pitch_type, size = pitches)) +
  geom_point(alpha = 0.7) +
  theme_few() +
  xlim(-20,20) +
  ylim(-20,20) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_line(color = "grey", size = 0.5, linetype = "dashed"),
        plot.caption.position = "plot") +
  labs(color = "Pitch Type",x = "Horizontal Movement", y = "Vertical Movement",
       title = "Pitch Movement Chart", size = "# of Pitches",
       caption = "Negative Horizontal Movement: Arm-Side Run
                           Positive Horizontal Movement: Glove-Side Run")

