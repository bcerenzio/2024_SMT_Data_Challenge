library(tidyverse)
library(ggthemes)



find_midpoint_outfield_wall <- tibble(x = 0,
                                      y = seq(0,330*sin(pi/4), by = 0.01),
                                      dist_center = 400 - y,
                                      dist_foul_line = round(sqrt((330*sin(pi/4))^2 + (330*cos(pi/4)-y)^2),2))

midpoint_outfield_wall <- find_midpoint_outfield_wall %>% filter(dist_center == dist_foul_line)

outfield_wall <- tibble(
  angles = seq(asin(330*sin(-pi/4)/midpoint_outfield_wall$dist_center), 
               asin(330*sin(pi/4)/midpoint_outfield_wall$dist_center), length.out = 200),
  radius = midpoint_outfield_wall$dist_center,
  center_y = midpoint_outfield_wall$y,
  x = radius*sin(angles),
  y = center_y + radius*cos(angles)
)

write_csv(outfield_wall, "outfield_wall.csv")

left_field_line <- tibble(
  x = c(0,-233.35),
  y = c(0,233.35)
)

write_csv(left_field_line, "left_field_line.csv")

right_field_line <- tibble(
  x = c(0,233.35),
  y = c(0,233.35)
)

write_csv(right_field_line, "right_field_line.csv")

find_midpoint_infield_outline <- tibble(
  x = 0,
  y = seq(0,120*sin(pi/4),by = 0.01),
  dist_back_dirt = (127.25+30) - y,
  dist_foul_line = round(sqrt((120*sin(pi/4))^2 + (120*cos(pi/4) - y)^2),2)
)



midpoint_infield_outline <- find_midpoint_infield_outline %>% filter(dist_back_dirt == dist_foul_line)

infield_outline <- tibble(
  angles = seq(asin(120*sin(-pi/4)/midpoint_infield_outline$dist_back_dirt), 
               asin(120*sin(pi/4)/midpoint_infield_outline$dist_back_dirt), length.out = 200),
  radius = midpoint_infield_outline$dist_back_dirt,
  center_y = midpoint_infield_outline$y,
  x = radius*sin(angles),
  y = center_y + radius*cos(angles)
)

write_csv(infield_outline, "infield_outline.csv")

outfield_grass <- tibble(
  x = c(-233.35, infield_outline$x, 233.35, outfield_wall$x),
  y = c(233.35, infield_outline$y, 233.35, outfield_wall$y)
)

write_csv(outfield_grass, "outfield_grass.csv")

infield_grass <- tibble(
  x = c(0,-63.64, 0, 63.64, 0),
  y = c(0, 63.64, 127.25, 63.64, 0)
)

write_csv(infield_grass, "infield_grass.csv")

infield_dirt <- tibble(
  x = c(infield_outline$x,65.64, 0, -65.64),
  y = c(infield_outline$y, 57.64, 125.25, 57.64)
)

write_csv(infield_dirt, "infield_dirt.csv")

find_midpoint_warning_track <- tibble(x = 0,
                                      y = seq(0,314*sin(pi/4), by = 0.01),
                                      dist_center = 384 - y,
                                      dist_foul_line = round(sqrt((314*sin(pi/4))^2 + (314*cos(pi/4)-y)^2),2))

midpoint_warning_track <- find_midpoint_warning_track %>% filter(dist_center == dist_foul_line) %>% slice(1)

warning_track <- tibble(
  angles = seq(asin(314*sin(-pi/4)/midpoint_warning_track$dist_center), 
               asin(314*sin(pi/4)/midpoint_warning_track$dist_center), length.out = 200),
  radius = midpoint_warning_track$dist_center,
  center_y = midpoint_warning_track$y,
  x = radius*sin(angles),
  y = center_y + radius*cos(angles)
)

write_csv(warning_track, 'warning_track.csv')

pitching_mound <- tibble(
  angles = seq(0,2*pi, length.out = 200),
  radius = 9,
  center_y = 60.5,
  x = radius*sin(angles),
  y = center_y + radius*cos(angles)
)

write_csv(pitching_mound, "pitching_mound.csv")

home_plate_circle <- tibble(
  angles = seq(0,2*pi, length.out = 200),
  radius = 13,
  x = radius*sin(angles),
  y = radius*cos(angles)
)

write_csv(home_plate_circle, "home_plate_circle.csv")


#### Note: Got Hexibals from the SportyR geom_baseball Function ####
p <- ggplot()+
  #outfield grass
  geom_polygon(data = outfield_grass, aes(x,y), fill = "#395d33") + 
  #infield grass
  geom_polygon(data = infield_grass,  aes(x,y), fill = "#395d33") +
  #infield dirt
  geom_polygon(data = infield_dirt, aes(x,y), fill = "#9b7653") +
  #warning track
  geom_polygon(data = tibble(
    x = c(warning_track$x, 233.35, outfield_wall$x*-1),
    y = c(warning_track$y, 233.35, outfield_wall$y)
  ), aes(x,y), fill = "#9b7653") +
  theme_few() +
  xlim(c(-250,250)) +
  ylim(c(-15,410)) +
  xlab("") +
  ylab("") +
  #first base line (infield)
  geom_polygon(data = tibble(
    x = c(86.85, 83.95,0,0),
    y = c(76.82, 89.62, 5,-5)
  ), aes (x,y),fill = "#9b7653", color = "#9b7653")+
  #3rd base line (inflield)
  geom_polygon(data = tibble(
    x = c(-86.85, -83.95,0,0),
    y = c(76.82, 89.62, 5,-5)
  ), aes (x,y),fill = "#9b7653", color = "#9b7653")+
  #home plate circle
  geom_polygon(data = home_plate_circle, aes(x,y), fill = "#9b7653", color = "#9b7653") +
  geom_line(data = left_field_line, aes(x,y), color = "white", linewidth = 0.2) +
  geom_line(data = right_field_line, aes(x,y), color = "white", linewidth = 0.2)+
  #annotate(geom = 'curve', x = -233.35, xend = 233.35, y = 233.35, yend = 233.35, curvature = -0.9) +
  geom_line(data = outfield_wall, aes(x,y), color = "yellow") +
  geom_polygon(data = pitching_mound, aes(x,y), fill = "#9b7653", color = "#9b7653") +
  annotate(geom = "rect", xmin = -1,xmax = 1, ymin = 60, ymax = 61,fill = "white") +
  #third base
  geom_polygon(data = tibble(
    x = c(-63.64, -65.64, -63.64, -61.64),
    y = c(63.64, 65.64, 67.64, 65.64)
  ), aes(x,y), fill = "white") + 
  #first base
  geom_polygon(data = tibble(
    x = c(63.64, 65.64, 63.64, 61.64),
    y = c(63.64, 65.64, 67.64, 65.64)
  ), aes(x,y), fill = "white") +
  #second base
  geom_polygon(data = tibble(
    x = c(0, -2, 0, 2),
    y = c(127.25, 129.25, 131.25, 129.25)
  ), aes(x,y), fill = "white") +
  #Home Plate
  geom_polygon(data = tibble(
    x = c(0,-1.5,-1.5,1.5,1.5),
    y = c(0,1.5,3,3,1.5)), aes(x,y), fill = "white"
  ) +
  geom_line(data = infield_outline, aes(x,y), color = "#9b7653") + 
  annotate(geom = 'segment', x = -61.64, xend = 0, y = 61.64, yend = 125.25, color = "#9b7653") +
  annotate(geom = 'segment', x = 0, xend = 61.64, y = 125.25, yend = 61.64, color = "#9b7653") +
  annotate(geom = "text", x = -245.35, y = 250.35, label = '330', color = "white", family = 'serif', size = 5) +
  annotate(geom = 'text', x = 245.35, y = 250.35, label = '330', color = "white", family = 'serif', size = 5) +
  annotate(geom = 'text', x = 0, y = 407, label = '400', color = "white", family = 'serif', size = 5) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "#395d33", color = "#395d33"),
        panel.background = element_rect(fill = "#395d33"))
p

