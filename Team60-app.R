#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
library(shinydashboard)
library(bslib)
library(tidyverse)
library(ggthemes)
library(ranger)
library(DT)
library(plotly)
library(shinyWidgets)

# warning_track <- read_csv("warning_track.csv")
# right_field_line <- read_csv("right_field_line.csv")
# left_field_line <- read_csv("left_field_line.csv")
# home_plate_circle <- read_csv("home_plate_circle.csv")
# infield_dirt <- read_csv("infield_dirt.csv")
# infield_grass <- read_csv("infield_grass.csv")
# infield_outline <- read_csv("infield_outline.csv")
# outfield_grass <- read_csv("outfield_grass.csv")
# outfield_wall <- read_csv("outfield_wall.csv")
# pitching_mound <- read_csv("pitching_mound.csv")
oaa_data <- read_csv("oaa_data.csv")
route_data <- read_csv("route_data.csv")
#final_dataset <- read_csv("final_dataset.csv")
#load("Field_Model_X1.RData")
#print("Field Model X Loaded")
# load("Field_Model_Y1.RData")
# print("Field Model Y Loaded")
#field_model_Ay <- read_rds("Field_Model_Ay.RDS")
#print("field_model_Ay Loaded")
#load("plot_outline.RData")
load("accumulate_by.RData")
print("Accumation Function Loaded")
load("Optimization_Field_Model.RData")
print("Optimzation Field Model Loaded")
#load("Optimization_Example_Plots.RData")
#print("Optimization Example Plots Loaded")
#load("Catch_Prob_Model.RData")
catch_prob_model <- read_rds('Catch_Prob_Model.RDS')
print("Catch Prob Model Loaded")
p <- read_rds("plot_outline.RDS")
print("Plot Outline Loaded")
load("Optimization_Example_Plots.RData")
print("Optimization Example Plots Loaded")








ui <- navbarPage(title = "Team 60 SMT Data Challenge",
tabPanel(title = "OAA & Route Stats",
         fluidPage(theme = bs_theme(preset = "sandstone"),
                   tags$head(
                     tags$style(HTML("
      .highlight {
        background-color: yellow;
        color: black;
      }
    "))
                   ),
verticalLayout(fluidRow(uiOutput("info")),
            tabsetPanel(
                          tabPanel(title = "OAA", dataTableOutput("oaa_stats", height = '350px')),
                          tabPanel(title = "Route", dataTableOutput("route_stats", height = '350px')),
                          tabPanel(title = "All", dataTableOutput("all_stats", height = '350px')))
))),
tabPanel(
  "Fielding Across Levels",
  fluidPage(tags$head(
    tags$style(HTML("
      .highlight {
        background-color: yellow;
        color: black;
      }
    "))
  ),
    fluidRow(span(textOutput("instructions2"), class = "highlight")),
    sidebarLayout(
      sidebarPanel(
        textOutput("reminder2"),
        actionButton("reset2", "Reset"),
        actionButton("fielding_graphic", "Generate"),
        numericInput("field_x2", "Field_X", value = NULL),
        numericInput("field_y2", "Field_Y", value = NULL, max = 400),
        numericInput("ball_x2", "Final Ball Position X", value = NULL),
        numericInput("ball_y2", "Final Ball Position Y", value = NULL, max = 400),
        numericInputIcon("time_remaining2", "Length of Play (in seconds)", value = 5, 
                     min = 2, max = 8, step = 0.1, help_text = "Must Be a Value Between 2 & 8."),
        textOutput("level2a_note")
      ),
      
      mainPanel(
        uiOutput("dynamic_plot2"),
        progressBar(id = "progress2", value = 0, display_pct = TRUE)
      )
    )
  )
),
tabPanel("Route Optimization Model Examples",
         fluidPage( 
           #highlights the text above the plot
           tags$head(
           tags$style(HTML("
      .highlight {
        background-color: yellow;
        color: black;
      }
    "))
         ),
           fluidRow(span(textOutput("explanation"), class = 'highlight')),
           sidebarLayout(
             sidebarPanel(selectInput('optimization_examples', 'Plays:', 
                                      choices = c('Play 1', 'Play 2', 
                                                  'Play 3', 'Play 4', 
                                                  'Play 5', 'Play 6'), 
                                      selected = 'Play 1'), width = 2),
             mainPanel(plotlyOutput("graph3", height = '650px', width = '1150px'))
           )
         )
),
tabPanel("Route Optimized Fielding Path",
  fluidPage(
    tags$head(
      tags$style(HTML("
      .highlight {
        background-color: yellow;
        color: black;
      }
    "))
    ),
  fluidRow(span(textOutput("instructions"), class = 'highlight')),
  sidebarLayout(
    sidebarPanel(textOutput("reminder"),     
                 actionButton("reset", "Reset"),
                 actionButton("opt_graph", "Generate"),
      numericInput("field_x", "Field_X", value = NULL),
      numericInput("field_y", "Field_Y", value = NULL, max = 400),
      numericInput("ball_x", "Final Ball Position X", value = NULL),
      numericInput("ball_y", "Final Ball Position Y", value = NULL, max = 400),
      numericInputIcon("max_fielder_speed", "Max Fielder Speed (ft/s)", value = 27,
                   min = 20, max = 35, step = 0.1, help_text = "Must Be a Value Between 20 & 35."),
      numericInputIcon("time_remaining", "Length of Play (in seconds)", value = 5, 
                   min = 2, max = 8, step = 0.1, help_text = "Must Be a Value Between 2 & 8.")
      ),

    mainPanel(
      uiOutput("dynamic_plot"),
      progressBar(id = "progress", value = 0, display_pct = TRUE)
    )
  )
))
)

server <- function(input, output, session) {
  #### Start of OAA & Route Code ####
  output$info <- renderUI({
    div(class = 'highlight',
    HTML("Below are the OAA and Route Leaderboards Grouped by Position, Level, and Season. <br>
    OAA is calculated exactly how it is seen on statcast: OAA = Outs Made - Probability an Out is Made (Catch Probability). <br>
    Route Score is a metric that I created that compares each outfielder's catch probability at the point of contact to the catch probabilty 
    at the end of the play (catch/bounce). <br>
    It is calculated as follows: Route Score = (Catch Probability at time of Catch/Ball Bounce) - (Catch Probabilty at 
    Time of Contact). <br>
    This metric gives credit to outfielders who put themselves in the best position to catch the ball after contact, which allows fielders to 
    gain a positive Route Score even on near uncatchable balls at contact, while this same action will hurt a player's OAA since he didn't make the out."
  ))
    })
  
  
  all_data <- oaa_data %>% 
    left_join(route_data, by = c("fielder_id", "fielder_position", "Level", "Season", "plays"))
  
  all_data <- all_data %>% 
    mutate(Level = case_when(
      Level == 'AAA' ~ '4A',
      Level == 'AA' ~ '3A',
      Level == 'A+' ~ '2A',
      Level == 'A' ~ '1A'
    )) %>% 
    reframe("Fielder ID" = as.factor(fielder_id),
            "Position" = as.factor(fielder_position),
            "Level" = as.factor(Level),
            "Plays" = plays,
            "OAA Percentile" = round(oaa_percentile, 1),
            "Route Score Percentile" = round(route_percentile,1),
            "Avg. OAA Per Play Percentile" = round(oaa_per_play_percentile,1),
            "Avg. Route Score Per Play Percentile" = round(route_score_per_play_percentile,1)
            )
  
  oaa_data <- oaa_data %>%
    mutate(Level = case_when(
      Level == "AAA" ~ '4A',
      Level == 'AA' ~ '3A',
      Level == 'A+' ~ '2A',
      Level == 'A' ~ '1A'
    )) %>% 
    reframe("Fielder ID" = as.factor(fielder_id),
            "Position" = as.factor(fielder_position),
            "Level" = as.factor(Level),
            "Season" = as.factor(Season),
            "Plays" = plays,
            OAA = round(OAA,2),
            "Avg. OAA Per Play" = round(OAA_Per_Play,2),
            "OAA Percentile" = round(oaa_percentile, 1),
            "Avg. OAA Per Play Percentile" = round(oaa_per_play_percentile,1))
  
  route_data <- route_data %>% 
    mutate(Level = case_when(
      Level == 'AAA' ~ '4A',
      Level == 'AA' ~ '3A',
      Level == 'A+' ~ '2A',
      Level == 'A' ~ '1A'
    )) %>%
    reframe("Fielder ID" = as.factor(fielder_id),
            "Position" = as.factor(fielder_position),
            "Level" = as.factor(Level),
            "Season" = as.factor(Season),
            "Plays" = plays,
            "Route Score" = round(route_score,2),
            "Avg. Route Score Per Play" = round(Route_Score_Per_Play, 2),
            "Route Score Percentile" = round(route_percentile,1),
            "Avg. Route Score Per Play Percentile" = round(route_score_per_play_percentile,1))
  

  
  output$oaa_stats <- renderDataTable({
    datatable(oaa_data, class = c("display", "cell-border"), filter = "top", options = list(scrollY = '350px')) %>% 
      formatStyle(c('OAA Percentile','Avg. OAA Per Play Percentile'), 
                  backgroundColor = styleInterval(c(25,50,75), c('tomato', 'yellow', 'lightgreen', 'lawngreen')))
  })
  
  # Render Route stats table based on tab selection
  output$route_stats <- renderDataTable({ 
      datatable(route_data, class = c("display", "cell-border"), filter = "top", options = list(scrollY = "350px"))  %>% 
      formatStyle(c('Route Score Percentile', 'Avg. Route Score Per Play Percentile'),
                  backgroundColor = styleInterval(c(25,50,75), c('tomato', 'yellow', 'lightgreen', 'lawngreen')))
  })
  
  
  output$all_stats <- renderDataTable({ 
    datatable(all_data, class = c("display", "cell-border"), filter = "top", options = list(scrollY = "350px")) %>% 
      formatStyle(c('OAA Percentile','Avg. OAA Per Play Percentile','Route Score Percentile', 'Avg. Route Score Per Play Percentile'),
                  backgroundColor = styleInterval(c(25,50,75), c('tomato', 'yellow', 'lightgreen', 'lawngreen')))
  })
  #### End of OAA & Route Code ####
  #### Start of Fielding Across Levels Code ####
  output$dynamic_plot2 <- renderUI({
    plotOutput("plot2", click = 'plot_click2', height = "650px")
  })
  
  output$plot2 <- renderPlot({
    p
  })
  values <- reactiveValues(field_x2 = NULL, field_y2 = NULL, final_ball_position_x2 = NULL, final_ball_position_y2 = NULL)
  observeEvent(input$plot_click2, {
    click2 <- input$plot_click2
    if (is.null(values$field_x2)) {
      values$field_x2 <- click2$x
      values$field_y2 <- click2$y
      updateNumericInput(session, "field_x2", value = round(click2$x,2))
      updateNumericInput(session, "field_y2", value = round(click2$y,2))
      output$dynamic_plot2 <- renderUI({
        plotOutput("plot_outfielder2", click = "plot_click2", height = '650px')
      })
      output$plot_outfielder2 <- renderPlot({
        p +
          geom_point(data = data.frame(x = input$field_x2,
                                       y = input$field_y2), aes(x,y), color = "lightgreen")
      })
    } else {
      values$final_ball_position_x2 <- click2$x2
      values$final_ball_position_y2 <- click2$y2
      updateNumericInput(session, "ball_x2", value = round(click2$x,2))
      updateNumericInput(session, "ball_y2", value = round(click2$y,2))
      output$dynamic_plot2 <- renderUI({
        plotOutput("plot_ball2", height = '650px')
      })
      output$plot_ball2 <- renderPlot({
        p +
          geom_point(data = data.frame(x = input$field_x2,
                                       y = input$field_y2), aes(x,y), color = "lightgreen") +
          geom_point(data = data.frame(x = input$ball_x2,
                                       y = input$ball_y2), aes(x,y), color = "white")
      })
    }
  })
  
  
  
  
  #  observeEvent(input$field_x2, {
  #    updateNumericInput(session, "field_x2", value = round(input$field_x2,2))
  #  })
  # 
  #  observeEvent(input$field_y2, {
  #    updateNumericInput(session, "field_y2", value = round(input$field_y2,2))
  #  })
  # 
  # 
  # observeEvent(input$ball_x2, {
  #   updateNumericInput(session, "ball_x2", value = round(input$ball_x2,2))
  # })
  # 
  # observeEvent(input$ball_y2, {
  #   updateNumericInput(session, "ball_y2", value = round(input$ball_y2,2))
  # })

  observeEvent(input$reset2, {
    values$field_x2 <- NULL
    values$field_y2 <- NULL
    values$final_ball_position_x2 <- NULL
    values$final_ball_position_y2 <- NULL
    updateNumericInput(session, "field_x2", value = NULL)
    updateNumericInput(session, "field_y2", value = NULL)
    updateNumericInput(session, "ball_x2", value = NULL)
    updateNumericInput(session, "ball_y2", value = NULL)
    output$dynamic_plot2 <- renderUI({
      plotOutput("plot2", click = 'plot_click2', height = "650px")
    })
    output$plot2 <- renderPlot({
      p
    })
    updateProgressBar(session, id = "progress2", value = 0)
  })
  
  output$instructions2 <- renderText({
    "Click on the graph to select Starting Fielder Position and Final Ball Position.
    The first point you click will be the Fielder Coordinates and the second point you click will be the Final Ball Position Coordinates.
    Use the reset button below to change your inputs. 
    Note: Press the reset button when you first open the app or else the app won't work.
    (Plot takes roughly 1-2 min to generate. Progress Bar Below)."
  })
  
  output$level2a_note <- renderText({
    "Note: Level 2A is a lot more unreliable than the other paths because of lack of data."
  })
  output$reminder2 <- renderText({
    "Reminder: Always Press the Reset Button Before Changing Inputs."
  })
  
  
  observeEvent(input$fielding_graphic, {
    #remove(play1, play2, play3, play4, play5, play6)
    #remove(catch_prob_model)
    #final_dataset <- read_csv("final_dataset.csv")
   # print('Final Dataset Loaded')
    #mean_max_speed <- mean(final_dataset$max_fielder_speed, na.rm = TRUE)
   # remove(final_dataset)
    #print("Final Dataset Deleted")
    
    field_model_AAAx <- read_rds("Field_Model_AAAx.RDS")
    print("field_model_AAAx Loaded")
    field_model_AAAy <- read_rds("Field_Model_AAAy.RDS")
    print("field_model_AAAy Loaded")
    AAA_data1 <- tibble(
      field_x = input$field_x2,
      field_y = input$field_y2,
      max_fielder_speed = 20,
      time_remaining = seq(round(input$time_remaining2,1)*1000, round(input$time_remaining2,1)*1000-400, -50),
      time_in_play = seq(0,400,50),
      final_ball_position_x = input$ball_x2,
      final_ball_position_y = input$ball_y2,
      Level = "AAA",
      prev_field_x = NA,
      prev_field_y = NA,
      prev_distance = NA,
      prev_dist_x = NA,
      prev_dist_y = NA,
      distance_from_ball = NA,
      dist_x = NA,
      dist_x1 = NA,
      dist_y = NA,
      dist_y1 = NA,
      catch_probability = NA
    )
    
    AAA_data1$ball_position_x <- 0
    AAA_data1$ball_position_y <- seq(54.5,2,length.out = nrow(AAA_data1))
    
    AAA_data2 <- tibble(
      field_x = NA,
      field_y = NA,
      max_fielder_speed = 20,
      time_remaining = seq(round(input$time_remaining2,1)*1000-450, 0, -50),
      time_in_play = seq(450,round(input$time_remaining2,1)*1000,50),
      final_ball_position_x = input$ball_x2,
      final_ball_position_y = input$ball_y2,
      Level = "AAA",
      prev_field_x = NA,
      prev_field_y = NA,
      prev_distance = NA,
      prev_dist_x = NA,
      prev_dist_y = NA,
      distance_from_ball = NA,
      dist_x = NA,
      dist_x1 = NA,
      dist_y = NA,
      dist_y1 = NA
    )
    
    AAA_data2$ball_position_x <- seq(0,input$ball_x2,length.out = nrow(AAA_data2))
    AAA_data2$ball_position_y <- seq(2,input$ball_y2,length.out = nrow(AAA_data2))
    
    AAA_data2 <- AAA_data2 %>% 
      mutate(field_x = ifelse(time_in_play == 450, input$field_x2,NA),
             field_y = ifelse(time_in_play == 450, input$field_y2, NA),
             dist_x1 = ifelse(time_in_play == 450, final_ball_position_x - field_x, NA),
             dist_y1 = ifelse(time_in_play == 450, final_ball_position_y - field_y, NA),
             distance_from_ball = ifelse(time_in_play == 450, sqrt((dist_x1)^2 + (dist_y1)^2), NA)
             )
    
    
    for (i in 2:nrow(AAA_data2)){
      AAA_data2[i, "prev_field_x"] <- AAA_data2[i-1, "field_x"]
      AAA_data2[i,"prev_field_y"] <- AAA_data2[i-1, "field_y"]
      AAA_data2[i,"prev_distance"] <- AAA_data2[i-1, "distance_from_ball"]
      AAA_data2[i,"prev_dist_x"] <- AAA_data2[i-1, "dist_x1"]
      AAA_data2[i,"prev_dist_y"] <- AAA_data2[i-1, "dist_y1"]
      preds_field_position_x <- predict(field_model_AAAx, AAA_data2[i,])$predictions
      AAA_data2[i,"field_x"] <- preds_field_position_x
      #remove(field_model_AAAx)
      preds_field_position_y <- predict(field_model_AAAy, AAA_data2[i,])$predictions
      AAA_data2[i,"field_y"] <- preds_field_position_y
     # remove(field_model_AAAy)
      AAA_data2 <- AAA_data2 %>%
        mutate(dist_x1 = final_ball_position_x - field_x,
               dist_y1 = final_ball_position_y - field_y,
               distance_from_ball = sqrt((dist_x1)^2 + (dist_y1)^2))
      print(i)
      progress <- round((i/nrow(AAA_data2))*23,0)
      updateProgressBar(session, "progress2", value = progress)
    }
    
    
    AAA_data2 <- AAA_data2 %>% 
      mutate(dist_x = abs(dist_x1),
             dist_y = abs(dist_y1))
    #catch_prob_model <- read_rds('Catch_Prob_Model.RDS')
    AAA_data2 <- AAA_data2 %>% 
      mutate(catch_probability = predict(catch_prob_model, AAA_data2)$predictions[,2])
    AAA_data <- bind_rows(AAA_data1, AAA_data2)
    remove(AAA_data1, AAA_data2)
    
    updateProgressBar(session, 'progress2', value = 24)
    
    remove(field_model_AAAx,field_model_AAAy)
    print("Field Model AAA Deleted")
    
    field_model_AAx <- read_rds("Field_Model_AAx.RDS")
    print("Field Model AAx Loaded")
    field_model_AAy <- read_rds("Field_Model_AAy.RDS")
    print("Field Model AAy Loaded")
    
    AA_data1 <- tibble(
      field_x = input$field_x2,
      field_y = input$field_y2,
      max_fielder_speed = 20,
      time_remaining = seq(round(input$time_remaining2,1)*1000, round(input$time_remaining2,1)*1000-400, -50),
      time_in_play = seq(0,400,50),
      final_ball_position_x = input$ball_x2,
      final_ball_position_y = input$ball_y2,
      Level = "AA",
      prev_field_x = NA,
      prev_field_y = NA,
      prev_distance = NA,
      prev_dist_x = NA,
      prev_dist_y = NA,
      distance_from_ball = NA,
      dist_x = NA,
      dist_x1 = NA,
      dist_y = NA,
      dist_y1 = NA,
      catch_probability = NA
    )
    AA_data1$ball_position_x <- 0
    AA_data1$ball_position_y <- seq(54.5,2,length.out = nrow(AA_data1))
    
    AA_data2 <- tibble(
      field_x = NA,
      field_y = NA,
      max_fielder_speed = 20,
      time_remaining = seq(round(input$time_remaining2,1)*1000-450, 0, -50),
      time_in_play = seq(450, round(input$time_remaining2,1)*1000,50),
      final_ball_position_x = input$ball_x2,
      final_ball_position_y = input$ball_y2,
      Level = "AA",
      prev_field_x = NA,
      prev_field_y = NA,
      prev_distance = NA,
      prev_dist_x = NA,
      prev_dist_y = NA,
      distance_from_ball = NA,
      dist_x = NA,
      dist_x1 = NA,
      dist_y = NA,
      dist_y1 = NA
    )

    AA_data2$ball_position_x <- seq(0,input$ball_x2,length.out = nrow(AA_data2))
    AA_data2$ball_position_y <- seq(2,input$ball_y2,length.out = nrow(AA_data2))
    
    AA_data2 <- AA_data2 %>% 
      mutate(field_x = ifelse(time_in_play == 450, input$field_x2,NA),
             field_y = ifelse(time_in_play == 450, input$field_y2, NA),
             dist_x1 = ifelse(time_in_play == 450, final_ball_position_x - field_x, NA),
             dist_y1 = ifelse(time_in_play == 450, final_ball_position_y - field_y, NA),
             distance_from_ball = ifelse(time_in_play == 450, sqrt((dist_x1)^2 + (dist_y1)^2), NA))
    
    
    for (i in 2:nrow(AA_data2)){
      AA_data2[i, "prev_field_x"] <- AA_data2[i-1, "field_x"]
      AA_data2[i,"prev_field_y"] <- AA_data2[i-1, "field_y"]
      AA_data2[i,"prev_distance"] <- AA_data2[i-1, "distance_from_ball"]
      AA_data2[i,"prev_dist_x"] <- AA_data2[i-1, "dist_x1"]
      AA_data2[i,"prev_dist_y"] <- AA_data2[i-1, "dist_y1"]
      preds_field_position_x <- predict(field_model_AAx, AA_data2[i,])$predictions
      preds_field_position_y <- predict(field_model_AAy, AA_data2[i,])$predictions
      AA_data2[i,"field_x"] <- preds_field_position_x
      AA_data2[i,"field_y"] <- preds_field_position_y
      AA_data2 <- AA_data2 %>% 
        mutate(dist_x1 = final_ball_position_x - field_x,
               dist_y1 = final_ball_position_y - field_y,
               distance_from_ball = sqrt((dist_x1)^2 + (dist_y1)^2))
      progress <- round((((i/nrow(AA_data2))*23)+24),0)
      updateProgressBar(session, 'progress2', value = progress)
    }
    
    
    AA_data2 <- AA_data2 %>% 
      mutate(dist_x = abs(dist_x1),
             dist_y = abs(dist_y1))
    AA_data2 <- AA_data2 %>% 
      mutate(catch_probability = predict(catch_prob_model, AA_data2)$predictions[,2])
    
    AA_data <- bind_rows(AA_data1, AA_data2)
    
    remove(AA_data1, AA_data2)
    
    updateProgressBar(session, 'progress2', value = 48)
    
    remove(field_model_AAx, field_model_AAy)
    print("Field Model AA Deleted")
    field_model_Aplusx <- read_rds("Field_Model_Aplusx.RDS")
    print("Field Model Aplusx Laoded")
    field_model_Aplusy <- read_rds("Field_Model_Aplusy.RDS")
    print("Field Model Aplusy Loaded")
    
    Aplus_data1 <- tibble(
      field_x = input$field_x2,
      field_y = input$field_y2,
      max_fielder_speed = 20,
      time_remaining = seq(round(input$time_remaining2,1)*1000, round(input$time_remaining2,1)*1000-400, -50),
      time_in_play = seq(0,400,50),
      final_ball_position_x = input$ball_x2,
      final_ball_position_y = input$ball_y2,
      Level = "A+",
      prev_field_x = NA,
      prev_field_y = NA,
      prev_distance = NA,
      prev_dist_x = NA,
      prev_dist_y = NA,
      distance_from_ball = NA,
      dist_x = NA,
      dist_x1 = NA,
      dist_y = NA,
      dist_y1 = NA,
      catch_probability = NA
    )

    Aplus_data1$ball_position_x <- 0
    Aplus_data1$ball_position_y <- seq(54.5,2,length.out = nrow(Aplus_data1))
    
    Aplus_data2 <- tibble(
      field_x = NA,
      field_y = NA,
      max_fielder_speed = 20,
      time_remaining = seq(round(input$time_remaining2,1)*1000-450, 0, -50),
      time_in_play = seq(450,round(input$time_remaining2,1)*1000,50),
      final_ball_position_x = input$ball_x2,
      final_ball_position_y = input$ball_y2,
      Level = "A+",
      prev_field_x = NA,
      prev_field_y = NA,
      prev_distance = NA,
      prev_dist_x = NA,
      prev_dist_y = NA,
      distance_from_ball = NA,
      dist_x = NA,
      dist_x1 = NA,
      dist_y = NA,
      dist_y1 = NA
    )
    
    Aplus_data2$ball_position_x <- seq(0,input$ball_x2,length.out = nrow(Aplus_data2))
    Aplus_data2$ball_position_y <- seq(2,input$ball_y2,length.out = nrow(Aplus_data2))
    
    Aplus_data2 <- Aplus_data2 %>% 
      mutate(field_x = ifelse(time_in_play == 450, input$field_x2,NA),
             field_y = ifelse(time_in_play == 450, input$field_y2, NA),
             dist_x1 = ifelse(time_in_play == 450, final_ball_position_x - field_x, NA),
             dist_y1 = ifelse(time_in_play == 450, final_ball_position_y - field_y, NA),
             distance_from_ball = ifelse(time_in_play == 450, sqrt((dist_x1)^2 + (dist_y1)^2), NA))
    
    for (i in 2:nrow(Aplus_data2)){
      Aplus_data2[i, "prev_field_x"] <- Aplus_data2[i-1, "field_x"]
      Aplus_data2[i,"prev_field_y"] <- Aplus_data2[i-1, "field_y"]
      Aplus_data2[i,"prev_distance"] <- Aplus_data2[i-1, "distance_from_ball"]
      Aplus_data2[i,"prev_dist_x"] <- Aplus_data2[i-1, "dist_x1"]
      Aplus_data2[i,"prev_dist_y"] <- Aplus_data2[i-1, "dist_y1"]
      preds_field_position_x <- predict(field_model_Aplusx, Aplus_data2[i,])$predictions
      preds_field_position_y <- predict(field_model_Aplusy, Aplus_data2[i,])$predictions
      Aplus_data2[i,"field_x"] <- preds_field_position_x
      Aplus_data2[i,"field_y"] <- preds_field_position_y
      Aplus_data2 <- Aplus_data2 %>% 
        mutate(dist_x1 = final_ball_position_x - field_x,
               dist_y1 = final_ball_position_y - field_y,
               distance_from_ball = sqrt((dist_x1)^2 + (dist_y1)^2))
      progress <- round((((i/nrow(Aplus_data2))*23)+48),0)
      updateProgressBar(session, 'progress2', value = progress)
    }
    
    
    Aplus_data2 <- Aplus_data2 %>% 
      mutate(dist_x = abs(dist_x1),
             dist_y = abs(dist_y1))
    Aplus_data2 <- Aplus_data2 %>% 
      mutate(catch_probability = predict(catch_prob_model, Aplus_data2)$predictions[,2])
    
    Aplus_data <- bind_rows(Aplus_data1, Aplus_data2)
    
    remove(Aplus_data1, Aplus_data2)
    
    updateProgressBar(session, 'progress2',value = 72)
    
    remove(field_model_Aplusx, field_model_Aplusy)
    #remove(catch_prob_model)
    print("Field Model Aplus Deleted")
    field_model_Ax <- read_rds("Field_Model_Ax.RDS")
    print("Field Model Ax Loaded")
    field_model_Ay <- read_rds("Field_Model_Ay.RDS")
    print("Field Model Ay Loaded")
    
    A_data1 <- tibble(
      field_x = input$field_x2,
      field_y = input$field_y2,
      max_fielder_speed = 20,
      time_remaining = seq(round(input$time_remaining2,1)*1000, round(input$time_remaining2,1)*1000-400, -50),
      time_in_play = seq(0,400,50),
      final_ball_position_x = input$ball_x2,
      final_ball_position_y = input$ball_y2,
      Level = "A",
      prev_field_x = NA,
      prev_field_y = NA,
      prev_distance = NA,
      prev_dist_x = NA,
      prev_dist_y = NA,
      distance_from_ball = NA,
      dist_x = NA,
      dist_x1 = NA,
      dist_y = NA,
      dist_y1 = NA,
      catch_probability = NA
    )

    A_data1$ball_position_x <- 0
    A_data1$ball_position_y <- seq(54.5,2,length.out = nrow(A_data1))
    
    A_data2 <- tibble(
      field_x = NA,
      field_y = NA,
      max_fielder_speed = 20,
      time_remaining = seq(round(input$time_remaining2,1)*1000-450, 0, -50),
      time_in_play = seq(450,round(input$time_remaining2,1)*1000,50),
      final_ball_position_x = input$ball_x2,
      final_ball_position_y = input$ball_y2,
      Level = "A",
      prev_field_x = NA,
      prev_field_y = NA,
      prev_distance = NA,
      prev_dist_x = NA,
      prev_dist_y = NA,
      distance_from_ball = NA,
      dist_x = NA,
      dist_x1 = NA,
      dist_y = NA,
      dist_y1 = NA
    )
    
    A_data2$ball_position_x <- seq(0,input$ball_x2,length.out = nrow(A_data2))
    A_data2$ball_position_y <- seq(2,input$ball_y2,length.out = nrow(A_data2))
    
    A_data2 <- A_data2 %>% 
      mutate(field_x = ifelse(time_in_play == 450, input$field_x2,NA),
             field_y = ifelse(time_in_play == 450, input$field_y2, NA),
             dist_x1 = ifelse(time_in_play == 450, final_ball_position_x - field_x, NA),
             dist_y1 = ifelse(time_in_play == 450, final_ball_position_y - field_y, NA),
             distance_from_ball = ifelse(time_in_play == 450, sqrt((dist_x1)^2 + (dist_y1)^2), NA))
    
    for (i in 2:nrow(A_data2)){
      A_data2[i, "prev_field_x"] <- A_data2[i-1, "field_x"]
      A_data2[i,"prev_field_y"] <- A_data2[i-1, "field_y"]
      A_data2[i,"prev_distance"] <- A_data2[i-1, "distance_from_ball"]
      A_data2[i,"prev_dist_x"] <- A_data2[i-1, "dist_x1"]
      A_data2[i,"prev_dist_y"] <- A_data2[i-1, "dist_y1"]
      preds_field_position_x <- predict(field_model_Ax, A_data2[i,])$predictions
      A_data2[i,"field_x"] <- preds_field_position_x
      preds_field_position_y <- predict(field_model_Ay, A_data2[i,])$predictions
      A_data2[i,"field_y"] <- preds_field_position_y
      A_data2 <- A_data2 %>% 
        mutate(dist_x1 = final_ball_position_x - field_x,
               dist_y1 = final_ball_position_y - field_y,
               distance_from_ball = sqrt((dist_x1)^2 + (dist_y1)^2))
      progress <- round((((i/nrow(A_data2))*23)+72),0)
      print(i)
      updateProgressBar(session, 'progress2', value = progress)
    }
    
    
    A_data2 <- A_data2 %>% 
      mutate(dist_x = abs(dist_x1),
             dist_y = abs(dist_y1))
    #catch_prob_model <- read_rds('Catch_Prob_Model.RDS')
    A_data2 <- A_data2 %>% 
      mutate(catch_probability = predict(catch_prob_model, A_data2)$predictions[,2])
    
    A_data <- bind_rows(A_data1, A_data2)
    
    remove(A_data1, A_data2)
    
    updateProgressBar(session, 'progress2', value = 96)
    remove(field_model_Ax, field_model_Ay)
    print("Field Model A Deleted")
    
    fielding_data <- bind_rows(AAA_data, AA_data, Aplus_data, A_data) %>% 
      arrange(time_in_play)
    
    
    fielding_data <- fielding_data %>% 
      mutate(Level = case_when(
        Level == "AAA" ~ "4A",
        Level == "AA" ~ "3A",
        Level == 'A+' ~ "2A",
        Level == "A" ~ '1A'
      ))
    
    updateProgressBar(session, 'progress2', value = 97)
    
    #### copied function from stack overflow & plotly documentation: 
    #https://plotly.com/r/cumulative-animations/
    
    fielding_data1 <- fielding_data %>% accumulate_by(~time_in_play/1000)
    head(fielding_data1, 10)
    
    updateProgressBar(session, 'progress2', value = 98)
    
    p2 <- p + 
      geom_path(data = fielding_data1, aes(field_x, field_y, color = Level, frame = time_in_play_s), alpha = 0.5) +
      geom_point(data = fielding_data, aes(field_x, field_y, color = Level, frame = time_in_play/1000), alpha = 0.5) +
      geom_point(data = fielding_data, aes(ball_position_x, ball_position_y, frame = time_in_play/1000), color = "white")
    
    p2 <- ggplotly(p2)  %>% 
      layout(
        dragmode = "pan",
        modebar = list(
          remove = c("lasso", 'select')
        )
      ) %>% 
      add_trace(data = AAA_data,
                text = ~paste("4A Catch Prob: ", round(catch_probability * 100, 1), "%"),
                x = -160,y = 30, type = 'scatter', mode = "text",
                showlegend = FALSE, textfont = list(color = 'plum', size = 12, family = 'serif'),
                frame = ~time_in_play/1000
      ) %>% 
      add_trace(data = AA_data,
                text = ~paste("3A Catch Prob: ", round(catch_probability * 100, 1), "%"),
                x = -160, y = 0, type = 'scatter', mode = "text",
                showlegend = FALSE,textfont = list(color = 'lightblue', size = 12, family = 'serif'),
                frame = ~time_in_play/1000
      ) %>%
      add_trace(data = Aplus_data,
                text = ~paste("2A Catch Prob: ", round(catch_probability * 100, 1), "%"),
                x = 150, y = 30,
                type = 'scatter', mode = "text",
                showlegend = FALSE, textfont = list(color = 'greenyellow', size = 12, family = 'serif'),
                frame = ~time_in_play/1000
      ) %>%
      add_trace(data = A_data,
                text = ~paste("1A Catch Prob: ", round(catch_probability * 100, 1), "%"),
                x = 150, y = 0,type = 'scatter',mode = "text",
                showlegend = FALSE, textfont = list(color = 'tomato', size = 12, family = 'serif'),
                frame = ~time_in_play/1000
      ) %>%
      animation_opts(frame = 100, redraw = FALSE) %>% 
      animation_slider(currentvalue = list(prefix = "Time in Play (s): ", font = list(color = "white")))
    
    remove(AAA_data, AA_data, Aplus_data, A_data)
    remove(fielding_data1, fielding_data)
    
    updateProgressBar(session, 'progress2', value = 100)
    
    output$dynamic_plot2 <- renderUI({
      plotlyOutput("fielding_graph", height = '650px')
    })
    output$fielding_graph <- renderPlotly({
      p2
    })
  })
 #### End of Fielding Across Levels Code ####
 
  #### Start of Optimization Example Code ####
  output$explanation <- renderText({
    "The animations below show how my optimization model works on actual plays. My optimization model attempts 
    to identify what's the best route to the ball in order to maximize catch probability. The light green point is
    the optimal route that maximizes catch probability and the light blue point is the route that the fielder ultimately took. 
    Use the dropdown menu on the left to change plays."
  })
  
  observeEvent(input$optimization_examples,{
    selectedPlay <- switch(
      input$optimization_examples,
      'Play 1' = 'Play 1',
      'Play 2' = 'Play 2', 
      'Play 3' = 'Play 3',
      'Play 4' = 'Play 4',
      'Play 5' = 'Play 5',
      'Play 6' = 'Play 6'
    )
    if(selectedPlay == "Play 1"){
      output$graph3 <-  renderPlotly({
        play1
      })
    } 
    if(selectedPlay == 'Play 2'){
      output$graph3 <-  renderPlotly({
        play2
      })
    } 
    if(selectedPlay == 'Play 3'){
      output$graph3 <-  renderPlotly({
        play3
      })
    }
    if(selectedPlay == 'Play 4'){
      output$graph3 <-  renderPlotly({
        play4
      })
    }
    if(selectedPlay == 'Play 5'){
      output$graph3 <-  renderPlotly({
        play5
      })
    }
    if(selectedPlay == 'Play 6'){
      output$graph3 <-  renderPlotly({
        play6
      })
    }
  })
  #### End of Optimization Example Code ####
 #### Start of Optimization Fielding Path Code ####
  output$dynamic_plot <- renderUI({
    plotOutput("plot1", click = 'plot_click', height = "650px")
  })
  output$reminder <- renderText({
    "Reminder: Always Press the Reset Button Before Changing Inputs."
  })
  output$plot1 <- renderPlot({
    p
  })
  values <- reactiveValues(field_x = NULL, field_y = NULL, final_ball_position_x = NULL, final_ball_position_y = NULL)
  observeEvent(input$plot_click, {
    click <- input$plot_click
    if (is.null(values$field_x)) {
      values$field_x <- click$x
      values$field_y <- click$y
      updateNumericInput(session, "field_x", value = round(click$x,2))
      updateNumericInput(session, "field_y", value = round(click$y,2))
      output$dynamic_plot <- renderUI({
        plotOutput("plot_outfielder", click = 'plot_click',height = "650px")
      })
      output$plot_outfielder <- renderPlot({
        p +
          geom_point(data = data.frame(x = input$field_x,
                                       y = input$field_y), aes(x,y), color = "lightgreen")
      })
    } else {
      click <- input$plot_click
      values$final_ball_position_x <- click$x
      values$final_ball_position_y <- click$y
      updateNumericInput(session, "ball_x", value = round(click$x,2))
      updateNumericInput(session, "ball_y", value = round(click$y,2))
      output$dynamic_plot <- renderUI({
        plotOutput("plot_ball", height = "650px")
      })
      output$plot_ball <- renderPlot({
      p +
        geom_point(data = data.frame(x = input$field_x,
                                       y = input$field_y), aes(x,y), color = "lightgreen")+
          geom_point(data = data.frame(x = input$ball_x,
                                       y = input$ball_y), aes(x,y), color = "white")
      })
    }
  })

  

  
  # observeEvent(input$field_x, {
  #   values$field_x <- input$field_x
  # })
  # 
  # observeEvent(input$field_y, {
  #   values$field_y <- input$field_y
  # })
  # 
  # output$field_x <- renderValueBox({
  #   value_box(
  #     value = round(values$field_x,2),
  #     title = "Field X",
  #     height = '100px'
  #   )
  # })
  
  # observeEvent(input$final_ball_position_x, {
  #   values$final_ball_position_x <- input$final_ball_position_x
  # })
  # 
  # observeEvent(input$final_ball_position_y, {
  #   values$final_ball_position_y <- input$final_ball_position_y
  # })

  observeEvent(input$reset, {
    values$field_x <- NULL
    values$field_y <- NULL
    values$final_ball_position_x <- NULL
    values$final_ball_position_y <- NULL
    updateNumericInput(session, "field_x", value = NULL)
    updateNumericInput(session, "field_y", value = NULL)
    updateNumericInput(session, "ball_x", value = NULL)
     updateNumericInput(session, "ball_y", value = NULL)
    output$dynamic_plot <- renderUI({
      plotOutput("plot1", click = 'plot_click', height = "650px")
    })
    output$plot1 <- renderPlot({
      p
    })
    updateProgressBar(session, id = "progress", value = 0)
  })
  
  output$instructions <- renderText({
    "Click on the graph to select Starting Fielder Position and Final Ball Position.
    The first point you click will be the Fielder Coordinates and the second point you click will be the Final Ball Position Coordinates.
    Use the reset button below to change your inputs. 
    Note: Press the reset button when you first open the app or else the app won't work.
    (Plot takes 1-2 min to generate. Progress Bar Below)"
  })
  

  
  observeEvent(input$opt_graph, {
    #catch_prob_model <- read_rds('Catch_Prob_Model.RDS')
    opt_data1 <- tibble(
      field_x = input$field_x,
      field_y = input$field_y,
      final_ball_position_x = input$ball_x,
      final_ball_position_y = input$ball_y,
      max_fielder_speed = input$max_fielder_speed,
      time_remaining = seq(input$time_remaining*1000, input$time_remaining*1000-400, -200),
      time_in_play = seq(0, 400, 200),
      dist_x = NA,
      dist_y = NA,
      distance_from_ball = NA,
      prev_field_x = NA,
      prev_field_y = NA,
      catch_prob = NA
    )
    
    opt_data1$ball_position_x <- 0
    opt_data1$ball_position_y <- seq(54.5, 2, length.out = nrow(opt_data1))
    
    
    opt_data2 <- tibble(
      final_ball_position_x = input$ball_x,
      final_ball_position_y = input$ball_y,
      max_fielder_speed = input$max_fielder_speed,
      time_remaining = seq(input$time_remaining*1000-600, 0,-200),
      time_in_play = seq(600, input$time_remaining*1000, 200),
      dist_x = NA,
      dist_y = NA,
      distance_from_ball = NA,
      prev_field_x = NA,
      prev_field_y = NA
    )
    
    opt_data2 <- opt_data2 %>% 
      mutate(field_x = ifelse(time_in_play == 600, input$field_x,NA),
             field_y = ifelse(time_in_play == 600, input$field_y,NA)) %>% 
      mutate(dist_x = abs(final_ball_position_x - field_x),
             dist_y = abs(final_ball_position_y - field_y),
             distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2))
    opt_data2 <- opt_data2 %>% 
      mutate(catch_prob = ifelse(time_in_play == 400, predict(catch_prob_model, opt_data2[1,])$predictions[,2],NA))
    #opt_data2 <- opt_data2 %>% filter(time_remaining %% 200 == 0)
    opt_data2$ball_position_x <- seq(0, input$ball_x, length.out = nrow(opt_data2))
    opt_data2$ball_position_y <- seq(2, input$ball_y, length.out = nrow(opt_data2))
    

    total_steps <- nrow(opt_data2)
        
    for (i in 2:nrow(opt_data2)){
      if(opt_data2[i,"time_in_play"] < (400+800)){
      opt_data2[i,"prev_field_x"] <- opt_data2[i-1,"field_x"]
      opt_data2[i,"prev_field_y"] <- opt_data2[i-1, "field_y"]
      combinations <- find_combinations(pull(opt_data2[i,"time_remaining"]),
                                        pull(opt_data2[i,"prev_field_x"]),
                                        pull(opt_data2[i,"prev_field_y"]),
                                        pull(round((opt_data2[i,"max_fielder_speed"]/5)*0.5,1)))
      df <- combinations %>% 
        left_join(opt_data2, by = c("time_remaining")) %>% 
        mutate(field_x = coalesce(field_x, field_x2),
               field_y = coalesce(field_y, field_y2)) %>% 
        select(-field_x2, -field_y2) %>% 
        mutate(dist_x = abs(final_ball_position_x - field_x),
               dist_y = abs(final_ball_position_y - field_y),
               distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2)
        )
      
      df <- df %>% 
        slice_min(distance_from_ball, n = 50)
      df <- df %>% 
        mutate(catch_prob1 = predict(catch_prob_model, df)$predictions[,2])
      df <- df %>% 
        slice_max(catch_prob1, n = 1) %>% 
        slice_min(dist_y, n = 1) %>% #dist_y was deemed more important in the
        # catch probability model so I'm using that first
        # after total distance from ball
        slice_min(dist_x,n = 1) %>%
        slice(1) %>%
        select(time_remaining, field_x, field_y, catch_prob1, distance_from_ball, dist_x, dist_y) %>%
        rename(field_x2 = field_x, field_y2 = field_y)
      
      opt_data2 <- opt_data2 %>% 
        left_join(df, by = c('time_remaining')) %>% 
        mutate(field_x = coalesce(field_x, field_x2),
               field_y = coalesce(field_y, field_y2),
               catch_prob = coalesce(catch_prob, catch_prob1)) %>% 
        select(-field_x2, -field_y2, -catch_prob1)
   progress <- round((i/total_steps)*95)
   updateProgressBar(session, id = "progress", value = progress)
      } else if(opt_data2[i,"time_in_play"] < (400 + 2000)){
        opt_data2[i,"prev_field_x"] <- opt_data2[i-1,"field_x"]
        opt_data2[i,"prev_field_y"] <- opt_data2[i-1, "field_y"]
        combinations <- find_combinations(pull(opt_data2[i,"time_remaining"]),
                                          pull(opt_data2[i,"prev_field_x"]),
                                          pull(opt_data2[i,"prev_field_y"]),
                                          pull(round((opt_data2[i,"max_fielder_speed"]/5)*0.8,1)))
        df <- combinations %>% 
          left_join(opt_data2, by = c("time_remaining")) %>% 
          mutate(field_x = coalesce(field_x, field_x2),
                 field_y = coalesce(field_y, field_y2)) %>% 
          select(-field_x2, -field_y2) %>% 
          mutate(dist_x = abs(final_ball_position_x - field_x),
                 dist_y = abs(final_ball_position_y - field_y),
                 distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2)
          )
        
        df <- df %>% 
          slice_min(distance_from_ball, n = 50)
        df <- df %>% 
          mutate(catch_prob1 = predict(catch_prob_model, df)$predictions[,2])
        df <- df %>% 
          slice_max(catch_prob1, n = 1) %>% 
          slice_min(dist_y, n = 1) %>% #dist_y was deemed more important in the
          # catch probability model so I'm using that first
          # after total distance from ball
          slice_min(dist_x,n = 1) %>%
          slice(1) %>%
          select(time_remaining, field_x, field_y, catch_prob1, distance_from_ball, dist_x, dist_y) %>%
          rename(field_x2 = field_x, field_y2 = field_y)
        
        opt_data2 <- opt_data2 %>% 
          left_join(df, by = c('time_remaining')) %>% 
          mutate(field_x = coalesce(field_x, field_x2),
                 field_y = coalesce(field_y, field_y2),
                 catch_prob = coalesce(catch_prob, catch_prob1)) %>% 
          select(-field_x2, -field_y2, -catch_prob1)
        progress <- round((i/total_steps)*95)
        updateProgressBar(session, id = "progress", value = progress)
      } else{
        opt_data2[i,"prev_field_x"] <- opt_data2[i-1,"field_x"]
        opt_data2[i,"prev_field_y"] <- opt_data2[i-1, "field_y"]
        combinations <- find_combinations(pull(opt_data2[i,"time_remaining"]),
                                          pull(opt_data2[i,"prev_field_x"]),
                                          pull(opt_data2[i,"prev_field_y"]),
                                          pull(round((opt_data2[i,"max_fielder_speed"]/5),1)))
        df <- combinations %>% 
          left_join(opt_data2, by = c("time_remaining")) %>% 
          mutate(field_x = coalesce(field_x, field_x2),
                 field_y = coalesce(field_y, field_y2)) %>% 
          select(-field_x2, -field_y2) %>% 
          mutate(dist_x = abs(final_ball_position_x - field_x),
                 dist_y = abs(final_ball_position_y - field_y),
                 distance_from_ball = sqrt((dist_x)^2 + (dist_y)^2)
          )
        
        df <- df %>% 
          slice_min(distance_from_ball, n = 50)
        df <- df %>% 
          mutate(catch_prob1 = predict(catch_prob_model, df)$predictions[,2])
        df <- df %>% 
          slice_max(catch_prob1, n = 1) %>% 
          slice_min(dist_y, n = 1) %>% #dist_y was deemed more important in the
          # catch probability model so I'm using that first
          # after total distance from ball
          slice_min(dist_x,n = 1) %>%
          slice(1) %>%
          select(time_remaining, field_x, field_y, catch_prob1, distance_from_ball, dist_x, dist_y) %>%
          rename(field_x2 = field_x, field_y2 = field_y)
        
        opt_data2 <- opt_data2 %>% 
          left_join(df, by = c('time_remaining')) %>% 
          mutate(field_x = coalesce(field_x, field_x2),
                 field_y = coalesce(field_y, field_y2),
                 catch_prob = coalesce(catch_prob, catch_prob1)) %>% 
          select(-field_x2, -field_y2, -catch_prob1)
        progress <- round((i/total_steps)*95)
        updateProgressBar(session, id = "progress", value = progress)
      }
       }
    opt_data <- bind_rows(opt_data1, opt_data2)
    
    opt_data %>% select(dist_x, dist_y, ball_position_x, ball_position_y) %>% print(n = nrow(opt_data))
    
    opt_data1 <- opt_data %>% accumulate_by(~time_in_play/1000)
    
    p1 <- p + 
      geom_path(data = opt_data1, aes(field_x, field_y, frame = time_in_play_s), color = "lightgreen")
    
    p1 <- ggplotly(p1) %>%
      layout(
        dragmode = "pan",
        modebar = list(
          remove = c("lasso", 'select')
        )
      ) %>% 
      add_trace(
        data = opt_data,
        x = ~ball_position_x,
        y = ~ball_position_y,
        type = 'scatter',
        mode = 'markers',
        marker = list(color = 'white'),
        frame = ~time_in_play/1000
      ) %>%
      add_trace(
        data = opt_data,
        x = ~field_x,
        y = ~field_y,
        type = 'scatter',
        mode = 'markers',
        marker = list(color = 'lightgreen'),
        frame = ~time_in_play/1000
      ) %>%
      add_trace(
        text = ~paste("Catch Prob: ", round(catch_prob * 100, 1), "%"),
        x = 150,
        y = 30,
        type = 'scatter',
        mode = "text",
        showlegend = FALSE,
        textfont = list(color = 'white', size = 12, family = 'serif'),
        frame = ~time_in_play/1000
      ) %>% animation_opts(frame = 400, redraw = FALSE) %>% 
      animation_slider(currentvalue = list(prefix = "Time in Play (s):", font = list(color = "white")))
    output$dynamic_plot <- renderUI({
      plotlyOutput("optimal_graph", height = '650px')
    })
    updateProgressBar(session, id = "progress", value = 100)
    output$optimal_graph <- renderPlotly({
      p1
    })
  })
#### End of Optimization Fielding Path Code ####
}

shinyApp(ui = ui, server = server)
