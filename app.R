# packages ----
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(shinydashboard)
library(fontawesome)
library(rsconnect)
library(ggplot2)
library(readr)
library(ggridges)
library(ggdist)
library(DT)

# get data ----
soreness_url <- "https://raw.githubusercontent.com/ConnorFewster/EI/refs/heads/main/soreness.csv"
soreness <- read_csv(soreness_url)

sor_alt_results_url <- "https://raw.githubusercontent.com/ConnorFewster/EI/refs/heads/main/sor_alt_results.csv"
sor_alt_results <- read_csv(sor_alt_results_url)

dis_GD_long_url <- "https://raw.githubusercontent.com/ConnorFewster/EI/refs/heads/main/dis_GD_long.csv"
dis_GD_long <- read_csv(dis_GD_long_url)

ext_load_mean_url <- "https://raw.githubusercontent.com/ConnorFewster/EI/refs/heads/main/ext_load_mean.csv"
ext_load_mean <- read_csv(ext_load_mean_url)

# Define UI ----
ui <- dashboardPage(
  skin = "black",
  
  # Application title ----
  dashboardHeader(
    title = div(
      img(src = "PU_logo.png", height = 50),
      span("Does Altitude Influence Perceived Soreness & External Load in Football Players?", 
           style = "font-size: 18px; font-weight: bold;")  
    ),
    titleWidth = 900
  ),
  
  # Sidebar ----
  dashboardSidebar(
    sidebarMenu(
      menuItem("Perceived Soreness", tabName = "PerceivedSoreness", icon = icon("user-injured")),
      menuItem("External Load", tabName = "ExternalLoad", icon = icon("person-running"))
    )
  ),
  
  # Body content ----
  dashboardBody(
    tabItems(
      # Perceived Soreness Tab
      tabItem(tabName = "PerceivedSoreness",
              fluidPage(
                fluidRow(
                  box(plotOutput("perceived_soreness"), height = 500, width = 6),
                  box(plotOutput("alt_forest"), height = 500, width = 6)
                ),
                fluidRow(
                  box(textOutput("perceived_soreness_text"), height = 150, width = 6),
                  box(textOutput("alt_forest_text"), height = 150, width = 6)
                )
              )
      ),
      
      # External Load Tab ----
      tabItem(tabName = "ExternalLoad",
              fluidPage(
                fluidRow(
                  box(
                    title = "Load Measure", width = 12,
                    selectInput("ext_load", "Select a Variable:", choices = NULL, multiple = FALSE)
                  )
                ),
                fluidRow(
                  box(plotOutput("extload", height = 500), width = 6),
                  box(plotOutput("manova", height = 500), width = 6)
                ),
                  fluidRow(
                  box(DTOutput("ext_load_mean", height = 175), 
                      width = 12, 
                      title = "Average External Loads Depending on Altitude")
                   ),
                fluidRow(
                  box(textOutput("ext_load_text"), height = 115, width = 12)
                   )
                 )
              ) 
           )  
        )  
      ) 
   

# Define server ----
server <- function(input, output, session) {
  
  # soreness pre / post game ----
  output$perceived_soreness <- renderPlot({
    ggplot(soreness, aes(x = game_day, y = Soreness_1)) +
      stat_boxplot(geom = "errorbar") +
      geom_boxplot(aes(fill = Altitude_Desc)) +
      facet_wrap(~ Altitude_Desc) +
      scale_fill_manual(values = c("Sea Level" = "slategray4", 
                                   "Low" = "lightskyblue",
                                   "Moderate" = "gold")) +
      theme_minimal() +
      labs(title = "Differences in Soreness Pre & Post Game",
           subtitle = "Depending on Altitude | MLS",
           y = "Perceived Soreness") +
      theme(
        # axis
        axis.title.x = element_blank(),
        
        # strip text
        strip.text = element_text(face = "bold"),
        
        # legend 
        legend.position = "none",
        
        # plot
        plot.title = element_text(face = "bold")
      )
  })
  
  # text for soreness plot ----
  output$perceived_soreness_text <- renderText({
    "The exploratory data analysis revealed a steep difference in players' 
    perceived muscular soreness at moderate altitudes compared to sea level. 
    It is expected that soreness remains relatively consistent between GD-1 and 
    GD due to the controlled training environment and the delayed onset of 
    soreness following competition. However, irrespective of altitude, soreness 
    was consistently reported to be higher on GD+1. The pronounced change 
    observed at moderate altitudes warranted a more comprehensive statistical analysis."
  })

  # model for soreness plot ----
  output$alt_forest <- renderPlot({
    ggplot(sor_alt_results, aes(x = estimate, y = reorder(term, estimate))) +
      geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
      geom_linerange(aes(xmin = conf.low,
                         xmax = conf.high), 
                     linewidth = 2, 
                     colour = "gold",
                     alpha = 0.3) +
      geom_point(aes(shape = GD, colour = GD), size = 4) +
      scale_colour_manual(values = c("+1" = "slategrey",
                                     "GD" = "lightskyblue",
                                     "intercept" = "gold")) +
      theme_minimal() +
      labs(title = "The Effect of Altitude On Muscular Soreness",
           subtitle = "Estimates (95% CI)") +
      theme(
        # axis
        axis.title = element_blank(),
        axis.text.y = element_text(colour = "black"),
        
        # legend 
        legend.position = "bottom",
        legend.title = element_blank(),
        
        # plot
        plot.title = element_text(face = "bold", colour = "black")
      )
  })
  
  # text for soreness model ----
  output$alt_forest_text <- renderText({
    "A linear model revealed two significant findings regarding perceived soreness
    and altitude. On GD+1 regardless of altitude it was found that soreness significantly (p <0.05)
    increased by 1.27 compared to GD-1, but, GD+1 soreness at moderate altitudes 
    significantly (p < 0.01) increased by 4.73 compared to GD-1. Therefore, 
    greater considerations for recovery are needed at higher altitudes."
  })
  
  # select input from dis_GD_long ----
  observe({
    req(dis_GD_long)
    updateSelectInput(session, "ext_load", choices = unique(dis_GD_long$Var))
  })
  
  # ext load rain cloud ----
  output$extload <- renderPlot({
    req(dis_GD_long, input$ext_load)
    
    df <- dis_GD_long %>%
      filter(Var == input$ext_load)
    
    ggplot(df, aes(x = Val, 
                   y = Altitude_Desc,
                   fill = Altitude_Desc,
                   colour = Altitude_Desc,
                   alpha = 0.8)) +
      stat_halfeye(adjust = 0.5,
                   justification = -0.2,
                   .width = 0,
                   point_colour = NA,
                   scale = 0.8) +
      geom_point(alpha = 0.9,
                 position = position_nudge(y = 0.1)) +
      scale_fill_manual(values = c("Sea Level" = "slategray4", 
                                   "Altitude" = "gold")) +
      scale_colour_manual(values = c("Sea Level" = "slategray4", 
                                     "Altitude" = "gold")) +
      theme_minimal() +
      labs(title = "External Load Volume",
           subtitle = "Depending on Altitude Descriptor") +
      theme(
        # legend
        legend.position = "none",
        
        # axis
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(face = "bold", colour = "black"),
        
        # plot
        plot.title = element_text(face = "bold", colour = "black")
      )
  })
  
  # manova scatter ----
  output$manova <- renderPlot({
    req(dis_GD_long, input$ext_load)
    
    df <- dis_GD_long %>%
      filter(Var == input$ext_load)
    
    ggplot(df, aes(x = Altitude_Desc, y = Val, colour = Altitude_Desc)) + 
      scale_colour_manual(values = c("Sea Level" = "slategray4",
                                     "Altitude" = "gold")) +
      geom_jitter(width = 0.2, alpha = 0.6, size = 5, aes(colour = Altitude_Desc)) +
      geom_smooth(method = "lm", aes(group = Var), 
                  se = TRUE, linewidth = 0.5, colour = "black") +
      labs(title = "MANOVA Results",
           subtitle = "Based on External Load Descriptor") +
      theme_minimal() +
      theme(
        # plot
        plot.title = element_text(face = "bold"),
        
        # axis
        axis.title = element_blank(),
        
        # legend
        legend.position = "none"
      )
  })
  
  # external load text ----
 output$ext_load_text <- renderText({
   "Descriptive statistical analysis indicates that certain external load measures 
   exhibit greater magnitudes under specific altitude conditions; however, this 
   effect is not consistently observed across all altitude levels. The most notable
   difference was identified in total distance covered, with players, on average, 
   covering 225 metres less when competing at altitude compared to sea level. 
   Despite this observed difference, the effect was not statistically significant 
   (p > 0.05), a trend that was consistent across all external load measures. 
   Nevertheless, this finding should not preclude further investigation into 
   the observed patterns, particularly the tendency for volume-based metrics 
   to be higher at sea level, while intensity-related metrics appear to be elevated
   at altitude."
 })
  
  # mean data table ----
  output$ext_load_mean <- renderDT({
    req(ext_load_mean)
    
    datatable(ext_load_mean, options = list(
      scrollx = TRUE,
      autoWidth = TRUE,
      pageLength = 3))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

