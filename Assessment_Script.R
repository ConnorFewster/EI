# packages ----
library(tidyverse)
library(ggplot2)
library(janitor)
library(readr)
library(readxl)
library(emmeans)
library(broom)
library(easystats)
library(corrplot)
library(lme4)
library(remotes)
library(lubridate)
library(broom.mixed)
library(ggridges)
library(ggdist)
library(rsconnect)
library(shiny)
library(shinyWidgets)

# import data ----
Phil_GPS <- read.csv("msc_data.csv")

# wrangle date codes ----
Phil_GPS$Date <- as.character(Phil_GPS$Date)

# Handle serial dates (Excel-style numeric dates)
df_serial <- Phil_GPS %>%
  filter(grepl("^[0-9]+$", Date)) %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
         Date = format(Date, "%d-%m-%Y")) 

# Handle proper date strings
df_date <- Phil_GPS %>%
  filter(!grepl("^[0-9]+$", Date) & !is.na(mdy(Date))) %>%
  mutate(Date = as.Date(mdy(Date)),
         Date = format(Date, "%d-%m-%Y")) 

# Combine both datasets
Phil_GPS_cleaned <- bind_rows(df_serial, df_date)

# bind new dates ----
Phil_GPS <- rbind(df_date, df_serial)

# alter ID Names ----
Phil_GPS <- Phil_GPS %>%
  mutate(ID = paste0("Ath", ID))
         
# alter alt codes ----
Phil_GPS <- Phil_GPS_cleaned %>%
  mutate(
    Altitude_Desc = case_when(
    Altitude_code == "1" ~ "Sea Level",
    Altitude_code == "2" ~ "Low",
    Altitude_code == "3" ~ "Moderate"),
    Altitude_Desc = factor(Altitude_Desc, levels = c("Sea Level",
                                                     "Low",
                                                     "Moderate")))

# alter GD codes ----
Phil_GPS <- Phil_GPS %>%
  mutate(game_day = case_when(
    GD == "0" ~ "GD",
    GD == "1" ~ "GD -1",
    GD ==  "2" ~ "GD -2"
  ))

# alter columns to numeric ----
Phil_GPS <- Phil_GPS %>%
  mutate(across(c(RPELeg, RPEBreathe, RPETech, RPESession, TimeinEnvironment,
                  Distance, Distancemin, HighSpeedRunning, HSRmin, PlayerLoad,
                  Playerloadmin, Soreness_1, Mood_1, Nutrition_1), as.numeric))

# wrangle for TIE and GD model ----
soreness <- Phil_GPS %>%
  select(ID, Date, Altitude_Desc, game_day, Soreness_1) %>%
  drop_na() %>%
  mutate(
    game_day = case_when(
    game_day == "GD" ~ "GD+1",
    game_day == "GD -1" ~ "GD",
    game_day == "GD -2" ~ "GD-1"),
    game_day = factor(game_day, levels = c("GD-1", "GD", "GD+1")))

# plot for post game soreness
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

# model & data frame
soreness_mod <- lmer(Soreness_1 ~ Altitude_Desc * game_day + (1 | ID),
                     data = soreness)

sor_alt_results <- tidy(soreness_mod, effects = "fixed", conf.int = TRUE)  
  

# ggplot
sor_alt_results <- sor_alt_results %>%
  filter(!term %in% c("(Intercept)", "Altitude_DescLow", "Altitude_DescModerate")) %>%
  mutate(term = recode(term,
    "game_dayGD" = "GD",
    "game_dayGD+1" = "GD +1", 
    "Altitude_DescLow:game_dayGD" = "GD Low Altitude",
    "Altitude_DescModerate:game_dayGD" = "GD Moderate Altitude",
    "Altitude_DescLow:game_dayGD+1" = "GD +1 Low Altitude",
    "Altitude_DescModerate:game_dayGD+1" = "GD +1 Moderate Altitude"),
    GD = case_when(
      term %in% c("GD", "GD Low Altitude", "GD Moderate Altitude") ~ "GD",
      term %in% c("GD +1", "GD +1 Low Altitude", "GD +1 Moderate Altitude") ~ "+1",
      TRUE ~ "intercept"
    ))

# Model ggplot
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

# external load ~ Altitude ----
ext_sor <- Phil_GPS %>%
  select(ID, game_day, Altitude_Desc, Soreness_1, Distance, Distancemin,
         HighSpeedRunning, HSRmin, PlayerLoad, Playerloadmin)

# distance raincloud
dis_GD <- ext_sor %>%
  filter(game_day == "GD") 

ggplot(dis_GD, aes(x = Distance, 
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
                               "Low" = "lightskyblue",
                               "Moderate" = "gold")) +
  scale_colour_manual(values = c("Sea Level" = "slategray4", 
                               "Low" = "lightskyblue",
                               "Moderate" = "gold")) +
  theme_minimal() +
  labs(x = "Distance (m)",
       title = "Distance Covered",
       subtitle = "Depending on Altitude Descriptor") +
  theme(
    # legend
    legend.position = "none",
    
    # axis
    axis.title.y = element_blank(),
    axis.text.y = element_text(face = "bold", colour = "black"),
    
    # plot
    plot.title = element_text(face = "bold", colour = "black")
  )

dis_GD_long <- ext_sor %>%
  filter(game_day == "GD") %>%
  select(Altitude_Desc, Distance, Distancemin, HighSpeedRunning, 
         HSRmin, PlayerLoad, Playerloadmin) %>%
  pivot_longer(cols = -Altitude_Desc,
               names_to = "Var",
               values_to = "Val")

# model for differences
ext_mod <- manova(cbind(Distance, Distancemin, HighSpeedRunning, HSRmin,
                        PlayerLoad, Playerloadmin) ~ Altitude_Desc, 
                  data = dis_GD)

tidy(ext_mod)

summary.aov(ext_mod)

# plot for external loads
ggplot(dis_GD_long, aes(x = Altitude_Desc, y = Val, colour = Altitude_Desc)) + 
  scale_colour_manual(values = c("Sea Level" = "slategray4",
                                 "Low" = "lightskyblue",
                                 "Moderate" = "gold")) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2, aes(colour = Altitude_Desc)) +
  geom_smooth(method = "lm", aes(group = Var), 
              se = TRUE, linewidth = 0.5, colour = "black") +
  theme_minimal() +
  theme(
    # axis
    axis.title = element_blank(),
    
    # legend
    legend.position = "none"
  )

