# Library import
library(methods)
library(moveHMM)
library(tidyverse)
library(lubridate)
set.seed(1729)

# Dataset for overlap between PSG and actigraphy
overlap <- read_csv("/home/janique/mesa/overlap/mesa-actigraphy-psg-overlap.csv",
                    col_types = cols_only(
                        mesaid = col_integer(),
                        line = col_integer()
                    )) 

# Function for importing actigraphy
df <- read_csv("/home/janique/mesa/actigraphy/mesa-sleep-0001.csv",
               col_types = cols_only(
                   mesaid = col_integer(),
                   linetime = col_character(),
                   offwrist = col_integer(),
                   activity = col_double(),
                   marker = col_integer(),
                   whitelight = col_double(),
                   wake = col_factor(),
                   interval = col_factor(),
                   daybymidnight = col_integer(),
                   daybynoon = col_integer()
               )) %>% 
    filter(offwrist == 0 & !is.na(activity) & daybymidnight < 8) %>% 
    rename(tm_char = linetime, ID = mesaid) %>% 
    mutate(linetime = parse_time(tm_char), 
           date_var = ymd("1960-01-01") + days(daybymidnight),
           dt = ymd_hms(paste(date_var, linetime)),
           dt5 = floor_date(dt, unit = "5 minutes"),
           .keep = "unused")
    
# Summarising activity in 5-minute intervals
df2 <- df %>% select(ID, activity, dt5) %>% 
    group_by(ID, dt5) %>% 
    summarise(act5_mean = mean(activity),
              dummy = 0, .groups = "drop")
    
# Calculate preparatory dataframe
df_prep <- df2 %>%
    select(all_of(c("act5_mean", "dummy", "ID"))) %>% 
    as.data.frame() %>% 
    prepData(type = "UTM", coordNames = c("act5_mean", "dummy"))
    
# Aesthetics for plotting
facet_names <- c("x" = "Averaged actigraphy",
                 "step" = "Absolute differences of consecutive averaged values")

# Plot of actigraphy differences
df_prep %>% select(step, x) %>% 
    pivot_longer(cols = c("step", "x"),
                 names_to = "type", values_to = "values") %>% 
    mutate(across(type, factor, levels=c("x", "step"))) %>% 
    filter(!is.na(values)) %>% ggplot(aes(x = values)) +
    geom_histogram(bins = 30) +
    facet_wrap(vars(type), scales = "free", 
               labeller = as_labeller(facet_names)) +
    theme_classic() +
    labs(x = "Value", y = "Count") +
    theme(strip.text.x = element_text(size = 12))
