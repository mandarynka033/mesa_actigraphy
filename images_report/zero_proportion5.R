# Libraries
library(tidyverse)
library(lubridate)

# setwd("/home/janique/mesa/actigraphy")

files <- list.files()

# PSG overlap dataset
overlap <- read_csv("/home/janique/mesa/overlap/mesa-actigraphy-psg-overlap.csv",
                    col_types = cols_only(
                        mesaid = col_integer(),
                        line = col_integer()
                    )) 

# Participants with available PSG
indices <- str_extract(files, "[\\d]+") %>% as.numeric()
files_psg <- files[indices %in% overlap$mesaid]

# Empty vector for the proportion of zeros
zero_prop <- rep(NA, length(files_psg))

# Importing actigraphy files
for (i in 1:length(files_psg)){
    dt <- read_csv(files_psg[i],
             col_types = cols_only(
                 mesaid = col_integer(),
                 offwrist = col_integer(),
                 activity = col_double(),
                 daybymidnight = col_integer(),
                 linetime = col_character()
             )) %>% 
        filter(offwrist == 0 & !is.na(activity) & daybymidnight < 8) %>% 
        select(-offwrist) %>% 
        mutate(
            date_var = ymd("1960-01-01") + days(daybymidnight),
            dt = ymd_hms(paste(date_var, linetime)),
            dt5 = floor_date(dt, unit = "5 minutes")
        ) %>% 
        select(mesaid, activity, dt5) %>% 
        group_by(mesaid, dt5) %>% 
        # Compute 5-minute averaged actigraphy
        summarise(act5_mean = mean(activity), .groups = "drop")
    
    # Calculate proportion of zeros
    mean_zero_prop <- round(sum(dt$act5_mean == 0) / nrow(dt), digits = 3)
    zero_prop[i] <- mean_zero_prop
}

# save(zero_prop, file = "/home/janique/mesa/R/zero_proportion_averaged.RData")

print(mean(zero_prop, na.rm = TRUE))
print(paste("Number of participants:", length(zero_prop)))

# Result -> 17% of all averaged data is 0


