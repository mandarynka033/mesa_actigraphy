# Libraries
library(tidyverse)

# setwd("/home/janique/mesa/actigraphy")

files <- list.files()

# PSG and actigraphy overlap dataset
overlap <- read_csv("/home/janique/mesa/overlap/mesa-actigraphy-psg-overlap.csv",
                    col_types = cols_only(
                        mesaid = col_integer(),
                        line = col_integer()
                    )) 

# Only participants with available PSG
indices <- str_extract(files, "[\\d]+") %>% as.numeric()
files_psg <- files[indices %in% overlap$mesaid]

# Empty vector for zero proportion
zero_prop <- rep(NA, length(files_psg))

# Importing actigraphy datasets
for (i in 1:length(files_psg)){
    dt <- read_csv(files_psg[i],
             col_types = cols_only(
                 mesaid = col_integer(),
                 offwrist = col_integer(),
                 activity = col_double(),
                 daybymidnight = col_integer(),
             )) %>% 
        filter(offwrist == 0 & !is.na(activity) & daybymidnight < 8) %>% 
        select(-offwrist, -daybymidnight)
    
    # Calculating zero proportion
    mean_zero_prop <- round(sum(dt$activity == 0) / nrow(dt), digits = 3)
    zero_prop[i] <- mean_zero_prop
}

# save(zero_prop, file = "/home/janique/mesa/R/zero_proportion_vector.RData")

print(mean(zero_prop, na.rm = TRUE))
print(paste("Number of participants:", length(zero_prop)))

# Result -> 0.403 of all data is 0


