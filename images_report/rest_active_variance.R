# Libraries
library(tidyverse)

# Function for obtaining mode
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

# setwd("/home/janique/mesa/actigraphy")

# All actigraphy files
files <- list.files()

# Dataset for PSG overlap
overlap <- read_csv("/home/janique/mesa/overlap/mesa-actigraphy-psg-overlap.csv",
                    col_types = cols_only(
                        mesaid = col_integer(),
                        line = col_integer()
                    )) 

indices <- str_extract(files, "[\\d]+") %>% as.numeric()
files_psg <- files[indices %in% overlap$mesaid]

# Empty vectors for results
rests_vec_full <- integer(0)
active_vec_full <- integer(0)

# Importing actigraphy files
for (i in 1:length(files_psg)){
    dt <- read_csv(files_psg[i],
             col_types = cols_only(
                 mesaid = col_integer(),
                 offwrist = col_integer(),
                 activity = col_double(),
                 daybymidnight = col_integer(),
                 interval = col_character()
                 )) %>% 
        filter(offwrist == 0 & !is.na(activity) & daybymidnight < 8) %>% 
        select(-offwrist, -daybymidnight, -mesaid) %>% 
        mutate(
            # Combining rest and rest-s
            rests = if_else(interval == "ACTIVE", "ACTIVE", "REST")
        ) %>% 
        select(-interval) 
    
    vec_rest <- dt %>% filter(rests == "REST") %>% pull(activity)
    vec_active <- dt %>% filter(rests == "ACTIVE") %>% pull(activity)
    
    rests_vec_full <- c(rests_vec_full, vec_rest)
    active_vec_full <- c(active_vec_full, vec_active)
   
}

# Statistics for activity for rest and active
summary(rests_vec_full)
summary(active_vec_full)

# save(rests_vec_full, file = "/home/janique/mesa/R/vector_rest.RData")
# save(active_vec_full, file = "/home/janique/mesa/R/vector_active.RData")


