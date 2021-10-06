# Loading all participants with available PSG
load("/home/janique/mesa/R/subjects_psg.RData")
all_participants_nbs <- as.numeric(str_extract(files, "[\\d]+"))

# Library import
library(tidyverse)
set.seed(1337)

# Randomly selecting a single participant from every age
chosen_ids <- rep(NA, 50)
for (i in 54:94){
    chosen_id <- person %>% select(mesaid, sleepage5c) %>%
        filter(sleepage5c == i) %>%
        filter(mesaid %in% all_participants_nbs) %>%
        select(mesaid) %>%
        sample_n(1) %>% as.numeric()
    chosen_ids[i - 53] <- chosen_id
}

# Adding 9 participants randomly
chosen_ids[42:50] <- person %>% select(mesaid) %>%
    filter(mesaid %in% all_participants_nbs, !(mesaid %in% chosen_ids)) %>%
    sample_n(9) %>% pull(mesaid)

# save(chosen_ids, file = "/home/janique/mesa/R/chosen_people.RData")
# load("/home/janique/mesa/R/chosen_people.RData")

# Importing dataset with the sleep questionnaire
person1 <- read_csv("/home/janique/mesa/datasets/mesa-sleep-dataset-0.4.0.csv")

# Extracting all relevant statistics
person <- person1 %>% filter(mesaid %in% all_participants_nbs)
rel_person <- person %>%
    select(mesaid, sleepage5c, gender1, trbleslpng5, wakeup5, overallqual5) %>%
    filter(mesaid %in% chosen_ids)

# Histogram of age
rel_person %>% ggplot() + geom_histogram(aes(x = sleepage5c), bins = 10)
