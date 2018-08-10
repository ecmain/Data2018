library(tidyverse)
library(lubridate)
library(magrittr)


# define a funtion that load bike counts data
travel_behavior <- read_csv("data/NHTS2009_dd.csv")

travel_behavior%<>% mutate(driving=ifelse(TRPTRANS %in% c("01", "02", "03", "04", "05", "06", "07"), 1, 0),
          driving=ifelse(TRPTRANS %in% c("-1", "-7", "-8", "-9"), NA, driving))    # retain missing values as NA %>% 
  
filter(travel_behavior, driving == 1) %>% 
  print()
  group_by(HOUseID) %>% 
  print()
  summarize(total_vmt=sum(TRPMILES))
