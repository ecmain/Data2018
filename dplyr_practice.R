library(tidyverse)
library(magrittr)
library(gapminder)

source("load_data.R")

print(bikecounts)

filter(bikecounts, is.na(westbound)) %>% 
  tally()

filter(bikecounts, is.na(westbound|eastbound)) %>% 
  tally()

mutate(bikecounts, year=year(date)) %>% 
  group_by(year, name) %>% 
  summarise(sum(total))

mutate(bikecounts, month=month(date)) %>% 
  group_by(month, name) %>% 
  summarise(sum(total))

mutate(bikecounts, week=week(date)) %>% 
  group_by(week, name) %>% 
  summarise(sum(total))
  

