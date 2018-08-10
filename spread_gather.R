library(tidyverse)
library(magrittr)

source("load_data.R")

print(bikecounts)

bikecounts <- bikecounts %>% mutate(dow=weekdays(date))

gather(bikecounts, westbound, eastbound, key = "direction", value = "counts")

print(bikecounts)

bikecounts_days<- spread(bikecounts, key = "dow", value="westbound")

print(bikecounts_days)

#bikecounts %>% 
#  group_by(name, dow)%>% 
#  summarise(average_daily_counts = mean(total, na.rm=TRUE))



  
  

