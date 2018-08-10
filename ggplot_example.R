library(readxl)
library(lubridate)
library(tidyverse)
library(ggplot2)

input_file <- "data/Hawthorne Tilikum Steel daily bike counts 073118.xlsx"
bridge_name <- "Hawthorne"

# define a funtion that load bike counts data
load_data <- function(input_file, bridge_name) {
  bikecounts <- read_excel(input_file,
                           sheet = bridge_name,
                           skip = 1)
  bikecounts$name <- bridge_name
  bikecounts
}

Tilikum <- load_data(input_file, "Tilikum")
Hawthorne <- load_data(input_file, "Hawthorne")

# use the column names of Tilikum for Hawthorne
names(Hawthorne) <- names(Tilikum)

Steel <- load_data(input_file, "Steel")
names(Steel) <- c("date", "lower", "westbound", "eastbound", "total", "name")

# combine all three data frame for all three bridges
bikecounts <- bind_rows(Hawthorne, 
                        Tilikum, 
                        Steel %>% select(-lower)) # exclude the `lower` col in Steel data frame

# average daily bike counts by bridge
#bikecounts %>% 
#  group_by(name) %>% 
#  summarize(avg_daily_counts=mean(total, na.rm=TRUE))

# average monthly bike counts by bridge
bikecounts_month <- bikecounts %>% 
  # first create ym column as a unique month identifier
  group_by(name, ym=floor_date(date, "month")) %>%
  summarize(total_monthly_counts=sum(total), counts=n()) %>% 
  # then average by month over years for each bridge
  group_by(name, month(ym)) %>% 
  summarize(avg_monthly_counts=mean(total_monthly_counts)) 
 
Hawthorne_month <- Hawthorne %>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(name, month) %>%
  summarise(totalmonth = sum(total))

ggplot(data = Hawthorne) +
  geom_point(mapping = aes(x = date, y = total))

ggplot(data = bikecounts) +
  geom_point(mapping = aes(x = date, y = total), color = "blue") +
  facet_wrap(~ name, nrow = 1)

ggplot(data = Hawthorne) +
  geom_smooth(mapping = aes(x = date, y = total))

ggplot(data = bikecounts) +
  geom_smooth(mapping = aes(x = date, y = total, color = name))

ggplot(data = Hawthorne) +
  geom_point(mapping = aes(x = date, y = total)) +
  geom_smooth(mapping = aes(x = date, y = total))

ggplot(data = Hawthorne) +
  geom_point(mapping = aes(x = date, y = total), position = "jitter")

ggplot(data = bikecounts_month, mapping = aes(x = name, y = avg_monthly_counts)) + 
  geom_boxplot()

ggplot(data = Hawthorne_month) +
  geom_bar(mapping = aes(x = month, y = totalmonth), stat = "identity")

bikecounts_wide <- bikecounts %>% 
  select(date, name, total) %>% 
  mutate(date= ymd(date)) %>% 
  spread(name, total)