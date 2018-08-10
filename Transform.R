library(tidyverse)
library(lubridate)
library(readxl)
library(magrittr)

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
Steele <- load_data(input_file, "Steel")

#bikecounts <- bikecounts %>% 
#  gather(westbound, eastbound, key="direction", value="counts")

#bikecounts %>% 
#  gather(westbound, eastbound, key="direction", value="counts") -> bikecounts

weather_raw <- read_csv("data/NCDC_CDO_USC00356750.csv")
weather_df <- weather_raw %>% 
  filter(STATION=="USC00356750") %>% 
  transmute(date=DATE, PRCP, TMIN=as.numeric(TMIN), TMAX=as.numeric(TMAX), SNOW)

bike_weather <- bikecounts %>% 
  mutate(date=as_date(date)) %>% 
  left_join(weather_df) %>% 
  print(bikecounts)

bikecounts %<>% 
  mutate(week=floor_date(date, "week"),
         month=floor_date(date, "month"),
         year=year(date))

#filter any days w/ missing values as they throw off the plot
bikecounts <- bikecounts %>%
  filter(complete.cases(eastbound, westbound, total))

## generate summaries for plotting
bikecounts_week <- bikecounts %>% 
  group_by(name, week) %>% 
  summarize(total=sum(total))


bikecounts_month <- bikecounts %>% 
  group_by(name, month) %>% 
  summarize(total=sum(total))
bikecounts_month

bikecounts_year <- bikecounts %>% 
  group_by(name, year) %>% 
  summarize(total=sum(total))
  bikecounts_year
  
  

ggplot(bikecounts) +
  geom_line(aes(x=date, y=total, color=name)) +
  scale_y_log10()

bike_week <- ggplot(bikecounts_week) +
  geom_line(aes(x=week, y=total, color=name)) +
  scale_y_log10()

weather_all <- ggplot(bike_weather) +
  geom_line(aes(x=date, y=PRCP)) +
  labs(y="Daily Percipitation")+
  scale_y_log10()

bike_month <- ggplot(bikecounts_month) +
  geom_line(aes(x=month, y=total, color=name)) +
  scale_y_log10()


ggplot(bikecounts_year) +
  geom_line(aes(x=year, y=total, color=name)) +
  scale_y_log10()

ggplot(bike_weather) +
  geom_line(aes(x=date, y=PRCP)) +
  labs(y="Daily Percipitation")

ggplot(bike_weather) +
  geom_line(aes(x=date, y=TMAX)) +
  labs(y="Daily Max Temperature")

ggplot(bikecounts_month) + 
  geom_smooth(mapping = aes(x = month, y = total))

# Example of decompose timeseries day
#install.packages("ggfortify")

library(ggfortify)
prcp_ts <- ts(bike_weather$PRCP, start=2011, frequency = 365)
autoplot(stl(prcp_ts, s.window = 'periodic'), ts.colour = 'blue')

tmax_ts <- ts(weather_df$TMAX, start=2011, frequency = 365)
autoplot(stl(tmax_ts, s.window = 'periodic'), ts.colour = 'blue')