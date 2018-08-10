library(tidyverse)
library(readxl)
library(tidyr)


weather_raw <- read_csv("data/NCDC_CDO_USC00356750.csv")
weather_df <- weather_raw %>% 
  filter(STATION=="USC00356750") %>% 
  transmute(date=DATE, PRCP, TMIN=as.numeric(TMIN), TMAX=as.numeric(TMAX), SNOW)

print(weather_df)

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

bike_weather <- bikecounts %>% 
  mutate(date=as_date(date)) %>% 
  left_join(weather_df) %>% 
  print(bikecounts)

bikeweathermodel <- lm(total ~ PRCP + TMIN + TMAX, data = bike_weather) %>% 
  summary() %>% 
  print()


bikeweathermodel <- lm(total ~ PRCP, data = bike_weather, subset = name == "Hawthorne") %>% 
  summary() %>% 
  print()

bikeweathermodel <- lm(total ~ PRCP, data = bike_weather, subset = name == "Tilikum") %>% 
  summary() %>% 
  print()

bikeweathermodel <- lm(total ~ PRCP, data = bike_weather, subset = name == "Steel") %>% 
  summary() %>% 
  print()


library(gapminder)

bike_weather %>%
  group_by(name) %>%
  summarize_each(funs(min, max), total) %>% 
print()

(gap_nested <- bike_weather %>% 
    group_by(name) %>% 
    nest())

gap_nested[[1, "data"]]



library(purrr)
library(broom)
model_df_glance <- gap_nested %>% 
  mutate(fit=map(data, ~lm(total ~ TMIN+TMAX+PRCP, data=.)),
         glance=map(fit, glance)
         ) %>%
  unnest(glance) %>% 
  print()

model_df_tidy <- gap_nested %>% 
  mutate(fit=map(data, ~lm(total ~ TMIN+TMAX+PRCP, data=.)),
         tidy=map(fit, tidy)
  ) %>%
  unnest(tidy) %>% 
  print()

