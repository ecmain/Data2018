## Install the tidycensus package if you haven't yet
#install.packages("tidycensus")

library(tidycensus)
library(ggplot2)
library(dplyr)

## setup cenus api key
## signup your census api key at http://api.census.gov/data/key_signup.html
census_api_key("c149605c8b02f5293ad937c4163ce152af10da53", install=TRUE) # 
portland_tract_medhhinc <- get_acs(geography = "tract", 
                                   year = 2016, # 2012-2016
                                   variables = "B19013_001",  # Median Household Income in the Past 12 Months
                                   state = "OR",
                                   geometry = TRUE) # load geometry/gis info

ggplot(portland_tract_medhhinc) + 
  geom_sf(aes(fill = estimate)) +
  coord_sf(datum = NA) + theme_minimal()

ggsave("output/mymap")

## Install the mapview package if you haven't yet
#install.packages("mapview")
library(sf)


library(mapview)
library(dplyr)

mapview(portland_tract_medhhinc %>% select(estimate), 
        col.regions = sf.colors(10), alpha = 0.1)

library(sf)
library(readr)
# read 1994 Metro TAZ shape file
taz_sf <- st_read("data/taz1260.shp", crs=2913)