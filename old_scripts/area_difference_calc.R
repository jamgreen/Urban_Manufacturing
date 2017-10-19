#calculate area differences between 1990 and 2012 place boundaries
#note ALAND variable in the 2012 file is in square meters

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(tidyverse, sf)

places_2012 <- read_rds("data/census_shapes/largest_places_acs2012_sf.rds")
places1990 <- read_rds("data/census_tables/place_density/largest_places_1990.RDS")

places_2012 <- inner_join(places_2012, places1990, by = c("place" = "PLACE_2012", "state" = "ST"), 
                          suffix = c("_2012", "_1990")) 

places_2012 <- places_2012 %>% mutate(SQ_KM2012 = ALAND/1000000, AREA_CHANGE_KM = (SQ_KM2012 - SQ_KM)/SQ_KM)
