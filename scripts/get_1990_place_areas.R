#land area for places 1990
if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(purrr, tigris,tidyverse, sf)
options(tigris_class = "sf")

large_places2012 <- readRDS("data/census_shapes/largest_places_acs2012_sf.rds")

fipstab <- fips_codes

large_places2012 <- left_join(large_places2012, fips_codes, by = c("state" = "state_code"))
large_places2012 <- large_places2012 %>% select(1:12) %>% distinct()

large_place_states <- unique(large_places2012$state.y)

name_list <- paste0(large_places2012$state, tolower(large_places2012$state.y))
name_list <- unique(name_list)

urls <- paste0("https://www.census.gov/population/www/censusdata/files/places/", name_list,".txt")
filename <- paste0(name_list,"_1990_density.txt")

setwd("data/census_tables/place_density")

get_density <- function(df){
  
  urls <- paste0("https://www.census.gov/population/www/censusdata/files/places/", df,".txt")
  filename <- paste0(df,"_1990_density.txt")
  
  download.file(urls, dest = filename)
  
}

map(name_list, get_density)
