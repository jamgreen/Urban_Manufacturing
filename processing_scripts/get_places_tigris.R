#download the fifty largest places form tigris

#get fifty largest cities and census tracts and
#make first map looking at mfg and industrial stuff

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(purrr, tigris, sf, dplyr)
options(tigris_class = "sf", tigris_use_cache = TRUE)

#had to reproject using GDAL
cbd_sf <- st_read("data/spatial/cbd_NAD83.shp") 


ref_state <- unique(cbd_sf$State)

cities_sf <- map(ref_state, places)
cities_sf <- rbind_tigris(cities_sf)

cities_50 <- cities_sf[cbd_sf,]

st_write(cities_50, "./data/spatial/places_nad83.shp")

rm(list = c("cbd_sf", "cities_sf", "ref_state"))