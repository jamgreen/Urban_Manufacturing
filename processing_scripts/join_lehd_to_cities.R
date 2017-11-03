#get fifty largest cities and block groups and
#make first map looking at mfg and industrial stuff

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(purrr, tigris, sf, dplyr)
options(tigris_class = "sf", tigris_use_cache = TRUE)



cities_50 <- st_read("./data/spatial/places_nad83.shp")

cities_50 <- cities_50 %>% select(3:5)


lehd_sf <- readRDS("./data/spatial/lehd_cbsa_sf.RDS")

lehd_cities <- st_join(lehd_sf, cities_50, join = st_intersects, left = FALSE)
lehd_cities <- lehd_cities %>% select(-c(25:33))

saveRDS(lehd_cities, "./data/spatial/lehd_citiesNAD83.rds")
st_write(lehd_cities, "./data/spatial/lehd_citiesNAD83.geojson")

rm(list = ls())

