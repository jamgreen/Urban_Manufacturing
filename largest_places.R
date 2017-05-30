#get fifty largest cities and census tracts and
#make first map looking at mfg and industrial stuff

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(purrr, tigris, sf, dplyr)
options(tigris_class = "sf")

cbd_sf <- st_read("cbdnad83.shp") 


ref_state <- unique(cbd_sf$State)

cities_sf <- map(ref_state, places)
cities_sf <- rbind_tigris(cities_sf)

cities_50 <- cities_sf[cbd_sf,]

st_write(cities_50, "data/places_nad83.shp")

rm(list = c("cbd_sf", "cities_sf"))

cities_50 <- cities_50 %>% select(3:5)


lehd_2004 <- readRDS("lodes_data/lodes_2004/lehd_2004_sf.rds")

lehd_2004_cities <- st_join(lehd_2004, cities_50, join = st_within, left = FALSE)

lehd_2014 <- readRDS("lodes_data/lodes_2014/lehd_2014_sf.rds")

lehd_2014_cities <- st_join(lehd_2014, cities_50, join = st_within, left = FALSE)

saveRDS(lehd_2004_cities, "lodes_data/lodes_2004/lehd_2004_places.rds")
saveRDS(lehd_2014_cities, "lodes_data/lodes_2014/lehd_2014_places.rds")

