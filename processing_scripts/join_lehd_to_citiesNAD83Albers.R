if(!require(pacman)){installed.packages("pacman"); library(pacman)}
p_load(sf, tigris, dplyr)
options(tigris_class = "sf", tigris_use_cache = TRUE)

#import the national 2010 block group file and the fifty places boundaries
#run a spatial join on the two in order to get the block groups for the cities
#join to the lehd cbsa file to get lehd data matched then upload to postGIS

blk_grp <- st_read(unzip("data/spatial/NAD83/nhgis0012_shapefile_tl2010_us_blck_grp_2010.zip")) 
file.remove(list.files(pattern = "US_blck*"), recursive = FALSE)

blk_grp <- blk_grp %>% select(1:5, 13)

places <- st_read("data/spatial/NAD83/places_nad83.shp")
places <- places %>% select(1:6)

blk_grp <- st_transform(blk_grp, 5070)
places <- st_transform(places, 5070)

place_blk_grp <- blk_grp %>% st_join(places, left = FALSE)
place_blk_grp <- place_blk_grp %>% select(-7, -8)

#bring in lehd and join to place block groups

lehd <- readRDS("data/spatial/lehd_cbsa_sf.RDS")
lehd <- lehd %>% select(-26:-36)
lehd <- lehd %>% as.data.frame() %>% select(-geometry)

place_blk_grp <- place_blk_grp %>% 
  left_join(lehd, by = c("GEOID10" = "BG_FIPS")) %>% 
  st_as_sf()

place_blk_grp <- place_blk_grp %>% select(-1:-4, -10)

st_write(place_blk_grp,"data/spatial/NAD83/lehd_cities_NAD83Albers.shp", delete_dsn = TRUE)

#also fix the city center spelling for joining
#this is for reference

cbd <- st_read("data/spatial/cbd_NAD83Albers.shp")
cbd$City <- as.character(cbd$City)
cbd$City <- ifelse(cbd$City == "Nashville-Davidson", 
                   "Nashville-Davidson metropolitan government (balance)",
                   as.character(cbd$City))

cbd$City <- ifelse(cbd$City == "Indianapolis",
                   "Indianapolis city (balance)",
                   as.character(cbd$City))

cbd$City <- ifelse(cbd$City == "Lousiville-Jefferson",
                   "Louisville",
                   as.character(cbd$City))

cbd <- st_as_sf(cbd)

st_write(cbd, "data/spatial/cbd_NAD83Albers.shp", delete_dsn = TRUE)

rm(list = ls())

