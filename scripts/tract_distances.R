if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(ggplot2, purrr,dplyr, RQGIS,sf)
set_env()

#need to calculate distances from CBDs to census tract centroids using SF
#and then trying through RQGIS using the distance matrix tool

lehd04 <- readRDS("lodes_data/lodes_2004/lehd_2004_places.rds")
cbd <- st_read("data/cbdnad83.shp")

cbd <- st_join(cbd, lehd04, join = st_within)
cbd <- select(cbd, -4:-6,-8:-16)

#reproject to albers equal conic HARN EPSG: 5071

lehd_albers <- st_transform(lehd04, crs = 5071)
cbd_albers <- st_transform(cbd, crs = 5071)
lehd_centroid <- st_centroid(lehd_albers)

#first try using st_distance in sf
#lehd_dist <- st_distance(cbd_albers, lehd_albers)
#this gave a dense matrix i don't know what to do with. trying RQGIS

#in order to try and create a more parsimonious distance matrix
#i will create a nested dataframe based on city and run the 
#distance matrix function on that instead of all census tracts

#lehd_nested <- lehd_centroid %>% nest_.sf(GEOID10, NAME)

#nesting seems not to be working. going to try split()

find_algorithms(search_term = "(distance)(matrix)")
get_usage(alg = "qgis:distancematrix")

lehd_split <- split(lehd_centroid, lehd_centroid$NAME)

 lehd_split %>%
  map(~run_qgis(alg = "qgis:distancematrix",
                INPUT_LAYER = cbd_albers,
                INPUT_FIELD = "NAME",
                TARGET_LAYER = .,
                TARGET_FIELD = "GEOID10",
                MATRIX_TYPE = 0,
                DISTANCE_MATRIX = "data/matrix.csv"))


