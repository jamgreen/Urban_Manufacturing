#get block groups for the states in the model

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(RPostgreSQL, tigris, here,dplyr, sf)
options(tigris_class = "sf")

host <- "pgsql102.rc.pdx.edu"
user <- "jamgreen"
pw <- scan(here("batteries.pgpss"), what = "")
dbname <- "industrial_land"


con <- dbConnect("PostgreSQL", host = host, user = user, dbname = dbname, 
                 password = pw)

city_lehd <- st_read_db(con, "lehd_places")
city_lehd <- city_lehd %>% filter(fullname != "Bakersfield city, California")



state_fips <- city_lehd %>% select(bg_fips) %>% 
  mutate(state_fips = stringr::str_sub(bg_fips, 1, 2)) %>% 
  group_by(state_fips) %>%
  summarise() %>% as.data.frame() %>% 
  select(-geom)


fips <- fips_codes
fips <- fips_codes %>% group_by(state_code, state_name) %>% 
  summarise()

state_fips <- state_fips %>% 
  left_join(fips, by = c("state_fips" = "state_code")) 


state_blk_grps <- lapply(state_fips$state_fips, block_groups, year = 2010)

state_blk_grps_test <- rbind_tigris(state_blk_grps)

state_blk_grps_test <- state_blk_grps_test %>% rename(geom = geometry)

names(state_blk_grps_test) <- tolower(names(state_blk_grps_test))

# st_write(state_blk_grps_test, here("/data/spatial/blk_grp_shapes.shp"), 
#          delete_dsn = TRUE)

st_write(dsn = con, obj = state_blk_grps_test, geom_name = "geom", 
            table = "blk_grp_shapes", drop = TRUE, binary = TRUE)

dbDisconnect(con)
