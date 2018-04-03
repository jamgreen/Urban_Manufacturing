#download census block association files

#part 1 is getting the urls and downloading---------------------

if(!require(pacman)){installed.packages("pacman"); library(pacman)}
p_load(purrr, glue, RPostgreSQL, tidycensus, tigris, sf, readr, dplyr)
options(tigris_class = "sf", tigris_use_cache = TRUE)

state_fips <- tidycensus::fips_codes
state_fips <- state_fips %>% 
  group_by(state, state_code, state_name) %>% summarise()

state_fips <- state_fips %>% filter(state_code < "59")



urls_build <- function(state_id, state_abb) {
  
  urls <- glue("https://www2.census.gov/geo/docs/maps-data/data/baf/BlockAssign_ST{state_id}_{state_abb}.zip")
  
  return(urls)
  
}

baf_urls <- map2_chr(state_fips$state_code, state_fips$state, urls_build)

write.table(baf_urls, file = "data/block_association.txt", col.names = FALSE, 
            row.names = FALSE, sep = "\t", quote = FALSE)

#run wget on the block_association.txt file
# wget -i block_association.txt


#part 2, moving zip files to new directory and unzipping-------------
#after unzipping we'll read in the incorporated place files 
#filter out non-place blocks, grab places and join it in
#then erase all the text files leaving just the zips to save space


#create new directory and move zip files to be unzipped and then read in the 
#incplace files
#mkdir block_assigment_zips
#mv BlockAssign_ST* block_assignment_zips/
#cd block_assignment_zips/
#unzip '*.zip'

place_txt <- list.files(path = "data/block_assignment_zips", 
                        pattern = "*CDP.txt", full.names = TRUE)

block_assign_place <- map_df(place_txt, read_csv, 
                             cols(.default = col_character()),
                             col_names = TRUE)

block_assign_place <- block_assign_place %>% filter(!is.na(PLACEFP))

block_assign_place <- block_assign_place %>% 
  mutate(STATEFIPS = stringr::str_sub(block_assign_place$BLOCKID, 1, 2)) %>% 
  left_join(state_fips, by = c("STATEFIPS" = "state_code"))

block_assign_place <- block_assign_place %>% 
  mutate(BLKGRPFIPS = stringr::str_sub(BLOCKID, 1, 12))

#part 3, get places, join to block_assignment for place names------ 

state_list <- state_fips$state

places_sf <- map_df(state_list, places)
places_df <- places_sf %>% data.frame() %>% select(-geometry)
places_df <- places_df %>% select(PLACEFP, GEOID, NAME, NAMELSAD)

block_assign_place <- block_assign_place %>% 
  left_join(places_df, by = c("PLACEFP" = "PLACEFP"))
#part 4, then grab pop data for 2012, get top 50------

#tot_pop <- "B01003"

# place_by_pop <- get_acs(geography = "place", table = tot_pop, 
#                         survey = "acs5", year = 2012, output = "wide",
#                         geometry = FALSE)

place_by_pop <- readr::read_csv("data/placepop_ACS2012.csv")

place_by_pop <- place_by_pop %>% 
  mutate(STATEFIPS = stringr::str_sub(GEOID,1, 2)) %>% 
  filter(STATEFIPS < "59")



place_by_pop <- place_by_pop %>% arrange(desc(B01003_001E)) %>% 
  slice(1:51)

block_place_pop <- place_by_pop %>% 
  inner_join(block_assign_place, 
             by = c("GEOID" = "GEOID", "STATEFIPS" = "STATEFIPS"))

block_place_pop <- block_place_pop %>% 
  mutate(blk_grp_fips = stringr::str_sub(BLOCKID, 1, 12)) 

block_place_pop <- block_place_pop %>% 
  select(GEOID, FULLNAME = NAME.x, place_pop2012 = B01003_001E, STATEFIPS, BLOCKID, PLACEFP, NAMELSAD, blk_grp_fips)

block_place_pop <- block_place_pop %>% 
  group_by(blk_grp_fips, GEOID, place_pop2012, PLACEFP, FULLNAME, NAMELSAD) %>% 
  summarise()

block_place_pop <- block_place_pop %>% data.frame() %>% 
  ungroup() %>% rename_all(funs(tolower))

#replace the block group place table in db: industrial_land------

host <- "pgsql.rc.pdx.edu"
user <- "jamgreen"
pw <- scan("batteries.pgpss", what = "")
dbname <- "industrial_land"

con <- dbConnect("PostgreSQL", host = host, user = user, dbname = dbname, password = pw)

dbSendQuery(conn = con, statement = "DROP TABLE IF EXISTS blkgrp_place_xwalk;")

copy_to(dest = con, block_place_pop, name = "blkgrp_place_xwalk",
        indexes = list("blk_grp_fips", "geoid", "namelsad"), overwrite = TRUE,
        temporary = FALSE)

dbDisconnect(conn = con)

rm(list = ls())
