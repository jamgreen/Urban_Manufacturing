#this is for gathering and processing the lehd data for my modeling study
#this involves gathering the data for the states my cities are in,
#aggregating to the block group level and adding some additional geographic ids
#in this case county and CBSA ids

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(tigris,purrr, stringr, sf, dplyr)
options(tigris_class = "sf", tigris_use_cache = TRUE)

devtools::install_github("jamgreen/lehdr")
library(lehdr)

study_states <- readRDS("./data/lehd_state_list.rds")

study_states <- study_states %>% filter( ! (State %in% c('MA', 'DC')))

states_distinct <- study_states %>%  distinct(State)

#download 2004 and 2015 to the data/lehd folder

lehd04 <- map_df(states_distinct$State, grab_lodes,year = 2004, lodes_type = "wac", job_type = "JT01", segment = "S000", 
       download_dir = "./data/lehd")

lehd15 <- map_df(states_distinct$State, grab_lodes,year = 2015, lodes_type = "wac", job_type = "JT01", segment = "S000", 
        download_dir = "./data/lehd")

#filter out all non-aggregate job variables, keep all jobs for entropy measures

lehd04 <- lehd04 %>% select(1, 9:28)
lehd15 <- lehd15 %>% select(1, 9:28)

#make a new county id from w_geocode

lehd04 <- lehd04 %>% mutate(CountyFIPS = str_sub(w_geocode, 1, 5))
lehd15 <- lehd15 %>% mutate(CountyFIPS = str_sub(w_geocode, 1, 5))

#bring in county->msa crosswalk from NBER: http://www.nber.org/data/cbsa-msa-fips-ssa-county-crosswalk.html

cbsatocountycrosswalk <- read_csv("data/cbsatocountycrosswalk.csv", col_types = cols(cbsa = col_character(), 
                                  cbsaold = col_character()))

cbsatocountycrosswalk <- cbsatocountycrosswalk %>% select(1:5, 7:9)

#join both files to the crosswalk so we have MSA ids

lehd04 <- lehd04 %>% left_join(cbsatocountycrosswalk, by = c("CountyFIPS" = "fipscounty"))
lehd15 <- lehd15 %>% left_join(cbsatocountycrosswalk, by = c("CountyFIPS" = "fipscounty"))

lehd04_bg <- lehd04 %>% mutate(BG_FIPS = str_sub(w_geocode, 1, 12)) %>% 
  group_by(BG_FIPS, cbsa, cbsaname) %>% summarise_if(is.numeric, funs(sum))

lehd15_bg <- lehd15 %>% mutate(BG_FIPS = str_sub(w_geocode, 1, 12)) %>% 
  group_by(BG_FIPS, cbsa, cbsaname) %>% summarise_if(is.numeric, funs(sum))

#keep all block groups within CBSAs and drop the rest

lehd04_bg <- lehd04_bg %>% filter(!is.na(cbsa))
lehd15_bg <- lehd15_bg %>% filter(!is.na(cbsa))

#join to block group shapefile for now

bg_sf <- lapply(states_distinct$State, function(x) block_groups(state = x))
bg_sf <- rbind_tigris(bg_sf)

lehd04_bg <- lehd04_bg %>% left_join(bg_sf, by = c("BG_FIPS" = "GEOID"))
lehd04_bg <- lehd04_bg %>% ungroup() %>% st_as_sf()

lehd15_bg <- lehd15_bg %>%  left_join(bg_sf, by = c("BG_FIPS" = "GEOID"))
lehd15_bg <- lehd15_bg %>% ungroup() %>% st_as_sf()

saveRDS(lehd04_bg, file = "data/spatial/lehd_2004_sf.RDS")
saveRDS(lehd15_bg, file = "data/spatial/lehd_2015_sf.RDS")
