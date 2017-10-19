#download and join the lehd data to spatial tract data

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

#filter out total and industrial jobs 

lehd04 <- lehd04 %>% select(1, 10:14, 16)
lehd15 <- lehd15 %>% select(1, 10:14, 16)

#make a new county id from w_geocode

lehd04 <- lehd04 %>% mutate(CountyFIPS = str_sub(w_geocode, 1, 5))
lehd15 <- lehd15 %>% mutate(CountyFIPS = str_sub(w_geocode, 1, 5))

#bring in county->msa crosswalk from NBER: http://www.nber.org/data/cbsa-msa-fips-ssa-county-crosswalk.html

msa_crosswalk <- readr::read_csv("./data/cbsatocountycrosswalk.csv", col_types = cols(.default = "c"))
