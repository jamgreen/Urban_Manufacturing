#this is for gathering and processing the lehd data for my modeling study
#this involves gathering the data for the states my cities are in,
#aggregating to the block group level and adding some additional geographic ids
#in this case county and CBSA ids

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(tigris,purrr, RPostgreSQL,readr, stringr, sf, dplyr, dbplyr)
options(tigris_class = "sf", tigris_use_cache = TRUE)

devtools::install_github("jamgreen/lehdr")
library(lehdr)

study_states <- readRDS("./data/lehd_state_list.rds")

study_states <- study_states %>% filter( ! (State %in% c('MA', 'DC')))

states_distinct <- study_states %>%  distinct(State)

#download 2004 and 2015 to the data/lehd folder

years <- 2004:2015

big_lehd <- grab_lodes(state = states_distinct$State, year = years, 
                       lodes_type = "wac", job_type = "JT01", segment = "S000",
                       download_dir = "./data/lodes_raw")
 

#filter out all non-aggregate job variables, keep all jobs for entropy measures
big_lehd <- big_lehd %>% select(1:2, 9:28, 54:55)

#make a new county id from w_geocode

big_lehd <- big_lehd %>% mutate(CountyFIPS = str_sub(w_geocode, 1, 5))


#bring in county->msa crosswalk from NBER: http://www.nber.org/data/cbsa-msa-fips-ssa-county-crosswalk.html

cbsatocountycrosswalk <- read_csv("data/cbsatocountycrosswalk.csv", col_types = cols(cbsa = col_character(), 
                                  cbsaold = col_character()))

cbsatocountycrosswalk <- cbsatocountycrosswalk %>% select(1:5, 7:9)

#join both files to the crosswalk so we have MSA ids

big_lehd <- big_lehd %>% left_join(cbsatocountycrosswalk, 
                                   by = c("CountyFIPS" = "fipscounty"))


big_lehd <- big_lehd %>% mutate(bg_fips = str_sub(w_geocode, 1, 12)) %>% 
  group_by(bg_fips, cbsa, cbsaname, year) %>% summarise_if(is.numeric, funs(sum))



#keep all block groups within CBSAs and drop the rest

big_lehd <- big_lehd %>% filter(!is.na(cbsa))

big_lehd <- big_lehd %>% ungroup() %>% 
  rename_all(funs("tolower"))

#copy into industrial_land db for joining later


#logistic table processing from industrial_land db for MANUFACTURING JOBS ONLY------
host <- "pgsql.rc.pdx.edu"
user <- "jamgreen"
pw <- "&zSpR-rmd&v5REgZ"
dbname <- "industrial_land"

con <- dbConnect("PostgreSQL", host = host, user = user, dbname = dbname, password = pw)

big_lehd <- data.frame(big_lehd)

DBI::dbWriteTable(conn = con, "lehd_cbsa", big_lehd, overwrite = TRUE)

dbDisconnect(con)


rm(list = ls())
