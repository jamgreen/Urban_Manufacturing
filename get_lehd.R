if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(tigris,purrr, dplyr)

us_states <- tolower(unique(fips_codes$state)[1:51])

year <- c(2004, 2014)

get_lehd <- function(states, year) {
  #grabbing all private jobs WAC
  lehd_url <- paste0("https://lehd.ces.census.gov/data/lodes/LODES7/", states,"/wac/", states,"_wac_S000_JT02_",year,".csv.gz")
  filenames <- paste0(states,"_", year,".csv.gz")
  download.file(lehd_url, dest = filenames)
}

possible_get_lehd <- possibly(get_lehd, otherwise = NA)
#setwd("D:/Dissertation/UrbMfg/Urban_Manufacturing/lodes_data")
map(us_states, possible_get_lehd,year = 2004)
map(us_states, possible_get_lehd,year = 2014)
