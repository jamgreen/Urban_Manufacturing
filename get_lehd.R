#download and join the lehd data to spatial tract data

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(tigris,purrr, sf, dplyr)
options(tigris_class = "sf", tigris_use_cache = TRUE)

devtools::install_github("jamgreen/lehdr")
library(lehdr)

study_states <- readRDS("./data/lehd_state_list.rds")

#download 2004 and 2015 to the data/lehd folder

lehd04 <- map_df(study_states, grab_lodes,year = 2004, lodes_type = "wac", job_type = "JT01", segment = "S000", 
       download_dir = "./data/lehd")

lehd15 <- map_df(study_states, grab_lodes,year = 2015, lodes_type = "wac", job_type = "JT01", segment = "S000", 
        download_dir = "./data/lehd")
