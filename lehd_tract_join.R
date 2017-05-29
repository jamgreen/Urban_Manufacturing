#lodes processing
#pull out industrial employment categories along with total employment
#create a new census tract ID, then join to tract file from tigris
#run both for 2004 and 2014



if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(purrr, tigris, sf, readr, stringr, dplyr)
options(tigris_class = "sf")

#setwd("lodes_data/lodes_2004")
setwd("lodes_data/lodes_2014")

#make a list of the gz files and then read them in using read_csv 
#read_csv automatically decompresses so it saves a step

temp <- list.files()

process_wac <- function(dframe) {
  lehd <- read_csv(dframe, col_types = cols(w_geocode = col_character()))
  
  lehd <- lehd %>% mutate(tract_id = str_sub(w_geocode,1,11)) %>%
    select(tract_id, tot_emp = C000, ag_emp = CNS01, mining_emp = CNS02,
           utilities_emp = CNS03, construct_emp = CNS04, mfg_emp = CNS05, wholetrade_emp = CNS06,
           transpo_emp = CNS08)
  
  lehd <- lehd %>% group_by(tract_id) %>% summarise_all(funs(sum)) 
  lehd
}

#map_df will automataically bind the list into one large dataframe
#this is awesome for lehd type data where columns and the like are standardized
lehd <- map_df(temp, process_wac, .id = "state")

#get national 2010 census tract sf
#took state names by grabbing the first two characters from the .gz filenames


state_list <- str_sub(temp, 1, 2)

tract_list <- map(state_list, tracts, year = "2010")
tract_national <- rbind_tigris(tract_list)

tract_national <- tract_national %>% select(1:4)


lehd_sf <- tract_national %>% left_join(lehd, by = c("GEOID10" = "tract_id"))

#save new lehd sf as RDS
#saveRDS(lehd_sf, "lehd_2004_sf.rds")
saveRDS(lehd_sf, "lehd_2014_sf.rds")


#remember to clean out the tract zip files from the directory

