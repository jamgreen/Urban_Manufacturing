#download the census shape for the 50 largest cities in 2012
#using the new censusapi package from Hannah Trecht (sp?)
#compare it to what I alreayd have

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(tigris, censusapi, tidyverse, sf)
options(tigris_class = "sf")

apis <- listCensusApis()
acs2012_meta <- listCensusMetadata(name="acs5", vintage = 2012,type = "v")
acs2012_meta_g <- listCensusMetadata(name="acs5", vintage = 2012,type = "g")

#get 2012 places pop data and join to tigris sf

places_acs <- getCensus(name="acs5",vars=c("NAME", "PLACE", "B01003_001E"), vintage=2012, key = ACS_KEY,
                         region = "place:*")

fifty_places2012 <- places_acs %>% arrange(desc(B01003_001E)) %>% slice(1:51)
fifty_places2012 <- fifty_places2012 %>% mutate(GEOID = paste0(state, PLACE), YEAR = "2012")
fifty_places2012 <- fifty_places2012 %>% filter(state != "72")

#join fifty places df to places simple features for 2012
state_codes <- unique(fifty_places2012$state)
state.sf <- lapply(state_codes, function(x){
  sfdf <- places(x, year = 2012)
})

state_places <- rbind_tigris(state.sf)
state_places <- state_places %>% select(4:5,13:14)
state_places <- inner_join(state_places, fifty_places2012, by = c("GEOID" = "GEOID"), 
                           suffix = c("_sf", "_acs"))
