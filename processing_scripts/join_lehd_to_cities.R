#create new lehd places file in db with correct geoms...

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(RPostgreSQL, sf, dplyr)

host <- "pgsql.rc.pdx.edu"
user <- "jamgreen"
pw <- scan("batteries.pgpss", what = "")
dbname <- "industrial_land"


con <- dbConnect("PostgreSQL", host = host, user = user, dbname = dbname, password = pw)


lehd_cbsa <- tbl(con, "lehd_cbsa")
block_place_xwalk <- tbl(con, "blkgrp_place_xwalk")

lehd <- lehd_cbsa %>% 
  inner_join(block_place_xwalk, by = c("bg_fips"="blk_grp_fips"))


lehd <- collect(lehd)

lehd <- lehd %>% select(2:31) %>% rename(place_geoid = geoid)

copy_to(dest = con, df = lehd, name = "lehd_places", overwrite = TRUE,
        indexes = list("bg_fips", "place_geoid"), temporary = FALSE)

#created a new table of blockgroups for the fifty largest cities,
#joined to the national block group shapefile

#  "create table blk_grp_place_nad83albers as
# select a.blk_grp_fips as blk_grp_fips, a.geoid as place_geoid, a.placefp as placefp,
# a.fullname as city_name, b.geom as geom
# from blkgrp_place_xwalk a JOIN us_blck_grp_2010_nad83albers b
# ON a.blk_grp_fips = b.geoid10;"

#next step add geom column to my lehd_places file
# 
# ALTER TABLE lehd_places DROP COLUMN geom;
# ALTER TABLE lehd_places ADD COLUMN geom geometry(MultiPolygon, 5070);
# 
# UPDATE lehd_places SET geom =
#   (SELECT geom FROM 
#    blk_grp_place_nad83albers WHERE 
#    blk_grp_place_nad83albers.blk_grp_fips = lehd_places.bg_fips 
#    LIMIT 1);

#add street network density
# ALTER TABLE lehd_places ADD COLUMN network_density numeric;
# update lehd_places

# set network_density = NULL;
# update lehd_places 
# set network_density = epa_sld.d3a  
# from epa_sld 
# where lehd_places.bg_fips = epa_sld.geoid10;


#I need to update the city_dist_km table as it missed out on Louisville
#To do this I will bring in the cbd table and the lehd_places table and match
#on varied city name IDs

cbd <- tbl(con, "cbd_nad83albers")
lehd <- tbl(con, "lehd_places")

cbd <- collect(cbd)
lehd <- collect(lehd)

lehd <- lehd %>% group_by(place_geoid, placefp, fullname, namelsad) %>% 
  summarise()

require("fuzzyjoin")

lehd <- lehd %>% ungroup()

cbd_match_left <- stringdist_left_join(cbd, lehd, 
                                       by = c("city" = "namelsad",
                                              "fullname" = "namelsad"),
                                         method = "lv", max_dist = 6, 
                                        distance_col = "str_dist")


cbd_match_left <- cbd_match_left %>% 
  mutate(place_geoid = case_when(fullname.x == "Indianapolis, IN" ~ "1836003",
                                 fullname.x == "Nashville-Davidson, TN" ~ "4752006",
                                 fullname.x == "Baltimore, MD" ~ "2404000",
                                 fullname.x == "Lousiville-Jefferson, KY" ~ "2148006",
                                 TRUE ~ place_geoid),
         placefp = ifelse(is.na(placefp), stringr::str_sub(place_geoid, 3, 7), placefp))

cbd_match_left <- cbd_match_left %>% 
  select(-gid, -8:-11) %>% rename(fullname = fullname.x)

cbd_match_left <- cbd_match_left %>% select(-geom)

dbWriteTable(conn = con, name = "cbd_placefp_xwalk", value = cbd_match_left, row.names = FALSE, overwrite = TRUE)

# alter table cbd_nad83albers add column place_geoid varchar(7), add column placefp varchar(5);
# 
# update cbd_nad83albers
# set place_geoid = cbd_placefp_xwalk.place_geoid,
# placefp = cbd_placefp_xwalk.placefp
# from cbd_placefp_xwalk
# where cbd_nad83albers.fullname = cbd_placefp_xwalk.fullname;

#add pmd policy to the data table, create proper dummy variable in R
# alter table lehd_places drop column pmd_policy;
# alter table lehd_places add column pmd_policy varchar;
# update lehd_places
# set pmd_policy = pmd_full_dissolve.policy
# from pmd_full_dissolve
# where ST_Intersects(pmd_full_dissolve.geom, ST_Centroid(lehd_places.geom));