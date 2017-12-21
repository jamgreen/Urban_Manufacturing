#call the database i got from research computing
if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(RPostgreSQL, sf, postGIStools,tidyverse)

dbname <- "industrial_land"
host <- "pgsql.rc.pdx.edu"
user <- "jamgreen"
password <- "&zSpR-rmd&v5REgZ"

con <- dbConnect(dbDriver("PostgreSQL"), user=user,
                 password=password, dbname=dbname, host=host)

table_list <- dbListTables(con)

lehd <- tbl(con, "lehd_propensity04")
pmd <- tbl(con, "lehd04_pmd")

lehd_pmd <- lehd %>% inner_join(pmd, by = c("bg_fips", "bg_fips")) %>% collect()
lehd_pmd <- lehd_pmd %>% st_as_sf()


blk_grp <- tbl(con, "us_blck_grp_2010_nad83albers")
propensity_vars <- tbl(con, "propensity_blkgrp")

non_white_blkgrp <- propensity_vars %>% 
  summarise(TotPop = RLIE001, WhitePop = RLIE003, NonWhite = RLIE001 - RLIE003) %>% 
  inner_join(blk_grp, by = c("GISJOIN" = "gisjoin"))

non_white <- dbGetQuery(con, "SELECT * FROM propensity_blkgrp;")
non_white <- non_white %>% mutate(TotPop = RK9E001, WhiteAlone = RLIE003,
                                  NonWhite = RLIE004 + RLIE005 + RLIE006 + 
                                  RLIE007 + RLIE008 + RLIE009 + RLIE010 + 
                                  RLIE011 + RLIE012, MHI = RNHE001) %>% 
  select(GEOID, GISJOIN, TotPop, WhiteAlone, NonWhite, MHI)

blkgrp <- get_postgis_query(con, "SELECT geoid10, gisjoin, geom FROM us_blck_grp_2010_nad83albers", 
                                     geom_name = "geom")

blkgrp <- st_as_sf(blkgrp)
