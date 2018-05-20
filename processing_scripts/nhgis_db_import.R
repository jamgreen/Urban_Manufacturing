#bring the 2006-2010 ACS from NHGIS into the db 


if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(RPostgreSQL, here,dplyr, dbplyr)


host <- "pgsql102.rc.pdx.edu"
user <- "jamgreen"
pw <- scan(here("batteries.pgpss"), what = "")
dbname <- "industrial_land"


con <- dbConnect("PostgreSQL", host = host, user = user, dbname = dbname, 
                 password = pw)

acs <- readr::read_csv(here("data/nhgis/nhgis0019_ds176_20105_2010_blck_grp.csv"))

acs <- acs %>% 
  select(-REGIONA, -DIVISIONA, -COUSUBA, -PLACEA, -c(13:35))

acs <- acs %>% 
  select(1:9, totpop = JMJE001, white_nh = JMJE003, black_nh = JMJE004, asian_nh = JMJE006,
         pi = JMJE007, hispanic = JMJE012, mhi = JOIE001, housing_units = JRKE001,
         owner_occ = JRKE002, renter_occ = JRKE003)

acs <- acs %>% 
  mutate(geoid10 = paste0(STATEA, COUNTYA, TRACTA, BLKGRPA),
         api = asian_nh + pi)

acs <- acs %>% select(-NAME_E)

dbWriteTable(conn = con, name = "propensity_blkgrp_2006_2010", value = acs,
             row.names = FALSE, overwrite = TRUE)

