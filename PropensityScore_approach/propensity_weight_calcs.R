
if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(RPostgreSQL, postGIStools, sf,dplyr, dbplyr)


#logistic table processing from industrial_land db for MANUFACTURING JOBS ONLY------
host <- "pgsql.rc.pdx.edu"
user <- "jamgreen"
pw <- "&zSpR-rmd&v5REgZ"
dbname <- "industrial_land"


con <- dbConnect("PostgreSQL", host = host, user = user, dbname = dbname, password = pw)




city_lehd_dist <- dbGetQuery(con, "select distinct a.*, b.totpop as totpop, b.white_nh as white_nh, 
                                    b.black_nh as black_nh, b.hispanic as hispanic, b.mhi as
                                    mhi, b.housing_units as tot_units, b.owner_occ as 
                                    owner_occ, b.renter_occ as renter_occ, c.pmd_dummy as
                                    pmd_dummy
                                  FROM lehd_places_2004_nad83albers a LEFT JOIN
                                      propensity_blkgrp b ON
                                      a.blk_grp_id = b.geoid 
                                      LEFT JOIN
                                      lehd04_pmd c ON
                                      a.blk_grp_id = c.blk_grp_id;")



city_lehd_dist <- city_lehd_dist %>% mutate(MfgShare = mfg_emp/tot_emp)


dbClearResult(rs)
dbDisconnect(con)
#filter out blockgroups with less that 5% mfg share and replace NAs with 0
city_lehd_mfg <- city_lehd_dist %>% filter(MfgShare >= .05)
city_lehd_mfg[is.na(city_lehd_mfg)] <- 0

#calculate population shares for white and non-white
city_lehd_mfg <- city_lehd_mfg %>% mutate(BlackPer = black_nh/totpop, HispPer = hispanic/totpop,
                                          RenterPer = renter_occ/tot_units)

city_lehd_mfg$BlackPer[is.nan(city_lehd_mfg$BlackPer)] <- 0
city_lehd_mfg$HispPer[is.nan(city_lehd_mfg$HispPer)] <- 0
city_lehd_mfg$RenterPer[is.nan(city_lehd_mfg$RenterPer)] <- 0

#join to the city_dist table for final calcs

city_dist <- tbl(con, "city_dist_km")
city_dist <- collect(city_dist)

city_lehd_mfg <- city_lehd_mfg %>% left_join(city_dist, by = c("blk_grp_id" = "blk_grp_id"))

#logistic regression for calculating propensity weights----

log_m1 <- glm(pmd_dummy ~ MfgShare + BlackPer + HispPer + RenterPer + dist_km, data = city_lehd_mfg,
              family = binomial(link = "logit"))

city_lehd_mfg$m1_value <- predict.glm(log_m1, type = "response")

#use the inverse weighting approach for weights, source: http://pareonline.net/pdf/v20n13.pdf
city_lehd_mfg$m1_ATE_wgt <- ifelse(city_lehd_mfg$pmd_dummy == TRUE, 1/city_lehd_mfg$m1_value,
                                   1/(1 - city_lehd_mfg$m1_value))

#Create new propensity score table for MANUFACTURING ONLY and for blkgrps with 5%> mfg_emp

prop_gt_table <- city_lehd_mfg %>% 
  select(geoid10 = blk_grp_id, pmd_dummy, prop_score = m1_ATE_wgt)  
  

copy_to(con, prop_gt_table ,"prop_score_mfg_only", temporary = FALSE, 
        indexes = list("geoid10"), overwrite = TRUE )

#weighted regression model for MFG ONLY block groups with 5%> mfg_emp in 2004------

blkgrp_2015 <- readr::read_csv("data/nhgis0013_ds215_20155_2015_blck_grp.csv")
