
if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(RPostgreSQL, car, cobalt, ipw,  dplyr, dbplyr)


#logistic table processing from industrial_land db for MANUFACTURING JOBS ONLY------
host <- "pgsql.rc.pdx.edu"
user <- "jamgreen"
pw <- scan("batteries.pgpss", what = "")
dbname <- "industrial_land"


con <- dbConnect("PostgreSQL", host = host, user = user, dbname = dbname, password = pw)




city_lehd_mfg <- dbGetQuery(con, "select distinct a.blk_grp_id, a.tot_emp, a.ag_emp, a.mining_emp,
a.util_emp, a.mfg_emp, a.wholesale_emp, a.transpo_emp,
a.place_geoid, a.city_name, b.totpop as totpop, b.white_nh as white_nh, 
b.black_nh as black_nh, b.hispanic as hispanic, b.mhi as mhi, 
b.housing_units as tot_units, b.owner_occ as owner_occ, 
b.renter_occ as renter_occ,
d.d3a as network_density, 
b.totpop/(ST_Area(a.geom)/1000000) as pop_density,
c.pmd_dummy as pmd_dummy, a.geom
	FROM lehd_places_2004_nad83albers a 
		LEFT JOIN propensity_blkgrp b ON
		a.blk_grp_id = b.geoid 
		LEFT JOIN lehd04_pmd c ON
		a.blk_grp_id = c.blk_grp_id
		LEFT JOIN epa_sld d ON
		a.blk_grp_id = d.geoid10;")



city_lehd_mfg <- city_lehd_mfg %>% 
  mutate(ind_emp = ag_emp + mining_emp + util_emp + mfg_emp + wholesale_emp + transpo_emp,
  IndShare = ind_emp/tot_emp, MfgShare = mfg_emp/tot_emp) %>% 
  distinct(blk_grp_id, .keep_all = TRUE)


#filter out blockgroups with greater than 34 tot_emp (first quartile) and replace NAs with 0
#city_lehd_mfg <- city_lehd_dist %>% filter(tot_emp > 34)
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

city_lehd_mfg <- city_lehd_mfg %>% left_join(city_dist, by = c("blk_grp_id" = "blk_grp_id")) %>% 
  distinct(blk_grp_id, .keep_all = TRUE)

city_lehd_mfg$pmd <- ifelse(city_lehd_mfg$pmd_dummy == TRUE, 1, 0)

#city_lehd_mfg <- city_lehd_mfg[!duplicated(city_lehd_mfg$blk_grp_id), ] 


#using ipw to compare the weights calculation-----
temp_mfg_stabilized <- ipwpoint(exposure = pmd_dummy, family = "binomial", link = "logit",
                                numerator = ~ 1, 
                                denominator = ~ MfgShare + BlackPer + HispPer + RenterPer + dist_km + network_density,
                                data = city_lehd_mfg)

temp_mfg_unstabilized <- ipwpoint(exposure = pmd_dummy, family = "binomial", link = "logit",
                                  denominator = ~ MfgShare + BlackPer + HispPer + RenterPer + dist_km + network_density,
                                  data = city_lehd_mfg)


temp_ind_stable <- ipwpoint(exposure = pmd_dummy, family = "binomial", link = "logit",
                            numerator = ~ 1, 
                            denominator = ~ IndShare + BlackPer + HispPer + RenterPer + dist_km + 
                              network_density,
                            data = city_lehd_mfg)

temp_ind_unstable <- ipwpoint(exposure = pmd_dummy, family = "binomial", link = "logit",
                              denominator = ~ IndShare + BlackPer + HispPer + RenterPer + dist_km + network_density,
                              data = city_lehd_mfg)

city_lehd_mfg$ipw_ind_stable <- temp_ind_stable$ipw.weights
city_lehd_mfg$ipw_ind_unstable <- temp_ind_unstable$ipw.weights

city_lehd_mfg$ipw_mfg_stable <- temp_mfg_stabilized$ipw.weights
city_lehd_mfg$ipw_mfg_unstable <- temp_mfg_unstabilized$ipw.weights

#testing balance with cobalt package
covs1 <- city_lehd_mfg %>% select(MfgShare, BlackPer, HispPer, RenterPer, dist_km, network_density)

bal.tab(covs1, treat = city_lehd_mfg$pmd, weights = city_lehd_mfg$ipw_mfg_stable,
        method = "weighting")

covs2 <- city_lehd_mfg %>% select(IndShare, BlackPer, HispPer, RenterPer, dist_km, network_density)

bal.tab(covs2, treat = city_lehd_mfg$pmd, weights = city_lehd_mfg$ipw_ind_stable,
        method = "weighting")


#Create new propensity score table for manufacturing and industrial employment

prop_gt_table <- city_lehd_mfg %>% 
  select(geoid10 = blk_grp_id, pmd_dummy, mfg_ps, ipw_mfg_stable, ipw_mfg_unstable ,
         ind_ps, ipw_ind_stable, ipw_ind_unstable , city_name)  
  

copy_to(con, prop_gt_table ,"prop_score_mfg_ind", temporary = FALSE, 
        indexes = list("geoid10"), overwrite = TRUE )

#weighted regression models, have to query the db to get 2009 and 2015 emp numbers------
#**first steps are to create the final model table----

prop_2009 <- dbGetQuery(con, "DROP TABLE IF EXISTS lehd_model_2009_emp;
CREATE TABLE lehd_model_2009_emp AS
select *
from prop_score_mfg_ind  LEFT JOIN
(SELECT bg_fips,'C000' as tot_emp, 'CNS01' as ag2009, 'CNS02' as mining2009, 'CNS03' as util2009, 
'CNS05' as mfg2009, 'CNS06' as wholesale2009, 'CNS08' as transpo2009
FROM lehd_cbsa
where year = '2009') b ON
prop_score_mfg_ind.geoid10 = b.bg_fips;
")

prop_2015 <- dbGetQuery(con, "DROP TABLE IF EXISTS lehd_model_2009_emp;
CREATE TABLE lehd_model_2009_emp AS
select *
from prop_score_mfg_ind  LEFT JOIN
(SELECT bg_fips,'C000' as tot_emp, 'CNS01' as ag2009, 'CNS02' as mining2009, 'CNS03' as util2009, 
'CNS05' as mfg2009, 'CNS06' as wholesale2009, 'CNS08' as transpo2009
FROM lehd_cbsa
where year = '2015') b ON
prop_score_mfg_ind.geoid10 = b.bg_fips;
")
#tbl(con, "lehd_model_2009_emp")
#prop_2009 <- collect(prop_2009)
#prop_2015 <- tbl(con, "lehd_model_2015_emp")
#prop_2015 <- collect(prop_2015)

prop_final <- prop_2015 %>% select(1, 12:17) %>% inner_join(prop_2009, by = "geoid10")

prop_final <- prop_final %>% mutate(mfg_change = mfg2015 - mfg2009,
            ind_2009 = ag2009 + mining2009 + util2009 + mfg2009 + wholesale2009 + transpo2009,
            ind_2015 = ag2015 + mining2015 + util2015 + mfg2015 + wholesale2015 + transpo2015,
            ind_change = ind_2015 - ind_2009)

prop_final <- prop_final %>% select(geoid10, city = city_name, bg_fips,
                                    pmd_dummy, mfg_ps, ipw_mfg_stable, ipw_mfg_unstable,
                                    ind_ps, ipw_ind_stable, ipw_ind_unstable,
                                    mfg_change, ind_change)

readr::write_csv(prop_final, "data/model/propensity_score_mfg_ind_table.csv")

#dbClearResult(con)
dbDisconnect(con)

rm(list = ls())

