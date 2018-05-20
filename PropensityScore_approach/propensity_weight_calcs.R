
if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(RPostgreSQL, car, here, cobalt, ipw,  sf, dplyr, dbplyr)


#logistic table processing from industrial_land db for MANUFACTURING JOBS ONLY------
host <- "pgsql102.rc.pdx.edu"
user <- "jamgreen"
pw <- scan(here("batteries.pgpss"), what = "")
dbname <- "industrial_land"


con <- dbConnect("PostgreSQL", host = host, user = user, dbname = dbname, 
                 password = pw)


#pull in lehd and calc basic industrial shares, filter to 2008 

city_lehd <- st_read(dsn = con, "lehd_places") 



city_lehd <- city_lehd %>% 
  mutate(ind_emp = cns01 + cns02 + cns03 + cns05 + cns06 + cns08,
  indshare = ind_emp/c000, mfgshare = cns05/c000)


#bring in the acs census file and calc pop percent shares

acs <- tbl(con, "propensity_blkgrp_2006_2010")
acs <- collect(acs)

acs <- acs %>% 
  mutate(white_per = white_nh/totpop, black_per = black_nh/totpop,
         api_per = api/totpop, hispanic_per = hispanic/totpop,
          owner_per = owner_occ/housing_units, 
         renter_per = renter_occ/housing_units)

#join acs to lehd 2008

city_lehd_acs <- city_lehd %>% 
  inner_join(acs, by = c("bg_fips" = "geoid10"))
#filter out blockgroups with greater than 34 tot_emp (first quartile) and replace NAs with 0
#city_lehd_mfg <- city_lehd_dist %>% filter(tot_emp > 34)
city_lehd_acs$pmd_policy <- if_else(is.na(city_lehd_acs$pmd_policy), "Non-PMD", city_lehd_acs$pmd_policy)


#calculate population shares for white and non-white
city_lehd_mfg <- city_lehd_mfg %>% mutate(BlackPer = black_nh/totpop, HispPer = hispanic/totpop,
                                          RenterPer = renter_occ/tot_units)

city_lehd_mfg$BlackPer[is.nan(city_lehd_mfg$BlackPer)] <- 0
city_lehd_mfg$HispPer[is.nan(city_lehd_mfg$HispPer)] <- 0
city_lehd_mfg$RenterPer[is.nan(city_lehd_mfg$RenterPer)] <- 0



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

prop_2015 <- dbGetQuery(con, "DROP TABLE IF EXISTS lehd_model_2015_emp;
CREATE TABLE lehd_model_2015_emp AS
select *
from prop_score_mfg_ind  LEFT JOIN
(SELECT bg_fips,'C000' as tot_emp2015, 'CNS01' as ag2015, 'CNS02' as mining2015, 
'CNS03' as util2015, 'CNS05' as mfg2015, 'CNS06' as wholesale2015, 
'CNS08' as transpo2015
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

