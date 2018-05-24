
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

city_lehd <- city_lehd %>% 
  filter(namelsad != "Bakersfield city")


#bring in the acs census file and calc pop percent shares

acs <- tbl(con, "propensity_blkgrp_2006_2010")
acs <- collect(acs)

acs <- acs %>% 
  mutate(white_per = white_nh/totpop, black_per = black_nh/totpop,
         api_per = api/totpop, hispanic_per = hispanic/totpop,
          owner_per = owner_occ/housing_units, 
         renter_per = renter_occ/housing_units)

#join acs to lehd 

city_lehd_acs <- city_lehd %>% 
  inner_join(acs, by = c("bg_fips" = "geoid10"))

#fix pmd_policy vars
city_lehd_acs$pmd_policy <- if_else(is.na(city_lehd_acs$pmd_policy), "Non-PMD", city_lehd_acs$pmd_policy)
city_lehd_acs$pmd_dummy <- if_else(city_lehd_acs$pmd_policy == "PMD", 1, 0)

#replace nulls with zeroes and pull out 2004, 2009, and 2015 years

city_lehd_acs$black_per[is.nan(city_lehd_acs$black_per)] <- 0
city_lehd_acs$hispanic_per[is.nan(city_lehd_acs$hispanic_per)] <- 0
city_lehd_acs$renter_per[is.nan(city_lehd_acs$renter_per)] <- 0

city_lehd_acs04 <- city_lehd_acs %>% filter(year == 2004)
city_lehd_acs09 <- city_lehd_acs %>% filter(year == 2009)
city_lehd_acs15 <- city_lehd_acs %>% filter(year == 2015)


city_lehd_acs04$indshare[is.na(city_lehd_acs04$indshare)] <- 0
city_lehd_acs04$mfgshare[is.na(city_lehd_acs04$mfgshare)] <- 0
city_lehd_acs04$owner_per[is.na(city_lehd_acs04$owner_per)] <- 0

city_lehd_acs09$indshare[is.na(city_lehd_acs09$indshare)] <- 0
city_lehd_acs09$mfgshare[is.na(city_lehd_acs09$mfgshare)] <- 0
city_lehd_acs09$owner_per[is.na(city_lehd_acs09$owner_per)] <- 0

city_lehd_acs15$indshare[is.na(city_lehd_acs15$indshare)] <- 0
city_lehd_acs15$mfgshare[is.na(city_lehd_acs15$mfgshare)] <- 0
city_lehd_acs15$owner_per[is.na(city_lehd_acs15$owner_per)] <- 0

#calculate difference between 2009-2015 ind and mfg

city_lehd_acs09 <- city_lehd_acs09 %>% select(-geom) %>% data.frame()
city_lehd_acs15 <- city_lehd_acs15 %>% select(-geom) %>% data.frame()

city_lehd_acs09 <- city_lehd_acs09 %>% 
  select(bg_fips, cns05, ind_emp)

city_lehd_acs15 <- city_lehd_acs15 %>% 
  select(bg_fips, cns05, ind_emp)

city_lehd_combine_full <- city_lehd_acs09 %>% 
  inner_join(city_lehd_acs15, by = "bg_fips", suffix = c("_09", "_15"))

city_lehd_combine_full <- city_lehd_combine_full %>% 
  mutate(mfg_emp_change = cns05_15 - cns05_09,
         ind_emp_change = ind_emp_15 - ind_emp_09,
         mfg_per_change = (cns05_15 - cns05_09)/cns05_09,
         ind_per_change = (ind_emp_15 - ind_emp_09)/ind_emp_09)

city_lehd_combine_full$mfg_per_change[is.nan(city_lehd_combine_full$mfg_per_change)] <- 0
city_lehd_combine_full$ind_per_change[is.nan(city_lehd_combine_full$ind_per_change)] <- 0

city_lehd_combine_full$mfg_per_change[is.infinite(city_lehd_combine_full$mfg_per_change)] <- 1
city_lehd_combine_full$ind_per_change[is.infinite(city_lehd_combine_full$ind_per_change)] <- 1

city_lehd_acs04 <- city_lehd_acs04 %>% 
  inner_join(city_lehd_combine_full, by = "bg_fips")


rm(list = c("city_lehd_acs09", "city_lehd_acs15", "city_lehd_combine_full"))
#create a restricted data set limited to block groups with at least 4 industrial jobs
city_lehd_acs04_restricted <- city_lehd_acs04 %>% 
  filter(ind_emp >= median(ind_emp))

#calculate weights, both predicted logit coefficients and inverse probability------

#predicted weights restricted and non-restricted sample

ind_full_logit <- glm(pmd_dummy ~ network_density + dist_km + indshare + owner_per,
                      data = city_lehd_acs04, family = "binomial")

city_lehd_acs04$logit_wgt_full <- predict(ind_full_logit, type = "response")
city_lehd_acs04$logit_ipw_full <- if_else(city_lehd_acs04$pmd_dummy == 1,
                                          1/city_lehd_acs04$logit_wgt_full, 
                                          1/(1 - city_lehd_acs04$logit_wgt_full))

ind_restricted_logit <-  glm(pmd_dummy ~ network_density + dist_km + indshare + owner_per,
                             data = city_lehd_acs04_restricted, family = "binomial")

city_lehd_acs04_restricted$logit_wgt_restricted <- predict(ind_restricted_logit, type = "response")
city_lehd_acs04_restricted$logit_ipw_restricted <- if_else(city_lehd_acs04_restricted$pmd_dummy == 1,
                                                      1/city_lehd_acs04_restricted$logit_wgt_restricted, 
                                                      1/(1 - city_lehd_acs04_restricted$logit_wgt_restricted))


#until i can fix the ipw issue i'll export the model to csv

readr::write_csv(city_lehd_acs04, "data/model/city_lehd_2004_full_propensity.csv")
readr::write_csv(city_lehd_acs04_restricted, "data/model/city_lehd_2004_restricted_propensity.csv")

saveRDS(city_lehd_acs04, "data/model/city_lehd_2004_full_propensity.RDS")
saveRDS(city_lehd_acs04_restricted, "data/model/city_lehd_2004_restricted_propensity.RDS")

#test balance with cobalt




# #using ipw to compare the weights calculation-----
# mfg_stabilized_full <- ipwpoint(exposure = pmd_dummy, family = "binomial", link = "logit",
#                                 numerator = ~1,
#                                 denominator = ~ mfgshare + owner_per + dist_km + 
#                                   network_density,
#                                 data = city_lehd_acs04)
# 
# 
# 
# mfg_stabilized_restricted <- ipwpoint(exposure = pmd_dummy, family = "binomial", 
#                                       link = "logit",
#                                       numerator = ~1,
#                                       denominator = ~ mfgshare +  renter_per + dist_km + 
#                                         network_density,
#                                       data = city_lehd_acs04_restricted)
# 
# 
# ind_stable_full <- ipwpoint(exposure = pmd_dummy, family = "binomial", link = "logit",
#                             numerator = ~ 1, 
#                             denominator = ~ indshare + owner_per  +
#                               dist_km + network_density,
#                             data = city_lehd_acs04_restricted)
# 
# ind_stable_restricted <- ipwpoint(exposure = pmd_dummy, family = "binomial", link = "logit",
#                                   numerator = ~ 1, 
#                                   denominator = ~ mfgshare + black_per + hispanic_per + 
#                                   renter_per + dist_km + network_density,
#                                   data = city_lehd_acs04)
# 
# city_lehd_acs04$ipw_ind<- ind_stable_full$ipw.weights
# city_lehd_acs04$ipw_ind_restrict <- ind_stable_restricted$ipw.weights
# 
# city_lehd_acs04$ipw_mfg <- mfg_stabilized_full$ipw.weights
# city_lehd_acs04$ipw_mfg_restrict <- mfg_stabilized_restricted$ipw.weights
# 
# #testing balance with cobalt package-------------
# covs1 <- city_lehd_acs04 %>% select(MfgShare, BlackPer, HispPer, RenterPer, dist_km, network_density)
# 
# bal.tab(covs1, treat = city_lehd_acs04$pmd, weights = city_lehd_acs04$ipw_mfg_stable,
#         method = "weighting")
# 
# covs2 <- city_lehd_acs04 %>% select(IndShare, BlackPer, HispPer, RenterPer, dist_km, network_density)
# 
# bal.tab(covs2, treat = city_lehd_acs04$pmd, weights = city_lehd_acs04$ipw_ind_stable,
#         method = "weighting")
# 
# 
# #Create new propensity score table for manufacturing and industrial employment
# 
# prop_gt_table <- city_lehd_acs04 %>% 
#   select(geoid10 = blk_grp_id, pmd_policy, mfg_ps, ipw_mfg_stable, ipw_mfg_unstable ,
#          ind_ps, ipw_ind_stable, ipw_ind_unstable , city_name)  
#   
# 
# copy_to(con, prop_gt_table ,"prop_score_mfg_ind", temporary = FALSE, 
#         indexes = list("geoid10"), overwrite = TRUE )
# 
# #weighted regression models, have to query the db to get 2009 and 2015 emp numbers------
# #**first steps are to create the final model table----
# 
# 
# 
# #dbClearResult(con)
 dbDisconnect(con)
 
rm(list = ls())

