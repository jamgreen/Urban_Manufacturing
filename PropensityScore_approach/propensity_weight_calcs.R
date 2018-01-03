
if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(RPostgreSQL, car, safeBinaryRegression, brglm, lmtest, sandwich, dplyr, dbplyr)


#logistic table processing from industrial_land db for MANUFACTURING JOBS ONLY------
host <- "pgsql.rc.pdx.edu"
user <- "jamgreen"
pw <- scan("batteries.pgpss", what = "")
dbname <- "industrial_land"


con <- dbConnect("PostgreSQL", host = host, user = user, dbname = dbname, password = pw)




city_lehd_dist <- dbGetQuery(con, "select distinct a.blk_grp_id, a.tot_emp, a.ag_emp, a.mining_emp,
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



city_lehd_dist <- city_lehd_dist %>% 
  mutate(ind_emp = ag_emp + mining_emp + util_emp + mfg_emp + wholesale_emp + transpo_emp,
  IndShare = ind_emp/tot_emp, MfgShare = mfg_emp/tot_emp)


#filter out blockgroups with greater than 34 tot_emp (first quartile) and replace NAs with 0
city_lehd_mfg <- city_lehd_dist %>% filter(tot_emp > 34)
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

# log_m1 <- glm(pmd_dummy ~ MfgShare + BlackPer + HispPer + RenterPer + dist_km + pop_density, 
#               data = city_lehd_mfg, family = binomial(link = "logit"))
# 
# log_m1_brglm <- brglm(pmd_dummy ~ MfgShare + BlackPer + HispPer + RenterPer + dist_km + pop_density, 
# data = city_lehd_mfg, family = binomial(link = "logit"), method = "brglm.fit")

log_mfg_brglm <- brglm(pmd_dummy ~ MfgShare + BlackPer + HispPer + RenterPer + dist_km + pop_density + 
                         mfg_emp, data = city_lehd_mfg, family = binomial(link = "logit"))

vif(log_mfg_brglm)

log_ind_brglm <- brglm(pmd_dummy ~ IndShare + BlackPer + HispPer + RenterPer + dist_km + pop_density + 
                        ind_emp, data = city_lehd_mfg, family = binomial(link = "logit"))

city_lehd_mfg$m1_mfg_val <- predict.glm(log_mfg_brglm, type = "response")
city_lehd_mfg$m1_ind_val <- predict.glm(log_ind_brglm, type = "response")

#use the inverse weighting approach for weights, source: http://pareonline.net/pdf/v20n13.pdf
city_lehd_mfg$mfg_ATE_wgt <- ifelse(city_lehd_mfg$pmd_dummy == TRUE, 1/city_lehd_mfg$m1_mfg_val,
                                   1/(1 - city_lehd_mfg$m1_mfg_val))

city_lehd_mfg$ind_ATE_wgt <- ifelse(city_lehd_mfg$pmd_dummy == TRUE, 1/city_lehd_mfg$m1_ind_val,
                                    1/(1 - city_lehd_mfg$m1_ind_val))

#Create new propensity score table for MANUFACTURING ONLY and for blkgrps with 5%> mfg_emp

prop_gt_table <- city_lehd_mfg %>% 
  select(geoid10 = blk_grp_id, pmd_dummy, prop_score_mfg = mfg_ATE_wgt, 
         prop_score_ind = ind_ATE_wgt, city_name)  
  

copy_to(con, prop_gt_table ,"prop_score_mfg_ind", temporary = FALSE, 
        indexes = list("geoid10"), overwrite = TRUE )

#weighted regression models, have to query the db to get 2009 and 2015 emp numbers------
#**first steps are to create the final model table----

prop_2009 <- tbl(con, "lehd_model_2009_emp")
prop_2009 <- collect(prop_2009)
prop_2015 <- tbl(con, "lehd_model_2015_emp")
prop_2015 <- collect(prop_2015)

prop_final <- prop_2015 %>% select(1, 5:12) %>% inner_join(prop_2009, by = "geoid10")
prop_final <- prop_final %>% mutate(mfg_change = mfg2015 - mfg2009,
            ind_2009 = ag2009 + mining2009 + util2009 + mfg2009 + wholesale2009 + transpo2009,
            ind_2015 = ag2015 + mining2015 + util2015 + mfg2015 + wholesale2015 + transpo2015,
            ind_change = ind_2015 - ind_2009)

prop_final <- prop_final %>% select(geoid10, city = city_name.x, bg_fips = bg_fips.x,
                                    pmd_dummy, prop_score_mfg, prop_score_ind,
                                    mfg_change, ind_change)

#dbClearResult(con)
dbDisconnect(con)


# running the models  city clustered SE

prop_mfg1 <- lm(mfg_change ~ pmd_dummy + factor(geoid10), weights = prop_score_mfg,
                data = prop_final)

