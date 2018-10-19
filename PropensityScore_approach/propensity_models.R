if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(lmtest, multiwayvcov, sandwich, survey, RPostgreSQL, dplyr, dbplyr)

#import tables from industrial_land db and run models

host <- "pgsql102.rc.pdx.edu"
user <- "jamgreen"
pw <- scan(here("batteries.pgpss"), what = "")
dbname <- "industrial_land"


con <- dbConnect("PostgreSQL", host = host, user = user, dbname = dbname, 
                 password = pw)

prop_full <- tbl(con, "prop_score_mfg_ind") %>% 
  collect()

prop_restricted <- tbl(con, "prop_score_mfg_ind_restricted") %>% 
  collect()

# prop_final_full <- readRDS("data/model/city_lehd_2004_full_propensity.RDS")
# prop_final_restricted <- readRDS("data/model/city_lehd_2004_restricted_propensity.RDS")


#Estimating final propensity score models using both survey and non-survey package-----
#with stabilized and unstabilized weights when I can fix impw issues

#set up survey objects


#survey objects with clustering id


prop_full_cluster_mfg <- svydesign(ids = ~ namelsad, weights = ~ ipw_mfg, data = prop_full)
prop_full_cluster_ind <- svydesign(ids = ~ namelsad, weights = ~ ipw_ind, data = prop_full)


prop_restricted_mfg <- svydesign(ids = ~ namelsad, weights = ~ ipw_mfg_restrict, 
                                         data = prop_restricted)

prop_restricted_ind <- svydesign(ids = ~ namelsad, weights = ~ ipw_ind_restrict, 
                                 data = prop_restricted)


#unstabilized weight models, both survey and non-survey with clustered SEs-----  

#survey stabilized manufacturing and industry


#survey


mfg_full_svy <- svyglm(mfg_emp_change ~ pmd_dummy, design = prop_full_cluster_mfg)
mfg_restricted_svy <- svyglm(mfg_emp_change ~ pmd_dummy, design = prop_restricted_mfg)

ind_full_svy <- svyglm(ind_emp_change ~ pmd_dummy, design = prop_full_cluster_ind)
ind_restricted_svy <- svyglm(ind_emp_change ~ pmd_dummy, design = prop_restricted_ind)



#non-survey  mfg and industry--------------
#non-survey

# #run models for manufacturing
# 
# mfg1_full <- lm(mfg_emp_change ~ pmd_dummy, weights = ipw_mfg, data = prop_full)
# mfg1_restricted <- lm(mfg_emp_change ~ pmd_dummy, weights = ipw_mfg_restrict,
#                       data = prop_restricted)
# 
# #calculate covariance matrix for clustered standard errors
# mfg_cluster_full <- multiwayvcov::cluster.vcov(mfg1_full, cluster = prop_full$namelsad)
# 
# mfg_cluster_restricted <- multiwayvcov::cluster.vcov(mfg1_restricted,
#                                                      cluster = prop_restricted$namelsad)
# #get actual significance tests
# cf_mfg1 <- lmtest::coeftest(mfg1_full, mfg_cluster_full)
# 
# 
# cf_mfg2 <- lmtest::coeftest(mfg1_restricted, mfg_unstable_cluster_restricted)
# 
# 
# #run models for industry
# 
# ind1_full <- lm(ind_emp_change ~ pmd_dummy, weights = ipw_ind, data = prop_full)
# ind1_restricted <- lm(ind_emp_change ~ pmd_dummy, weights = ipw_ind_restrict,
#                       data = prop_restricted)
# 
# #calculate covariance matrix for clustered standard errors
# 
# ind_cluster_full <- multiwayvcov::cluster.vcov(ind1_full, cluster = prop_full$namelsad)
# 
# ind_cluster_restricted <- multiwayvcov::cluster.vcov(ind1_restricted,
#                                                      cluster = prop_restricted$namelsad)
# 
# #get actual significance tests
# 
# lmtest::coeftest(ind1_full, ind_cluster_full)
# lmtest::coeftest(ind1_restricted, ind_cluster_restricted)




dbDisconnect(conn = con)

rm(list = c("prop_full", "prop_restricted", "prop_full_cluster_mfg",
            "prop_full_cluster_ind", "prop_restricted_mfg", "prop_restricted_ind", 
            "con"))

