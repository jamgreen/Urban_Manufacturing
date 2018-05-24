if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(lmtest, sandwich, survey)

prop_final_full <- readRDS("data/model/city_lehd_2004_full_propensity.RDS")
prop_final_restricted <- readRDS("data/model/city_lehd_2004_restricted_propensity.RDS")
#Estimating final propensity score models using both survey and non-survey package-----
#with stabilized and unstabilized weights when I can fix impw issues

#set up survey objects


#survey objects with clustering id
prop_full_cluster_svy <- svydesign(ids = ~ namelsad, weights = ~ logit_wgt_full, data = prop_final_full)
prop_restricted_cluster_svy <- svydesign(ids = ~ namelsad, weights = ~ logit_wgt_restricted, 
                                         data = prop_final_restricted)

prop_full_cluster_ipw <- svydesign(ids = ~ namelsad, weights = ~ logit_ipw_full, data = prop_final_full)
prop_restricted_cluster_svy_ipw <- svydesign(ids = ~ namelsad, weights = ~ logit_ipw_restricted, 
                                         data = prop_final_restricted)


#unstabilized weight models, both survey and non-survey with clustered SEs-----  

#survey unstable manufacturing and industry


#survey
mfg1_full_svy <- svyglm(mfg_emp_change ~ pmd_dummy, design = prop_full_cluster_svy)
mfg1_restricted_svy <- svyglm(mfg_emp_change ~ pmd_dummy, design = prop_restricted_cluster_svy)

ind1_full_svy <- svyglm(ind_emp_change ~ pmd_dummy, design = prop_full_cluster_svy)
ind1_restricted_svy <- svyglm(ind_emp_change ~ pmd_dummy, design = prop_restricted_cluster_svy)

mfg2_full_svy_ipw <- svyglm(mfg_emp_change ~ pmd_dummy, design = prop_full_cluster_ipw)
mfg2_restricted_svy_ipw <- svyglm(mfg_emp_change ~ pmd_dummy, design = prop_restricted_cluster_svy_ipw)

ind2_full_svy_ipw <- svyglm(ind_emp_change ~ pmd_dummy, design = prop_full_cluster_ipw)
ind2_restricted_svy_ipw <- svyglm(ind_emp_change ~ pmd_dummy, design = prop_restricted_cluster_svy_ipw)

rm(list = c("prop_full_cluster_svy", "prop_restricted_cluster_svy", "prop_full_cluster_ipw", 
            "prop_restricted_cluster_svy_ipw"))

#non-survey unstable mfg and industry--------------
#non-survey


# mfg1_full <- lm(mfg_emp_change ~ pmd_dummy, weights = logit_wgt_full, data = prop_final_full)
# mfg1_restricted <- lm(mfg_emp_change ~ pmd_dummy, weights = logit_wgt_restricted, 
#                       data = prop_final_restricted)
# 
# mfg_unstable_cluster_full <- multiwayvcov::cluster.vcov(mfg1_full, cluster = prop_final_full$namelsad)
# cf_mfg1 <- lmtest::coeftest(mfg1_full, mfg_unstable_cluster_full)
# 
# mfg_unstable_cluster_restricted <- multiwayvcov::cluster.vcov(mfg1_restricted, 
#                                                               cluster = prop_final_restricted$namelsad)
# cf_mfg2 <- lmtest::coeftest(mfg1_restricted, mfg_unstable_cluster_restricted)
# 
# ind1_full <- lm(ind_emp_change ~ pmd_dummy, weights = logit_wgt_full, data = prop_final_full)
# ind1_restricted <- lm(ind_emp_change ~ pmd_dummy, weights = logit_wgt_restricted, 
#                       data = prop_final_restricted)
# 
# ind_unstable_cluster_full <- multiwayvcov::cluster.vcov(ind1_full, cluster = prop_final_full$namelsad)
# cf_ind1 <- lmtest::coeftest(ind1_full, ind_unstable_cluster_full)
# 
# ind_unstable_cluster_restricted <- multiwayvcov::cluster.vcov(ind1_restricted, 
#                                                               cluster = prop_final_restricted$namelsad)
# 
# cf_ind2 <- lmtest::coeftest(ind1_restricted, ind_unstable_cluster_restricted)




#unstabilized inverse probability weights for industry and manufacturing with clustered SEs

#stabilized weight models, both survey and non-survey -----

#stabilized manufacturing

stable_mfg1 <- lm(mfg_change ~ pmd_dummy, weights = ipw_mfg_stable,
                data = prop_final)

stable_mfg1_svy <- svyglm(mfg_change ~ pmd_dummy, design = prop_mfg_stable_svy)

stable_mfg2 <- lm(mfg_change ~ pmd_dummy + factor(city), weights = ipw_mfg_stable,
                data = prop_final)

stable_mfg2_svy <- svyglm(mfg_change ~ pmd_dummy + factor(city), design = prop_mfg_stable_svy)

#stabilized industry  

stable_ind1 <- lm(ind_change ~ pmd_dummy, weights = ipw_ind_stable,
                  data = prop_final)

stable_ind1_svy <- svyglm(ind_change ~ pmd_dummy, design = prop_ind_stable_svy)

stable_ind2 <- lm(ind_change ~ pmd_dummy + factor(city), weights = ipw_ind_stable,
                  data = prop_final)

stable_ind2_svy <- svyglm(ind_change ~ pmd_dummy + factor(city), design = prop_ind_stable_svy)



