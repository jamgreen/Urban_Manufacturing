if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(car, lmtest, sandwich, survey, tidyverse)

prop_final <- read_csv("data/model/propensity_score_mfg_ind_table.csv")

#Estimating final propensity score models using both survey and non-survey package-----
#with stabilized and unstabilized weights

#set up survey objects

prop_mfg_unstable_svy <- svydesign(ids = ~ 1, weights = ~ mfg_ps, data = prop_final)
prop_mfg_stable_svy <- svydesign(ids = ~ 1, weights = ~ ipw_mfg_stable, data = prop_final)

prop_ind_unstable_svy <- svydesign(ids = ~ 1, weights = ~ ind_ps, data = prop_final)
prop_ind_stable_svy <- svydesign(ids = ~ 1, weights = ~ ipw_ind_stable, data = prop_final)

#unstabilized weight models, both survey and non-survey-----  

#unstable manufacturing

unstable_mfg1 <- lm(mfg_change ~ pmd_dummy, weights = mfg_ps, data = prop_final)

unstable_mfg1_svy <- svyglm(mfg_change ~ pmd_dummy, design = prop_mfg_unstable_svy)

unstable_mfg2 <- lm(mfg_change ~ pmd_dummy + factor(city), weights = mfg_ps,data = prop_final)

unstable_mfg2_svy <- svyglm(mfg_change ~ pmd_dummy + factor(city), design = prop_mfg_unstable_svy)

#unstable industry

unstable_ind1 <- lm(ind_change ~ pmd_dummy, weights = ind_ps, data = prop_final)

unstable_ind1_svy <- svyglm(ind_change ~ pmd_dummy, design = prop_ind_unstable_svy)

unstable_ind2 <- lm(ind_change ~ pmd_dummy + factor(city), weights = ind_ps,data = prop_final)

unstable_ind2_svy <- svyglm(ind_change ~ pmd_dummy + factor(city), design = prop_ind_unstable_svy)

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



