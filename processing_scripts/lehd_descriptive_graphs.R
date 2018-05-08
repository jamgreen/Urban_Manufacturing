#making some of the visualizations for the methodology and data section of the modeling chapter

if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(RPostgreSQL, ggthemes, ggalt, here, ggplot2, dplyr, extrafont, hrbrthemes,viridis)

host <- "pgsql.rc.pdx.edu"
user <- "jamgreen"
pw <- scan(here("batteries.pgpss"), what = "")
dbname <- "industrial_land"


con <- dbConnect("PostgreSQL", host = host, user = user, 
                 dbname = dbname, password = pw)

city_lehd <- tbl(con, "lehd_places")
city_lehd <- city_lehd %>% filter(fullname != "Bakersfield city, California")
city_lehd <- collect(city_lehd)

city_lehd <- city_lehd %>% 
  mutate(pmd_policy = case_when(is.na(pmd_policy) ~ "Non-PMD", TRUE ~ pmd_policy))


#split out the manufacturing emp from lehd_places------

city_lehd_mfg <- city_lehd %>% select(1:5, cns05, 28, 30, 33, 34)

mfg_2004 <- city_lehd %>% filter(year == "2004") %>% 
  group_by(namelsad) %>% summarize(mfg_emp2004 = sum(cns05))

city_lehd_mfg <- city_lehd_mfg %>% 
  group_by(namelsad, placefp, year) %>% 
  summarise_if(is.numeric, sum)

city_lehd_mfg <- city_lehd_mfg %>% 
  left_join(mfg_2004, by = "namelsad" ) %>% 
  distinct()

city_lehd_mfg <- city_lehd_mfg %>% 
  mutate(mfg_2004_idx = (cns05/mfg_emp2004)*100)

city_lehd_mfg <- city_lehd_mfg %>% ungroup() %>% 
  mutate(mfg_share = cns05/c000)

#calculating change in manufacturing employment indexed to 2004 employment------
outlier_cities <- city_lehd_mfg %>% 
  filter(year == "2015") %>% 
  mutate(MfgIndexZ = scale(mfg_2004_idx)) %>% 
  select(namelsad, MfgIndexZ) 

city_lehd_mfg <- left_join(city_lehd_mfg, outlier_cities, 
                           by = "namelsad")

city_lehd_mfg <- city_lehd_mfg %>% 
  mutate(Outlier  = case_when(MfgIndexZ < -1 ~ "Low",
                              MfgIndexZ > 1 ~ "High",
                              TRUE ~ "Average"))

city_lehd_mfg$Outlier <- as.factor(city_lehd_mfg$Outlier)
levels(city_lehd_mfg$Outlier) <- c("Low", "Average", "High")

city_lehd_mfg <- city_lehd_mfg %>% 
  mutate(Policy = ifelse(namelsad %in% c("Los Angeles city", "Chicago city", 
                                         "San Diego city", "Jacksonville city", 
                                         "San Francisco city", "Seattle city",
                                         "Baltimore city", "Portland city", 
                                         "San Jose city", "Minneapolis city", 
                                         "New York city"), 1, 0))

city_lehd_mfg <- city_lehd_mfg %>% 
  mutate(city_name = gsub("city", "", city_lehd_mfg$namelsad))

city_lehd_mfg <- city_lehd_mfg %>% 
  mutate(city_name_short = 
case_when(city_name == "Indianapolis  (balance)" ~ "Indianapolis",
city_name == "Louisville/Jefferson County metro government (balance)" ~ "Louisville",
city_name == "Nashville-Davidson metropolitan government (balance)" ~ "Nashville",
TRUE ~ city_name))

city_lehd_mfg <- city_lehd_mfg %>% 
  mutate(mfg_emp_thousands = cns05/1000)

#create ordering variable of percent change between 2004 and 2015-----
mfg_2004_2015_emp <- city_lehd_mfg %>% filter(year == "2004" | year == "2015") %>% 
  select(city_name, city_name_short, year, mfg_emp_thousands) %>% 
  tidyr::spread(key = year, value = mfg_emp_thousands, sep = "_")

mfg_2004_2015_emp <- mfg_2004_2015_emp %>% 
  mutate(mfg_emp_change = (year_2015 - year_2004)/year_2015)

mfg_2004_2015_emp <- mfg_2004_2015_emp %>% 
  select(city_name_short, mfg_emp_change)
  
city_lehd_mfg <- city_lehd_mfg %>% 
  left_join(mfg_2004_2015_emp, by = c("city_name_short" = "city_name_short"))

city_lehd_mfg <- city_lehd_mfg %>% 
  mutate(emp_trend = case_when(mfg_emp_change < 0 ~ "Decreasing Trend",
                               mfg_emp_change >= 0 ~ "Increasing Trend"))

#the mfg employment index spaghetti graph with red and blue outliers-----

mfg_index_plot <- ggplot(city_lehd_mfg, aes(year, mfg_2004_idx, group = city_name, color = Outlier)) + 
  geom_line() +  
  scale_color_manual(values = c("#DCDCDC", "#FF0000", "#0000FF")) +
  theme_minimal() +
  labs(title = "Change in Mfg Employment for 48 Large Cities, 2004-2015",
       subtitle = "Mfg. Employment Indexed to 2004 measures",
       x = "Year", y = "Mfg. Employment Index", 
       caption = "Source: LEHD WAC Files 2004-2015") +
  theme(legend.position = "none", panel.grid = element_blank())



mfg_index_facet <- ggplot(city_lehd_mfg, aes(year, mfg_2004_idx, group = city_name_short)) +
  geom_line(color = "blue3") + theme_minimal() +
  labs(title = "Change in Mfg Employment for 48 Large Cities, 2004-2015",
       subtitle = "Mfg. Employment Indexed to 2004 measures",
       x = "Year", y = "Mfg. Employment Index", 
       caption = "Source: LEHD WAC Files 2004-2015",
       axis.text.y = 4) +
  theme(legend.position = "none", panel.grid = element_blank()) +
  geom_hline(yintercept = 100, color = "red3", linetype = "dotted") +
  scale_x_discrete(limits = c("2004", "2007", "2010", "2015"))+
  facet_wrap(~ city_name_short, ncol = 6) 

#line graphs of change in absolute mfg employment and mfg employment share over time----

mfg_tot_emp_line <- ggplot(city_lehd_mfg, 
                           aes(year, mfg_emp_thousands, 
                               group = city_name_short, 
                               colour = emp_trend)) +
  geom_line() + theme_minimal() + 
  theme(legend.position = "none", 
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 6)) +
  labs(title = "Total Mfg. Employment for 48 Large Cities, 2004-2015",
       x = "Year", y = "Manufacturing Employment (thousands)", 
       caption = "Source: LEHD WAC Files 2004-2015") +
  facet_wrap(~ forcats::fct_reorder(city_name_short, mfg_emp_change), ncol = 6, scales = "free") +
  scale_x_discrete(limits = c("2004", "2007", "2010", "2015")) +
  scale_y_continuous(labels = scales::comma)

mfg_2015 <- city_lehd_mfg %>% filter(year == "2015")

mfg_tot_mfg_emp_lolli <- ggplot(mfg_2015, 
                            aes(x = forcats::fct_reorder(city_name_short, mfg_emp_thousands), 
                                y = mfg_emp_thousands)) +
  geom_lollipop(point.colour = "blue3") + theme_minimal() +
  labs(title = "Total Manufacturing Employment, 2015",
        x = "City", y = "Total Employment (thousands)",
        caption = "Source: LEHD WAC Files 2015") +
        scale_y_continuous(labels = scales::comma) +
  theme(panel.grid.minor = element_blank()) +
  coord_flip()

mfg_share_emp_lolli <- ggplot(mfg_2015, 
                              aes(x = forcats::fct_reorder(city_name_short, mfg_share), 
                                  y = mfg_share)) +
  geom_lollipop(point.colour = "blue3") + theme_minimal() +
  labs(title = "Large City Manufacturing Employment Share, 2015",
       x = "City", y = "Employment Share (%)",
       caption = "Source: LEHD WAC Files 2015") +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.grid.minor = element_blank()) +
  coord_flip()



#split out "industrial" jobs-----

city_lehd_ind <- city_lehd %>% 
  select(1:8, cns05, cns06, cns08, placefp, namelsad)

city_lehd_ind <- city_lehd_ind %>% 
  mutate(ind_emp = cns01 + cns02 + cns03 + cns05 + cns06 + cns08)


ind_2004 <- city_lehd_ind %>% filter(year == "2004") %>% 
  group_by(namelsad) %>% summarize(ind_emp2004 = sum(ind_emp))

city_lehd_ind <- city_lehd_ind %>% 
  group_by(namelsad, placefp, year) %>% 
  summarise_if(is.numeric, sum)

city_lehd_ind <- city_lehd_ind %>% 
  left_join(ind_2004, by = "namelsad" ) %>% 
  distinct()

#calculate the industrial employment index values-------
city_lehd_ind <- city_lehd_ind %>% 
  mutate(ind_2004_idx = (ind_emp/ind_emp2004)*100)

city_lehd_ind <- city_lehd_ind %>% ungroup() %>% 
  mutate(ind_share = ind_emp/c000)

city_lehd_ind <- city_lehd_ind %>% 
  mutate(Policy = ifelse(namelsad %in% 
  c("Los Angeles city", "Chicago city", "San Diego city", "Jacksonville city", "San Francisco city", "Seattle city", "Baltimore city", "Portland city", "San Jose city", "Minneapolis city", "New York city"), 1, 0))

city_lehd_ind <- city_lehd_ind %>% 
  mutate(city_name = gsub("city", "", city_lehd_ind$namelsad))

city_lehd_ind <- city_lehd_ind %>% 
  mutate(city_name_short = case_when(city_name == "Indianapolis  (balance)" ~ "Indianapolis",
                                     city_name == "Louisville/Jefferson County metro government (balance)" ~ "Louisville",
                                     city_name == "Nashville-Davidson metropolitan government (balance)" ~ "Nashville",
                                     TRUE ~ city_name))

city_lehd_ind$namelsad <- stringr::str_trim(city_lehd_ind$namelsad, side = "both")
city_lehd_ind$city_name <- stringr::str_trim(city_lehd_ind$city_name, side = "both")
city_lehd_ind$city_name_short <- stringr::str_trim(city_lehd_ind$city_name_short, side = "both")

city_lehd_ind <- city_lehd_ind %>% 
  mutate(ind_emp_thousands = ind_emp/1000)

#calc industrial employment change 2004-2015-----

ind_2004_2015_emp <- city_lehd_ind %>% 
  filter(year == "2004" | year == "2015") %>% 
  select(city_name, city_name_short, year, ind_emp_thousands) %>% 
  tidyr::spread(key = year, value = ind_emp_thousands, sep = "_")

ind_2004_2015_emp <- ind_2004_2015_emp %>% 
  mutate(ind_emp_change = (year_2015 - year_2004)/year_2015)

ind_2004_2015_emp <- ind_2004_2015_emp %>% 
  select(city_name_short, ind_emp_change)

city_lehd_ind <- city_lehd_ind %>% 
  left_join(ind_2004_2015_emp, 
            by = c("city_name_short" = "city_name_short"))

city_lehd_ind <- city_lehd_ind %>% 
  mutate(emp_trend = case_when(ind_emp_change < 0 ~ "Decreasing Trend",
                               ind_emp_change >= 0 ~ "Increasing Trend"))

#industriel total employment line---------------

ind_tot_emp_line <- ggplot(city_lehd_ind, 
                           aes(year, ind_emp_thousands, 
                               group = city_name_short, 
                               colour = emp_trend)) +
  geom_line() + theme_minimal() + 
  theme(legend.position = "none", 
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 6)) +
  labs(title = "Total Industrial Employment for 48 Large Cities, 2004-2015",
       x = "Year", y = "Industrial Employment (thousands)", 
       caption = "Source: LEHD WAC Files 2004-2015") +
  facet_wrap(~ forcats::fct_reorder(city_name_short, ind_emp_change), 
             ncol = 6, scales = "free") +
  scale_x_discrete(limits = c("2004", "2007", "2010", "2015")) +
  scale_y_continuous(labels = scales::comma)


#industry employment share graph

ind_2015 <- city_lehd_ind %>% filter(year == "2015")

ind_tot_ind_emp_lolli <- ggplot(ind_2015, 
                                aes(x = forcats::fct_reorder(city_name_short, ind_emp_thousands), 
                                    y = ind_emp_thousands)) +
  geom_lollipop(point.colour = "blue3") + theme_minimal() +
  labs(title = "Total Industrial Employment, 2015",
       x = "City", y = "Total Employment (thousands)",
       caption = "Source: LEHD WAC Files 2015") +
  scale_y_continuous(labels = scales::comma) +
  theme(panel.grid.minor = element_blank()) +
  coord_flip()

ind_share_emp_lolli <- ggplot(ind_2015, 
                              aes(x = forcats::fct_reorder(city_name_short, ind_share), 
                                  y = ind_share)) +
  geom_lollipop(point.colour = "blue3") + theme_minimal() +
  labs(title = "Large City Industrial Employment Share, 2015",
       x = "City", y = "Employment Share (%)",
       caption = "Source: LEHD WAC Files 2015") +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.grid.minor = element_blank()) +
  coord_flip()

rm(city_lehd_ind)
#pmd_differences-----------

city_pmd_emp <- city_lehd %>% 
  select(1:8, cns05, cns06, cns08, placefp, namelsad, pmd_policy) %>% 
  group_by(namelsad, pmd_policy, year) %>% summarise_if(is.numeric, sum)


city_pmd_emp <- city_pmd_emp %>% 
  mutate(ind_emp = cns01 + cns02 + cns03 + cns05 + cns06 + cns08)

city_pmd04 <- city_pmd_emp %>% filter(year == 2004) %>% 
  select(namelsad, pmd_policy, mfg_2004 = cns05, ind_2004 = ind_emp)

city_pmd_emp <- city_pmd_emp %>% 
  left_join(city_pmd04, 
            by = c("namelsad" = "namelsad", "pmd_policy" = "pmd_policy"))

city_pmd_emp <- city_pmd_emp %>% 
  mutate(mfg_idx = (cns05/mfg_2004)*100, ind_idx = (ind_emp/ind_2004)*100)

pmd_cities <- city_pmd_emp %>%  
  filter(namelsad %in% c("Baltimore city", "Chicago city", "San Diego city", 
                         "New York city",
                         "Jacksonville city", "Seattle city", "San Jose city",
                         "Minneapolis city",
                         "San Francisco city", "Portland city"))

pmd_cities <- pmd_cities %>% 
  mutate(city_name = stringr::str_replace(namelsad, "city", ""))


pmd_lehd <- city_lehd %>% 
  filter(namelsad%in% c("Baltimore city", "Chicago city", "San Diego city", 
                        "New York city",
                        "Jacksonville city", "Seattle city", "San Jose city",
                        "Minneapolis city","San Francisco city", 
                        "Portland city")) %>% 
  mutate(city_name = stringr::str_replace(namelsad, "city", ""))

#mean/median mfg and industrial employment by pmd-----

pmd_emp <- city_lehd %>% 
  select(1:8, cns05, cns06, cns08, placefp, namelsad, pmd_policy) 

pmd_emp <- pmd_emp %>% 
  mutate(ind_emp = cns01 + cns02 + cns03 + cns05 + cns06 + cns08) %>% 
  mutate(city_name = stringr::str_replace(namelsad, "city", ""))



#pmd vs non-pmd 

pmd_med_emp <- pmd_emp %>% filter(ind_emp > 0) %>% 
  group_by(pmd_policy, year) %>% 
  mutate(mfg_per = cns05/c000, ind_per = ind_emp/c000) %>% 
  summarise(avg_mfg_emp = mean(cns05, na.rm = TRUE),
            med_mfg_emp = median(cns05, na.rm = TRUE),
            avg_ind_emp = mean(ind_emp, na.rm = TRUE),
            med_ind_emp = median(ind_emp, na.rm = TRUE),
            avg_mfg_per = mean(mfg_per, na.rm = TRUE), 
            avg_ind_per = mean(ind_per, na.rm = TRUE))


  
  
#pmd faceted line plot and 2015 boxplot-----

pmd_mfg_idx_facet <- ggplot(pmd_cities, aes(x = year, y = mfg_idx, 
                            group = pmd_policy,
                            fill = pmd_policy, color = pmd_policy)) 

pmd_mfg_idx_facet <- pmd_mfg_idx_facet + geom_line() + theme_minimal() + 
  theme(legend.position = "bottom", 
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 6),
        legend.title = element_blank()) +
  labs(title = "Change in Mfg. Employment in Cities with PMDs, 2004-2015",
       x = "Year", y = "Manufacturing Employment index (2004)", 
       caption = "Source: LEHD WAC Files 2004-2015",
       fill = "PMD Policy") +
  facet_wrap(~ city_name, ncol = 2, scales = "free") +
  scale_x_discrete(limits = c("2004", "2007", "2010", "2015")) +
  scale_y_continuous(labels = scales::comma)



pmd_mfg_emp_share <- ggplot(pmd_med_emp, aes(x = year, y = avg_mfg_per,
                        group = pmd_policy, fill = pmd_policy, 
                        color = pmd_policy)) +
  geom_line() + theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 6)) +
  labs(title = "PMD Average Mfg. Employment Share is Much Higher than Non-PMDs",
       x= "Year", y = "Average Employment Share",
       caption = "Source: LEHD WAC Files 2015") +
  scale_y_continuous(labels = scales::percent) +
  scale_shape_manual(guide_legend(title = "PMD Policy"))

pmd_ind_emp_share <- ggplot(pmd_med_emp, 
                      aes(x = year, y = avg_ind_per,group = pmd_policy, 
                      fill = pmd_policy, color = pmd_policy)) +
  geom_line() + theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 6),
        legend.title = "PMD Policy") +
  labs(title = "PMD Average Employment Share is Much Higher than Non-PMDs",
       x= "Year", y = "Average Employment Share",
       caption = "Source: LEHD WAC Files 2015") +
  scale_y_continuous(labels = scales::percent)



dbDisconnect(con)


