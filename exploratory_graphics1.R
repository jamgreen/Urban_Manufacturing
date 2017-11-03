if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(ggplot2, ggthemes, hrbrthemes,viridis, sf, dplyr)

lehd <- readRDS("./data/spatial/lehd_citiesNAD83.rds")
city_growth <- lehd %>% as.data.frame() %>%  group_by(NAME, year) %>% 
  summarise(TotEmp = sum(C000), MfgEmp = sum(CNS05), 
            IndEmp = sum(CNS02 + CNS03 + CNS05 + CNS06 + CNS08))

city_growth <- city_growth %>% group_by(NAME) %>%  
  mutate(MfgBase = first(MfgEmp), IndBase = first(IndEmp),
         MfgIndex = (MfgEmp - MfgBase)/MfgBase * 100,
         IndIndex = (IndEmp - IndBase)/IndBase * 100) %>% ungroup()

city_growth$MfgPer <- city_growth$MfgEmp/city_growth$TotEmp

outlier_cities <- city_growth %>% filter(year == "2015") %>% 
  mutate(MfgIndexZ = scale(MfgIndex),
         IndIndexZ = scale(IndIndex)) %>% 
  select(NAME, MfgIndexZ, IndIndexZ)

city_growth <- left_join(city_growth, outlier_cities)

city_growth <- city_growth %>% 
  mutate(OutlierDummy1 = ifelse(MfgIndexZ > 1, "High", ifelse(MfgIndexZ < -1, "Low", "Average")),
         OutlierDummy2 = ifelse(IndIndexZ > 1, "High", ifelse(IndIndexZ < -1, "Low", "Average")))

city_growth$OutlierDummy1 <- as.factor(city_growth$OutlierDummy1)
levels(city_growth$OutlierDummy1) <- c("Low", "Average", "High")

city_growth$OutlierDummy2 <- as.factor(city_growth$OutlierDummy2)
levels(city_growth$OutlierDummy2) <- c("Low", "Average", "High")

city_growth <- city_growth %>% mutate(Policy = ifelse(NAME %in% c("Los Angeles", "Chicago", "San Diego", "Jacksonville",
                                                                  "San Francisco", "Boston", "Seattle", "Baltimore", "Portland",
                                                                  "San Jose", "Minneapolis", "New York"), 1, 0))

#show general mfg patterns across largest cities

mfg_index_plot <- ggplot(city_growth, aes(year, MfgIndex, group = NAME, color = OutlierDummy1)) + 
  geom_line() +  
  scale_color_manual(values = c("#DCDCDC", "#FF0000", "#0000FF")) +
  theme_ipsum_rc(grid = "Y") +
  labs(title = "Change in Mfg Employment for 48 Large Cities, 2005-2015",
       subtitle = "Mfg. Employment Indexed to 2005 measures",
       x = "Year", y = "Mfg. Employment Index") +
  theme(legend.position = "none")

mfg_index_plot

ind_index_plot <- ggplot(city_growth, aes(year, IndIndex, group = NAME, color = OutlierDummy2)) + 
  geom_line() +  
  scale_color_manual(values = c("#DCDCDC", "#FF0000", "#0000FF")) +
  theme_ipsum_rc(grid = "Y") +
  labs(title = "Change in Industrial Employment for 48 Large Cities, 2005-2015",
       subtitle = "Ind. Employment Indexed to 2005 measures",
       x = "Year", y = "Ind. Employment Index") +
  theme(legend.position = "none")

ind_index_plot

#subset out protective cities and show decling mfg share

policy_cities <- city_growth %>% filter(Policy == 1)

policy_p1 <- ggplot(data = policy_cities, aes(year, MfgPer)) + geom_bar(stat = "identity", fill = "steelblue") +
  theme_ipsum_rc(plot_title_size = 14, strip_text_face = "bold") + 
  facet_wrap(~NAME, scales = "free", ncol = 4) + scale_y_percent() +
  labs(title = "Mfg. Employment Share for Protective Cities", 
       x = "Year", y = "Mfg. Employment Share (%)")

policy_p1
