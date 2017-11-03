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
  mutate(OutlierDummy1 = ifelse(MfgIndexZ > 1, 1, 0),
         OutlierDummy2 = ifelse(IndIndexZ > 1, 1, 0))


p2 <- ggplot(city_growth, aes(year, MfgIndex, group = NAME)) + 
  geom_line(colour = "grey") + 
  geom_line(aes(year, MfgIndex, group = NAME, 
                             color = factor(OutlierDummy1)))
p2
