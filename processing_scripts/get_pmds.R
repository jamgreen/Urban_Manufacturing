if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(rio, stringr, dplyr, sf)

#grab restricted zones by filtering on code and write
#to new shapefiles, eventually will need to re-project and combine


#zoning code for bmore seems to lack marine overlay
#went to baltimore open data site to d/l marine overlay
#link:https://data.baltimorecity.gov/Geographic/MIZOD/hkck-wphk

#baltimore
mizod <- read_sf("data/spatial/zoning/data/mizod.shp")
mizod$City <- "Baltimore"
st_write(mizod, "data/spatial/zoning/data/Baltimore_PMD.shp", delete_dsn = TRUE)

#portland
pdx <-  st_read(unzip("data/spatial/zoning/data/Portland_OR.zip"), quiet = F)
pdx <- pdx %>% filter(Zoning %in% c("IG1", "IG2", "IH"))
pdx <- pdx %>%  group_by(Zoning, Label) %>% summarise() %>% ungroup() %>% 
  st_as_sf()
pdx$City <- "Portland"
st_write(pdx, "data/spatial/zoning/data/Portland_PMD.shp", delete_dsn = TRUE)
file.remove(list.files(pattern = "Portland_OR*",recursive = F))

#boston
boston <- st_read(unzip("data/spatial/zoning/data/Boston_MA.zip"), quiet = F)
mer <- boston %>% filter(grepl("MARITIME ECONOMY RESERVE", Zoning))
mer$City <- "Boston"
st_write(mer, "data/spatial/zoning/data/Boston_PMD.shp", delete_dsn = TRUE)
file.remove(list.files(pattern = "Boston_MA*",recursive = F))

#chicago
chicago <- st_read(unzip("data/spatial/zoning/data/Chicago_IL.zip"), quiet = F)
chicago_pmd <- chicago %>% filter(Label == "Planned Manufacturing Districts")
chicago_pmd$City <- "Chicago"
chicago_pmd <- chicago_pmd %>% group_by(Zoning, Label) %>% summarise() %>% ungroup() %>%  
  st_as_sf()
st_write(chicago_pmd, "data/spatial/zoning/data/Chicago_PMD.shp", delete_dsn = TRUE)
file.remove(list.files(pattern = "Chicago_IL*"), recursive = F)

#san diego
sandiego <- st_read(unzip("data/spatial/zoning/data/Prime_Industrial_Lands.zip"), quiet = F)
sandiego$City <- "San Diego"
st_write(st_union(sandiego, by_feature = FALSE), "data/spatial/zoning/data/SanDiego_PMD.shp", delete_dsn = TRUE)
file.remove(list.files(pattern = "Prime_Industrial*",recursive = F))


#san jose
san_jose <- st_read(unzip("data/spatial/zoning/data/ZONING_201607291505515388.zip"), quiet = F)
san_jose_pmd <- san_jose %>% filter(ZONING == "LI" | ZONING == "HI" | ZONING == "LI(PD)" | ZONING == "HI(PD)")
san_jose_pd <- san_jose_pmd %>% filter(!is.na(PD_USE) & PD_USE == "Ind")
san_jose_pmd <- san_jose_pmd %>%  filter(is.na(PD_USE))
san_jose_pmd <- rbind(san_jose_pmd, san_jose_pd)
san_jose_pmd <- san_jose_pmd %>% group_by(ZONING) %>% summarise() %>% ungroup() %>% 
  st_as_sf()
san_jose_pmd$City <- "San Jose"
st_write(san_jose_pmd, "data/spatial/zoning/data/SanJose_PMD.shp", delete_dsn = TRUE)
file.remove(list.files(pattern = "ZONING_201*"), recursive = F)

#jacksonville
jacksonville <- st_read(unzip("data/spatial/zoning/data/jacksonville_pmd.zip")) 
jacksonville$City <- "Jacksonville"
st_write(jacksonville, "data/spatial/zoning/data/Jacksonville_PMD.shp", delete_dsn = TRUE)
file.remove(list.files(pattern = "ITAC*"), recursive = F)



#san francisco
sanfran <- st_read(unzip("data/spatial/zoning/data/SanFrancisco_CA.zip"), quiet = F)
sanfran_pmd <- sanfran %>% filter(grepl("PDR*", Zoning))
sanfran_pmd <- sanfran_pmd %>% group_by(Zoning, Label) %>% summarise() %>% ungroup() %>% 
  st_as_sf()
st_write(sanfran_pmd, "data/spatial/zoning/data/SanFrancisco_PMD.shp", delete_dsn = TRUE)
file.remove(list.files(pattern = "SanFrancisco*"), recursive = F)

#new york city, pre-processed beforehand
#seattle, pre-processed beforehand
