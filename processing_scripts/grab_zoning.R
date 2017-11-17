#downloaded 47 cities' zoning codes using links cribbed from the page
#source of https://www.policymap.com/data/lincoln-land-data-directory/#Zoning


if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(rio, stringr, purrr, dplyr)

setwd("~/DissProposal/zoning")

citynames <- import("citynames.csv")
citynames$City <- str_replace(citynames$City, " ", "")
citynames$City <- str_replace(citynames$City, "-Davidson", "")
citynames$City <- str_replace(citynames$City, "-Jefferson", "")

citynames <- citynames %>% filter(City %in% 
                                    c("LosAngeles", "Chicago", "SanDiego",
                                      "Jacksonville", "SanFrancisco",
                                      "Boston", "Seattle",
                                      "Washington", "Baltimore",
                                      "Portland", "LongBeach", "Oakland",
                                      "Minneapolis", "NewYork"))


url <- "https://s3.amazonaws.com/nationsland-lincoln/files/"

city_ls <- paste0(url, citynames$City,"_" ,citynames$State ,".zip")
#city_zip <- paste0(citynames$City,".zip")

grab_zoning <- function(cities){
  
  system(paste0("wget ", cities))

  #unzip command failed, need to research why
  #  system(paste0("unzip *.zip"))
}

setwd("~/DissProposal/zoning/data")

map(city_ls, grab_zoning)
