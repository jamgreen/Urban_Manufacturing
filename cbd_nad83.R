#get CBD points
#had to repoejct the shapefile using GDAL in bash 
if(!require(pacman)){install.packages("pacman"); library(pacman)}
p_load(sf,ggmap, dplyr)

cbd <- tibble::tribble(
                           ~City, ~State,
                   "Los Angeles",   "CA",
                       "Chicago",   "IL",
                       "Houston",   "TX",
                  "Philadelphia",   "PA",
                       "Phoenix",   "AZ",
                   "San Antonio",   "TX",
                     "San Diego",   "CA",
                        "Dallas",   "TX",
                      "San Jose",   "CA",
                        "Austin",   "TX",
                  "Jacksonville",   "FL",
                  "Indianapolis",   "IN",
                 "San Francisco",   "CA",
                      "Columbus",   "OH",
                    "Fort Worth",   "TX",
                     "Charlotte",   "NC",
                       "Detroit",   "MI",
                       "El Paso",   "TX",
                       "Memphis",   "TN",
                        "Boston",   "MA",
                       "Seattle",   "WA",
                    "Washington",   "DC",
            "Nashville-Davidson",   "TN",
                     "Baltimore",   "MD",
          "Lousiville-Jefferson",   "KY",
                      "Portland",   "OR",
                 "Oklahoma City",   "OK",
                     "Milwaukee",   "WI",
                     "Las Vegas",   "NV",
                   "Albuquerque",   "NM",
                        "Tucson",   "AZ",
                        "Fresno",   "CA",
                    "Sacramento",   "CA",
                    "Long Beach",   "CA",
                   "Kansas City",   "MO",
                          "Mesa",   "AZ",
                "Virginia Beach",   "VA",
                       "Atlanta",   "GA",
              "Colorado Springs",   "CO",
                       "Raleigh",   "NC",
                         "Omaha",   "NE",
                         "Miami",   "FL",
                       "Oakland",   "CA",
                         "Tulsa",   "OK",
                   "Minneapolis",   "MN",
                     "Cleveland",   "OH",
                       "Wichita",   "KS",
                     "Arlington",   "TX",
                        "Denver",   "CO",
                      "New York",   "NY"
         )

cbd$FullName <- paste0(cbd$City,","," ",cbd$State)

locations <- geocode(cbd$FullName, output = "latlon", source = "google")
#locations2 <- geocode(cbd$FullName, output = "all", source = "dsk")
cbd$lat <- locations$lat
cbd$long <- locations$lon

cbd_Sf <- st_as_sf(cbd, coords = c("long", "lat"), crs = 4326, agr = "identity")

cbd_nad83 <- st_transform(cbd_Sf, 4269)

st_write(cbd_nad83, "cbd.shp")
