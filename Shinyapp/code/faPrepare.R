library(tidyverse)
library(readxl)
setwd("CURR_Work/Pulling Data/FoodAtlas/")
rm(list = ls())

#<!----------------------------------------------------->
#<- This block of code is just for the Prepare.R 
#<- but the rest of the file will be merging it
#<!----------------------------------------------------->

foodAccess15 <- read_xlsx("Original/FoodAccessResearchAtlasData2015.xlsx", sheet = 3) %>% mutate(year = 2015)
foodAccess19 <- read_xlsx("Original/FoodAccessResearchAtlasData2019.xlsx", sheet = 3) %>% mutate(year = 2019)

foodAccess15 <- foodAccess15 %>% rename(Pop2010 = POP2010)

masterFoodAccess <- rbind(foodAccess15, foodAccess19)

#<!----------------------------------------------------->
#<!----------------------------------------------------->


foodAccess_Va <- masterFoodAccess %>% filter(State == "Virginia")
counties <- c("Chesapeake", "Chesapeake city", 
              "Franklin city", "Franklin City", 
              "Hampton", "Hampton city",
              "Newport News", "Newport News city",
              "Norfolk", "Norfolk city",
              "Poquoson", "Poquoson city", 
              "Portsmouth", "Portsmouth city",
              "Suffolk", "Suffolk city",
              "Virginia Beach", "Virginia Beach city",
              "Williamsburg", "Williamsburg city",
              "Gloucester", "Gloucester County",
              "Isle of Wight", "Isle of Wight County",
              "James City", "James City County",
              "Mathews", "Mathews County",
              "Southampton", "Southampton County",
              "York", "York County")

foodAccess_VaTr <- foodAccess_Va %>% filter(County %in% counties)

#<!----------------------------------------------------------------->
#<!----------------- Getting Hyesoo's Variables--------------------->
#<!----------------------------------------------------------------->

desiredColumns <- "CensusTract
State
County
Urban
year
OHU2010
PCTGQTRS
LILATracts_1And10
LILATracts_halfAnd10
LILATracts_1And20
LILATracts_Vehicle
LowIncomeTracts
PovertyRate
LA1and10
LAhalfand10
LA1and20
LATracts_half
LATracts1
LATracts10
LATracts20
LATractsVehicle_20
LAPOP1_10
LAPOP05_10
LAPOP1_20
LALOWI1_10
LALOWI05_10
LALOWI1_20
lapophalf
lalowihalf
laseniorshalf
laseniorshalfshare
lablackhalf
lablackhalfshare
laasianhalfshare
lahunvhalf
lahunvhalfshare
lasnaphalf
lasnaphalfshare
lapop1
lapop1share
lalowi1
lalowi1share
laseniors1
laseniors1share
lawhite1
lawhite1share
lablack1
lablack1share
lahunv1
lahunv1share
lasnap1
lasnap1share
lalowi10
lalowi10share
laseniors10
laseniors10share
lablack10
lablack10share
lahunv10
lahunv10share
lasnap10
lasnap10share
lapop20
lapop20share
lalowi20
lalowi20share
lablack20
lablack20share
lahunv20
lahunv20share
lasnap20
lasnap20share
TractLOWI
TractSeniors
TractBlack
TractHUNV
TractSNAP"

desiredColumns <- unlist(strsplit(desiredColumns, "\n")) 

vatr_foodAccess_FIN <- foodAccess_VaTr %>% select(desiredColumns)


write.csv(x = vatr_foodAccess_FIN, "Working/vatr_foodAccess.csv", row.names = FALSE)
# Commented out because not needed after first is saved


#<!---------------------------------------------------------------->
#< Trying to figure out how to get polygon / shape into file
#<!---------------------------------------------------------------->

rm(list = ls())
setwd("CURR_Work/Pulling Data/FoodAtlas")
library(tidyverse)
library(tidycensus)
library(sf)
library(sp)


foodAccess <- read.csv("Working/vatr_foodAccess.csv", )

hamptonGeoid <- unique(substr(foodAccess$CensusTract, 1, 5))

countyOutlines <- county_laea %>% filter(GEOID %in% hamptonGeoid) %>% st_transform(crs = st_crs("EPSG:4326"))

acsGeoBase <- get_acs(geography = "tract", variables = "B19013_001",
                state = "VA", geometry = TRUE, year = 2019)

useable <- acsGeoBase %>% select(GEOID, NAME, geometry)
useableHampton <- useable %>% filter(substr(GEOID, 1, 5) %in% hamptonGeoid)

geometry <- NULL
for(i in 1:nrow(foodAccess)){
  geometry <- rbind(geometry, st_as_sf(data.frame(useableHampton[useableHampton$GEOID == foodAccess$CensusTract[i], "geometry"])))
}


#<!---------------------------------------------------------------->
#< we have geometry in order, and food Access, we need to merge
#< so that foodAccess is an sf and contians MULTIPOLYGON data
#<!---------------------------------------------------------------->
foodAccess.sf <- NULL
for(i in 1:nrow(foodAccess)){
  foodAccess.sf <- rbind(foodAccess.sf, st_as_sf(merge(foodAccess[i,], geometry[i,])))
}

write_sf(foodAccess.sf, "Distribution/masterData.shp", )
write_sf(st_as_sf(countyOutlines), "Distribution/countyOutlines.shp")

#<!---------------------------------------------------------------->
#<!-                      Example case                        ----->
#<!---------------------------------------------------------------->


example <- foodAccess.sf %>% select(lapophalf)
example$lapophalf <- as.double(example$lapophalf)
plot(example)


#<!---------------------------------------------------------------->
#<!-                      Zip Code Playground                 ----->
#<!---------------------------------------------------------------->

zipCodeOutlines <- read_sf("Original/zipCodeOutlines.shp")
View(zipCodeOutlines)
zipCodeOutlines[,"PERIMETER"] <- zipCodeOutlines$PERIMETER**3
plot(zipCodeOutlines[,"PERIMETER"])



library(leaflet)
my_map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron")
my_map


my_map <- my_map %>%
  addMarkers(lat=36.7682, lng=-76.2875, 
             popup="Chesapeak VA")

my_map <- my_map %>%
  addMarkers(lat=36.6777, lng=-76.9225, 
             popup="Franklin VA")


my_map <- my_map %>%
  addMarkers(lat=37.0299, lng=-76.3452, 
             popup="Hampton VA")

my_map <- my_map %>%
  addMarkers(lat=37.0871, lng=- 76.4730, 
             popup="Newport News  VA")



my_map <- my_map %>%
  addMarkers(lat=36.8508, lng=-76.2859, 
             popup="Norfolk  VA")


my_map <- my_map %>%
 addMarkers(lat=37.1224, lng= -76.4258,
            popup="Poquoson VA")


my_map <- my_map %>%
  addMarkers(lat=36.8754, lng=-76.3683, 
             popup="Portsmouth VA")



my_map <- my_map %>%
  addMarkers(lat=36.7282, lng=-76.5836, 
             popup=" Suffolk VA")



my_map <- my_map %>%
  addMarkers(lat=36.8516, lng=-76.1032, 
             popup=" Virginia Beach VA")


my_map <- my_map %>%
  addMarkers(lat=37.2707, lng=-76.7075, 
             popup="Williamsburg VA")


my_map <- my_map %>%
  addMarkers(lat=37.4128, lng=-76.5026, 
             popup="Gloucester County VA")


my_map <- my_map %>%
  addMarkers(lat=36.9289, lng=-76.6875, 
             popup=" Isle of Wight County VA")


my_map <- my_map %>%
  addMarkers(lat=37.3100, lng=-76.7700, 
             popup="James City County  VA")


my_map <- my_map %>%
  addMarkers(lat=37.3912, lng= -76.3174, 
             popup="Mathews County  VA")


my_map <- my_map %>%
  addMarkers(lat=36.6789, lng= -77.1025, 
             popup=" Southampton County VA")


my_map <- my_map %>%
  addMarkers(lat=37.1304, lng=-76.3869, 
             popup="York County  VA")

countyOutline <- read_sf("Distribution/countyOutlines.shp")
my_map <- my_map %>% 
  addPolylines(data = countyOutline, color = "black", weight = 1.2, smoothFactor = .5,
             fillOpacity = 0, fillColor = "transparent")



my_map






