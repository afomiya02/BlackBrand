install.packages("googleway")

library(googleway)
library(tidyverse)
library(leaflet)
library(sf)

key <- "" # Will send you key if need be

set_key(key)


something <- google_places(key = key, search_string = 'Food Bank', location=c(36.899761,-76.546286), radius=50000)
results <- something$results


coords1 <- c(37.446883,-76.598472)
coords2 <- c(36.754660,-77.062644)
coords3 <- c(36.807456,-76.126059)
radius <- 50000


results1_page1 <- google_places(key = key, search_string = 'Food Bank, Food Pantry, Food Assistance', location=coords1, radius=radius)
results1_page2 <- google_places(key = key, search_string = 'Food Bank, Food Pantry, Food Assistance', location=coords1, radius=radius, page_token = results1_page1$next_page_token)
results1_page3 <- google_places(key = key, search_string = 'Food Bank, Food Pantry, Food Assistance', location=coords1, radius=radius, page_token = results1_page2$next_page_token)

results2_page1 <- google_places(key = key, search_string = 'Food Bank, Food Pantry, Food Assistance', location=coords2, radius=radius)
results2_page2 <- google_places(key = key, search_string = 'Food Bank, Food Pantry, Food Assistance', location=coords2, radius=radius, page_token = results2_page1$next_page_token)
results2_page3 <- google_places(key = key, search_string = 'Food Bank, Food Pantry, Food Assistance', location=coords2, radius=radius, page_token = results2_page2$next_page_token)

results3_page1 <- google_places(key = key, search_string = 'Food Bank, Food Pantry, Food Assistance', location=coords3, radius=radius)
results3_page2 <- google_places(key = key, search_string = 'Food Bank, Food Pantry, Food Assistance', location=coords3, radius=radius, page_token = results3_page1$next_page_token)
results3_page3 <- google_places(key = key, search_string = 'Food Bank, Food Pantry, Food Assistance', location=coords3, radius=radius, page_token = results3_page2$next_page_token)


result_ids <- c(results1_page1$results$place_id, 
                         results1_page2$results$place_id, 
                         results1_page3$results$place_id,
                         results2_page1$results$place_id,
                         results2_page2$results$place_id,
                         results2_page3$results$place_id,
                         results3_page1$results$place_id,
                         results3_page2$results$place_id,
                         results3_page3$results$place_id)


distinct_result_ids <- distinct(data.frame(result_ids))

foodBankLocation <- data.frame()
for(i in 1:nrow(distinct_result_ids)){
  result.obj <- google_place_details(distinct_result_ids[i,])$result
  result.lat <- result.obj$geometry$location$lat
  result.lng <- result.obj$geometry$location$lng
  result.selection <- data.frame(result.obj[c("name", "formatted_address", "place_id", "url")])
  result.fin <- result.selection %>% mutate(lat = result.lat, lng = result.lng)
  
  foodBankLocation <- rbind(foodBankLocation, result.fin)
}

write.csv(foodBankLocation, "CURR_Work/Pulling Data/GoogleMapsPlaces/FoodBanks.csv")

# <!---------------------------------- PLAYGROUND ---------------------------------------->





