library(jsonlite)
library(tibble)
library(dplyr)
library(tidyr)
setwd("C:/Users/Anand/Documents/SMU/DDS/GITHUB/Case-Study-2/MSDS_CaseStudy2/")

path= "Responses/1/"
files <- list.files(path, pattern="brewery.checkins.*.json", full.names=TRUE)
checkInData <- lapply(files, function(x) read_json(path=x,simplifyVector=TRUE)) # a list in which each element is one of the JSON files
checkInData <- lapply(checkInData, function(x) {data.frame(x$response$checkins$items)}) 
my_data <- data.frame()
for(i in 1 : length(checkInData)){
  beerCheckIn_flat <- flatten(checkInData[[i]])
  beerCheckIn_flat <- as_data_frame(beerCheckIn_flat)
  beer_data <- beerCheckIn_flat %>% 
    select(starts_with("checkin_id"),starts_with("rating_score"),
           starts_with("user.location"), starts_with("beer.bid"),starts_with("beer.beer_name"),
           starts_with("beer.beer_abv"), starts_with("beer.beer_ibu"), 
           starts_with("beer.beer_description"), starts_with("beer.beer_style"),
           starts_with("brewery.brewery_id"),starts_with("brewery.brewery_name"),
           starts_with("brewery.location.lat"),starts_with("brewery.location.lng"))
  #starts_with("venue[1].foursquare.foursquare_id"),starts_with("venue[1].foursquare.foursquare_url"),
  #starts_with("venue[1].venue_id"),starts_with("venue[1].venue_name"))
  #starts_with("venue.categories.items"))
  my_data <- bind_rows(my_data, beer_data)
}
write.csv(my_data, file = "Beer_CheckIns.csv")
  