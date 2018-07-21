setwd("/Users/christopherballenger/Documents/Data Science/MSDS 6306/Projects/CaseStudy2")

library(jsonlite)

source("env.R")
source("Untappd_api.R")

# Set Brewery Id
# newBreweryId = 13688 # Peticolas Brewering Company
newBreweryId = 45011 # Nobel Ray Brewing Company

# Do we already have data on it?
paginations <- read.csv("Brewery Checkins.csv")
if( length( which(paginations$breweryId == newBreweryId) ) == 0 ) {
    response <- getBreweryCheckIns( breweryId = newBreweryId, writeFile=TRUE )
    counter <- response$headers$`x-ratelimit-remaining`
    checkins <- fromJSON(content(response,"text") )
    newMaxId <- checkins$response$pagination$max_id
    paginations <- data.frame(breweryId = newBreweryId, maxId = newMaxId)
} else {
    newMaxId <- paginations[length(paginations[paginations$breweryId==newBreweryId,1]),2]
    
    #Get latest JSO
    #Get new max_id
    jsonFile <- paste("/Responses/brewery.checkins",newBreweryId, newMaxId, "json",sep=".")
    checkins <- fromJSON(paste(getwd(), jsonFile, sep=""))
    spiltUrl <- strsplit( checkins$response$pagination$since_url, "=" )
    newMaxId <- spiltUrl[[1]][2]
    newMaxId <- as.numeric(newMaxId)
    counter <- 100
}
# Run Loop
while(counter >= 1){
    #Download JSON
    response <- getBreweryCheckIns( breweryId = newBreweryId, maxId = newMaxId, writeFile=TRUE )
    counter <- response$headers$`x-ratelimit-remaining`
    paginations <- paginations <- rbind( paginations, c(13688,newMaxId) )
    
    #Get new max_id
    checkins <- fromJSON(content(response,"text") )
    spiltUrl <- strsplit( checkins$response$pagination$since_url, "=" )
    newMaxId <- spiltUrl[[1]][2]
    newMaxId <- as.numeric(newMaxId)
    print( counter )
    # paginations
}

write.csv(paginations, "Brewery Checkins.csv", row.names = FALSE)
