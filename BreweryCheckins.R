setwd("/Users/christopherballenger/Documents/Data Science/MSDS 6306/Projects/CaseStudy2")

library(jsonlite)

source("env.R")
source("Untappd_api.R")

# Set Brewery Id
newBreweryId = 13688 # Peticolas Brewering Company
# newBreweryId = 45011 # Nobel Ray Brewing Company
# newBreweryId = 48372 # Community Brewing Company
# newBreweryId <- 29815 # Four Corners
# newBreweryId <- 185850 # Braindead Brewing
# newBreweryId <- 134551 # Texas Ale Project
# newBreweryId <- 11028 # Deep Ellum Brewing Company

# Get the jsons we already saved
jsonObjects <- list.files("Responses/")
maxIds <- numeric()
breweryIds <- numeric()
filenames <- character()
i = 1
while( i <= length(jsonObjects) ){
    x <- strsplit(jsonObjects[i], "\\.")
    if( jsonObjects[i] != "errors"){
        breweryIds[i] <- as.numeric(x[[1]][3])
        maxIds[i] <- as.numeric(x[[1]][4])
    }
    i = i + 1
}
paginations <- data.frame(breweryId = breweryIds, maxId = maxIds)
paginations <- paginations[
    with(paginations, order(breweryId, maxId,decreasing = TRUE)),]

if( length( which(paginations$breweryId == newBreweryId) ) == 0 ) {
    
    response <- getBreweryCheckIns( breweryId = newBreweryId, writeFile=TRUE )
    counter <- response$headers$`x-ratelimit-remaining`
    checkins <- fromJSON(content(response,"text") )
    newMaxId <- checkins$response$pagination$max_id
    if( length( paginations$breweryId ) == 0 ) {
        paginations <- data.frame(breweryId = newBreweryId, maxId = newMaxId)
    } else {
        paginations <- paginations <- rbind( paginations, c(newBreweryId,newMaxId) )
    }
} else {
    newMaxId <- tail(paginations[paginations$breweryId==newBreweryId,2],n=1)
    
    #Get latest JSO
    #Get new max_id
    jsonFile <- paste("/Responses/brewery.checkins",newBreweryId, newMaxId, "json",sep=".")
    checkins <- fromJSON(paste(getwd(), jsonFile, sep=""))
    spiltUrl <- strsplit( checkins$response$pagination$next_url, "=" )
    newMaxId <- spiltUrl[[1]][2]
    newMaxId <- as.numeric(newMaxId)
    counter <- 100
}
# Run Loop
while(counter >= 1){
    #Download JSON
    response <- getBreweryCheckIns( breweryId = newBreweryId, maxId = newMaxId, writeFile=TRUE )
    
    if( response$status_code == 200 ){
        counter <- response$headers$`x-ratelimit-remaining`
        paginations <- rbind( paginations, c(13688,newMaxId) )
        
        #Get new max_id
        checkins <- fromJSON(content(response,"text") )
        spiltUrl <- strsplit( checkins$response$pagination$next_url, "=" )
        newMaxId <- spiltUrl[[1]][2]
        newMaxId <- as.numeric(newMaxId)
        print( counter )
    } else {
        print ("Status Code Error")
        break
    }
}

write.csv(paginations, "Brewery Checkins.csv", row.names = FALSE)

