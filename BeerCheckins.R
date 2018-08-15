setwd("/Users/christopherballenger/Documents/Data Science/MSDS 6306/Projects/CaseStudy2")

library(jsonlite)

source("env.R")
source("Untappd_api.R")
y <- 1


while( y <= length(breweries$beerId) ){
    if( breweries[y,2] < 300){
        # Set Beer Id
        newBeerId <- distinctBeerIds[y,1]
        
        # Get the jsons we already saved
        jsonObjects <- list.files("Responses/")
        maxIds <- numeric()
        beerIds <- numeric()
        filenames <- character()
        i <- 1
        while( i <= length(jsonObjects) ){
            x <- strsplit(jsonObjects[i], "\\.")
            if( length(grep("*beer.checkins*",jsonObjects[i] )) >= 1 && jsonObjects[i] != "errors" ){
                beerIds[i] <- as.numeric(x[[1]][3])
                maxIds[i] <- as.numeric(x[[1]][4])
            }
            i = i + 1
        }
        paginations <- data.frame(beerId = beerIds, maxId = maxIds)
        paginations <- paginations[
            with(paginations, order(beerId, maxId,decreasing = TRUE)),]
        
        if( length( which(paginations$beerId == newBeerId) ) == 0 ) {
            response <- getBeerCheckIns( beerId = newBeerId, writeFile=TRUE )
            counter <- response$headers$`x-ratelimit-remaining`
            checkins <- fromJSON(content(response,"text") )
            if(checkins$response$checkins$count == 25 ) {
                newMaxId <- checkins$response$pagination$max_id
            } else {
                counter <- 0
            }
            if( length( paginations$beerId ) == 0 ) {
                paginations <- data.frame(beerId = newBeerId, maxId = newMaxId)
            } else {
                paginations <- paginations <- rbind( paginations, c(newBeerId,newMaxId) )
            }
        } else {
            newMaxId <- tail(paginations[paginations$beerId==newBeerId,2],n=1)
            
            #Get latest JSO
            #Get new max_id
            jsonFile <- paste("/Responses/beer.checkins",newBeerId, newMaxId, "json",sep=".")
            checkins <- fromJSON(paste(getwd(), jsonFile, sep=""))
            if(checkins$response$checkins$count == 25 ) {
                spiltUrl <- strsplit( checkins$response$pagination$next_url, "=" )
                newMaxId <- spiltUrl[[1]][2]
                newMaxId <- as.numeric(newMaxId)
                counter <- 100
            } else {
                counter <- 0
            }
        }
        # Run Loop
        while(counter >= 1){
            #Download JSON
            response <- getBeerCheckIns( beerId = newBeerId, maxId = newMaxId, writeFile=TRUE )
            
            if( response$status_code == 200 ){
                counter <- response$headers$`x-ratelimit-remaining`
                paginations <- rbind( paginations, c(13688,newMaxId) )
                
                #Get new max_id
                checkins <- fromJSON(content(response,"text") )
                
                if( checkins$response$checkins$count == 25 ) {
                    spiltUrl <- strsplit( checkins$response$pagination$next_url, "=" )
                    newMaxId <- spiltUrl[[1]][2]
                    newMaxId <- as.numeric(newMaxId)
                    if( is.na(newMaxId) ){
                        counter <- 0
                    }
                } else {
                    print( "count is zero" )
                    counter <- 0
                }
                print( counter )
            } else {
                print ("Status Code Error")
                break
            }
        }
    }
    y = y + 1
}
write.csv(paginations, "Beer Checkins.csv", row.names = FALSE)