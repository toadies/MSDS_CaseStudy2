setwd("/Users/christopherballenger/Documents/Data Science/MSDS 6306/Projects/CaseStudy2")

library(jsonlite)
source("env.R")
source("Untappd_api.R")

## get beer list from a brewery id
breweries <- read.csv("breweries.csv")
breweries <- breweries[order(breweries$lastCreatedDate),]

direction <- "new"

i <- 1
counter <- 100
while(i <= length(breweries[,1]) && counter > 0){
    
    if(breweries[i,]$totalCheckins >= 200){
        
        # get older records
        if(direction == "old"){
            maxId <- breweries[i,]$minId
        } else {
            maxId <- 0
        }
        
        # request new records
        err <- 0
        while(err == 0){
            response <- getBeerCheckIns( beerId = breweries[i,]$beerId, maxId = maxId, writeFile=TRUE )
            counter <- response$headers$`x-ratelimit-remaining`
            
            if( response$status_code == 200 ){
                
                #Get new max_id
                checkins <- fromJSON(content(response,"text") )
                
                if( checkins$response$checkins$count == 25 ) {
                        spiltUrl <- strsplit( checkins$response$pagination$next_url, "=" )
                        maxId <- spiltUrl[[1]][2]
                        maxId <- as.numeric(maxId)
                        err <- 0
 
                    if( direction == "new" && maxId >= breweries[i,]$maxId){
                        err <- 0
                    } else {
                        err <- 1
                    }
                    
                } else {
                    # Fail \ Skip to next brewery if no more data
                    err <- 1
                }
            } else {
                # Fail if not Status Code 200
                err <- 1
            }
            print(counter)
        }
    }
    i = i + 1
}