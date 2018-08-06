library(httr)

fetchUntappd <- function(method = "", qryStr = "", writeFile = FALSE, fileName = ""){
    base_url <- "https://api.untappd.com/v4/"
    key <- paste("client_id=",CLIENT_ID,"&client_secret=",CLIENT_SECRET,sep="")
    requestUrl <- URLencode( paste(base_url, method,"?",key,"&",qryStr,sep="") )
    print(requestUrl)
    response <- GET( requestUrl )
    if(writeFile){
        if( nchar(fileName) == 0 ){
            fileName = paste( gsub( "/", ".", method ), qryStr, "json",sep="." )
        }
        
        if( response$status_code != 200 ){
            fileName <- paste( "errors", fileName, sep="/")
        }
        
        fileName <- paste( "Responses", fileName, sep="/" )
        print(fileName)
        write( content( response, "text"), fileName) 
    }
    response
}

getBrewerySearch <- function(brewery = "", writeFile = FALSE){
    qryStr <- paste( "q=", brewery, sep="" )
    response <- fetchUntappd( "search/brewery", qryStr, writeFile )
    response
    
    # breweries <- fromJSON( content( response,"text" ) )
    # breweries$response$brewery
}

getBreweryCheckIns <- function(breweryId, maxId = 0,  writeFile = FALSE){
    pathStr <- paste("brewery/checkins/",breweryId,sep="")
    qryStr <- ""
    
    if( maxId > 0 ){
        qryStr <- paste("max_id=", maxId, sep="")
    } else {
        # default the fileName maxId to 999999999
        maxId = as.numeric(Sys.time())*1000
    }
        
    response <- fetchUntappd( 
        pathStr, 
        qryStr, 
        writeFile, 
        fileName = paste( "brewery.checkins", breweryId, maxId, "json",sep="." )
    )
}

getBeerCheckIns <- function(beerId, maxId = 0, writeFile = FALSE){
    pathStr <- paste("beer/checkins/",beerId,sep="")
    qryStr <- ""
    if( maxId > 0 ){
        qryStr <- paste("max_id=", maxId, sep="")
    } else {
        # default the fileName maxId to 999999999
        maxId = as.numeric(Sys.time())*1000
    }
    
    response <- fetchUntappd( 
        pathStr, 
        qryStr, 
        writeFile, 
        fileName = paste( "beer.checkins", beerId, maxId, "json",sep="." )
    )
}

# getVenueInfo <- function(venueId, writeFile = FALSE){
#     pathStr <- paste("venue/info/",venueId,sep="")
#     response <- fetchUntappd( 
#         pathStr,
#         writeFile, 
#         fileName = paste( "venue>info", venueId, "json",sep="." )
#     )
# }

readBeerCheckins <- function(checkins = list()){
    items <- checkins$response$checkins$items
    # Loop Through Venue Data since its a list
    venueId <- numeric()
    venueName <- character()
    venueParentCategory <- character()
    venueParentCategoryId <- character()
    venueCategory <- character()
    venueCategoryId <- character()
    venueCategoryIsPrimary <- logical()
    venueCity <- character()
    venueState <- character()
    venueCountry <- character()
    venueLat <- numeric()
    venueLng <- numeric()
    venueFourSquareId <- character()
    venueFourSquareUrl <- character()
    for(i in 1:checkins$response$checkins$count){
        
        if(class(checkins$response$checkins$items[i,]$venue) == "data.frame"){
            venue <- checkins$response$checkins$items[i,]$venue
        } else {
            venue <- checkins$response$checkins$items[i,]$venue[[1]]
        }
        
        if( length(venue) > 0 ){
            venueId[i] <- venue$venue_id
            venueName[i] <- venue$venue_name
            venueParentCategory[i] <- ifelse(!is.null(venue$primary_category),venue$primary_category,NA)
            venueParentCategoryId[i] <- ifelse(!is.null(venue$parent_category_id),venue$parent_category_id,NA)
            if(venue$categories$count > 0){
                if(class(venue$categories$items) == "data.frame"){
                    categories <- venue$categories$items[1,]
                } else {
                    categories <- venue$categories$items[[1]]
                }
                venueCategory[i] <- ifelse(!is.null(categories$category_name),categories$category_name,NA)
                venueCategoryId[i] <- ifelse(!is.null(categories$category_id),categories$category_id,NA)
                venueCategoryIsPrimary[i] <- ifelse(!is.null(categories$is_primary),categories$is_primary,NA)
            } else {
                venueCategory[i] <- NA
                venueCategoryId[i] <- NA
                venueCategoryIsPrimary[i] <- NA
            }
            venueCity[i] <- ifelse(!is.null(venue$location$venue_city),venue$location$venue_city,NA)
            venueState[i] <- ifelse(!is.null(venue$location$venue_state),venue$location$venue_state,NA)
            venueCountry[i] <- ifelse(!is.null(venue$location$venue_country),venue$location$venue_country,NA)
            venueLat[i] <- ifelse(!is.null(venue$location$lat),venue$location$lat,NA)
            venueLng[i] <- ifelse(!is.null(venue$location$lng),venue$location$lng,NA)
            venueFourSquareId[i] <- ifelse(!is.null(venue$foursquare$foursquare_id),venue$foursquare$foursquare_id,NA)
            venueFourSquareUrl[i] <- ifelse(!is.null(venue$foursquare$foursquare_url),venue$foursquare$foursquare_url,NA)
        } else {
            venueId[i] <- NA
            venueName[i] <- NA
            venueParentCategory[i] <- NA
            venueParentCategoryId[i] <- NA
            venueCategory[i] <- NA
            venueCategoryId[i] <- NA
            venueCategoryIsPrimary[i] <- NA
            venueCity[i] <- NA
            venueState[i] <- NA
            venueCountry[i] <- NA
            venueLat[i] <- NA
            venueLng[i] <- NA
            venueFourSquareId[i] <- NA
            venueFourSquareUrl[i] <- NA
        }
    }

    data.frame(
        breweryId = items$brewery$brewery_id,
        breweryName = items$brewery$brewery_name,
        beerId = items$beer$bid,
        beerName = items$beer$beer_name,
        beerDescription = items$beer$beer_description,
        beerStyle = items$beer$beer_style,
        beerAbv = items$beer$beer_abv,
        beerIbu = items$beer$beer_ibu, 
        checkinId = items$checkin_id,
        createdAt = items$created_at,
        ratingScore = items$rating_score,
        venueId = venueId,
        venueName = venueName,
        venueParentCategory = venueParentCategory,
        venueParentCategoryId = venueParentCategoryId,
        venueCategory = venueCategory,
        venueCategoryId = venueCategoryId,
        venueCategoryIsPrimary = venueCategoryIsPrimary,
        venueCity = venueCity,
        venueState = venueState,
        venueCountry = venueCountry,
        venueLat = venueLat,
        venueLng = venueLng,
        venueFourSquareId = venueFourSquareId,
        venueFourSquareUrl = venueFourSquareUrl
    )
}