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
        maxId = 999999999
    }
        
    response <- fetchUntappd( 
        pathStr, 
        qryStr, 
        writeFile, 
        fileName = paste( "brewery.checkins", breweryId, maxId, "json",sep="." )
    )
}
