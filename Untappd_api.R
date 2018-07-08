setwd("/Users/christopherballenger/Documents/Data Science/MSDS 6306/Projects/CaseStudy2")

library(httr)
library(jsonlite)

#Include Environment Variables
source("env.R")
# 
# 
# method_url <- paste( base_url, "user/checkins/chrisballenger",sep="")
# 
# 
# result <- GET(request)
# user <- fromJSON(content(result,"text") )
# 
# method_url <- user$response$pagination$next_url
# 
# beers <- user$response$checkins$items$beer
# dim(beers)
# 
# # paginate
# request <- paste(method_url,"&",key,sep="")
# result <- GET(request)
# user <- fromJSON(content(result,"text") )
# beers <- user$response$checkins$items$beer
# dim(beers)

fetchUntappd <- function(method = "", qryStr = "", writeFile = FALSE){
    base_url <- "https://api.untappd.com/v4/"
    key <- paste("client_id=",CLIENT_ID,"&client_secret=",CLIENT_SECRET,sep="")
    requestUrl <- URLencode( paste(base_url, method,"?",key,"&",qryStr,sep="") )
    
    response <- GET( requestUrl )
    if(writeFile){
        write( content( response, "text"), paste( gsub( "/", "", method ), "qrtStr", "json",sep="." ) ) 
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

r <- fetchUntappd( "search/brewery", qryStr="q=Community", TRUE )
r
