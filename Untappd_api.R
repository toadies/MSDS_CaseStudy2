setwd("/Users/christopherballenger/Documents/Data Science/MSDS 6306/Projects/CaseStudy2")

library(httr)
library(jsonlite)

#Include Environment Variables
source("env.R")

base_url <- "https://api.untappd.com/v4/"
method_url <- paste( base_url, "user/checkins/chrisballenger",sep="")

key <- paste("client_id=",CLIENT_ID,"&client_secret=",CLIENT_SECRET,sep="")

request <- paste(method_url,"?",key,sep="")

result <- GET(request)
user <- fromJSON(content(result,"text") )

method_url <- user$response$pagination$next_url

beers <- user$response$checkins$items$beer
dim(beers)

# paginate
request <- paste(method_url,"&",key,sep="")
result <- GET(request)
user <- fromJSON(content(result,"text") )
beers <- user$response$checkins$items$beer
dim(beers)
