setwd("/Users/christopherballenger/Documents/Data Science/MSDS 6306/Projects/CaseStudy2")

library(jsonlite)
library(plyr)
library(dplyr)
source("Untappd_api.R")

result <- data.frame()
jsonObjects <- list.files("Responses/")
length(jsonObjects)
filterObjects <- jsonObjects[grep("*.json",jsonObjects)]
length(filterObjects)
for(i in 1:length(filterObjects) ){
    # if( length(grep("*beer.checkins*",jsonObjects[i] )) >= 1 ){
    checkins <- fromJSON(paste("Responses/", filterObjects[i], sep=""))
    if( checkins$response$checkins$count > 0 ){
        result <- rbind( result, readBeerCheckins(checkins) )
    }
    # }
    print( paste(i, "-", dim(result)[1] ) )
}

dim(result)

# remove duplicates
result.distinct <- unique(result)
names(result.distinct)
result.distinct.count <- plyr::count(result.distinct$checkinId)
result.distinct <- dplyr::inner_join(result.distinct, result.distinct.count, by=c("checkinId"="x"))
dim(result.distinct)
result.distinct <- result.distinct[result.distinct$freq == 1,]
dim(result.distinct)
#Identify if checkin was at brewery
breweryIdToVenueId <- data.frame(
    breweryId = c(48372,29815,11028,185850,45011,134551,13688), 
    venueId = c(556774,390025,113377,1950200,2308401,2482920,219154),
    isBreweryLocation = rep("Y",7)
)

result.distinct <- left_join(
    result.distinct,
    breweryIdToVenueId
)
dim(result.distinct)
summary(result.distinct)
result.distinct$isBreweryLocation <- as.character(result.distinct$isBreweryLocation)
result.distinct[is.na(result.distinct$isBreweryLocation)&!is.na(result.distinct$venueId),31] <- "N"

sum(result.distinct$isBreweryLocation=="Y",na.rm=T)
sum(result.distinct$isBreweryLocation=="N",na.rm=T)
sum(is.na(result.distinct$isBreweryLocatio))

# Simplfy Beer Styles with a BeerCategory
beerCategories <- read.csv("beerstyle.csv", stringsAsFactors = FALSE)
# Remove Factors
result.distinct$beerStyle <- as.character(result.distinct$beerStyle)
result.distinct <- left_join(result.distinct, beerCategories, by = c("beerStyle" = "beerStyle"))
dim(result.distinct)

# Save Data
names(result.distinct)
write.csv(result.distinct, "data/checkins.csv", row.names=FALSE)


beerIds <- plyr::count(result.distinct, "beerId")
distinctBeerIds <- beerIds[order(beerIds$freq, decreasing = F),]



# Get min \ max checkin details for future checkin data gathering
maxIds <- aggregate( result.distinct$checkinId, list( result.distinct$beerId ), max )
names(maxIds) <- c("beerId","maxId")
minIds <- aggregate( result.distinct$checkinId, list( result.distinct$beerId ), min )
names(minIds) <- c("beerId","minId")
lastCreatedDate <- aggregate( 
    strptime( result.distinct$createdAt, "%a, %d %b %Y %T"),
    list(result.distinct$beerId ), 
    max,
    na.rm=TRUE 
)
names(lastCreatedDate) <- c("beerId","lastCreatedDate")

names(result.distinct)

breweries <- unique(result.distinct[,1:8])
str(breweries)
breweries <- inner_join(breweries, distinctBeerIds, by = c("beerId" = "beerId") )
breweries <- inner_join(breweries, maxIds, by = c("beerId" = "beerId") )
breweries <- inner_join(breweries, minIds, by = c("beerId" = "beerId") )
breweries <- inner_join(breweries, lastCreatedDate, by = c("beerId" = "beerId") )

names(breweries)
write.csv(breweries, "data/breweries.csv",row.names = FALSE)

head(breweries)

# 15663
# 15688
# 17813
# 19698
# 22797
# 24975
# 27129
# 29252
# 31277
# 33268
# 35182
# 35275
# 36879
# 38053
dim(result.distinct)

length(result.distinct$checkinId) - length( unique(result.distinct$checkinId) )


