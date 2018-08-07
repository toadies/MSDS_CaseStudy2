setwd("/Users/christopherballenger/Documents/Data Science/MSDS 6306/Projects/CaseStudy2")

library(jsonlite)
library(plyr)
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

#remove duplicates
result.distinct <- unique(result)

result.distinct.count <- count(result.distinct$checkinId)
result.distinct <- merge(result.distinct, result.distinct.count, by.x="checkinId", by.y="x")

result.distinct <- result.distinct[result.distinct$freq == 1,]

#Identify if checkin was at brewery
breweryIdToVenueId <- data.frame(
    breweryId = c(48372,29815,11028,185850,45011,134551,13688), 
    venueId = c(556774,390025,113377,1950200,2308401,2482920,219154),
    isBreweryLocation = rep("Y",7)
)

result.distinct <- merge(result.distinct, breweryIdToVenueId, by.x = c("breweryId","venueId"), by.y = c("breweryId","venueId"), all.x = TRUE)
result.distinct$isBreweryLocation <- as.character(result.distinct$isBreweryLocation)
result.distinct[is.na(result.distinct$isBreweryLocation)&!is.na(result.distinct$venueId),30] <- "N"


# str(result.distinct[is.na(result.distinct$isBreweryLocation),])

# result.distinct <- result.distinct[,]
# str(result.distinct)

# Save Data
write.csv(result.distinct, "data/checkins.csv", row.names=FALSE)


beerIds <- plyr::count(result.distinct, "beerId")
distinctBeerIds <- beerIds[order(beerIds$freq, decreasing = F),]



# Get min \ max checkin details for future checkin data gathering
maxIds <- aggregate( result.distinct$checkinId, list( result.distinct$beerId ), max )
minIds <- aggregate( result.distinct$checkinId, list( result.distinct$beerId ), min )
lastCreatedDate <- aggregate( 
    strptime( result.distinct$createdAt, "%a, %d %b %Y %T"),
    list(result.distinct$beerId ), 
    max,
    na.rm=TRUE 
)

str(result.distinct)

breweries <- unique(result.distinct[,2:10])
str(breweries)
breweries <- merge(breweries, distinctBeerIds, by.x = "beerId", by.y = "beerId" )
breweries <- merge(breweries, maxIds, by.x = "beerId", by.y = "Group.1")
breweries <- merge(breweries, minIds, by.x = "beerId", by.y = "Group.1")
breweries <- merge(breweries, lastCreatedDate, by.x = "beerId", by.y = "Group.1")

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
