setwd("/Users/christopherballenger/Documents/Data Science/MSDS 6306/Projects/CaseStudy2")

library(jsonlite)
library(plyr)
source("Untappd_api.R")

result <- data.frame()
jsonObjects <- list.files("Responses/")
filterObjects <- jsonObjects[grep("*json",jsonObjects)]
for(i in 1:length(filterObjects) ){
    # if( length(grep("*beer.checkins*",jsonObjects[i] )) >= 1 ){
    checkins <- fromJSON(paste("Responses/", filterObjects[i], sep=""))
    if( checkins$response$checkins$count > 0 ){
        result <- rbind( result, readBeerCheckins(checkins) )
    }
    # }
    print( paste(i, "-", dim(result)[1] ) )
}

# a list in which each element is one of the JSON files
# result <- lapply(
#     filterObjects,
#     function(x){
#         print(x)
#         readBeerCheckins(
#             fromJSON(paste("Responses/", x, sep=""))
#         )
#     }
# )
# result <- do.call(rbind.data.frame, result)
# write.csv(result,"checkins1.csv")

# dim(result)
# str(result)
result.distinct <- unique(result)
beerIds <- plyr::count(result.distinct, "beerId")
distinctBeerIds <- beerIds[order(beerIds$freq, decreasing = F),]
# distinctBeerIds
# dim(result)
# dim(result.distinct)
str(result.distinct)
write.csv(result.distinct, "checkins.csv", row.names=FALSE)

maxIds <- aggregate( result.distinct$checkinId, list( result.distinct$beerId ), max )
minIds <- aggregate( result.distinct$checkinId, list( result.distinct$beerId ), min )
lastCreatedDate <- aggregate( 
    strptime( result.distinct$createdAt, "%a, %d %b %Y %T"),
    list(result.distinct$beerId ), 
    max,
    na.rm=TRUE 
)

breweries <- unique(result.distinct[,1:4])
str(breweries)
breweries <- merge(breweries, distinctBeerIds, by.x = "beerId", by.y = "beerId" )
breweries <- merge(breweries, maxIds, by.x = "beerId", by.y = "Group.1")
breweries <- merge(breweries, minIds, by.x = "beerId", by.y = "Group.1")
breweries <- merge(breweries, lastCreatedDate, by.x = "beerId", by.y = "Group.1")
names(breweries) <- c("beerId","breweryId","breweryName","beerName","totalCheckins","maxId","minId", "lastCreatedDate")

write.csv(breweries, "breweries.csv",row.names = FALSE)

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

length(unique(result.distinct$checkinId))
