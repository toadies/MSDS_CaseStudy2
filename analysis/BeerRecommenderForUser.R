library("recommenderlab")	 	 
library("sqldf")
library(dplyr)
setwd("C:/Users/Anand/Documents/SMU/DDS/GITHUB/Case-Study-2/MSDS_CaseStudy2/")
setwd("C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/")
masterCheckIn <- read.csv("data/MasterUserProfile.csv")

beerCheckInData <- data.frame(sqldf("select UserName,beerId,BeerRating,beerName from masterCheckIn"))
nrow(beerCheckInData)
head(beerCheckInData)
str(beerCheckInData)
summary(beerCheckInData)

beerReviewCount <- beerCheckInData %>% group_by(BeerId) %>% summarise(totalBeerReviews=n())
beerReviewCount[with(beerReviewCount, order(totalBeerReviews,decreasing = TRUE)), ]

#beerReviewAtBreweryCount %>% subset(totalBeerReviews==1) %>% dim()

reviewFrequency<-beerReviewCount %>% group_by(totalBeerReviews) %>% summarise(reviewOccurance=n())
head(reviewFrequency,25)


beerReviewCntsubset<-subset(beerReviewCount,beerReviewCount$totalBeerReviews>5)
beerReviewCntsubset

#ggplot(beerReviewCntsubset,aes(x=totalBeerReviews)) + geom_bar()

#Important User
userReviewsCount<- beerCheckInData %>% group_by(UserName) %>% summarise(totalUserReviews=n())
tail(userReviewsCount,10)
userReviewsCountSubset<-subset(userReviewsCount,userReviewsCount$totalUserReviews>=100)
#ggplot(userReviewsCountSubset,aes(x=totalUserReviews)) + geom_bar()


significantBeers<-merge(beerCheckInData,beerReviewCntsubset,by.x="BeerId",by.y="BeerId")
significantBeers<-merge(significantBeers,userReviewsCountSubset,by.x="UserName",by.y="UserName")

summary(significantBeers)
head(significantBeers,5)

beersRealRatingmatrix <- as(significantBeers[,c(1,2,3)], "realRatingMatrix")
class(beersRealRatingmatrix)

head(rowCounts(beersRealRatingmatrix))

head(colCounts(beersRealRatingmatrix))

str(beers_df)

##Popular
popularPediction <- Recommender(beersRealRatingmatrix, method = "POPULAR")
popularPediction
summary(popularPediction)

##UBCF
ubcfPred <- Recommender(beersRealRatingmatrix, method = "UBCF")
ubcfPred
summary(ubcfPred)

##IBCF
ibcfPred <- Recommender(beersRealRatingmatrix, method = "IBCF")
ibcfPred
summary(ibcfPred)

##POPULAR
recomndForChrisBallenger <- predict(popularPediction, beersRealRatingmatrix['ChrisBallenger'], n=5)
recomndedBeerIds <- as(recomndForChrisBallenger, "list")
chris_beerIds <- paste((recomndedBeerIds$ChrisBallenger), collapse=',')
chris_beerIds
sqldf(sprintf("select distinct BeerName from significantBeers where BeerId in (%s)", chris_beerIds))


##UBCF

recomndForChrisBallenger <- predict(ubcfPred, beersRealRatingmatrix['ChrisBallenger'], n=5)
recomndedBeerIds <- as(recomndForChrisBallenger, "list")
chris_beerIds <- paste((recomndedBeerIds$ChrisBallenger), collapse=',')
chris_beerIds
sqldf(sprintf("select distinct BeerName from significantBeers where BeerId in (%s)", chris_beerIds))

##IBCF
recomndForChrisBallenger <- predict(ibcfPred, beersRealRatingmatrix['ChrisBallenger'], n=5)
recomndedBeerIds <- as(recomndForChrisBallenger, "list")
chris_beerIds <- paste((recomndedBeerIds$ChrisBallenger), collapse=',')
chris_beerIds
sqldf(sprintf("select distinct BeerName from significantBeers where BeerId in (%s)", chris_beerIds))

