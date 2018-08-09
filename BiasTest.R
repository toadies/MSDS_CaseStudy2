setwd("/Users/christopherballenger/Documents/Data Science/MSDS 6306/Projects/CaseStudy2")

library(jsonlite)
library(plyr)
source("env.R")
source("Untappd_api.R")

## get beer list from a brewery id
checkins <- read.csv("data/checkins.csv")

# remove no venue checkins
checkins.filter <- checkins[!is.na(checkins$isBreweryLocation),]

# remove 0 rating scores
checkins.filter <- checkins.filter[checkins.filter$ratingScore != 0,]

dim(checkins.filter)

hist(checkins.filter$ratingScore)
qqnorm(checkins.filter$ratingScore, main='Rating Scores')
qqline(checkins.filter$ratingScore)

checkins.filter$ratingScoreExp <- exp(checkins.filter$ratingScore)

checkins.filter.deepellum <- checkins.filter[checkins.filter$breweryId==11028,]
str(checkins.filter)

dim(checkins.filter.deepellum[checkins.filter.deepellum$isBreweryLocation=="Y",])

boxplot(
    ratingScore ~ isBreweryLocation,
    checkins.filter.deepellum
)
hist(checkins.filter.deepellum[checkins.filter.deepellum$isBreweryLocation=="N",]$ratingScore, main="At the Brewery")
qqnorm(checkins.filter.deepellum[checkins.filter.deepellum$isBreweryLocation=="N",]$ratingScore, main="At the Brewery")
qqline(checkins.filter.deepellum[checkins.filter.deepellum$isBreweryLocation=="N",]$ratingScore)


length(checkins.filter.deepellum[checkins.filter.deepellum$isBreweryLocation=="N",]$ratingScore)
length(checkins.filter.deepellum[checkins.filter.deepellum$isBreweryLocation=="Y",]$ratingScore)
count(checkins.filter.deepellum$ratingScore)

breweryIds <- unique(checkins.filter[,c(1,4)])
for(i in 1:length(breweryIds$breweryId)){
    ttest <- t.test(
        ratingScore ~ isBreweryLocation,
        data=checkins.filter[checkins.filter$breweryId==breweryIds[i,1],],
        var.equal=T, conf.level=0.95
    )
    print( paste( breweryIds[i,1], "-", ttest$p.value, sep=" "))
}

ttest <- t.test(
    ratingScore ~ isBreweryLocation,
    data=checkins.filter[checkins.filter$breweryId==48372,],
    var.equal=T, conf.level=0.95
)
ttest

read.csv("beerstyle.csv")
