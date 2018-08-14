setwd("/Users/christopherballenger/Documents/Data Science/MSDS 6306/Projects/CaseStudy2")

library(jsonlite)
library(plyr)
library(dplyr)
source("env.R")
source("Untappd_api.R")

## get beer list from a brewery id
checkins <- read.csv("data/checkins.csv")

# remove no venue checkins
checkins.filter <- checkins[!is.na(checkins$isBreweryLocation),]
checkins.filter <- checkins.filter[checkins.filter$ratingScore != 0,]
dim(checkins.filter)

# reduce columes
names(checkins.filter)
checkins.final <- checkins.filter[,c(9,10,11,13,15,18,20,31,32)]
head(checkins.final)

checkins.category <- plyr::count(checkins.filter[!is.na(checkins.filter$venueParentCategory),]$venueParentCategory)

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

breweryIds <- unique(checkins.filter[,1:2])
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

# count how many checkins per beer
distinctBeerIds <- plyr::count(checkins.filter, "beerId")
names(distinctBeerIds) <- c("beerId","totalCheckins")
checkins.filter <- inner_join(checkins.filter, distinctBeerIds, by = c("beerId" = "beerId") )

breweryIds <- unique(checkins.filter[,1:2])
for(i in 1:length(breweryIds$breweryId)){
    ttest <- t.test(
        ratingScore ~ isBreweryLocation,
        data=checkins.filter[checkins.filter$totalCheckins >=100 & checkins.filter$breweryId==breweryIds[i,1],],
        var.equal=T, conf.level=0.95
    )
    print( paste( breweryIds[i,2], "-", breweryIds[i,1], "-", print(ttest$p.value, digits = 18), sep=" "))
}

# Accounting for independent
# creating an avg for users who rated multiple times
checkins.filter %>% count(userId, beerId, isBreweryLocation, sort = TRUE)

checkins.filter.independent <- checkins.filter %>% 
    group_by(breweryId, breweryName, userId, beerId, isBreweryLocation) %>% 
    summarise(
        iRatingScore = mean(ratingScore)
    )

checkins.filter.independent <- as.data.frame(checkins.filter.independent)
# checkins.filter.independent %>% count(breweryId, isBreweryLocation, sort = TRUE)
distinctBeerIds <- plyr::count(checkins.filter.independent, "beerId")
names(distinctBeerIds) <- c("beerId","totalCheckins")
checkins.filter.independent <- inner_join(checkins.filter.independent, distinctBeerIds, by = c("beerId" = "beerId") )

checkins.filter.independent[checkins.filter.independent$totalCheckins>=10,] %>% count(breweryName, isBreweryLocation, sort = F)

breweryIds <- unique(checkins.filter.independent[,1:2])
breweries <- data.frame(
    breweryName = character(),
    t.value = numeric(),
    p.value = numeric(),
    Brewery.Rating = numeric(),
    Not.At.Brewery.Rating = numeric(),
    avg.difference = numeric(),
    conf.int.lwr = numeric(),
    conf.int.upr = numeric()
)
for(i in 1:length(breweryIds$breweryId)){
    test.data <- checkins.filter.independent[ checkins.filter.independent$totalCheckins >= 10 & checkins.filter.independent$breweryId==breweryIds[i,1],]
    test.data <- test.data[order(test.data$isBreweryLocation, decreasing = T),]
    ttest <- t.test(
        iRatingScore ~ isBreweryLocation,
        data=test.data,
        var.equal=T, 
        conf.level=0.95,
        alternative = "less"
    )
    ttest.conf.int <- t.test(
        iRatingScore ~ isBreweryLocation,
        data=test.data,
        var.equal=T, 
        conf.level=0.90,
        alternative = "two.sided"
    )
    
    breweries <- rbind( breweries, data.frame(
        breweryName = c(as.character(test.data[1,]$breweryName)),
        t.value = c(ttest$statistic),
        p.value = c(round(ttest$p.value,5)),
        Brewery.Rating = c(ttest$estimate[2]),
        Not.At.Brewery.Rating = c(ttest$estimate[1]),
        avg.difference = c(ttest$estimate[2] - ttest$estimate[1]),
        conf.int.lwr = c(ttest.conf.int$conf.int[2]*-1),
        conf.int.upr = c(ttest.conf.int$conf.int[1]*-1)
    ))
}
breweries

