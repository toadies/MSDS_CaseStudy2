setwd("/Users/christopherballenger/Documents/Data Science/MSDS 6306/Projects/CaseStudy2")

library(jsonlite)
library(plyr)
library(dplyr)
library(GGally)
library(ggplot2)
source("env.R")
source("Untappd_api.R")

## get beer list from a brewery id
checkins <- read.csv("data/checkins.csv")

# Add missing ABVs
checkins[checkins$beerId==2730189,10] <- 6.6
checkins[checkins$beerId==2491888,10] <- 7.8
checkins[checkins$beerId==2753558,10] <- 10
checkins[checkins$beerId==2612544,10] <- 7.5
checkins[checkins$beerId==1045284,11] <- 11
checkins[checkins$beerId==1149406,11] <- 15
checkins[checkins$beerId==1205273,11] <- 23
checkins[checkins$beerId==1504257,11] <- 26
checkins[checkins$beerId==1675406,11] <- 100
checkins[checkins$beerId==1900155,11] <- 60
checkins[checkins$beerId==594726,11] <- 85
checkins[checkins$beerId==868007,11] <- 90
checkins[checkins$beerId==2612544,11] <- 33
checkins[checkins$beerId==2753558,11] <- 90
checkins[checkins$beerId==2596823,11] <- 80
checkins[checkins$beerId==2628356,11] <- 8

# remove no venue checkins
checkins.filter <- checkins[!is.na(checkins$isBreweryLocation),]
# Remove 0 ratings
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
    checkins.filter
)
hist(checkins.filter$ratingScore)
qqnorm(checkins.filter$ratingScore, main='Rating Scores')
qqline(checkins.filter$ratingScore)


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
    group_by(breweryId, breweryName, userId, beerId, beerName, beerAbv, beerIbu, isBreweryLocation, beerCategory) %>% 
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


# Multi Linear Regression Test to determine alcohol\ibu and beer style has impact with location
# Assume indpendence we will average checkins by brewery, beer, location

checkins.filter.multi.linear <- checkins.filter %>% 
    group_by(breweryId, breweryName, beerId, beerName, beerAbv, beerIbu, beerStyle, isBreweryLocation, beerCategory) %>% 
    summarise(
        iRatingScore = mean(ratingScore)
    )
checkins.filter.multi.linear <- checkins.filter.multi.linear[checkins.filter.multi.linear$beerAbv>0,]
checkins.filter.multi.linear <- checkins.filter.multi.linear[checkins.filter.multi.linear$beerIbu>0,]
dim(checkins.filter.multi.linear)

names(checkins.filter.multi.linear)
ggpairs(checkins.filter.multi.linear, columns = c(5,11,10))
checkins.filter.multi.linear$lBeerIbu <- log(checkins.filter.multi.linear$beerIbu)

qqnorm(checkins.filter.multi.linear$lBeerIbu, main="IBUs")
qqline(checkins.filter.multi.linear$lBeerIbu)


model2 <- lm(
            iRatingScore ~ 
            + beerIbu 
            + beerAbv 
            # + lBeerIbu:beerAbv
            + beerCategory:lBeerIbu
            # + beerCategory:beerAbv
            # + beerCategory:lBeerIbu:beerAbv
            # + isBreweryLocation:beerStyle, 
            ,data=checkins.filter.multi.linear
            )
plot(model2$residuals)
hist(model2$residuals)
summary(model2)

beer.predict <- data.frame(
    lBeerIbu = log(100), 
    beerAbv = 10,
    beerCategory = "IPA"
)

predict(model2, newdata = beer.predict, interval = c("confidence"), type = c("response"), level = .95)
