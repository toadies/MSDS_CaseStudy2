#
# R Code to scrape the user checkin data from Untappd Website
#
library(RCurl)
library(XML)
library(ggplot2)
library(httr)
library(rvest) 
library(kableExtra)
library(tidyr)
library(RSelenium)
library(stringr)

setwd('C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data')
source("../env.R")
source("Untappd_functions.R")

#
# Given a user name, the function will return a data frame of all the user checkin details
#
get_beer_details <- function(file.name) {
  
  beer.details <- data.frame(matrix(ncol = 46, nrow = 0))
  colnames(beer.details) <- c("BeerId", "BreweryName", "BreweryURL", "Style", "ABV", "IBU", "BeerRating", "TotalRatings", "TotalCheckins", "UniqueCheckins",
                              "MonthlyCheckins", "Light", "Smooth", "Clean", "Sweet", "Woody", "Zippy", "Strong", "Hoppy", "Floral", "Citrus", "Milk", "Dark", "Creamy",
                              "Mouthfeel", "Soft", "Boozy", "Caramel", "Oatmeal", "Dry", "Malty", "Piney", "Grassy", "Tart", "Sour", "Salty", "Fruity", "Heat", 
                              "Bitter", "Coffee", "Roasty", "Chocolate", "Juicy", "Funky", "Acidic", "Crushable")
  
  beer.ids <- read.csv(file.name)
  remDr <- get_remote_chrome_driver() 
  
  #for(row in 1:nrow(beer.ids)) {
  for(row in 1:nrow(beer.ids)) {
      
    print(row)
    tryCatch({
      message("Processing Beer Id... ", beer.ids[row,]$BeerId)
      newBeer <- data.frame(BeerId=beer.ids[row,]$BeerId, BreweryName="", BreweryURL="", Style="", ABV="0%", IBU=0, BeerRating=0, TotalRatings=0, TotalCheckins=0, UniqueCheckins=0,
                            MonthlyCheckins=0, Light=0, Smooth=0, Clean=0, Sweet=0, Woody=0, Zippy=0, Strong=0, Hoppy=0, Floral=0, Citrus=0, Milk=0, Dark=0, Creamy=0, Mouthfeel=0, 
                            Soft=0, Boozy=0, Caramel=0, Oatmeal=0, Dry=0, Malty=0, Piney=0, Grassy=0, Tart=0, Sour=0, Salty=0, Fruity=0, Heat=0, Bitter=0, Coffee=0, Roasty=0, 
                            Chocolate=0, Juicy=0, Funky=0, Acidic=0, Crushable=0)
      
      beer.url <- paste(base.url, "beer/", beer.ids[row, "BeerId"], sep = "")
      remDr$navigate(beer.url)
      window.handle <- remDr$getCurrentWindowHandle()
      
      beer.page <- read_html(as.character(remDr$getPageSource()))
      beer.detail.node <- html_nodes(beer.page, 'div[class="box b_info"]')
      
      newBeer$BreweryName <- html_text(html_nodes(beer.detail.node, 'div[class="name"] p[class="brewery"] a'))
      newBeer$BreweryURL <- html_attr(html_nodes(beer.detail.node, 'div[class="name"] p[class="brewery"] a'), "href")
      newBeer$Style <- html_text(html_nodes(beer.detail.node, 'div[class="name"] p[class="style"]'))
      newBeer$ABV <- tail(unlist(strsplit(trimws(html_text(html_nodes(beer.detail.node, 'div[class="details"] p[class="abv"]'))), " ")), n=2)[1]
      newBeer$IBU <- tail(unlist(strsplit(trimws(html_text(html_nodes(beer.detail.node, 'div[class="details"] p[class="ibu"]'))), " ")), n=2)[1]
      newBeer$TotalRatings <- tail(unlist(strsplit(trimws(html_text(html_nodes(beer.detail.node, 'div[class="details"] p[class="raters"]'))), " ")), n=2)[1]
      beer.rating.str <- html_attr(html_nodes(beer.detail.node, 'div[class="details"] p[class="rating"] span'), "class")[1]
      newBeer$BeerRating <- as.numeric(substring(beer.rating.str, nchar(beer.rating.str)-2))/100
      
      counts.list <- html_text(html_nodes(beer.detail.node, 'div[class="stats"] p span[class="count"]'))
      newBeer$TotalCheckins <- counts.list[1]
      newBeer$UniqueCheckins <- counts.list[2]
      newBeer$MonthlyCheckins <- counts.list[3]
      
      beer.description <- strsplit(trimws(html_text(html_nodes(beer.detail.node, 'div[class="bottom"] div[class="desc"] div[class="beer-descrption-read-less"]'))), " ")
      if(length(beer.description) > 0) {
        if(str_detect(beer.description, fixed("Light", ignore_case=TRUE))) { newBeer$Light = 1 }
        if(str_detect(beer.description, fixed("Smooth", ignore_case=TRUE))) { newBeer$Smooth = 1 }
        if(str_detect(beer.description, fixed("Clean", ignore_case=TRUE))) { newBeer$Clean = 1 }
        if(str_detect(beer.description, fixed("Sweet", ignore_case=TRUE))) { newBeer$Sweet = 1 }
        if(str_detect(beer.description, fixed("Woody", ignore_case=TRUE))) { newBeer$Woody = 1 }
        if(str_detect(beer.description, fixed("Zippy", ignore_case=TRUE))) { newBeer$Zippy = 1 }
        if(str_detect(beer.description, fixed("Strong", ignore_case=TRUE))) { newBeer$Strong = 1 }
        if(str_detect(beer.description, fixed("Hoppy", ignore_case=TRUE))) { newBeer$Hoppy = 1 }
        if(str_detect(beer.description, fixed("Floral", ignore_case=TRUE))) { newBeer$Floral = 1 }
        if(str_detect(beer.description, fixed("Citrus", ignore_case=TRUE))) { newBeer$Citrus = 1 }
        if(str_detect(beer.description, fixed("Milk", ignore_case=TRUE))) { newBeer$Milk = 1 }
        if(str_detect(beer.description, fixed("Dark", ignore_case=TRUE))) { newBeer$Dark = 1 }
        if(str_detect(beer.description, fixed("Creamy", ignore_case=TRUE))) { newBeer$Creamy = 1 }
        if(str_detect(beer.description, fixed("Mouthfeel", ignore_case=TRUE))) { newBeer$Mouthfeel = 1 }
        if(str_detect(beer.description, fixed("Soft", ignore_case=TRUE))) { newBeer$Soft = 1 }
        if(str_detect(beer.description, fixed("Boozy", ignore_case=TRUE))) { newBeer$Boozy = 1 }
        if(str_detect(beer.description, fixed("Caramel", ignore_case=TRUE))) { newBeer$Caramel = 1 }
        if(str_detect(beer.description, fixed("Oatmeal", ignore_case=TRUE))) { newBeer$Oatmeal = 1 }
        if(str_detect(beer.description, fixed("Dry", ignore_case=TRUE))) { newBeer$Dry = 1 }
        if(str_detect(beer.description, fixed("Malty", ignore_case=TRUE))) { newBeer$Malty = 1 }
        if(str_detect(beer.description, fixed("Piney", ignore_case=TRUE))) { newBeer$Piney = 1 }
        if(str_detect(beer.description, fixed("Grassy", ignore_case=TRUE))) { newBeer$Grassy = 1 }
        if(str_detect(beer.description, fixed("Tart", ignore_case=TRUE))) { newBeer$Tart = 1 }
        if(str_detect(beer.description, fixed("Sour", ignore_case=TRUE))) { newBeer$Sour = 1 }
        if(str_detect(beer.description, fixed("Salty", ignore_case=TRUE))) { newBeer$Salty = 1 }
        if(str_detect(beer.description, fixed("Fruity", ignore_case=TRUE))) { newBeer$Fruity = 1 }
        if(str_detect(beer.description, fixed("Heat", ignore_case=TRUE))) { newBeer$Heat = 1 }
        if(str_detect(beer.description, fixed("Bitter", ignore_case=TRUE))) { newBeer$Bitter = 1 }
        if(str_detect(beer.description, fixed("Coffee", ignore_case=TRUE))) { newBeer$Coffee = 1 }
        if(str_detect(beer.description, fixed("Roasty", ignore_case=TRUE))) { newBeer$Roasty = 1 }
        if(str_detect(beer.description, fixed("Chocolate", ignore_case=TRUE))) { newBeer$Chocolate = 1 }
        if(str_detect(beer.description, fixed("Juicy", ignore_case=TRUE))) { newBeer$Juicy = 1 }
        if(str_detect(beer.description, fixed("Funky", ignore_case=TRUE))) { newBeer$Funky = 1 }
        if(str_detect(beer.description, fixed("Acidic", ignore_case=TRUE))) { newBeer$Acidic = 1 }
        if(str_detect(beer.description, fixed("Crushable", ignore_case=TRUE))) { newBeer$Crushable = 1 }
      }
      print(newBeer)
      beer.details <- rbind(beer.details, newBeer)
      Sys.sleep(5)
    },
    error=function(e) {
      message("Error in: ", beer.url) 
      message(e)
      row <- row + 1
      Sys.sleep(5)
    })
  }
  close_remote_chrome_driver(remDr)
  write.csv(beer.details, file="C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/Chris-BeerDetails.csv", row.names = FALSE)
  beer.details
}


