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

setwd('C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/')
source("env.R")
source("Untappd_functions.R")

#
#
#
get_user_checkin_nodes <- function(user.url, remDr, user.name) {
  remDr$navigate(user.url)
  window.handle <- remDr$getCurrentWindowHandle()
  #
  # - Used Microsoft Edge with Developer Tools option to get to the right button to click
  # - Saved html file as XML and formatted it for easy readability
  #
  user.page <- read_html(as.character(remDr$getPageSource()))
  user.activity <- html_nodes(user.page, 'div[class="activity box"]')
  user.page.selector <- paste('div a[href="/user/', user.name, '"] span[class="stat"]', sep = "")
  #user.checkin.total <- html_nodes(user.page, 'div a[href="/user/Chrisballenger"] span[class="stat"]')
  user.checkin.total <- html_nodes(user.page, user.page.selector)
  user.checkin.total <- as.numeric(html_text(user.checkin.total))
  
  banner.frame <- remDr$switchToFrame("branch-banner-iframe")
  banner.button <- remDr$findElement(using = "css selector", "#branch-banner-close")
  banner.button$clickElement()
  
  remDr$navigate(user.url)
  load.button.selector <- paste('a[data-user-name="', user.name, '"]', sep = "")
  load.button <- remDr$findElement(using = "css selector", load.button.selector)
  
  # there are 15 checkins per scroll
  loop.count <- round(user.checkin.total / 15)
  
  for(counter in 1:13) {
    print(counter)
    load.button$clickElement()
    Sys.sleep(5)
  }
  
  user.page <- read_html(as.character(remDr$getPageSource()))
  user.activity <- html_nodes(user.page, 'div[class="activity box"]')
  user.checkins.nodes <- user.activity %>% html_nodes('div .item')
}

#
# Given a user name, the function will return a data frame of all the user checkin details
#
get_beer_details <- function(file.name) {
  
  user.checkins <- data.frame(matrix(ncol = 47, nrow = 0))
  colnames(user.checkins) <- c("BeerId", "BreweryName", "BreweryURL", "Style", "ABV", "IBU", "Rating", "BreweryId", "BeerRating", "ABV", "IBU", "Style",
                               "Light", "Smooth", "Clean", "Sweet", "Woody", "Zippy", "Strong", "Hoppy", "Floral", "Citrus", "Milk", "Dark", "Creamy", "Mouthfeel",
                               "Soft", "Boozy", "Caramel", "Oatmeal", "Dry", "Malty", "Piney", "Grassy", "Tart", "Sour", "Salty", "Fruity", "Heat", "Bitter", "Coffee",
                               "Roasty", "Chocolate", "Juicy", "Funky", "Acidic", "Crushable")
  
  remDr <- get_remote_chrome_driver()
  user.url <- paste(base.url, "user/", user.name, sep = "")
  
  user.checkins.nodes <- get_user_checkin_nodes(user.url, remDr, user.name)
  message("Number of checkin Nodes: ", length(user.checkins.nodes))
  write(as.character(user.checkins.nodes), file="C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/ChrisCheckins1.xml")
  
  for(counter in 1:length(user.checkins.nodes)) {
    
    print(counter)
    tryCatch({
      current.node <- user.checkins.nodes[counter]
      CheckinId <- html_attr(current.node, "data-checkin-id")
      message("Processing checkin... ", CheckinId)
      newCheckIn <- data.frame(CheckinId=CheckinId, UserName=user.name, Light=0, Smooth=0, Clean=0, Sweet=0, Woody=0, Zippy=0, Strong=0, Hoppy=0, Floral=0, Citrus=0,
                               Milk=0, Dark=0, Creamy=0, Mouthfeel=0, Soft=0, Boozy=0, Caramel=0, Oatmeal=0, Dry=0, Malty=0, Piney=0, Grassy=0, Tart=0, Sour=0, 
                               Salty=0, Fruity=0, Heat=0, Bitter=0, Coffee=0, Roasty=0, Chocolate=0, Juicy=0, Funky=0, Acidic=0, Crushable=0, LocationName="", LocationId=0)
      
      checkin.url <- paste(user.url, "/checkin/", CheckinId, sep = "")
      remDr$navigate(checkin.url)
      
      checkin.page <- read_html(as.character(remDr$getPageSource()))
      checkin.node <- html_nodes(checkin.page, 'div[class="checkin box"]')
      location.url <- html_attr(html_nodes(checkin.node, 'div p[class="location"] a'), "href")
      location.name <- html_text(html_nodes(checkin.node, 'div p[class="location"] a'))
      location.id <- tail(unlist(strsplit(location.url, "/")), n=1)
      if(length(location.id) > 0) { newCheckIn$LocationId <- location.id }
      if(length(location.name) > 0) { newCheckIn$LocationName <- location.name }
      
      beer.url <- html_attr(html_nodes(checkin.node, 'div[class="beer"] p a'), "href")
      beer.name <- html_text(html_nodes(checkin.node, 'div [class="beer"] p a'))
      beer.id <- tail(unlist(strsplit(beer.url, "/")), n=1)
      beer.rating.nodes <- html_nodes(checkin.node, 'div[class="rating-serving"] span')
      if(length(beer.rating.nodes) > 1) {
        beer.rating.str <- html_attr(html_nodes(checkin.node, 'div[class="rating-serving"] span')[2], "class")
      } else {
        beer.rating.str <- html_attr(html_nodes(checkin.node, 'div[class="rating-serving"] span')[1], "class")
      }
      beer.rating <- substring(beer.rating.str, nchar(beer.rating.str)-2)
      newCheckIn$BeerId <- beer.id
      newCheckIn$BeerName <- beer.name
      newCheckIn$BeerRating <- as.numeric(beer.rating)/100
      
      flavors.list <- html_text(html_nodes(checkin.node, 'div[class="flavor"] ul li span'))
      #print(flavors.list)
      if(length(flavors.list) > 0) {
        if("Light" %in% flavors.list) { newCheckIn$Light = 1 }
        if("Smooth" %in% flavors.list) { newCheckIn$Smooth = 1 }
        if("Clean" %in% flavors.list) { newCheckIn$Clean = 1 }
        if("Sweet" %in% flavors.list) { newCheckIn$Sweet = 1 }
        if("Woody" %in% flavors.list) { newCheckIn$Woody = 1 }
        if("Zippy" %in% flavors.list) { newCheckIn$Zippy = 1 }
        if("Strong" %in% flavors.list) { newCheckIn$Strong = 1 }
        if("Hoppy" %in% flavors.list) { newCheckIn$Hoppy = 1 }
        if("Floral" %in% flavors.list) { newCheckIn$Floral = 1 }
        if("Citrus" %in% flavors.list) { newCheckIn$Citrus = 1 }
        if("Milk" %in% flavors.list) { newCheckIn$Milk = 1 }
        if("Dark" %in% flavors.list) { newCheckIn$Dark = 1 }
        if("Creamy" %in% flavors.list) { newCheckIn$Creamy = 1 }
        if("Mouthfeel" %in% flavors.list) { newCheckIn$Mouthfeel = 1 }
        if("Soft" %in% flavors.list) { newCheckIn$Soft = 1 }
        if("Boozy" %in% flavors.list) { newCheckIn$Boozy = 1 }
        if("Caramel" %in% flavors.list) { newCheckIn$Caramel = 1 }
        if("Oatmeal" %in% flavors.list) { newCheckIn$Oatmeal = 1 }
        if("Dry" %in% flavors.list) { newCheckIn$Dry = 1 }
        if("Malty" %in% flavors.list) { newCheckIn$Malty = 1 }
        if("Piney" %in% flavors.list) { newCheckIn$Piney = 1 }
        if("Grassy" %in% flavors.list) { newCheckIn$Grassy = 1 }
        if("Tart" %in% flavors.list) { newCheckIn$Tart = 1 }
        if("Sour" %in% flavors.list) { newCheckIn$Sour = 1 }
        if("Salty" %in% flavors.list) { newCheckIn$Salty = 1 }
        if("Fruity" %in% flavors.list) { newCheckIn$Fruity = 1 }
        if("Heat" %in% flavors.list) { newCheckIn$Heat = 1 }
        if("Bitter" %in% flavors.list) { newCheckIn$Bitter = 1 }
        if("Coffee" %in% flavors.list) { newCheckIn$Coffee = 1 }
        if("Roasty" %in% flavors.list) { newCheckIn$Roasty = 1 }
        if("Chocolate" %in% flavors.list) { newCheckIn$Chocolate = 1 }
        if("Juicy" %in% flavors.list) { newCheckIn$Juicy = 1 }
        if("Funky" %in% flavors.list) { newCheckIn$Funky = 1 }
        if("Acidic" %in% flavors.list) { newCheckIn$Acidic = 1 }
        if("Crushable" %in% flavors.list) { newCheckIn$Crushable = 1 }
      }
      
      print(newCheckIn)
      user.checkins <- rbind(user.checkins, newCheckIn)
      Sys.sleep(5)
    },
    error=function(e) {
      message("Error in: ", checkin.url) 
      message(e)
      counter <- counter + 1
    })
  }
  close_remote_chrome_driver(remDr)
  write.csv(user.checkins, file=paste("C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/", user.name, "Profile.csv", sep = ""), row.names = FALSE)
  user.checkins
}


