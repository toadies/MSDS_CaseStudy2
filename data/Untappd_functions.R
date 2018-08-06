#
# File contains commonly used functions for the case study
#

#
# function to get a remote driver for chrome
#
get_remote_chrome_driver <- function() {
  #
  # Use RSelenium Package to navigate to multiple packages
  # - Install RSelenium package from tar/zip file
  # - Install Chrome driver
  # - Need to start Selenium server: java -D"webdriver.chrome.driver"="c:\tools\chromedriver.exe" -jar .\selenium-server-standalone-3.13.0.jar 
  #   before running the following code.
  #
  remDr <- remoteDriver(browserName="chrome")
  remDr$open()
  remDr$navigate(login.url)
  remDr$findElement("id", "username")$sendKeysToElement(list(username))
  remDr$findElement("id", "password")$sendKeysToElement(list(password))
  remDr$findElement(using = "css selector", "span input")$clickElement()
  remDr  
}

close_remote_chrome_driver <- function(remDr) {
  remDr$close()
}

combine_beer_files <- function() {
  # 
  # Read the files
  #
  beer1 <- read.csv("C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/temp/BeerDetails1-100.csv")
  beer2 <- read.csv("C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/temp/BeerDetails101-200.csv")
  beer3 <- read.csv("C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/temp/BeerDetails201-300.csv")
  beer4 <- read.csv("C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/temp/BeerDetails301-400.csv")
  beer5 <- read.csv("C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/temp/BeerDetails401-500.csv")
  beer6 <- read.csv("C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/temp/BeerDetails501-600.csv")
  beer7 <- read.csv("C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/temp/BeerDetails601-700.csv")
  beer8 <- read.csv("C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/temp/BeerDetails701-800.csv")
  beer9 <- read.csv("C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/temp/BeerDetails801-900.csv")
  beer10 <- read.csv("C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/temp/BeerDetails901-1000.csv")
  beer11 <- read.csv("C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/temp/BeerDetails1001-1100.csv")
  beer12 <- read.csv("C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/temp/BeerDetails1101-1176.csv")
  
  combined.beers <- rbind(beer1, beer2)
  combined.beers <- rbind(combined.beers, beer3)
  combined.beers <- rbind(combined.beers, beer4)
  combined.beers <- rbind(combined.beers, beer5)
  combined.beers <- rbind(combined.beers, beer6)
  combined.beers <- rbind(combined.beers, beer7)
  combined.beers <- rbind(combined.beers, beer8)
  combined.beers <- rbind(combined.beers, beer9)
  combined.beers <- rbind(combined.beers, beer10)
  combined.beers <- rbind(combined.beers, beer11)
  combined.beers <- rbind(combined.beers, beer12)
  
  write.csv(combined.beers, file="C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/MasterBeerList.csv", row.names = FALSE)
}

combine_flavor_profiles <- function() {
  
  beers.master <- read.csv("C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/MasterBeerList.csv", stringsAsFactors = FALSE)
  user.profile <- read.csv("C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/MasterUserProfile.Original.csv", stringsAsFactors = FALSE)
  
  # https://www.thebrewenthusiast.com/ibus/ says beers will have a rating of 5-120 IBU
  # we will replace beers with 'No' IBUs to 5
  beers.master$IBU = replace(beers.master$IBU, beers.master$IBU=='No', 5)
  
  for(row in 1:nrow(user.profile)) {
    
    #print(row)
    user.checkin <- user.profile[row,]
    beer.record <- beers.master[(beers.master$BeerId==user.checkin$BeerId),]
    
    #print(user.checkin)
    if(beer.record$Light == 1) { user.checkin$Light = 1 }
    if(beer.record$Smooth == 1) { user.checkin$Smooth = 1 }
    if(beer.record$Clean == 1) { user.checkin$Clean = 1 }
    if(beer.record$Sweet == 1) { user.checkin$Sweet = 1 }
    if(beer.record$Woody == 1) { user.checkin$Woody = 1 }
    if(beer.record$Zippy == 1) { user.checkin$Zippy = 1 }
    if(beer.record$Strong == 1) { user.checkin$Strong = 1 }
    if(beer.record$Hoppy == 1) { user.checkin$Hoppy = 1 }
    if(beer.record$Floral == 1) { user.checkin$Floral = 1 }
    if(beer.record$Citrus == 1) { user.checkin$Citrus = 1 }
    if(beer.record$Milk == 1) { user.checkin$Milk = 1 }
    if(beer.record$Dark == 1) { user.checkin$Dark = 1 }
    if(beer.record$Creamy == 1) { user.checkin$Creamy = 1 }
    if(beer.record$Mouthfeel == 1) { user.checkin$Mouthfeel = 1 }
    if(beer.record$Boozy == 1) { user.checkin$Boozy = 1 }
    if(beer.record$Caramel == 1) { user.checkin$Caramel = 1 }
    if(beer.record$Oatmeal == 1) { user.checkin$Oatmeal = 1 }
    if(beer.record$Dry == 1) { user.checkin$Dry = 1 }
    if(beer.record$Malty == 1) { user.checkin$Malty = 1 }
    if(beer.record$Piney == 1) { user.checkin$Piney = 1 }
    if(beer.record$Grassy == 1) { user.checkin$Grassy = 1 }
    if(beer.record$Tart == 1) { user.checkin$Tart = 1 }
    if(beer.record$Sour == 1) { user.checkin$Sour = 1 }
    if(beer.record$Salty == 1) { user.checkin$Salty = 1 }
    if(beer.record$Fruity == 1) { user.checkin$Fruity = 1 }
    if(beer.record$Heat == 1) { user.checkin$Heat = 1 }
    if(beer.record$Bitter == 1) { user.checkin$Bitter = 1 }
    if(beer.record$Coffee == 1) { user.checkin$Coffee = 1 }
    if(beer.record$Roasty == 1) { user.checkin$Roasty = 1 }
    if(beer.record$Chocolate == 1) { user.checkin$Chocolate = 1 }
    if(beer.record$Juicy == 1) { user.checkin$Juicy = 1 }
    if(beer.record$Funky == 1) { user.checkin$Funky = 1 }
    if(beer.record$Acidic == 1) { user.checkin$Acidic = 1 }
    if(beer.record$Crushable == 1) { user.checkin$Crushable = 1 }
    #print(user.checkin)
    
    user.checkin$IBU = beer.record$IBU
    user.checkin$ABV = beer.record$ABV
    user.checkin$Style = beer.record$Style
      
    user.profile[row,] <- user.checkin
  }
  write.csv(user.profile, file="C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/MasterUserProfile.csv", row.names = FALSE)
}

classify_checkins <- function() {
  checkins <- read.csv("C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/checkins-modified.csv", stringsAsFactors = FALSE)
  brewery.locations <- read.csv("C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/BreweryLocations.csv", stringsAsFactors = FALSE)
  
  checkins[,"isBreweryLocation"] <- c(0)
  for(row in 1:nrow(checkins)) {
    if(checkins[row,]$venueId %in% brewery.locations$BreweryVenue) {
      checkins[row,]$isBreweryLocation=1
    } else {
      checkins[row,]$isBreweryLocation=0
    }
  }
  write.csv(checkins, file="C:/SMU/Courses/MSDS 6306 - DoingDataScience/github/MSDS_CaseStudy2/data/checkins-modified1.csv", row.names = FALSE)
}