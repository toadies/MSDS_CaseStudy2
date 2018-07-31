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
# Given a user name, the function will return a data frame of all the user checkin details
#
get_user_checkins <- function(user.name) {

  remDr <- get_remote_chrome_driver()
  user.url <- paste(base.url, "user/", user.name, sep = "")
  
  remDr$navigate(user.url)
  window.handle <- remDr$getCurrentWindowHandle()
    
  #
  # - Used Microsoft Edge with Developer Tools option to get to the right button to click
  # - Saved html file as XML and formatted it for easy readability
  #
  user.page <- read_html(as.character(remDr$getPageSource()))
  user.activity <- html_nodes(user.page, 'div[class="activity box"]')
  user.checkin.total <- html_nodes(user.page, 'div a[href="/user/Chrisballenger"] span[class="stat"]')
  user.checkin.total <- as.numeric(html_text(user.checkin.total))

  banner.frame <- remDr$switchToFrame("branch-banner-iframe")
  banner.button <- remDr$findElement(using = "css selector", "#branch-banner-close")
  banner.button$clickElement()

  remDr$navigate(user.url)
  load.button <- remDr$findElement(using = "css selector", "a[data-user-name='Chrisballenger']")
  
  # there are 15 checkins per scroll
  loop.count <- round(user.checkin.total / 15)
  
  for(counter in 1:13) {
    print(counter)
    load.button$clickElement()
    Sys.sleep(2)
  }
  
  user.page <- read_html(as.character(remDr$getPageSource()))
  user.activity <- html_nodes(user.page, 'div[class="activity box"]')
  user.checkins <- user.activity %>% html_nodes('div .item')
  close_remote_chrome_driver(remDr)
  user.checkins
}


