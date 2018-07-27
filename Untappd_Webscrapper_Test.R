#
# R Code to scrape the data from Untappd Website
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

source("env.R")
#
# BEGIN - Code to play with
#

brewery_url <- "https://untappd.com/Peticolas"
  
untapped.brewery.raw <- read_html(brewery_url)

brewery.nodes <- html_nodes(untapped.brewery.raw, '.content')

brewery.name <- brewery.nodes %>% html_nodes('div h1')
brewery.style <- brewery.nodes %>% html_nodes('div p.style')
brewery.total.checkins <- brewery.nodes %>% html_nodes('div.stats p span.count')


brewery.text <- html_text(brewery.nodes)

brewry.top.beers <- brewery.nodes[5] %>% html_nodes('p a .name')


user_url <- "https://untappd.com/user/Chrisballenger"
user.profile <- read_html(user_url)
user.activity <- html_nodes(user.profile, 'div[class="activity box"]')
user.checkins <- user.activity %>% html_nodes('div .item')

login.url <- "https://untappd.com/login"
html.session <- html_session(login.url)

test.nodes <- html_nodes(html.session, 'span input')
  
html.form <- html_form(html.session)[[1]]

filled.form <- set_values(html.form, username="selwyn_samuel", password="1350HS()aaMI")

submit_form(html.session, filled.form)

user.profile <- jump_to(html.session, user_url)
user.activity <- html_nodes(user.profile, 'div[class="activity box"]')
user.checkin.total <- html_nodes(user.profile, 'div a[href="/user/Chrisballenger"] span[class="stat"]')
user.checkin.total <- as.numeric(html_text(user.checkin.total))
user.checkins <- user.activity %>% html_nodes('div .item')
user.showmore <- html_nodes(user.profile, 'a[data-user-name="Chrisballenger"]')

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
remDr$navigate(user_url)
remDr$findElement(using = "css selector", "a[data-user-name='Chrisballenger']")$clickElement()

#
# - Used Microsoft Edge with Developer Tools option to get to the right button to click
# - Saved html file as XML and formatted it for easy readability
#
user.page <- read_html(as.character(remDr$getPageSource()))
user.activity <- html_nodes(user.page, 'div[class="activity box"]')
user.checkin.total <- html_nodes(user.page, 'div a[href="/user/Chrisballenger"] span[class="stat"]')
user.checkin.total <- as.numeric(html_text(user.checkin.total))
user.checkins <- user.activity %>% html_nodes('div .item')

remDr$close()
#
# END - Code to play with
#

