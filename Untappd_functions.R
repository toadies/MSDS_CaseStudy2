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