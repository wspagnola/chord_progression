
###docker run -d -p 4445:4444 selenium/standalone-chrome

source('source.R')
url <- 'https://www.hooktheory.com/theorytab/view/the-beatles/hey-jude'
require(dplyr)

require(tidyr)
require(RSelenium)
eCaps <- list(chromeOptions = list(
  args = list('--user-agent="music_fan"')
))
remDr <- remoteDriver(remoteServerAddr = "localhost",
                      extraCapabilities = eCaps,
                      port = 4445L, 
                      browserName = "chrome")


remDr$open() #Open Driver
remDr$setTimeout(type = 'page load', milliseconds = 60e3) #Set Timeout time

remDr$navigate(url)
print(remDr$getCurrentUrl())

extract_key_tempo <- function(remDr){
  
  require(RSelenium)
  
  #First navigate to page and scroll down
  #Input remote driver (NOT SURE IF THIS WORKS)
  ###Execute to extract key/tempo (in BPM) for each song part
  #Returns as data.frame with 
  ##...rows equal to # of song partt
  ##...2 cols; 1 for key and 1 for tempo (BPM)
  
  #Extract Root (Key Letter) and Tempo(BPM)
  primary_elem <- remDr$findElements(using="class", value = 'primary')
  primary_txt <- sapply(primary_elem , function(x) {x$getElementAttribute("outerHTML")[[1]]})
  primary_html <- htmlTreeParse(primary_txt, useInternalNodes=T)
  primary_xml <- xpathApply(primary_html,  '//div[@class="primary"]')
  primary_vec <- sapply(primary_xml,xmlValue) 
  key_idx <- seq(1, length(primary_vec), 2)
  beat_idx <- (1:length(primary_vec))[-(key_idx)]
  key_root <- primary_vec[key_idx]
  bpm <- as.integer(primary_vec[beat_idx])
  
  #Extract Mode 
  secondary_elem <- remDr$findElements(using="class", value = 'secondary')
  secondary_txt <- sapply(secondary_elem , function(x) {x$getElementAttribute("outerHTML")[[1]]})
  secondary_html <- htmlTreeParse(secondary_txt, useInternalNodes=T)
  secondary_xml <- xpathApply(secondary_html,  '//div[@class="secondary"]')
  secondary_vec <-sapply(secondary_xml ,xmlValue)
  mode <- secondary_vec[seq(1, length(key), 2)] #Extract mode (even elems) from BPM (odd elems)
  
  #Combine Key with Mode
  key <- paste0(key_root, mode)
  
  #Store Key and Tempo in data.frame
  key_beat_df <- data.frame(Key = key, BPM = bpm)
  
  return(key_beat_df )
}




#### Note: Can't use rvest!
