#STEPS to Load Dockter in Terminal
###docker pull selenium/standalone-firefox
###docker run -d -p 4445:4444 selenium/standalone-firefox


library(rvest)
library(XML)
library(tidyverse)
library(seleniumPipes)
library(RSelenium)

#
remDr <- remoteDriver( remoteServerAddr = "localhost",
                      port = 4445L, 
                      browserName = "firefox")
remDr$open()
remDr$getStatus()
remDr$navigate("https://www.google.com/")
remDr$getCurrentUrl()



url <- 'https://www.hooktheory.com/theorytab/view/the-beatles/hey-jude'
remDr$navigate(url)
elem <- remDr$findElement(using="id", value="tab-284881hookpad-score-div")

elemtxt <- elem$getElementAttribute("outerHTML")[[1]]

# parse string into HTML tree to allow for querying with XPath
elemxml <- htmlTreeParse(elemtxt, useInternalNodes=T)

#### Extract 7th and Major Chords "

xpath <- '//tspan[@alignment-baseline]'
fundList <- unlist(xpathApply(elemxml, xpath))

#Convert from XML to Character
x <- fundList %>% 
  sapply( saveXML) 

#Clean Text and Collapse
x %>%
    str_remove_all("baseline") %>% 
    str_remove_all("alignment") %>% 
    str_remove_all("middle") %>% 
    str_extract_all('[A-G]|7|b|sus|m') %>%               
    unlist %>% 
    paste(collapse= ' ') %>% 
    str_replace_all(' 7', '7') %>% 
    str_replace_all(' b', 'b')
   

#### Extract Slash Chords and Suspended Chords ####

#????