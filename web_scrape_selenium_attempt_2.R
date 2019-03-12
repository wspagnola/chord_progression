#STEPS to Load Dockter in Terminal
###docker pull selenium/standalone-firefox
###docker run -d -p 4445:4444 selenium/standalone-firefox


library(rvest)
library(XML)
library(tidyverse)
library(seleniumPipes)
library(RSelenium)
library(httr)
#Extract Song Parts
extract_song_parts <- function(txt) {
  
  require(stringr)
  
  #Character Vector of Types of Song Parts
  part_types <- c('Intro', 'Verse', 'Pre-Chorus', 'Bridge', 
                  'Chorus', 'Outro')
  
  #Extract Each of the 
  song_parts <- lapply(txt,  function(x) {str_extract(x, part_types)}) 
  
  #Extract First Element from each vector that is not NA
  song_parts_first <- lapply(song_parts, function(x) x[!is.na(x)][1] )
  
  #Convert from List to Vector
  song_parts_first_vec <- unlist(song_parts_first)
  
  #Remove any Elements in List that are only NA (did not contain songpart)
  song_parts_final <-  song_parts_first_vec[!is.na(song_parts_first_vec)]
  
  return(song_parts_final)
}


#### Open Selenium ####

#Set up Driver ? 
remDr <- remoteDriver( remoteServerAddr = "localhost",
                      port = 4445L, 
                      browserName = "firefox")
#Open Selenium ?
remDr$open()

#TEST
# remDr$getStatus()
# remDr$navigate("https://www.google.com/")
# remDr$getCurrentUrl()


#### Navigate to URL ####


baseURL <- 'https://www.hooktheory.com/theorytab/view/'
#URLs to Test 
url <- paste0(baseURL, 'the-beatles/here-comes-the-sun')
url <- paste0(baseURL, 'the-beatles/eight-days-a-week')
url <- paste0(baseURL, 'the-beatles/i-want-to-hold-your-hand')
url <- paste0(baseURL, 'taylor-swift/shake-it-off')
url <- paste0(baseURL, 'john-mellencamp/pink-houses')
url <- paste0(baseURL, 'the-beatles/julia')
url <- paste0(baseURL, 'men-at-work/land-down-under')
url <- paste0(baseURL, 'the-beatles/help')
url <- paste0(baseURL, 'the-beatles/hey-jude')

#Navigate to Url
remDr$navigate(url)

#### Extract Song Parts ####

names <- remDr$findElements(using="class", value = 'margin-0')
names <- remDr$findElements(using="css selector", value = "h2")
namestxt <- sapply(names, function(x) 
                      {x$getElementAttribute("outerHTML")[[1]]})
extract_song_parts(txt=namestxt)


#### Get Chords ####
elem <- remDr$findElements(using="class", value="app-content-score")

#For Single Song Part
#elemtxt <- elem$getElementAttribute("outerHTML")[1]
elemtxt <- elem$getElementAttribute("outerHTML")[[1]]

# For Multiple Song parts
elemtxt <- lapply(elem, function(x) 
                {x$getElementAttribute("outerHTML")[1]})

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
    str_remove_all("tspan") %>% 
    str_remove_all("quot") %>% 
    str_remove_all('dx') %>% 
    str_remove_all('ex') %>% 
    str_remove_all('class') %>% 
    str_remove_all('scale') %>% 
    str_remove_all('degrees') %>% 
    str_remove_all('maj') %>% 
    str_remove_all(as.character(75)) %>% 
    str_extract_all('[A-G]|[a-g]|6|7|sus|m|#') %>%               
    unlist %>% 
    paste(collapse= ' ') %>% 
    str_replace_all(' 6', '6') %>% 
    str_replace_all(' 7', '7') %>% 
    str_replace_all(' 9', '9') %>% 
    str_replace_all(' b', 'b') %>% 
    str_replace_all(' m', 'm') %>% 
    str_replace_all(' #', '#') %>% 
    str_replace_all ('am', ' am') %>% 
    str_replace_all ('bm', ' bm') %>% 
    str_replace_all ('cm', ' cm') %>% 
    str_replace_all ('dm', ' dm') %>% 
    str_replace_all ('em', ' em') %>% 
    str_replace_all ('fm', ' fm') %>% 
    str_replace_all ('gm', ' gm') %>% 
    str_replace_all ('am7', ' am7') %>% 
    str_replace_all ('bm7', ' bm7') %>% 
    str_replace_all ('cm7', ' cm7') %>% 
    str_replace_all ('dm7', ' dm7') %>% 
    str_replace_all ('em7', ' em7') %>% 
    str_replace_all ('fm7', ' fm7') %>% 
    str_replace_all ('gm7', ' gm7')

   



#### Scroll to Bottom ####
elem$sendKeysToElement(list(key = "end")) 
elem <- remDr$findElement(using="class", value="app-content-score")


elem <- remDr$findElement("css", "body")
elem$sendKeysToElement(list(key = "end"))

elem$screenshot(display = TRUE)



# Keep scrolling down page, loading new content each time. 
last_height = 0 #
repeat {   
  remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
  Sys.sleep(3) #delay by 3sec to give chance to load. 
  
  # Updated if statement which breaks if we can't scroll further 
  new_height = remDr$executeScript("return document.body.scrollHeight")
  if(unlist(last_height) == unlist(new_height)) {
    break
  } else {
    last_height = new_height
  }
}

elemtxt <- elem$getElementAttribute("outerHTML")[[1]]
elemxml <- htmlTreeParse(elemtxt, useInternalNodes=T)
xpath1 <- '//svg//g//tspan[@alignment-baseline]'
xpath2 <- '//tspan[@class="gotham"][@baseline-shift = "sub"][@font-size = 11]'
xpath <- paste(xpath1, xpath2, sep = '|')

x <- xpathApply(elemxml, xpath)
chords <- sapply(x,xmlValue) %>% 
  paste(sep = '', collapse = ' ') %>% 
  str_replace_all(' 4', '4') %>% 
  str_replace_all(' 6', '6') %>% 
  str_replace_all(' 7', '7') %>% 
  str_replace_all('  7', '7') %>%
  str_replace_all(' 9', '9') %>% 
  str_replace_all(' b', 'b') %>% 
  str_replace_all(' m', 'm') %>% 
  str_replace_all(' #', '#') %>% 
  str_replace_all('  sus', 'sus') %>% 
  str_replace_all('C 7', 'C7') %>% 
  str_replace_all('F 7', 'F7')  %>% 
  str_split(pattern = ' ') %>% 
  unlist

chords <- chords[chords != '']
chords

#### Try to Separate by Song Part ###

elem <- remDr$findElement("css", "body")
elem$sendKeysToElement(list(key = "down_arrow")) 
elem$errorDetails()$message
# remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
#elem <- remDr$findElement(using="class", value="app-content-score")

?findElement
elemtxt <- elem$getElementAttribute("outerHTML")[[1]]
elemxml <- htmlTreeParse(elemtxt, useInternalNodes=T)
#xpath <- '(//svg)'
xpath1 <- '//svg//g//tspan[@alignment-baseline]'
xpath2 <- '//svg//g//tspan[@class="gotham"][@baseline-shift = "sub"][@font-size = 11]'
xpath <- paste(xpath1, xpath2, sep = '|')

x <- xpathApply(elemxml, xpath)

chords <- sapply(x,xmlValue) %>% 
  paste(sep = '', collapse = ' ') %>% 
  str_replace_all(' 4', '4') %>% 
  str_replace_all(' 6', '6') %>% 
  str_replace_all(' 7', '7') %>% 
  str_replace_all('  7', '7') %>%
  str_replace_all(' 9', '9') %>% 
  str_replace_all(' b', 'b') %>% 
  str_replace_all(' m', 'm') %>% 
  str_replace_all(' #', '#') %>% 
  str_replace_all('  sus', 'sus') %>% 
  str_replace_all('C 7', 'C7') %>% 
  str_replace_all('F 7', 'F7')  %>% 
  str_split(pattern = ' ') %>% 
  unlist

chords
#### NOTES ####

# Do I need to extract slash chords? 


#### Old Code ####

# #fundList <- unlist(xpathApply(elemxml, xpath))
# 
# #Convert from XML to Character
# x <- fundList %>% 
#   sapply( saveXML) 
# x  %>%  unlist
# #Clean Text and Collapse
# x %>%
#   str_remove_all("baseline") %>% 
#   str_remove_all("alignment") %>% 
#   str_remove_all("middle") %>% 
#   str_remove_all("tspan") %>% 
#   str_remove_all("quot") %>% 
#   str_remove_all('dx') %>% 
#   str_remove_all('ex') %>% 
#   str_remove_all('class') %>% 
#   str_remove_all('scale') %>% 
#   str_remove_all('degrees') %>% 
#   str_remove_all('maj') %>% 
#   str_remove_all(as.character(75)) %>% 
#   str_extract_all('[A-G]|[a-g]|6|7|>sus<|m|#') %>%               
#   unlist %>% 
#   paste(collapse= ' ') %>% 
#   str_replace_all(' 6', '6') %>% 
#   str_replace_all(' 7', '7') %>% 
#   str_replace_all(' 9', '9') %>% 
#   str_replace_all(' b', 'b') %>% 
#   str_replace_all(' m', 'm') %>% 
#   str_replace_all(' #', '#') %>% 
#   str_replace_all ('am', ' am') %>% 
#   str_replace_all ('bm', ' bm') %>% 
#   str_replace_all ('cm', ' cm') %>% 
#   str_replace_all ('dm', ' dm') %>% 
#   str_replace_all ('em', ' em') %>% 
#   str_replace_all ('fm', ' fm') %>% 
#   str_replace_all ('gm', ' gm') %>% 
#   str_replace_all ('am7', ' am7') %>% 
#   str_replace_all ('bm7', ' bm7') %>% 
#   str_replace_all ('cm7', ' cm7') %>% 
#   str_replace_all ('dm7', ' dm7') %>% 
#   str_replace_all ('em7', ' em7') %>% 
#   str_replace_all ('fm7', ' fm7') %>% 
#   str_replace_all ('gm7', ' gm7')
# 



#elem <- remDr$findElement(using="class", value="app-content-score")


#elem$sendKeysToElement(list(key = "end"))  

