#STEPS to Load Dockter in Terminal
###docker pull selenium/standalone-firefox
###docker run -d -p 4445:4444 selenium/standalone-firefox


source(source.R)


#### Open Selenium ####

#Set up Driver ? 
remDr <- remoteDriver( remoteServerAddr = "localhost",
                      port = 4445L, 
                      browserName = "chrome")
#Open Selenium ?
remDr$open()


#### Navigate to URL ####


baseURL <- 'https://www.hooktheory.com/theorytab/view/'
#URLs to Test 
#url <- paste0(baseURL, 'the-beatles/blackbird')
#url <- paste0(baseURL, 'led-zeppelin/tangerine')
#url <- 'http://www.hooktheory.com/theorytab/view/tom-petty-and-the-heartbreakers/mary-janes-last-dance'
url <- 'http://www.hooktheory.com/theorytab/view/paul-mccartney---michael-jackson/say-say-say'

#Navigate to Url
remDr$open()
remDr$navigate(url)
remDr$getCurrentUrl()
#### Extract Song Parts ####

names <- remDr$findElements(using="class", value = 'margin-0')
names <- remDr$findElements(using="css selector", value = "h2")
namestxt <- sapply(names, function(x) 
                      {x$getElementAttribute("outerHTML")[[1]]})
song_parts <- extract_song_parts(txt=namestxt)


#### PRACTICE SCRAPING ####
elem <-  elemtxt <- elemxml<- idx <- NA

elem <- remDr$findElement("css", "body")


elemtxt <- elem$getElementAttribute("outerHTML")[[1]]
elemxml <- htmlTreeParse(elemtxt, useInternalNodes=T)
#For Single Song Part
#elemtxt <- elem$getElementAttribute("outerHTML")[1]
elemtxt <- elem$getElementAttribute("outerHTML")[[1]]


# remDr$getCurrentUrl()
#  #Print screen shot to find current location on page
# elem$screenshot(display =T)

# parse string into HTML tree to allow for querying with XPath
elemxml <- htmlTreeParse(elemtxt, useInternalNodes=T)

idx <- 1:length(song_parts)*3
xpath <- paste0('(//svg)[', idx, ']//tspan[@alignment-baseline]',
                '|(//svg)[', idx, ']//tspan[@baseline-shift]')

chords <- rep(NA, length(song_parts))
x <- NA
x <- xpathApply(elemxml, xpath)  


for(j in 1:length(chords)){
  
  x <- xpathApply(elemxml, xpath[1])  
  
  chord_string <- character()
  chord_string <- sapply(x,xmlValue) %>% 
    paste(sep = '', collapse = ' ') %>%
    clean_song_contents %>% 
    str_split(pattern = ' ') %>% 
    unlist
  
  chord_string <- chord_string[chord_string != '']
  chord_string <- remove_dup_seqs(chord_string)
  chord_string <- paste(chord_string, collapse = '-')
  chords[j] <-  chord_string 
  
}
chords

remDr$close()



