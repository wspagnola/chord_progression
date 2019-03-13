#STEPS to Load Dockter in Terminal
###docker pull selenium/standalone-firefox
###docker run -d -p 4445:4444 selenium/standalone-firefox


###docker pull selenium/standalone-chrome
###docker run -d -p 4445:4444 selenium/standalone-chrome


#LIST DOCKER CONTAINERS: docker container ls
#STOP DOCKER CONTAINER: docker stop test01

require(rvest)
require(XML)
require(tidyverse)
require(seleniumPipes)
require(RSelenium)
require(httr)

#Clean song_contents
clean_song_contents <- function(x){
  require(tidyverse)
  clean_x <- x %>% 
                str_replace_all(' 4', '4') %>% 
                str_replace_all(' 6', '6') %>% 
                str_replace_all(' 7', '7') %>% 
                str_replace_all('  7', '7') %>%
                str_replace_all(' 9', '9') %>% 
                str_replace_all(' b', 'b') %>% 
                str_replace_all(' m', 'm') %>% 
                str_replace_all(' #', '#') %>% 
                str_replace_all('  sus', 'sus') %>% 
                str_replace_all('   sus', 'sus') %>% 
                str_replace_all('A 7', 'A7') %>% 
                str_replace_all('B 7', 'B7') %>% 
                str_replace_all('C 7', 'C7') %>% 
                str_replace_all('D 7', 'D7') %>% 
                str_replace_all('E 7', 'E7') %>% 
                str_replace_all('F 7', 'F7') %>% 
                str_replace_all('G 7', 'G7') %>% 
                str_replace_all('# 7', '#7') %>% 
                str_replace_all('b 7', 'b7') %>% 
                str_replace_all('     \\( ', '(') %>% 
                str_replace_all(' \\) ', ') ') 
              
  return(clean_x)
}



#Extract Song Parts
extract_song_parts <- function(txt) {
  
  require(stringr)
  
  #Character Vector of Types of Song Parts
  part_types <- c('Intro', 'Verse', 'Pre-Chorus', 'Bridge', 
                  'Chorus', 'Outro', 'Instrumental')
  
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


remove_dup_seqs <- function(v){
  #Takes a vector and removes duplicated sequences (e.g. repeated chords)
  
  if(sum(is.na(v) > 0)){
    warning('Vector cannot contain NAs. ')
    
  } else if(sum(is.na(v)==0)){
    idx <- c(NA, v) != c(v, NA)
    idx <- idx[!is.na(idx)]
    return(v[idx])
  }
}



#### Open Selenium ####

#Set up Driver ? 
remDr <- remoteDriver( remoteServerAddr = "localhost",
                      port = 4445L, 
                      browserName = "chrome")
#Open Selenium ?
remDr$open()

#TEST
# remDr$getStatus()
# remDr$navigate("https://www.google.com/")
# remDr$getCurrentUrl()


#### Navigate to URL ####

baseURL <- 'http://www.hooktheory.com/theorytab/view/'

url_ends <- paste(d$Artist,'/', d$Songs) %>%  
                  tolower %>% 
                  str_replace_all(pattern = ' / ', replacement = '/') %>% 
                  str_replace_all(pattern = ' ', replacement = '-') %>% 
                  str_remove_all("'")
song_urls <- paste0(baseURL, url_ends)

df_row_list <- list()
for(i in 1:length(song_urls)){
      #print(i)
        
      sleep_time <- sample(1:5, 1)
      print(paste('Sleep for ', sleep_time, ' seconds...'))
      #Sys.sleep(sleep_time)
      #Navigate to Url
    
      remDr$navigate(song_urls[i])
      print(remDr$getCurrentUrl())
      #remDr$screenshot(display = T)
      
      #### Extract Song Parts ####
      song_parts <- NA
      names <- remDr$findElements(using="class", value = 'margin-0')
      names <- remDr$findElements(using="css selector", value = "h2")
      namestxt <- sapply(names, function(x) 
                            {x$getElementAttribute("outerHTML")[[1]]})
      song_parts <- extract_song_parts(txt=namestxt)
      song_parts
      
      if(length(song_parts)==0){
        df_row_list[[i]] <- data.frame(song_parts = NA, chords = NA)
      } else if(length(song_parts) > 0) {
     
        
        #### Scroll to Bottom ####
        
        
        if(length(song_parts)==1){
          remDr$executeScript("window.scrollTo(0,300);")
        } else if(length(song_parts)==2){
          remDr$executeScript("window.scrollTo(0,600);") #Scroll down page
        } else if(length(song_parts) == 3){
          remDr$executeScript("window.scrollTo(0,1200);") #Scroll down page
        }else if(length(song_parts) == 4){
          remDr$executeScript("window.scrollTo(0,1500);") #Scroll down page
          
          sleep_time <- sample(1:3, 1)
          print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
          
        }

        
        #remDr$executeScript('window.scrollTo(0, document.body.scrollHeight);')
        elem <-  elemtxt <- elemxml<- idx <- NA
       
        elem <- remDr$findElement("css", "body")
        remDr$getCurrentUrl()
         #Print screen shot to find current location on page
        elem$screenshot(display =T)
        
        elemtxt <- elem$getElementAttribute("outerHTML")[[1]]
        elemxml <- htmlTreeParse(elemtxt, useInternalNodes=T)
        
        
        idx <- 1:length(song_parts)*3
        xpath <- paste0('(//svg)[', idx, ']//tspan[@alignment-baseline]',
                         '|(//svg)[', idx, ']//tspan[@class][@baseline-shift]')
        
       
        chords <- rep(NA, length(song_parts))
        x <- NA
     
        for(j in 1:length(chords)){
      
          x <- xpathApply(elemxml, xpath[j])  
          
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
        
        df_row_list[[i]] <- data.frame(artist = d$Artist[i], 
                                       song = d$Songs[i], 
                                       song_parts,
                                       chords)
    }
  
}
df_row_list %>%  
  lapply(function(x) mutate_all(x, as.character)) %>% 
  bind_rows %>% 
  View

#### NOTES ####

# Do I need to extract slash chords? 


#### Old Code ####



#elem <- remDr$findElement(using="class", value="app-content-score")


#elem$sendKeysToElement(list(key = "end"))  

# 
# 
# #### Get Chords ####
# elem <- remDr$findElements(using="class", value="app-content-score")
# 
# #For Single Song Part
# #elemtxt <- elem$getElementAttribute("outerHTML")[1]
# elemtxt <- elem$getElementAttribute("outerHTML")[[1]]
# 
# # For Multiple Song parts
# elemtxt <- lapply(elem, function(x) 
# {x$getElementAttribute("outerHTML")[1]})
# 
# # parse string into HTML tree to allow for querying with XPath
# elemxml <- htmlTreeParse(elemtxt, useInternalNodes=T)
# 
# 
# #### Extract 7th and Major Chords "
# 
# xpath <- '//tspan[@alignment-baseline]'
# fundList <- unlist(xpathApply(elemxml, xpath))
# 
# #Convert from XML to Character
# x <- fundList %>% 
#   sapply( saveXML) 



