#STEPS to Load Dockter in Terminal
###docker pull selenium/standalone-firefox
###docker run -d -p 4445:4444 selenium/standalone-firefox


###docker pull selenium/standalone-chrome
###docker run -d -p 4445:4444 selenium/standalone-chrome


#LIST DOCKER CONTAINERS: docker container ls
#STOP DOCKER CONTAINER: docker stop test01

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

source(file = 'source.R')

#### Navigate to URL ####


baseURL <- 'http://www.hooktheory.com'
song_urls <- paste0(baseURL, d$Links)



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
          remDr$executeScript("window.scrollTo(0,0);") #Scroll down page
          sleep_time <- sample(1:3, 1)
          print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
          
          remDr$executeScript("window.scrollTo(0,300);") #Scroll down page
          sleep_time <- sample(1:3, 1)
          print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
          
          
          remDr$executeScript("window.scrollTo(0,600);") #Scroll down page
          sleep_time <- sample(1:3, 1)
          print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
          
          
          remDr$executeScript("window.scrollTo(0,900);") #Scroll down page
          sleep_time <- sample(1:3, 1)
          print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
          
        }else if(length(song_parts) == 4){
          remDr$executeScript("window.scrollTo(0,0);") #Scroll down page
          sleep_time <- sample(1:3, 1)
          print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
          
          remDr$executeScript("window.scrollTo(0,300);") #Scroll down page
          sleep_time <- sample(1:3, 1)
          print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
          
          
          remDr$executeScript("window.scrollTo(0,600);") #Scroll down page
          sleep_time <- sample(1:3, 1)
          print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
          
          
          remDr$executeScript("window.scrollTo(0,900);") #Scroll down page
          sleep_time <- sample(1:3, 1)
          print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
          
        }

        
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
                                       chords,
                                       link = d$Links[i])
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


#baseURL <- 'http://www.hooktheory.com/theorytab/view/'

# url_ends <- paste(d$Artist,'/', d$Songs) %>%  
#                   tolower %>% 
#                   str_replace_all(pattern = ' / ', replacement = '/') %>% 
#                   str_replace_all(pattern = ' ', replacement = '-') %>% 
#                   str_remove_all("'")
# song_urls <- paste0(baseURL, url_ends)

#remDr$executeScript('window.scrollTo(0, document.body.scrollHeight);')

