#### Set Up Docker ####

#STEPS to Load Dockter in Terminal
###docker pull selenium/standalone-firefox (only need to do this once)
###docker run -d -p 4445:4444 selenium/standalone-firefox


###docker pull selenium/standalone-chrome (only need to do this once)
###docker run -d -p 4445:4444 selenium/standalone-chrome

#LIST DOCKER CONTAINERS: docker container ls
#STOP DOCKER CONTAINER: docker stop test01

#### Set Up Scraper ####

source(file = 'source.R')

artist_list <- read.csv('data/artist_list.csv')

this_decade <- artist_list %>%  filter(Decade == 2010)

artist <- as.character(this_decade$Performer)
#Create a vector of artists 

#artist <- c('taylor swift', 'justin bieber', 'katy perry', 'drake')
#artist <- c('tom petty', 'led zeppelin', 'elvis presley', 'ray charles', 'the beatles')

#Create Data.frame of available songs
#artist <- artist[11:20]  ##sub sample artist vector if needed
links <- artist[1:10] %>%  
  lapply(extract_song_links) %>%  
  bind_rows

#write.csv(links, file = 'data/links_2010s_1_10.csv')

#### Set Up ####

#Set up Driver 
rm(remDr)
eCaps <- list(chromeOptions = list(
  args = list('--user-agent="music_fan"')
))



remDr <- remoteDriver(remoteServerAddr = "localhost",
                      extraCapabilities = eCaps,
                      port = 4445L, 
                      browserName = "chrome")

#### Get URLs ###
baseURL <- 'http://www.hooktheory.com'
song_urls <- paste0(baseURL, links$Links[!is.na(links$Links)])

df_row_list <- list() #Create Blank List
remDr$open() #Open Driver
remDr$setTimeout(type = 'page load', milliseconds = 60e3) #Set Timeout time
start <- 1 #Song in url vector to start at
end <- length(song_urls)#Song to end at 
total_songs <- end - start + 1 #Number of songs to scrape 

#Run Loop and Pray
for(i in start:end){
  
  current_song <- i - start + 1
  print(paste('Scraping ',  current_song , 'th song out of ', total_songs,
              'songs'))
  
  remDr$navigate(song_urls[i])
  print(remDr$getCurrentUrl())
  
  sec <- sample(10:15, 1)
  split_secs <- runif(1)
  sleep_short <-  sec +   split_secs
  print(paste('Sleep for ', sleep_short, 'secs..'))
  Sys.sleep(sleep_short)
  
  
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
    scroll_time <- 10
    scroll_down(scroll_time)
    
    elem <-  elemtxt <- elemxml<- idx <- NA
    elem <- remDr$findElement("css", "body")
    elemtxt <- elem$getElementAttribute("outerHTML")[[1]]
    elemxml <- htmlTreeParse(elemtxt, useInternalNodes=T)
    
    #Create Xpaths
    idx <- 1:length(song_parts)*3
    xpath <- paste0('(//svg)[', idx, ']//tspan[@alignment-baseline]',
                    '|(//svg)[', idx, ']//tspan[@baseline-shift]')
    
    #Create Empty Vectors to store Xpaths and chords
    chords <- rep(NA, length(song_parts))
    x <- NA
    
    #### Extract Chords
    for(j in 1:length(chords)){
      
      x <- xpathApply(elemxml, xpath[j])  
      
      chord_string <- character()
      chord_string <- sapply(x,xmlValue) %>% 
        paste(sep = '', collapse = ' ') %>%
        clean_song_contents %>% 
        str_split(pattern = ' ') %>% 
        unlist
      
      chord_string <- chord_string[chord_string != '']
      
      #I was going to remove repeating chords here 
      #However, this caused problem scraping Bb from Baby by Justin Bieber
      #chord_string <- remove_dup_seqs(chord_string)
      
      
      chord_string <- paste(chord_string, collapse = '-')
      chords[j] <-  chord_string 
      
    }
    
    df_row_list[[i]] <- data.frame(artist = links$Artist[i], 
                                   song = links$Songs[i], 
                                   song_parts,
                                   chords,
                                   link = song_urls[i])
  }
  
    ### Close Restart Session  
    print('Close session')
    remDr$close()
    print('Open new session')
    
    remDr$open(silent =T)
    print('Set time out')
    remDr$setTimeout(type = 'page load', 
                     milliseconds = 60e3)
    
    sec <- sample(60:120, 1)
    split_secs <- runif(1)
    sleep_long <- sec + split_secs 
    print(paste('Sleep for ', sleep_long, 'secs..'))
    
    Sys.sleep(sleep_long)
  
}
remDr$close()


#Store in Data.Frame
chords_df <- df_row_list %>%  
  compact %>% 
  lapply(function(x) mutate_all(x, as.character)) %>% 
  bind_rows 
View(chords_df) #View Data


#write.csv(chords_df, file = 'data/sample_data_frame.csv')

#### ERROR MESSAGES ####

# #
# Selenium message:unknown error: session deleted because of page crash
# from unknown error: cannot determine loading status
# from tab crashed
# (Session info: chrome=72.0.3626.121)
# (Driver info: chromedriver=2.46.628388 (4a34a70827ac54148e092aafb70504c4ea7ae926),platform=Linux 4.9.125-linuxkit x86_64)
# 
# Error: 	 Summary: UnknownError
# Detail: An unknown server-side error occurred while processing the command.
# Further Details: run errorDetails method



# Selenium message:timeout
# (Session info: chrome=73.0.3683.86)
# (Driver info: chromedriver=73.0.3683.68 (47787ec04b6e38e22703e856e101e840b65afe72),platform=Linux 4.9.125-linuxkit x86_64)
# 
# Error: 	 Summary: Timeout
# Detail: An operation did not complete before its timeout expired.
# Further Details: run errorDetails method


#### NOTES ####

# Do I need to extract slash chords? 

#Is this useful?
#remDr$executeScript('window.scrollTo(0, document.body.scrollHeight);')



#RSelenium Commands
# remDr$getStatus()
# remDr$navigate("https://www.google.com/")
# remDr$getCurrentUrl()
# remDr$screenshot(display = T)


#PRACTICE NAVIGATING
# remDr$navigate('http://www.google.com')
# remDr$getCurrentUrl()
# remDr$screenshot(display =T)
# remDr$navigate('http://www.yahoo.com')
# remDr$getCurrentUrl()
# remDr$screenshot(display =T)
# remDr$navigate('http://www.facebook.com')
# remDr$getCurrentUrl()
# remDr$screenshot(display =T)
# remDr$navigate(song_urls[29])
# remDr$getCurrentUrl()
# remDr$screenshot(display =T)
# rm(remDr)
# remDr$open()
# remDr$navigate("http://www.google.com")
# remDr$executeScript("return navigator.userAgent;", list(""))
# remDr$setTimeout(type = 'page load', milliseconds = 60e3)



#Filter Out Cover Songs
## Watch out for Singers with Different Bands
#links <- links[tolower(links$Artist) %in% tolower(artist) ,]
#Don't Know if Need this



