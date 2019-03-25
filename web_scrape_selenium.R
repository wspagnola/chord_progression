#Create data.frame 
#Cols: artists, songs = available songs on hooktheory, links = links to pages with chords for each song

#OBTAIN LINKS FOR VECTOR OF ARTISTS: artist %>% lapply(extract_song_links) %>%  bind_rows
#SEARCH FOR SINGLE ARTIST: extract_song_links(artist) 

#Create a vector of artists 
artist <- c('tom petty', 
            'led zeppelin', 'elvis presley', 'ray charles', 'the beatles')
artist <- 'ray charles'

#Create Data.frame of available songs
links <- artist %>%  
  lapply(extract_song_links) %>%  
  bind_rows


#Filter Out Cover Songs
## Watch out for Singers with Different Bands
links <- links[tolower(links$Artist) %in% artist ,]


#### Web Scrape Chords ####

#STEPS to Load Dockter in Terminal
###docker pull selenium/standalone-firefox
###docker run -d -p 4445:4444 selenium/standalone-firefox


###docker pull selenium/standalone-chrome
###docker run -d -p 4445:4444 selenium/standalone-chrome


####  docker run -d -p 4445:4444 selenium/standalone-chrome:3.5.3

#LIST DOCKER CONTAINERS: docker container ls
#STOP DOCKER CONTAINER: docker stop test01

#### Set Up ####
source(file = 'source.R')

#Set up Driver 

remDr <- remoteDriver(remoteServerAddr = "localhost",
                      port = 4445L, 
                      browserName = "chrome")
remDr$open()

# extraCapabilities=fprof ?


#### Get URLs ###
baseURL <- 'http://www.hooktheory.com'
song_urls <- paste0(baseURL, links$Links)

####Extract Chords ####
df_row_list <- list() #Create Blank List

#Loop through urls
for(i in 1:length(song_urls)){
     
    
      sleep <- 4:10
      sleep_time <- sample(sleep, 1)
      print(paste('Sleep for ', sleep_time, ' seconds...'))
      Sys.sleep(sleep_time)
      #Navigate to Url
   
      remDr$navigate(song_urls[i])
      print(paste('Loading ',remDr$getCurrentUrl()[[1]]))
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
        scroll_time <- 5
        
        if(length(song_parts)==1){
          remDr$executeScript("window.scrollTo(0,300);")
          sleep_time <- sample(scroll_time , 1)
          print(paste(' Waiting ', sleep_time, ' seconds to load...'))
          Sys.sleep(sleep_time)
          
        } else if(length(song_parts)==2){
          remDr$executeScript("window.scrollTo(0,600);") #Scroll down page
          sleep_time <- sample(scroll_time, 1)     
          print(paste(' Waiting ', sleep_time, ' seconds to load...'))
          Sys.sleep(sleep_time)
          
          
          remDr$executeScript("window.scrollTo(0,900);") #Scroll down page
          sleep_time <- sample(scroll_time, 1)     
          print(paste(' Waiting ', sleep_time, ' seconds to load...'))
          Sys.sleep(sleep_time)

        } else if(length(song_parts) == 3){
          
          remDr$executeScript("window.scrollTo(0,0);") #Scroll down page
          sleep_time <- sample(scroll_time, 1)
          print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
          Sys.sleep(sleep_time)
          
          remDr$executeScript("window.scrollTo(0,300);") #Scroll down page
          sleep_time <- sample(scroll_time , 1)
          print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
          Sys.sleep(sleep_time)
          
          remDr$executeScript("window.scrollTo(0,600);") #Scroll down page
          sleep_time <- sample(scroll_time , 1)
          print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
          Sys.sleep(sleep_time)
        
          remDr$executeScript("window.scrollTo(0,900);") #Scroll down page
          sleep_time <- sample(scroll_time, 1)
          print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
          Sys.sleep(sleep_time)
          
          
        }else if(length(song_parts) >= 4){
          
          remDr$executeScript("window.scrollTo(0,0);") #Scroll down page
          sleep_time <- sample(scroll_time, 1)
          print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
          Sys.sleep(sleep_time)
          
          remDr$executeScript("window.scrollTo(0,300);") #Scroll down page
          sleep_time <- sample(scroll_time, 1)
          print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
          Sys.sleep(sleep_time)
          
          remDr$executeScript("window.scrollTo(0,600);") #Scroll down page
          sleep_time <- sample(scroll_time, 1)
          print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
          Sys.sleep(sleep_time)
          
          remDr$executeScript("window.scrollTo(0,900);") #Scroll down page
          sleep_time <- sample(scroll_time, 1)
          print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
          Sys.sleep(sleep_time)
          
          remDr$executeScript("window.scrollTo(0,1200);") #Scroll down page
          sleep_time <- sample(scroll_time, 1)
          print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
          Sys.sleep(sleep_time)
        }

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
        
        df_row_list[[i]] <- data.frame(artist = links$Artist[i], 
                                       song = links$Songs[i], 
                                       song_parts,
                                       chords,
                                       link = song_urls[i])
    }

}

chords_df <- df_row_list %>%  
  compact %>% 
  lapply(function(x) mutate_all(x, as.character)) %>% 
  bind_rows 
View(chords_df)

write.csv(chords_df, file = 'data/sample_data_frame.csv')

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


#### NOTES ####

# Do I need to extract slash chords? 

#Is this useful?
#remDr$executeScript('window.scrollTo(0, document.body.scrollHeight);')



#RSelenium Commands
# remDr$getStatus()
# remDr$navigate("https://www.google.com/")
# remDr$getCurrentUrl()
# remDr$screenshot(display = T)