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

#### Import song urls ####

#Use Pre-made Data File 
links <- read.csv('data/complete_links.csv')


# # 2010s
# sub_links <- links %>%
#               filter(Decade == 2010)
# url_stems <- sub_links %>%
#                   pull(Links) %>%
#                   as.character
# nrow(sub_links) #424 songs; Est time 9h 12m
# 

# # 2000s
# sub_links <- links %>%
#               filter(Decade == 2000)
# url_stems <- sub_links %>%
#                   pull(Links) %>%
#                   as.character
# nrow(sub_links) #252



# # 1990s
# sub_links <- links %>%
#               filter(Decade == 1990)
# url_stems <- sub_links %>%
#                   pull(Links) %>%
#                   as.character
# nrow(sub_links) #203

# #1980s
# sub_links <- links %>%
#               filter(Decade == 1980)
# url_stems <- sub_links %>%
#                   pull(Links) %>%
#                   as.character
# nrow(sub_links)  #523 songs; est time: 11h 20m

##1970s
# sub_links <- links %>%
#               filter(Decade == 1970)
# url_stems <- sub_links %>%
#                   pull(Links) %>%
#                   as.character
# nrow(sub_links) #131

## 1960s
sub_links <- links %>%
              filter(Decade == 1960)
sub_links <- sub_links%>%
              slice(-grep('hard-days-night', sub_links$Links)) #Hard's Day Night Link Doesn't Work
url_stems <- sub_links %>%
                  pull(Links) %>%
                  as.character
nrow(sub_links) #167

#1950s
# sub_links <- links %>%
#               filter(Decade == 1950)
# url_stems <- sub_links %>%
#                   pull(Links) %>%
#                   as.character
# nrow(sub_links) #21


# OR Select Artist individually
# artist <- 'Queen'
# sub_links <- artist %>%  
#              lapply(extract_song_links) %>%  
#               bind_rows

#Create Song Urls
baseURL <- 'http://www.hooktheory.com'
song_urls <- paste0(baseURL, url_stems[!is.na(url_stems)])


#### Set Up Remote Driver ####

eCaps <- list(chromeOptions = list(
  args = list('--user-agent="music_fan"')
))
remDr <- remoteDriver(remoteServerAddr = "localhost",
                      extraCapabilities = eCaps,
                      port = 4445L, 
                      browserName = "chrome")

#### Web Scraping ####

df_row_list <- list() #Create Blank List

#Set Up Scraper
remDr$open() #Open Driver
remDr$setTimeout(type = 'page load', milliseconds = 60e3) #Set Timeout time
start <- 1 #Song in url vector to start at
end <- length(song_urls)#Song to end at 

### Option to set different start/end time
#start <- 
#end <- 2

#Set Load Time for allowing page to load after navigating to url and after each scroll down
min_load_time <- 12
max_load_time <- 17

#Set Sleep Time in between songs
min_sleep_time <- 15
max_sleep_time <- 25

total_songs <- end - start + 1 #Number of songs to scrape 
start_time <- Sys.time() #Record Start Time

#Run Loop and Pray
for(i in start:end){
 
  #Print Out Estimated Time
  current_song <- start + i - 1
  est_time <- (total_songs - i + 1)*(mean(min_load_time:max_load_time)*4 +
                                              mean(min_sleep_time:max_sleep_time)) /60
  if(est_time >= 60){
    
    est_time_hour <- floor(est_time / 60)
    est_time_minute <- ceiling(est_time %% 60)
    
    print(paste('Estimate Time Remaining...', est_time_hour, 'hours &',  
                est_time_minute, 'minutes ....'))
    
  } else{
    est_time_minute <- ceiling(est_time)
    print(paste('Estimate Time Remaining...', est_time_minute , 'minutes ....'))
  }
  
  
  #Navigate to  page
  print(paste('Scraping ',  current_song , 'th song out of ', total_songs,
              'songs'))
  remDr$navigate(song_urls[i])
  print(remDr$getCurrentUrl())
  
  #Wait for Page to Load
  # sec <- sample(min_load_time:max_load_time, 1)
  # split_secs <- runif(1)
  # sleep_short <-  sec +   split_secs
  # print(paste('Loading page.  Wait for ', sleep_short, 'secs..'))
  # Sys.sleep(sleep_short)
  
  #Extract Song Parts
  song_parts <- NA
  names <- remDr$findElements(using="class", value = 'margin-0')
  names <- remDr$findElements(using="css selector", value = "h2")
  namestxt <- sapply(names, function(x) 
  {x$getElementAttribute("outerHTML")[[1]]})
  song_parts <- extract_song_parts(txt=namestxt)

  #Extract Chords for Each Song Part
  if(length(song_parts)==0){
    df_row_list[[i]] <- data.frame(song_parts = NA, chords = NA)
    
  } else if(length(song_parts) > 0) {
    
    
    #Scroll to Bottom; pause after each scroll down to allow page to Load
    scroll_down(min_scroll_time =  min_load_time, 
                max_scroll_time = max_load_time,
                song_parts = song_parts )
    
    #Scrape Data
    elem <-  elemtxt <- elemxml<- idx <- NA
    elem <- remDr$findElement("css", "body")
    elemtxt <- elem$getElementAttribute("outerHTML")[[1]]
    elemxml <- htmlTreeParse(elemtxt, useInternalNodes=T)
    
    #Create Xpaths for Chord Data
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
      chord_string <- paste(chord_string, collapse = '-')
      chords[j] <-  chord_string 
      
    }
    
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
    mode <- secondary_vec[seq(1, length(secondary_vec), 2)] #Extract mode (even elems) from BPM (odd elems)
    
    #Combine Key with Mode
    key <- paste0(key_root, mode)

    #Store song info as data.frame
    full_song_info_df <-  data.frame(artist = sub_links$Artist[i], 
                                    song = sub_links$Songs[i], 
                                    song_parts,
                                    key = key,
                                    bpm = bpm,
                                    chords,
                                    link = song_urls[i])
    
    #Store Chord Data into data.frame.  Then Store data.frame in list
    df_row_list[[i]] <-   full_song_info_df

      }
  
    ### Close Session,
    print('Close session')
    remDr$close()
    
    #If not at end of song_urls vector, restart Driver and sleep before loading next page
    ## Else: end loop 
    if(i < end) {
      
      #Restart Session  
      print('Open new session')
      remDr$open(silent =T)
      remDr$setTimeout(type = 'page load', 
                       milliseconds = 60e3)
      
      #Sleep Before moving to next page
      sec <- sample( min_sleep_time:max_sleep_time, 1)
      split_secs <- runif(1)
      sleep_long <- sec + split_secs 
      print(paste('Sleeping for ', sleep_long, 'secs..'))
      Sys.sleep(sleep_long)
      
    } else if (i == end){
      print('Session Complete!')
  
      #Calculate Actual Run Time
      end_time <- Sys.time()
      run_time <- end_time - start_time
      run_time <-  as.numeric(as.duration(run_time))
      run_time_hours <- floor(run_time / 3600)
      run_time_minutes <- ceiling((run_time /60) %% 60)
      
      #Print estimated and actual run times
      est_run_time(url_list = song_urls, start = start, end = end)
      print(paste('Actual Run Time:',   run_time_hours, 'hours &', run_time_minutes, 'minutes'))
    }
}


#Store in Data.Frame
chords_df <- df_row_list %>%  
  compact %>% 
  lapply(function(x) mutate_all(x, as.character)) %>% 
  bind_rows 
View(chords_df) #View Data

#write.csv(chords_df, file = 'data/songs_70s.csv')

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

###NOTE ON DUPLICATE CHORD SEQUENCES
#I was going to remove repeating chords here 
#However, this caused problem scraping Bb 
#chord_string <- remove_dup_seqs(chord_string)



