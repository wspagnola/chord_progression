require(rvest)
require(XML)
require(tidyverse)
require(seleniumPipes)
require(RSelenium)
require(httr)
require(lubridate)
require(spotifyr)
require(RCurl)

#Not sure if I need these
#require(jsonlite)
#require(xml2)
#require(RCurl)


#Get Spotify Token
my_token <- get_spotify_access_token(client_id = 'b7e786e6e51541e7b0e39a1c547e3434', 
                                     client_secret = '7164828c5b29439882806d654c6cfd73')


#Clean song_contents
clean_song_contents <- function(x){
  require(tidyverse)

    clean_x <- x %>% 
    str_replace_all('\\s+m', 'm') %>% 
    str_replace_all('\\s+#', '#') %>% 
    str_replace_all('\\s+o', 'o') %>% 
    str_replace_all('\\s+sus', 'sus') %>% 
    str_replace_all('\\s+sus2', 'sus2') %>% 
    str_replace_all('\\s_sus4', 'sus4') %>% 
    str_replace_all('\\s+\\) ', ') ') %>% 
    str_replace_all('\\s+\\( ', '(') %>% 
    str_replace_all('\\s+2', '2') %>% 
    str_replace_all('\\s+4', '4') %>% 
    str_replace_all('\\s+6', '6') %>% 
    str_replace_all('\\s+7', '7') %>% 
    str_replace_all('\\s+9', '9') %>% 
    str_replace_all('A\\s+b ', 'Ab') %>% 
    str_replace_all('B\\s+b ', 'Bb') %>% 
    str_replace_all('C\\s+b ', 'Cb') %>% 
    str_replace_all('D\\s+b ', 'Db') %>% 
    str_replace_all('E\\s+b ', 'Eb') %>% 
    str_replace_all('F\\s+b ', 'Fb') %>% 
    str_replace_all('G\\s+b ', 'Gb') %>% 
    str_replace_all('a\\s+b ', 'ab') %>% 
    str_replace_all('b\\s+b ', 'bb') %>% 
    str_replace_all('c\\s+b ', 'cb') %>% 
    str_replace_all('d\\s+b ', 'db') %>% 
    str_replace_all('e\\s+b ', 'eb') %>% 
    str_replace_all('f\\s+b ', 'fb') %>% 
    str_replace_all('g\\s+b ', 'gb') %>% 
    str_replace_all('A 7', 'A7') %>% 
    str_replace_all('B 7', 'B7') %>% 
    str_replace_all('C 7', 'C7') %>% 
    str_replace_all('D 7', 'D7') %>% 
    str_replace_all('E 7', 'E7') %>% 
    str_replace_all('F 7', 'F7') %>% 
    str_replace_all('G 7', 'G7') %>% 
    str_replace_all('b 5', 'b5') %>% 
    str_replace_all('# 7', '#7') %>% 
    str_replace_all('b 7', 'b7')  %>% 
    str_replace_all('bb\\s+b', 'b bb')  %>% 
    str_replace_all('bbb', 'b bb') %>%
    str_replace_all('B b7', 'Bb7')  
  
  
  return(clean_x)
}



#Extract Song Parts
extract_song_parts <- function(txt) {
  
  require(tidyverse)
  
  #Character Vector of Types of Song Parts
  part_types <- c('Intro', 'Verse', 'Pre-Chorus', 'Bridge', 
                  'Chorus', 'Instrumental', 'Solo', 'Outro') 
  
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


#Input a vector of artist a names 
#Gets all the song names & links on Hooktheory for said Artists
#Returns dataframe with Artist, Song, Links
extract_song_links <- function(artist){

  #Convert to lower case
  artist <- tolower(artist) 
  
  #Attach name to baseURL to create a vector of urls to search
  baseURL <- 'https://www.hooktheory.com/theorytab/results/path/'
  url <- paste0(baseURL, artist)
  url <- url %>% 
    str_replace(pattern = ' ', replacement = '+')
  
  #Check if URL exists
  url_exists <- RCurl::url.exists(url)
  
  
  if(url_exists == F){
    
        artist_vec <- artist
        songs <- NA
        href_vec <- NA
    
  } else if (url_exists == T){
    page_vec <- NA
    page_vec <- url %>% 
      read_html %>%
      html_nodes(xpath = '//a[@class="button button-xs button-browse button-primary-open "]') %>% 
      html_text()
    
    if(length(page_vec) == 0) {
      
      songs <- NA
      artist_vec <- NA
      songs <-  url %>% 
        read_html %>% 
        html_nodes(xpath = '//p[@class ="song"]') %>% 
        html_text()
      
      href_vec <-  url %>% 
        read_html %>% 
        html_nodes(xpath = '//li/a[@class="a-no-decoration"]') %>% 
        html_attr(name = 'href')
      
      artist_vec <-  url %>% 
        read_html %>% 
        html_nodes(xpath = '//p[@class ="artist"]') %>% 
        html_text()
      
      artist_vec <- artist_vec %>%  str_remove('by ')
      
    } else if(length(page_vec) > 1){
      
      num_pages <- page_vec %>%  as.numeric %>%  max
      
      url <- url %>% paste0('/page/', 1:num_pages )
      
      song_list <- url %>%  lapply(function(x){x %>% 
          read_html %>% 
          html_nodes(xpath = '//p[@class ="song"]') %>% 
          html_text()}
      )
      
      artist_list <- url %>% 
        lapply(function(x){x %>% 
            read_html %>% 
            html_nodes(xpath = '//p[@class ="artist"]') %>% 
            html_text()}
        )
      
      href_list <-  url %>%  
        lapply(function(x){x %>% 
            read_html %>% 
            html_nodes(xpath = '//li/a[@class="a-no-decoration"]') %>% 
            html_attr(name = 'href')}
        )
      
      
      songs <- unlist(song_list)
      
      artist_list <- artist_list %>% lapply(function(x) str_remove(x, pattern = 'by '))
      artist_vec <- unlist(artist_list)
      
      href_vec <- unlist(href_list)
    } 
    # #else if(is.na(page_vec)){
    #   artist_vec <- artist
    #   songs <- NA
    #   href_vec <- NA
    #   
    # }
    
  }
  
  #Store Artist, Song and Link in Data.frame
  d <- data.frame(Artist = artist_vec, 
                  Songs = songs, 
                  Links = href_vec)  
  d <- d %>%  
          mutate_all(as.character)
  
  
  Sys.sleep(5)
  
  return(d)
  
}


extract_song_key <- function(x) {
  key <- str_sub(x$chord[1], end = 4L)
  key <- key %>%  str_remove('-.*') 
  return(key)
}



get_artist_tracks <- function(artist_name, token){
  
  require(tidyr)
  require(dplyr)
  artists <- get_artists(artist_name, access_token = token) 
  Sys.sleep(2)
  albums <- get_albums(artists$artist_uri[1], access_token = token )
  Sys.sleep(2)
  songs <- get_album_tracks(albums, access_token = my_token)
  
  songs$artist <- artist_name
  songs <-   songs %>% select(artist, everything())
  return(songs)
}

# 
# scroll_down <- function(scroll_time){
#   
#   max_scroll_time <- scroll_time + 5
#   
#   
#   if(length(song_parts)==1){
#     
# 
#     remDr$executeScript("window.scrollTo(0,300);")
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     print(paste(' Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     
#     remDr$executeScript("window.scrollTo(0,600);")
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     print(paste(' Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     
#     # remDr$executeScript("window.scrollTo(0,900);")
#     # sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     # print(paste(' Waiting ', sleep_time, ' seconds to load...'))
#     # Sys.sleep(sleep_time)
#     
#     
#   } else if(length(song_parts)==2){
#     
#     
#     remDr$executeScript("window.scrollTo(0,300);")
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     print(paste(' Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     remDr$executeScript("window.scrollTo(0,600);") #Scroll down page
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)     
#     print(paste(' Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     
#     remDr$executeScript("window.scrollTo(0,900);") #Scroll down page
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)     
#     print(paste(' Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     # remDr$executeScript("window.scrollTo(0,1200);") #Scroll down page
#     # sleep_time <- sample(scroll_time:max_scroll_time, 1)     
#     # print(paste(' Waiting ', sleep_time, ' seconds to load...'))
#     # Sys.sleep(sleep_time)
#     
#   } else if(length(song_parts) == 3){
#     
#     remDr$executeScript("window.scrollTo(0,0);") #Scroll down page
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     remDr$executeScript("window.scrollTo(0,300);") #Scroll down page
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     remDr$executeScript("window.scrollTo(0,600);") #Scroll down page
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     remDr$executeScript("window.scrollTo(0,900);") #Scroll down page
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     remDr$executeScript("window.scrollTo(0,1200);") #Scroll down page
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     # remDr$executeScript("window.scrollTo(0,1500);") #Scroll down page
#     # sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     # print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
#     # Sys.sleep(sleep_time)
#     
#     
#     
#   }else if(length(song_parts) >= 3){
#     
#     remDr$executeScript("window.scrollTo(0,0);") #Scroll down page
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     remDr$executeScript("window.scrollTo(0,300);") #Scroll down page
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     remDr$executeScript("window.scrollTo(0,600);") #Scroll down page
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     remDr$executeScript("window.scrollTo(0,900);") #Scroll down page
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     remDr$executeScript("window.scrollTo(0,1200);") #Scroll down page
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     remDr$executeScript("window.scrollTo(0,1500);") #Scroll down page
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     
#     # remDr$executeScript("window.scrollTo(0,1800);") #Scroll down page
#     # sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     # print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
#     # Sys.sleep(sleep_time)
#   }else if(length(song_parts) == 5){
#     
#     remDr$executeScript("window.scrollTo(0,0);") #Scroll down page
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     remDr$executeScript("window.scrollTo(0,300);") #Scroll down page
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     remDr$executeScript("window.scrollTo(0,600);") #Scroll down page
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     remDr$executeScript("window.scrollTo(0,900);") #Scroll down page
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     remDr$executeScript("window.scrollTo(0,1200);") #Scroll down page
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     remDr$executeScript("window.scrollTo(0,1500);") #Scroll down page
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     
#     remDr$executeScript("window.scrollTo(0,1800);") #Scroll down page
#     sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
#     Sys.sleep(sleep_time)
#     
#     
#     # remDr$executeScript("window.scrollTo(0,2100);") #Scroll down page
#     # sleep_time <- sample(scroll_time:max_scroll_time, 1)
#     # print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
#     # Sys.sleep(sleep_time)
#   }
# }


scroll_down <- function(min_scroll_time = 15, max_scroll_time = 25, song_parts){
  
  if(min_scroll_time > max_scroll_time){
    
    warning('Min Scroll Time must be larger than max scroll time!')
  } else if(is.numeric(min_scroll_time) == F | is.numeric(max_scroll_time ) == F |
            min_scroll_time <= 0 | max_scroll_time <= 0){
    
    warning('Min Scroll Time and Max Scroll Time must be positive integers!')
    
  } else{
    
    #Set Number of Scroll Downs
    if(length(song_parts) < 5) {
      
      scroll_downs <- 1 + length(song_parts)
    } else {
      
      scroll_downs <- 2 + length(song_parts)

    }
   
    for(i in 1:scroll_downs){
      
      #Set sleep time 
      sleep_time <- sample(min_scroll_time:max_scroll_time, 1)
      split_secs <- runif(1)
      sleep_time <- sleep_time +   split_secs
      print(paste('Waiting ', sleep_time, ' seconds to load...'))
      Sys.sleep(sleep_time)
      
      #Scroll Down 
      scroll_down_length <- i*300
      scroll_command <-  paste0('window.scrollTo(0,', scroll_down_length, ');')
      print(paste('Scrolling down to page to line',  scroll_down_length))
      remDr$executeScript(scroll_command)
      
    }
  }
}


guess_key <- function(x){
  
  require(stringr)
  
  #Input character string of chords separated by '-'
  #Return Matrix with Boolean Values for possible keys
  
  x <- remove_features(x) #Remove all features except major, minor, and dominant 7th
  
  chord_vec <- unlist(str_split(x, pattern = '-'))   #Break chords up into character vector

  #Define Keys
  C_maj_chords <- c('C', 'dm', 'em', 'F', 'G', 'G7','am', 'B')
  D_maj_chords <- c('D', 'em', 'f#m', 'G', 'A', 'A7', 'bm', 'C')
  E_maj_chords <- c('E', 'f#m', 'g#m', 'A', 'B', 'B7', 'c#m', 'D#')
  F_maj_chords <- c('F', 'gm', 'am', 'Bb', 'C', 'C7', 'dm',  'A')
  G_maj_chords <- c('G', 'am', 'bm', 'C', 'D', 'D7', 'em', 'F#')
  A_maj_chords <- c('A', 'bm', 'c#m', 'D', 'E', 'E7', 'f#m', 'G#')
  B_maj_chords <- c('B', 'c#m', 'd#m', 'E', 'F#', 'F#7', 'g#m', 'A#')

  #Define Flat Keys
  Db_maj_chords <- c('Db', 'ebm', 'fm', 'Gb', 'Ab', 'Ab7', 'bbm', 'C')
  Eb_maj_chords <- c('E', 'fm', 'gm', 'Ab', 'Bb', 'Bb7', 'cm', 'D')
  Gb_maj_chords <- c('Gb', 'abm', 'bbm', 'Cb', 'Db', 'Db7', 'ebm', 'F')
  Ab_maj_chords <- c('Ab', 'bbm', 'cm', 'Db', 'Eb', 'Eb7', 'fm', 'G')
  Bb_maj_chords <- c('Bb', 'cm', 'dm', 'Eb', 'F', 'F7', 'gm', 'A')
  
  
  #Guess Key
  C_maj_pct <- mean(chord_vec %in% C_maj_chords)
  D_maj_pct <- mean(chord_vec %in% D_maj_chords)
  E_maj_pct <- mean(chord_vec %in% E_maj_chords)
  F_maj_pct <- mean(chord_vec %in% F_maj_chords)
  G_maj_pct <- mean(chord_vec %in% G_maj_chords) 
  A_maj_pct <- mean(chord_vec %in% A_maj_chords)
  B_maj_pct <- mean(chord_vec %in% B_maj_chords)
  
  #Flats
  Db_maj_pct <- mean(chord_vec %in% Db_maj_chords)
  Eb_maj_pct <- mean(chord_vec %in% Eb_maj_chords)
  Gb_maj_pct <- mean(chord_vec %in% Gb_maj_chords) 
  Ab_maj_pct <- mean(chord_vec %in% Ab_maj_chords)
  Bb_maj_pct <- mean(chord_vec %in% Bb_maj_chords)
  
  #Note Cb = B & Fb = E
  
  #Check C Major
  if(C_maj_pct==1 ){
        C_maj <- TRUE
  } else {
    C_maj <- FALSE
    
  }
  
  #Check D Major
  if(D_maj_pct==1){
    
        D_maj <- TRUE
  } else{
    D_maj <- FALSE
  }

  #Check E major
  if(E_maj_pct==1){
      
      E_maj <- TRUE
  } else{
    E_maj <- FALSE
  }
  
  #Check F major
  if(F_maj_pct==1){
    
    F_maj <- TRUE
  } else{
    F_maj <- FALSE
  }
  
  #Check G Major
  if(G_maj_pct==1){
    
    G_maj <- TRUE
  } else{
    G_maj <- FALSE
  }
  
  #Check A major
  if( A_maj_pct==1){
    
    A_maj <- TRUE
  } else{
    A_maj <- FALSE
  }
  
  #Check B major
  if( B_maj_pct ==1){
    
    B_maj <- TRUE
  } else{
    B_maj <- FALSE
  }
  
  #Check Db Major
  if(Db_maj_pct==1){
    
    Db_maj <- TRUE
  } else{
    Db_maj <- FALSE
  }
  
  #Check Eb major
  if(Eb_maj_pct==1){
    
    Eb_maj <- TRUE
  } else{
    Eb_maj <- FALSE
  }
  
  #Check G Major
  if(Gb_maj_pct==1){
    
    Gb_maj <- TRUE
  } else{
    Gb_maj <- FALSE
  }
  
  #Check A major
  if( Ab_maj_pct==1){
    
    Ab_maj <- TRUE
  } else{
    Ab_maj <- FALSE
  }
  
  #Check Bb major
  if( Bb_maj_pct ==1){
    
    Bb_maj <- TRUE
  } else{
    Bb_maj <- FALSE
  }
      
  possible_keys <- cbind(C_maj, D_maj, E_maj, F_maj, G_maj, A_maj, B_maj,
                         Db_maj, Eb_maj, Gb_maj, Ab_maj, Bb_maj )
  sum_keys <- sum(possible_keys)
  possible_keys <- as.data.frame(  possible_keys)
  
  if(sum_keys == 1){
    
    key <- names(possible_keys)[which(possible_keys==T)]
    return(key)
    
  }else if(sum_keys > 1){
    key <- 'Indeterminate'
    return(key)
    
  } else if(sum_keys == 0){
   
    pct_match <- cbind(C_maj_pct, D_maj_pct, E_maj_pct, F_maj_pct, 
                       G_maj_pct, A_maj_pct, B_maj_pct,
                       Db_maj_pct, Eb_maj_pct, Gb_maj_pct, Ab_maj_pct, Bb_maj_pct )
    return(pct_match)
    
  }


  
}

remove_features <- function(x){
  
  #Input character string of chords separated by '-'
  #Removes all features beyond major and minor and dominant 7
  #Returns 'simplified' character string
  

 x <- str_remove_all(x, '6')
 x <- str_remove_all(x, 'maj7')
 x <- str_replace_all(x, 'm7', 'm')
 x <- str_remove_all(x, 'sus4')
 x <- str_remove_all(x, 'o')
 x <- str_remove_all(x, '\\(add9\\)')
 x <- str_remove_all(x, '(b5)')
 
 return(x)
  
}


#Calculate estimated runtime

est_run_time <- function(url_list, 
                         start= 1, 
                         end= length(url_list),
                         min_load_time,
                         max_load_time,
                         min_sleep_time,
                         max_sleep_time,
                         return_time = FALSE){
  
  #Takes list of song urls
  #Also can adjust start and end
  #Prints time in hours and minutes
  #Returns estimated time in seconds if return_time = TRUE
  
  
  total_songs <- length(url_list[start:end])
  #Calculate Estimated Run Time 
  est_time <- (total_songs)*(mean(min_load_time:max_load_time)*4 +
                               mean(min_sleep_time:max_sleep_time)) 
  est_time_hours <- floor(as.numeric(as.duration(est_time), 'hours'))
  est_time_minutes <- ceiling((est_time /60) %% 60)
  
  print(paste('Estimated Run Time:', est_time_hours,'hours &',
              est_time_minutes, 'minutes' ))
  
  if(return_time){
    return(est_time)
    
  }
}


convert_to_roman <- function(chords, key){
  require(stringr)
  #Inputs string of chords separated by '-' and key 
  #Outputs string of chords in roman numerical analysis 
  ##This means the chords are represented according to their scale degree 
  ##Helps to compare songs with same progressions but written in different  keys
  
  #Remove Features
  chords_base <-  remove_features(chords)

  #Split chords into vector
  chord_vec <- unlist(str_split( chords_base , pattern = "-"))
  
  #Test
  #key <- 'Cmin' 
  
  #Determine mode
  if(grepl('maj', key)==T){
        mode <-'major'
        
        } else if(grepl('min', key)==T){
        mode <- 'minor' 
        
        } else if(grepl('mix', key)==T){
        mode <- 'mix'
        
        } else if(grepl('dor', key)==T){
        mode <- 'dor'
        return(mode)
    
        } else{
        mode <- 'undetermined mode'
        return(mode)
        
  }

  reorder_chrom <-  reorder_chrom_key(key)
  
  #Define Intervals
  minor_interval <- c(1, 3, 4, 6, 8, 9, 11)
  major_interval <- c(seq(1, 5, 2), seq(6, 11, 2), 12)
  mix_interval <- c(1, 3, 5, 6, 8, 10, 11)
  
  
#Find chords in each key 
if(mode == 'major'){
    scale_chords <-  reorder_chrom[ major_interval]
    scale_chords[c(2, 3, 6, 7)] <- tolower(scale_chords[c(2, 3, 6, 7)])
    mode_intervals <- c('', 'm', 'm', '', '', 'm', 'm', '' )
    scale_chords <- paste0( scale_chords , mode_intervals)
    
    #Get dominant 7th idx
    dom_7_idx <- 5
    
    #Borrow IV Scale
    # root_4 <-scale_chords[4]
    # key <- paste0(root_4, 'maj')
    # reorder_chrom_4 <- reorder_chrom_key(key)
    # borrow_IV_scale <- reorder_chrom_4[major_interval]
    # borrow_IV_scale[5] <- paste0(borrow_IV_scale[5], '7')
    # borrow_IV_scale[c(2, 3, 6, 7)] <-  tolower(borrow_IV_scale[c(2, 3, 6, 7)])
    # borrow_IV_scale <- paste0(  borrow_IV_scale, mode_intervals)
    # 
    # #Borrow V Scale
    # root_5 <-scale_chords[5]
    # key <- paste0(root_5, 'maj')
    # reorder_chrom_5 <- reorder_chrom_key(key)
    # borrow_V_scale <- reorder_chrom_5[major_interval]
    # borrow_V_scale[5] <- paste0(borrow_V_scale[5], '7')
    # borrow_V_scale[c(2, 3, 6, 7)] <-  tolower(borrow_V_scale[c(2, 3, 6, 7)])
    # borrow_V_scale <- paste0(  borrow_V_scale, mode_intervals)
    # 
    # #Borrow Minor Scale
    # borrow_min_scale <- reorder_chrom[minor_interval] 
    
  } else if(mode == 'minor'){
    
      
    scale_chords <-  reorder_chrom[  minor_interval]
    scale_chords[c(1,2,4, 7)] <- tolower(scale_chords[c(1,2,4, 7)])
    scale_chords <- paste0(scale_chords, c('m', 'm', '', 'm', '', '', 'm'))

  } else if(mode == 'mix'){
    
    scale_chords <-  reorder_chrom[mix_interval]
    
}
  
  
  


  roman_scale <- as.numeric(match(chord_vec, scale_chords))
  #roman_scale <- unlist(str_split(roman_scale, pattern = ' '))
  
  #Convert to Roman
  if(mode == 'major'){
    #Roman Scale Conversion
    roman <- c('I', 'ii', 'iii', 'IV', 'V', 'vi', 'VII')
    roman_vec <- roman[roman_scale]
    
  } else if (mode == 'minor'){
    #Roman Scale Conversion
    roman <- c('i', 'ii', 'III', 'IV', 'V', 'vi', 'vii')
    roman_vec <- roman[roman_scale]
 } else{
    return('undetermined mode')
}
  
  
  #Deal with Dominant 7ths
  if(mode == 'major'){
    dom_7 <- paste0(scale_chords[dom_7_idx], '7') 
    # scale_chords_dom_7 <- scale_chords
    # 
    # scale_chords_dom_7[dom_7_idx] <- paste0(scale_chords[dom_7_idx], '|', dom_7)
    # return( scale_chords_dom_7)
    #Deal with dominant 7ths
        dom_7_idx <- NA
      dom_7_idx <- grep(pattern = dom_7, x = chord_vec)
      roman_vec[dom_7_idx] <- 'V'
      
  }
  # 
  # borrowed_chords <- chord_vec[is.na(roman_vec)]
  # if(sum(  borrowed_chords %in% borrow_IV_scale_dom_7)){
  #   
  #   
  #   borrow_idx <- match(borrowed_chords, borrow_IV_scale_dom_7)
  #   
  #   if(borrow_idx > 1){
  #     
  #     warning('More than 1 borrowed chord!')
  #     return(borrow_idx)
  #     }
  #   
  #   
  #   fill_idx <- grep(chord_vec, borrow_IV_scale_dom_7[borrow_idx])
  #   roman_vec[fill_idx] <- paste0(roman[borrow_idx], 'IV', sep = '/')
  #  
  # 
  # } else if(sum(  borrowed_chords %in% borrow_V_scale_dom_7))
  #    
  
  roman_string <- paste(roman_vec, collapse = '-')
  
    
  
  return(roman_string)
}


#Notes
#1) Borrow minor chords (Flat 3, 6, 7)
#2) Borrowed 4th and 5th  notesfrom 4th scale (flat root chord)
#3) Deal with #m7(b5) (half-dim) and dim 
#4) Mixolydian mode 



shift_scale <- function(interval, shift){
  
  #May not need this function
  shift <- shift
  shift_interval <- ifelse(interval + shift <= 12, 
                                    interval + shift, 
                                    interval + shift - 12)
  return(shift_interval)
}



reorder_chrom_key <- function(key){
  #Get right chromatic scale (sharp or flat)
  if(grepl(key, 'Fmaj|Fmix|Fmin|Cmin|Gmin|Dmin|Abmaj|Dbmaj|Ebmaj|Bbmaj|Bbmix|Bbmin')==T){
    
    chromatic <- c('C', 'Db', 'D', 'Eb', 'E', 'F', 'Gb',
                   'G', 'Ab', 'A', 'Bb', 'B')
    
    
  } else{
    
    chromatic <- c('C', 'C#', 'D', 'D#', 'E',  'F', 'F#', 'G',
                   'G#', 'A', 'A#', 'B')
    
    
  }
  
  #Determine Tonic (Root of Key)
  key <- str_remove_all(key, 'maj')
  key <- str_remove_all(key, 'min')
  key <- str_remove_all(key, 'mix')
  
  
  #Match 
  root_idx <- match(key, chromatic)
 
  if(root_idx == 1){
    reorder_chrom <- chromatic
    return(reorder_chrom)
    
  } else{
    reorder_chrom <- chromatic[c(root_idx:length(chromatic), 1:(root_idx -1))]
    return(reorder_chrom)
    
  }
  
  
}


#### Function to scrape hook theory

scrape_hook_theory <- function(song_urls, remDr, start = 1, end = NULL,
                               min_load_time = 12, max_load_time = 17, 
                               min_sleep_time=15, max_sleep_time = 25) {

  if(is.null(end)){
    
      end <- length(song_urls) #Song to end at 
    
  }
  
  
  #start, end
  #Start and end points for which song urls to scrape 
  
  #min_load_time,max_load_time
  #Sets Load Time for allowing page to load after navigating to url and after each scroll down
  
  #min_sleep_time, max_sleep_time
  #Set Sleep Time in between songs

  #Set Up Scraper
  df_row_list <- list() #Create Blank List
  remDr$open() #Open Driver
  remDr$setTimeout(type = 'page load', milliseconds = 60e3) #Set Timeout time
  total_songs <- end - start + 1 #Number of songs to scrape 
  start_time <- Sys.time() #Record Start Time
  
  #Run Loop and Pray
  for(i in start:end){
    
    #Print Out Estimated Time
    current_song <-  i - start + 1
    
    est_time <- (total_songs -  current_song  + 1)*(mean(min_load_time:max_load_time)*4 +
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
        est_run_time(url_list = song_urls, 
                     min_load_time = min_load_time,
                     max_load_time = max_load_time,
                     min_sleep_time = min_sleep_time,
                     max_sleep_time = max_sleep_time,
                     start = start, 
                     end = end)
        print(paste('Actual Run Time:',   run_time_hours, 'hours &', run_time_minutes, 'minutes'))
      
        #Bind list into data.frame
        chords_df <- df_row_list %>%  
                            compact %>%
                            lapply(function(x) mutate_all(x, as.character)) %>%
                            bind_rows
        return(chords_df)
      }
    }

}

