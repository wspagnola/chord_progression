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


scroll_down <- function(scroll_time){
  
  max_scroll_time <- scroll_time + 5
  
  
  if(length(song_parts)==1){
    

    remDr$executeScript("window.scrollTo(0,300);")
    sleep_time <- sample(scroll_time:max_scroll_time, 1)
    print(paste(' Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    
    remDr$executeScript("window.scrollTo(0,600);")
    sleep_time <- sample(scroll_time:max_scroll_time, 1)
    print(paste(' Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    
    # remDr$executeScript("window.scrollTo(0,900);")
    # sleep_time <- sample(scroll_time:max_scroll_time, 1)
    # print(paste(' Waiting ', sleep_time, ' seconds to load...'))
    # Sys.sleep(sleep_time)
    
    
  } else if(length(song_parts)==2){
    
    
    remDr$executeScript("window.scrollTo(0,300);")
    sleep_time <- sample(scroll_time:max_scroll_time, 1)
    print(paste(' Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    remDr$executeScript("window.scrollTo(0,600);") #Scroll down page
    sleep_time <- sample(scroll_time:max_scroll_time, 1)     
    print(paste(' Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    
    remDr$executeScript("window.scrollTo(0,900);") #Scroll down page
    sleep_time <- sample(scroll_time:max_scroll_time, 1)     
    print(paste(' Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    # remDr$executeScript("window.scrollTo(0,1200);") #Scroll down page
    # sleep_time <- sample(scroll_time:max_scroll_time, 1)     
    # print(paste(' Waiting ', sleep_time, ' seconds to load...'))
    # Sys.sleep(sleep_time)
    
  } else if(length(song_parts) == 3){
    
    remDr$executeScript("window.scrollTo(0,0);") #Scroll down page
    sleep_time <- sample(scroll_time:max_scroll_time, 1)
    print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    remDr$executeScript("window.scrollTo(0,300);") #Scroll down page
    sleep_time <- sample(scroll_time:max_scroll_time, 1)
    print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    remDr$executeScript("window.scrollTo(0,600);") #Scroll down page
    sleep_time <- sample(scroll_time:max_scroll_time, 1)
    print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    remDr$executeScript("window.scrollTo(0,900);") #Scroll down page
    sleep_time <- sample(scroll_time:max_scroll_time, 1)
    print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    remDr$executeScript("window.scrollTo(0,1200);") #Scroll down page
    sleep_time <- sample(scroll_time:max_scroll_time, 1)
    print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    # remDr$executeScript("window.scrollTo(0,1500);") #Scroll down page
    # sleep_time <- sample(scroll_time:max_scroll_time, 1)
    # print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
    # Sys.sleep(sleep_time)
    
    
    
  }else if(length(song_parts) >= 3){
    
    remDr$executeScript("window.scrollTo(0,0);") #Scroll down page
    sleep_time <- sample(scroll_time:max_scroll_time, 1)
    print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    remDr$executeScript("window.scrollTo(0,300);") #Scroll down page
    sleep_time <- sample(scroll_time:max_scroll_time, 1)
    print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    remDr$executeScript("window.scrollTo(0,600);") #Scroll down page
    sleep_time <- sample(scroll_time:max_scroll_time, 1)
    print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    remDr$executeScript("window.scrollTo(0,900);") #Scroll down page
    sleep_time <- sample(scroll_time:max_scroll_time, 1)
    print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    remDr$executeScript("window.scrollTo(0,1200);") #Scroll down page
    sleep_time <- sample(scroll_time:max_scroll_time, 1)
    print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    remDr$executeScript("window.scrollTo(0,1500);") #Scroll down page
    sleep_time <- sample(scroll_time:max_scroll_time, 1)
    print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    
    # remDr$executeScript("window.scrollTo(0,1800);") #Scroll down page
    # sleep_time <- sample(scroll_time:max_scroll_time, 1)
    # print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
    # Sys.sleep(sleep_time)
  }else if(length(song_parts) == 5){
    
    remDr$executeScript("window.scrollTo(0,0);") #Scroll down page
    sleep_time <- sample(scroll_time:max_scroll_time, 1)
    print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    remDr$executeScript("window.scrollTo(0,300);") #Scroll down page
    sleep_time <- sample(scroll_time:max_scroll_time, 1)
    print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    remDr$executeScript("window.scrollTo(0,600);") #Scroll down page
    sleep_time <- sample(scroll_time:max_scroll_time, 1)
    print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    remDr$executeScript("window.scrollTo(0,900);") #Scroll down page
    sleep_time <- sample(scroll_time:max_scroll_time, 1)
    print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    remDr$executeScript("window.scrollTo(0,1200);") #Scroll down page
    sleep_time <- sample(scroll_time:max_scroll_time, 1)
    print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    remDr$executeScript("window.scrollTo(0,1500);") #Scroll down page
    sleep_time <- sample(scroll_time:max_scroll_time, 1)
    print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    
    remDr$executeScript("window.scrollTo(0,1800);") #Scroll down page
    sleep_time <- sample(scroll_time:max_scroll_time, 1)
    print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
    Sys.sleep(sleep_time)
    
    
    # remDr$executeScript("window.scrollTo(0,2100);") #Scroll down page
    # sleep_time <- sample(scroll_time:max_scroll_time, 1)
    # print(paste('Scrolling.  Waiting ', sleep_time, ' seconds to load...'))
    # Sys.sleep(sleep_time)
  }
}


scroll_down <- function(min_scroll_time = 15, max_scroll_time = 25, song_parts){
  
  if(min_scroll_time > max_scroll_time){
    
    warning('Min Scroll Time must be larger than max scroll time!')
  } else if(is.numeric(min_scroll_time) == F | is.numeric(max_scroll_time ) == F |
            min_scroll_time <= 0 | max_scroll_time <= 0){
    
    warning('Min Scroll Time and Max Scroll Time must be positive integers!')
    
  } else{
    
    scroll_downs <- 1 + length(song_parts)
    for(i in 1: scroll_downs){
      
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
 x <- str_remove_all(x, '(add9)')
 x <- str_remove_all(x, '(b5)')
 
 return(x)
  
}

