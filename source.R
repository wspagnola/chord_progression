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
    str_replace_all('Fbb', 'F bb')  %>% 
    str_replace_all('bbb', 'b bb') %>%
    str_replace_all(' b7', 'b7') %>% 
    str_replace_all('a ', 'a') %>% 
    str_replace_all('c ', 'c') %>% 
    str_replace_all('d ', 'd') %>% 
    str_replace_all('e ', 'e') %>% 
    str_replace_all('f ', 'f') %>% 
    str_replace_all('g ', 'g') %>% 
    str_replace_all(' b\\s+bm ', 'bbm') %>% 
    str_replace_all('^b\\s+bm ', 'bbm') %>% 
    str_replace_all(' b bm7 ', 'bbm7') %>% 
    str_replace_all('\\s+\\) ', ') ') %>% 
    str_replace_all('\\s+\\( ', '(') %>% 
    str_replace_all('\\)', ') ')  %>% 
      str_replace_all('add9\\s+\\)', 'add9\\)')  %>% 
    str_replace_all('b5 ', 'b5') %>% 
    str_replace_all('c xo', 'cxo') %>% 
    str_replace_all('A b6', 'Ab6') %>% 
    str_replace_all('B b6', 'Bb6') %>% 
    str_replace_all('D b6', 'Db6') %>% 
    str_replace_all('E b6', 'Eb6') %>% 
    str_replace_all('G b6', 'Gb6') %>% 
    str_replace_all('\\s+bsus4', 'bsus4')
      

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




#Input a vector of artist a names 
#Gets all the song names & links on Hooktheory for said Artists
#Returns dataframe with Artist, Song, Links
extract_song_links <- function(artist, sleep_time = 5){

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
    
  }
  
  #Store Artist, Song and Link in Data.frame
  d <- data.frame(Artist = artist_vec, 
                  Songs = songs, 
                  Links = href_vec)  
  d <- d %>%  
          mutate_all(as.character)
  
  
  print(paste('Sleep for...', sleep_time, '...seconds'))
  Sys.sleep(sleep_time)
  

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


scroll_down <- function(min_scroll_time = 15, max_scroll_time = 25, song_parts){
  
  if(min_scroll_time > max_scroll_time){
    
    warning('Min Scroll Time must be larger than max scroll time!')
  } else if(is.numeric(min_scroll_time) == F | is.numeric(max_scroll_time ) == F |
            min_scroll_time <= 0 | max_scroll_time <= 0){
    
    warning('Min Scroll Time and Max Scroll Time must be positive integers!')
    
  } else{
    
    #Set Number of Scroll Downs
    if(length(song_parts) < 3) {
      
      scroll_downs <- 1 + length(song_parts)
      
    }else if(length(song_parts) >= 3 & length(song_parts) < 5) {
      
      scroll_downs <- 4 + length(song_parts)
    } else{
      
      scroll_downs <- 5 + length(song_parts)

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



remove_features <- function(x){
  
  #Input character string of chords separated by '-'
  #Removes all features beyond major and minor and dominant 7
  #Returns 'simplified' character string
 x <- str_remove_all(x, '6')
 x <- str_remove_all(x, 'maj7')
 x <- str_remove_all(x, 'maj9')
 x <- str_replace_all(x, 'm7-', 'm-')
 x <-  str_replace_all(x, 'm7$', 'm')
 x <- str_replace_all(x, 'asus4', 'am')
 x <- str_remove_all(x, 'sus4')
 x <- str_remove_all(x, '\\(add9\\)')

 #x <- str_remove_all(x, 'o')
 #x <- str_remove_all(x, '(b5)')

 
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
  
  #Test
  chords <- chords
  key <- key
  
  #Remove Features; Then Split chords into vector
  chords_base <-  remove_features(chords)
  chord_vec <- unlist(str_split( chords_base , pattern = "-"))
  
  #Determine mode
  if(grepl('maj', key)==T){
        mode <-'major'
        
        } else if(grepl('min', key)==T){
        mode <- 'minor' 
        
        } else if(grepl('mix', key)==T){
        mode <- 'mix'
        
        } else if(grepl('dor', key)==T){
        mode <- 'dor'
    
        } else{
        mode <- 'Indeterminate mode!'
        warning(mode)
        
  }

  #Get chords from main scale and
  reorder_chrom <-  reorder_chrom_key(key)
  scale_chords <- get_chords_from_key(notes = reorder_chrom, mode = mode)
  arabic_scale <- as.numeric(match(chord_vec, scale_chords))
  
  #Define Roman Numeral Conversions
  roman_major <- c('I', 'ii', 'iii', 'IV', 'V', 'vi', 'vii')
  roman_minor <- c('i', 'ii', 'III', 'iv', 'v', 'VI', 'VII')
  roman_mix  <- c('I', 'ii', 'III', 'IV', 'v', 'vi', 'VII')
  roman_dor <- c('i', 'ii', 'III', 'IV', 'v', 'vi', 'VII' )
  
  #Convert to Roman
  if(mode == 'major'){
    roman_vec <-   roman_major[arabic_scale]
    
  } else if(mode == 'minor'){
    roman_vec <- roman_minor[arabic_scale]
    
  } else if(mode == 'mix'){
   roman_vec <-   roman_mix[arabic_scale]
  
  }else if(mode == 'dor'){
    roman_vec <-   roman_dor[ arabic_scale]
    
  }else{
    warning('Indeterminate mode!')
  }
  
  
 #### Deal with Dominant 7ths and Half-diminished [m7(b5)] Chords 
  if(mode == 'major'){
    
    #Chord Options
    dom_7_idx <- 5 #Get dominant 7th idx
    dim_idx <- 7 #Get diminished idx
    
    #Deal with Dominant 7ths
    dom_7 <- paste0(scale_chords[dom_7_idx], '7') 
    dom_7_idx <- grep(pattern = dom_7, x = chord_vec)
    roman_vec[dom_7_idx] <- 'V'
    
    #Deal with VII minor?

    #Deal with half dim (m7(b5))
    VII_dim <- str_remove_all(scale_chords[dim_idx], 'o')
    half_dim <- paste0(VII_dim, 'm7(b5)')
    half_dim_idx <- which(chord_vec== half_dim)
    roman_vec[half_dim_idx] <- 'vii'
  
  } else if(mode == 'minor'){
    half_dim <- str_remove(scale_chords[2], 'o')
    half_dim <- paste0(half_dim,  'm7(b5)')
    half_dim_idx <- which(chord_vec== half_dim)
    roman_vec[half_dim_idx] <- 'ii'
  }
  
  #### Deal with Borrow Chords
  
  #Borrow maj, min, dor
  #Borrow Major Scale
  root_1 <-  scale_chords[1]
  root_1 <- str_remove(root_1 , 'm')
  str_sub(  root_1 , start = 1, end = 1) <- toupper( str_sub(root_1 , start = 1, end = 1))
  
  #Borrow ii(maj) scale
  root_2 <- scale_chords[2]
  root_2 <- str_remove(root_2, 'm')
  root_2 <- str_remove(root_2, 'o')
  str_sub(root_2, start = 1, end = 1) <- toupper( str_sub(root_2, start = 1, end = 1))
  
  #Borrow IV Scale
  root_4 <- scale_chords[4]
  root_4 <- str_remove(root_4, 'm')
  str_sub(root_4, start = 1, end = 1) <- toupper(  str_sub(root_4, start = 1, end = 1))
  
  #Borrow V Scale
  root_5 <-scale_chords[5]
  root_5 <- str_remove(root_5, 'm')
  str_sub(root_5, start = 1, end = 1) <- toupper(  str_sub(root_5, start = 1, end = 1))

  #Borrow VI Scale
  root_6 <-scale_chords[6]
  root_6 <- str_remove(root_6, 'm')
  str_sub(root_6, start = 1, end = 1) <- toupper(  str_sub(root_6, start = 1, end = 1))
  
  
  #Borrow VII Scale
  root_7 <-scale_chords[7]
  root_7 <- str_remove(root_7, 'm')
  str_sub(root_7, start = 1, end = 1) <- toupper(  str_sub(root_7, start = 1, end = 1))
  
  #Borrow Dorian Scale
  root_dor <- scale_chords[1]
  root_dor <-  str_remove(root_dor, 'm')
  str_sub(root_dor, start = 1, end = 1) <- toupper(  str_sub(root_dor, start = 1, end = 1))
  
 
  if(mode == 'major'){
    
    borrow_key_2 <- paste0(root_2, 'maj')
    reorder_chrom_2 <- reorder_chrom_key(borrow_key_2)
    borrow_II_scale <- get_chords_from_key(reorder_chrom_2, 'major', T)

    borrow_key_4 <- paste0(root_4, 'maj')
    reorder_chrom_4 <- reorder_chrom_key(borrow_key_4)
    borrow_IV_scale <- get_chords_from_key( reorder_chrom_4 , 'major', T)
   
    #Borrow V Scale (without Dominant 7)
    borrow_key_5 <- paste0(root_5, 'maj')
    reorder_chrom_5 <- reorder_chrom_key(borrow_key_5)
    borrow_V_scale <- get_chords_from_key(reorder_chrom_5  , 'major', F)
    
    #Borrow V Scale (with Dominant 7) (MAY CHANGE THIS LATER)
    borrow_V_scale_dom_7 <- borrow_V_scale
    borrow_V_scale_dom_7[5] <- paste0(borrow_V_scale[5], '7')

    #Borrow VI Scale (without Dominant 7th)
    borrow_key_6 <- paste0(root_6, 'maj')
    reorder_chrom_6 <- reorder_chrom_key(borrow_key_6)
    borrow_VI_scale <-  get_chords_from_key(reorder_chrom_6, 'major', F)
    
    #Borrow VI Scale (with Dominant 7) (MAY CHANGE THIS LATER)
    borrow_VI_chord_dom_7 <- paste0(borrow_VI_scale[5], '7')

    #Borrow Minor Scale
    borrow_minor_key <-  paste0(root_1, 'min') #Add mode to root to get key
    reorder_chrom_min <- reorder_chrom_key(borrow_minor_key) #Reorder chromatic scale according to key
    borrow_min_scale <- get_chords_from_key(reorder_chrom_min , 'minor', T)
  
    borrow_key_dorian <- paste0(root_1, 'dor')
    reorder_chrom_dorian <- reorder_chrom_key( borrow_key_dorian )
    borrow_dor_scale <-  get_chords_from_key(reorder_chrom_dorian, 'dor', T)
  
    
    ### Other CHORDS not Found in listed keys
    #Lydian iv
    lydian_half_dim <- reorder_chrom_key(key)[7]
    lydian_half_dim  <- tolower(lydian_half_dim)
    lydian_half_dim <- paste0(lydian_half_dim, 'm7\\(b5\\)')
    
    #Lydian vii
    lydian_vii <- intersect( borrow_V_scale_dom_7, borrow_VI_scale )
    #Intersection of V and VI scale (7th note played as minor chord)
    
    #IV/IV
    borrow_IV_IV_chord <- intersect(borrow_IV_scale, borrow_min_scale)
    borrow_IV_IV_chord <- borrow_IV_IV_chord[borrow_IV_IV_chord %in% scale_chords == F] #Remove ii chord
    borrow_IV_IV_chord <- borrow_IV_IV_chord[grepl('m', borrow_IV_IV_chord )==F] #remove v-min chord
    
    #V/iii
    borrow_V_iii_chord <- str_remove(scale_chords[7], 'o')
    borrow_V_iii_chord <- toupper(borrow_V_iii_chord)
    
    #Null Scale
    borrow_maj_scale <- NULL
    borrow_maj_scale_dom_7 <- NULL
    borrow_ii_maj_scale <- NULL
    borrow_V_maj_scale <- NULL

    
  }else if(mode == 'minor'){
    dim_idx <- 2 #Get diminished idx DON'T KNOW IF I NEED THIS
    
    borrow_key_maj <- paste0(root_1, 'maj')
    reorder_chrom_maj <- reorder_chrom_key(borrow_key_maj)
    borrow_maj_scale <-  get_chords_from_key(reorder_chrom_maj, 'major', F)
    
    borrow_key_dorian <- paste0(root_1, 'dor')
    reorder_chrom_dorian <- reorder_chrom_key(borrow_key_dorian )
    borrow_key_dorian <- get_chords_from_key( reorder_chrom_dorian , 'dor', F)
    
    borrow_key_2_maj <- paste0(root_2, 'maj')
    reorder_chrom_2 <- reorder_chrom_key( borrow_key_2_maj)
    borrow_ii_maj_scale <- get_chords_from_key(reorder_chrom_4, 'major', F)
  
    borrow_key_4 <- paste0(root_4, 'min')
    reorder_chrom_4 <- reorder_chrom_key(borrow_key_4)
    borrow_IV_scale <- get_chords_from_key(reorder_chrom_4, 'minor', F)
 
    borrow_key_5 <- paste0(root_5, 'min')
    reorder_chrom_5 <- reorder_chrom_key(borrow_key_5)
    borrow_V_scale <- get_chords_from_key(reorder_chrom_5, 'minor', T)
    
    borrow_key_7_maj <- paste0(root_7, 'maj')
    
    
    
    # #Borrowed Chords 
    borrow_maj_scale_dom_7 <-  paste0(borrow_maj_scale[5], '7')
    borrow_loc_1 <- str_replace(scale_chords[1], 'm', 'o')

    #Set NULL scales'
    borrow_II_scale <- NULL
    borrow_IV_IV_chord <- NULL
    borrow_V_maj_scale <- NULL
    borrow_IV_scale_dom_7 <- NULL
    borrow_V_scale_dom_7 <- NULL
    borrow_VI_scale <- NULL
    borrow_VI_chord_dom_7 <- NULL
    borrow_min_scale <- NULL
    lydian_vii <- NULL
    
  } else if(mode == 'mix'){
    
    #Set scales and chords to be NULL
    borrow_II_scale <- NULL
    borrow_ii_maj_scale <- NULL
    borrow_IV_IV_chord <- NULL
    borrow_IV_scale <- NULL
    borrow_IV_scale_dom_7 <- NULL
    borrow_V_scale <- NULL
    borrow_V_scale_dom_7 <- NULL
    borrow_V_maj_scale <- NULL
    borrow_VI_scale <- NULL
    borrow_VI_chord_dom_7 <- NULL
    borrow_maj_scale <- NULL
    borrow_maj_scale_dom_7 <- NULL
    borrow_min_scale <- NULL
    borrow_dor_scale <- NULL
    lydian_vii <- NULL
    
  } else if (mode == 'dor'){
    borrow_key_5_maj <- paste0(root_5, 'maj')
    reorder_chrom_5_maj <- reorder_chrom_key(borrow_key_5_maj)
    borrow_V_maj_scale <- get_chords_from_key(reorder_chrom_5_maj, 'major', F)
    
    #Set NULL scales and chords
    borrow_II_scale <- NULL
    borrow_IV_IV_chord <- NULL
    borrow_IV_scale <- NULL
    borrow_IV_scale_dom_7 <- NULL
    borrow_V_scale <- NULL
    borrow_V_scale_dom_7 <- NULL
    borrow_VI_scale <- NULL
    borrow_VI_chord_dom_7 <- NULL
    borrow_maj_scale <- NULL
    borrow_maj_scale_dom_7 <- NULL
    borrow_min_scale <- NULL
    borrow_dor_scale <- NULL
    lydian_vii <- NULL
}


  #Find Chords that are borrowed
  borrowed_chords <- chord_vec[is.na(roman_vec)]
  unique_borrow_chords <- unique(borrowed_chords)
  
  #Match borrow chords to different scales
  borrowed_2_chords <-unique_borrow_chords[which(unique_borrow_chords %in% borrow_II_scale)]
  borrowed_4_chords <- unique_borrow_chords[which(unique_borrow_chords %in% borrow_IV_scale)]
  borrowed_5_chords <-  unique_borrow_chords[which(unique_borrow_chords %in% borrow_V_scale)]
  borrowed_5_dom_7_chords <-  unique_borrow_chords[which(unique_borrow_chords %in% borrow_V_scale_dom_7)]
  borrowed_6_chords <-  unique_borrow_chords[which(unique_borrow_chords %in% borrow_VI_scale)]
  
  
  borrowed_min_chords <-  unique_borrow_chords[which(unique_borrow_chords %in% borrow_min_scale)]
  borrowed_maj_chords <-  unique_borrow_chords[which(unique_borrow_chords %in% borrow_maj_scale)]
  borrowed_maj_dom_7_chords <- unique_borrow_chords[which(unique_borrow_chords %in% borrow_maj_scale_dom_7)] 
  borrowed_2_maj_chords <- unique_borrow_chords[which(unique_borrow_chords %in% borrow_ii_maj_scale)] 
  
  borrowed_dor_chords <-  unique_borrow_chords[which(unique_borrow_chords %in% borrow_dor_scale)]
  borrowed_5_maj_chords <-  unique_borrow_chords[which(unique_borrow_chords %in% borrow_V_maj_scale)]
  
  #Deal with Intersection of Scales
  borrowed_lydian_7_chord  <-   unique_borrow_chords[which(unique_borrow_chords %in% lydian_vii)]

  if(length(borrowed_2_chords) > 0){
    
    for(i in 1:length(borrowed_2_chords)){
      borrow_idx <- match(borrowed_2_chords[i], borrow_II_scale)
      fill_idx <- grep(pattern = borrowed_2_chords[i], x = chord_vec)
      roman_vec[fill_idx] <- paste(roman_major[borrow_idx], 'ii', sep = '/')
      
    }
    
    
  }
  
  
  #Borrow ii maj scale
  if(length(borrowed_2_maj_chords) > 0){
    for(i in 1:length(borrowed_2_maj_chords)){
      borrow_idx <- match(borrowed_2_maj_chords[i], borrow_ii_maj_scale)
      fill_idx <- grep(pattern = borrowed_2_maj_chords[i], x = chord_vec)
      roman_vec[fill_idx] <- paste(roman_major[borrow_idx], 'iio(maj)', sep = '/')
      
    }
    
  }
  
  
  if(length(borrowed_dor_chords) > 0 ){
    for(i in 1:length(borrowed_dor_chords))  {
      borrow_idx <- match(borrowed_dor_chords[i], borrow_dor_scale)
      fill_idx <- grep(pattern =  borrowed_dor_chords[i], 
                       x = chord_vec)
      roman_vec[fill_idx] <- paste(roman_major[borrow_idx], '(dor)', sep = '/')
      
    } 
    
  }
  
  #Borrow IV Scale
  if(length(borrowed_4_chords) > 0 ){
    for(i in 1:length(borrowed_4_chords)){
      
        borrow_idx <- match(borrowed_4_chords[i], borrow_IV_scale)
        fill_idx <- grep(pattern = borrow_IV_scale[borrow_idx], x = chord_vec)
        roman_vec[fill_idx] <- paste(roman_major[borrow_idx], 'IV', sep = '/')
      }
  }
  
  #Borrow VI Scale
  if(length(borrowed_6_chords) > 0 ){
    
      for(i in 1:length(borrowed_6_chords)){
        borrow_idx <- match(borrowed_6_chords[i], borrow_VI_scale)
        fill_idx <- grep(pattern = borrow_VI_scale[borrow_idx], x = chord_vec)
        roman_vec[fill_idx] <- paste(roman_major[borrow_idx], 'vi', sep = '/')
      } 
  }

  #Borrow V Scale
  if( length(borrowed_5_chords) > 0  ){
    
    for(i in 1:length(borrowed_5_chords)){
      
      borrow_idx <- match(borrowed_5_chords[i], borrow_V_scale)
      fill_idx <- grep(pattern = borrow_V_scale[borrow_idx], x = chord_vec)
      roman_vec[fill_idx] <- paste(roman_major[borrow_idx], 'V', sep = '/')
    }
    
  }
  
  
  
  
  #Borrow V Scale (with dom 7)
  if( length(borrowed_5_dom_7_chords) > 0  ){
    
    for(i in 1:length(borrowed_5_dom_7_chords)){
      
      borrow_idx <- match(borrowed_5_dom_7_chords[i], borrow_V_scale_dom_7)
      fill_idx <- grep(pattern =borrow_V_scale_dom_7[borrow_idx], x = chord_vec)
      roman_vec[fill_idx] <- paste(roman_major[borrow_idx], 'V', sep = '/')
    }
    
  }

  
  #Borrow min Scale
  if( length(borrowed_min_chords) > 0 ){
    
      for(i in 1:length(borrowed_min_chords)){
        borrow_idx <- match(borrowed_min_chords[i], borrow_min_scale)
        fill_idx <- grep(pattern = borrow_min_scale[borrow_idx], x = chord_vec)
        
        #Flat 3, 6, 7
        if(borrow_idx %in% c(3, 6, 7)){
          roman_vec[fill_idx] <- paste0( 'b', roman_minor[borrow_idx], '/(min)')  
          
        } else {
          roman_vec[fill_idx] <- paste(roman_minor[borrow_idx], '(min)', sep = '/')  
        }
    }
  }
  

  
  
  #Borrow maj Scale
  if( length(borrowed_maj_chords) > 0 ){
    
    for(i in 1:length(borrowed_maj_chords)){
      borrow_idx <- match(borrowed_maj_chords[i], borrow_maj_scale)
      fill_idx <- grep(pattern = borrow_maj_scale[borrow_idx], x = chord_vec)
      roman_vec[fill_idx] <- paste(roman_major[borrow_idx], '(maj)', sep = '/')
      
    }
  }
  
  

  #Borrow maj Scale (with dominany 7th)
  if(length(borrowed_maj_dom_7_chords) > 0 ){
    
    for(i in 1:length(borrowed_maj_dom_7_chords)){
      borrow_idx <- match(borrowed_maj_dom_7_chords[i], borrow_maj_scale_dom_7)
      fill_idx <- grep(pattern =borrow_maj_scale_dom_7[borrow_idx], x = chord_vec)
      roman_vec[fill_idx] <- paste(roman_major[borrow_idx], '(maj)', sep = '/')
      
    }
  }
  #Borrow  V(maj) Scale
  if( length(borrowed_5_maj_chords) > 0 ){
    
    for(i in 1:length(borrowed_5_maj_chords)){
      borrow_idx <- match(borrowed_5_maj_chords[i], borrow_V_maj_scale)
      fill_idx <- grep(pattern = borrow_V_maj_scale[borrow_idx], x = chord_vec)
      roman_vec[fill_idx] <- paste(roman_major[borrow_idx], 'v(maj)', sep = '/')
      
    }
  }
  
  
  #Borrowed Specific Chords (Special cases; ignore scale)
  if(length(borrowed_lydian_7_chord) > 0 ){
    roman_vec[grep(lydian_vii, chord_vec)] <- 'vii/(lyd)'
  }
  if(length(borrow_loc_1) > 0){
    roman_vec[grep(borrow_loc_1, chord_vec)] <- 'i/(loc)'
  }
  if(length(borrow_min_dom_7) > 0){
    roman_vec[grep(borrow_min_dom_7, chord_vec)] <- 'bVII/(min)'
    
  }
  if(length(lydian_half_dim) > 0){
    roman_vec[grep(lydian_half_dim, chord_vec)] <- '#iv/(lyd)'
  }
  if(length(borrow_VI_chord_dom_7 ) > 0) {
    roman_vec[grep( borrow_VI_chord_dom_7, chord_vec)] <- 'V/vi'
  }
  if(length(borrow_V_iii_chord) > 0){
    roman_vec[grep(borrow_V_iii_chord, chord_vec)] <- 'V/iii'
    
  }
  
  
  #Borrow IV chord from IV
  if( length(borrow_IV_IV_chord) > 0 ){
    fill_idx <- which(chord_vec == borrow_IV_IV_chord)
    roman_vec[fill_idx] <- 'IV/IV'
    
  }


  #Collapse vector into a single string
  roman_string <- paste(roman_vec, collapse = '-')


  return(roman_string)
}


#Notes
#1) Borrow minor chords (Flat 3, 6, 7)
#2) Borrowed 4th and 5th  notesfrom 4th scale (flat root chord)
#3) Deal with #m7(b5) (half-dim) and dim 
#4) Mixolydian mode 




reorder_chrom_key <- function(key){
  #Get right chromatic scale (sharp or flat)
  
  
  flat_major_keys <- 'Fmaj|Bbmaj|Ebmaj|Abmaj|Dbmaj|Gbmaj'
  flat_minor_keys <- 'Dmin|Gmin|Cmin|Fmin|Bbmin|Ebmin||Abmin|Dbmin'
  flat_mix_keys <- 'Fmix|Bbmix'
  flat_dor_keys <- 'Gdor|Dbdor|Ebdor|Bbdor|Abdor'
  flat_keys <- paste(flat_major_keys, flat_minor_keys, flat_mix_keys, flat_dor_keys, sep = '|')
  if(grepl(key, flat_keys )==T){
    
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
  key <- str_remove_all(key, 'dor')
  
  
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
                               min_load_time = 17, max_load_time = 23, 
                               min_sleep_time=25, max_sleep_time = 35, 
                               artist, songs) {

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
      full_song_info_df <-  data.frame(artist = artist[i], 
                                       song = songs[i], 
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




get_chords_from_key <- function(notes, mode, dom = F){
  #Input
  #notes: chromatic scale beginning with root (output from reorder_chrom_key)
  #mode: major, minor, mix, or dor
  
  #Output: Chords from Each key
  #Dom7th refers to optional 7th chord override
  
  #Define Intervals
  major_interval <- c(seq(1, 5, 2), seq(6, 11, 2), 12)
  minor_interval <- c(1, 3, 4, 6, 8, 9, 11)
  mix_interval <- c(1, 3, 5, 6, 8, 10, 11)
  dor_interval <- c(1, 3, 4, 6, 8, 10, 11)
  
  #Define minor & dims (lower case)
  major_lowers <- c(2, 3, 6, 7)
  minor_lowers <- c(1, 2, 4, 5)
  mix_lowers <- c(2, 3, 5, 6)
  dor_lowers <-c(1,2, 5, 6)
  
  #Define Triads (Which chords are major and which are minor in a given mode)
  major_mode_triads  <- c('', 'm', 'm', '', '', 'm', 'o' )
  minor_mode_triads <- c('m', 'o', '', 'm', 'm', '', '')
  mix_mode_triads <- c('', 'm', 'o', '', 'm', 'm', '')
  dor_mode_triads <- c('m', 'm', '', '', 'm', 'm', '')
  
  
  #Select notes from based on mode interval 
  #Make minor chord root notes lowercase and add 'm' 
  if(mode == 'major'){
    notes <- notes[major_interval]
    notes[major_lowers] <- tolower(notes[major_lowers] )
    notes <-  paste0(notes , major_mode_triads)
    
    #Convert 5th to Dom7 if TRUE
    if(dom == T){
      notes[5] <- paste0(notes[5], '7')
    }
  } else if(mode == 'minor'){
    notes <- notes[minor_interval]
    notes[minor_lowers] <- tolower(notes[minor_lowers] )
    notes <-  paste0(notes , minor_mode_triads)
    
    if(dom == T){
      notes[7] <-  paste0(notes[7], '7')
    }
    
    
  } else if(mode == 'mix'){
    notes <- notes[mix_interval]
    notes[major_lowers] <- tolower(notes[major_lowers] )
    notes <-  paste0(notes , major_mode_triads)
    
  }else if(mode == 'dor'){
    notes <- notes[dor_interval]
    notes[dor_lowers] <- tolower(notes[dor_lowers] )
    notes <-paste0(notes , dor_mode_triads)
    
    if(dom == T){
      notes[4] <-  paste0(notes[4], '7')
    }
    

    
  } else{
    warning('Do not recognize mode!')
  }
  
  return(notes)
  
}  




check_roman <- function(x){
  
  require(dplyr)
  require(stringr)
  
  #Split X into dataframe: one with roman, one with chords
  roman <- x %>%  
    select(song, song_parts, key, roman) %>% 
    rename(chords = roman) %>% 
    mutate(roman = 1)
  chords  <- x %>%  
    select(song, song_parts, key, chords) %>% 
    mutate(roman = 0)
  
  #Bind dataframes together and rearrange
  df <- rbind(roman, chords) %>% 
    as.data.frame %>% 
    arrange(song, song_parts, roman)
  
  #Split dataframes
  list_df <- split(df,f = list(df$song, df$song_parts), drop =T)
  
  z <- list()
  for(i in 1:length(list_df)){
    
    x <- str_split(list_df[[i]][1 , 'chords'], '-') %>%  unlist
    y <- str_split(list_df[[i]][2 , 'chords'], '-') %>%  unlist
    w <- rbind(x,y)
    w <- as.data.frame(w)
    song_info <- list_df[[i]][1:2 , 1:3]
    row.names(song_info) <- NULL
    z[[i]] <- cbind(song_info, w)
    
    
  }
  
  return(z)
  
}


##### NOTES ####

#Clean up code
#1) Create function to reorder scales in one line
#2) In chromatic alignment, find rel major first for minor, mix, and dor



# em7 in the key of F
##vii(lydian ) or iii in C 
