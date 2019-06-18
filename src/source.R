require(rvest)
require(XML)
require(tidyverse)
require(seleniumPipes)
require(RSelenium)
require(httr)
require(lubridate)
require(spotifyr)
require(RCurl)
require(wordcloud)

#Not sure if I need these
#require(jsonlite)
#require(xml2)

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
    str_replace_all('\\s+bsus4', 'bsus4') %>% 
    str_replace_all('\\s+bmaj7', 'bmaj7') 
      
      

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
 x <- str_remove_all(x, 'sus#4')
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
  
  #Full features
  chord_vec_full <- unlist(str_split(chords , pattern = "-"))
  
  
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
      warning('UNRECOGNIZED MODE!')
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
  roman_lower_case <- c('i', 'ii', 'ii', 'iv', 'v', 'vi', 'vii' )
  
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
    dom_7_idx <- 5 
    dim_idx <- 7 
    dom_7_roman <- 'V'
    dim_roman <- 'vii'
    
  } else if(mode == 'minor'){
    dom_7_idx <- 7
    dim_idx <- 2 
    dom_7_roman <- 'VII'
    dim_roman <- 'ii'
    
  } 
  
  
  if(mode == 'major' | mode == 'minor'){
    
    #Deal with Dominant 7ths
    dom_7 <- paste0(scale_chords[dom_7_idx], '7') 
    dom_7_idx <- grep(pattern = dom_7, x = chord_vec)
    roman_vec[dom_7_idx] <-  dom_7_roman
    
    #Deal wth Half diminished 
    half_dim <- str_remove(scale_chords[dim_idx], 'o')
    half_dim_regex <- paste0(half_dim,  'm7\\(b5\\)', '|', half_dim, 'm\\(b5\\)')
    half_dim_idx <- grep(half_dim_regex, chord_vec)
    roman_vec[half_dim_idx] <- dim_roman
    
  }
    
    
  
  #Deal with suspended chords that are substitutes for minor chords th
  sus_idx <- grep('[a-g]sus4',   chord_vec_full)
  sus_as_minor_chord <- str_replace(chord_vec_full[sus_idx], 'sus4', 'm') 
  sus_arabic <- match(sus_as_minor_chord,  scale_chords)
  roman_vec[sus_idx] <- roman_lower_case[sus_arabic]
  
  #This function finds the root of each possible borrowed scale 
  convert_to_root <- function(key_chords, root_index){
        #Input: chords in key, root index of borrowed key
        #Outputs: Root as upper case letter; can be used later as a root for a borrowed scale
        root <- key_chords[root_index]
        root <- str_remove(root, 'm')
        root <- str_remove(root, 'o')
        str_sub(root, start = 1, end = 1) <- toupper( str_sub(root, start = 1, end = 1))
        return(root)
  }
  
  
  #### Deal with Borrow Chords

  #Get Roots
  root_1 <- convert_to_root(scale_chords, 1) #Borrow maj, min, dor scales
  root_2b <- reorder_chrom[2] #Borrow ii flat root; root is in chromatic scale but not diatonic
  root_2 <- convert_to_root(scale_chords, 2) #Borrow ii(maj) scale
  root_4 <- convert_to_root(scale_chords, 4) #Borrow IV Scale
  root_5 <- convert_to_root(scale_chords, 5) #Borrow V Scale
  root_6 <- convert_to_root(scale_chords, 6) #Borrow VI Scale
  root_7 <- convert_to_root(scale_chords, 7) #Borrow VII Scale

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
    
    #Borrow VI Scale (with Dominant 7th or 9th) (MAY CHANGE THIS LATER)
    borrow_VI_chord_dom_7 <- paste0(borrow_VI_scale[5], '7')
    borrow_VI_chord_dom_9 <- paste0(borrow_VI_scale[5], '9')
    borrow_VI_dom <- paste0( borrow_VI_chord_dom_7, '|',  borrow_VI_chord_dom_9) 
    
    #Borrow Minor Scale
    borrow_minor_key <-  paste0(root_1, 'min') #Add mode to root to get key
    reorder_chrom_min <- reorder_chrom_key(borrow_minor_key) #Reorder chromatic scale according to key
    borrow_min_scale <- get_chords_from_key(reorder_chrom_min , 'minor', T)
  
    #Borrow Dorian Scale
    borrow_key_dorian <- paste0(root_1, 'dor')
    reorder_chrom_dorian <- reorder_chrom_key( borrow_key_dorian )
    borrow_dor_scale <-  get_chords_from_key(reorder_chrom_dorian, 'dor', T)

    ### Other CHORDS not Found in listed keys
    
    #lydian iv (full dim)
    lydian_dim <-  reorder_chrom[6]
    lydian_dim  <- tolower(lydian_dim)
    lydian_dim <- paste0(lydian_dim, 'o')
    
    #Lydian #iv (half dim) [Note that this is half step higher than iv]
    lydian_half_dim <- reorder_chrom[7] #Is this correct? 
    lydian_half_dim  <- tolower(lydian_half_dim)
    lydian_half_dim <- paste0(lydian_half_dim, 'm7\\(b5\\)')
    
    #Dorian vi half_dim 
    borrow_dor_half_dim_chord <-  str_remove(borrow_dor_scale[6], 'o')
    borrow_dor_half_dim_chord <-paste0(borrow_dor_half_dim_chord, 'm7\\(b5\\)')

    #lydian vii
    lydian_vii <- intersect( borrow_V_scale_dom_7, borrow_VI_scale )
    #Intersection of V and VI scale (7th note played as minor chord)
    
    #IV/IV
    borrow_IV_IV_chord <- intersect(borrow_IV_scale, borrow_min_scale)
    borrow_IV_IV_chord <- borrow_IV_IV_chord[borrow_IV_IV_chord %in% scale_chords == F] #Remove ii chord
    borrow_IV_IV_chord <- borrow_IV_IV_chord[grepl('m', borrow_IV_IV_chord )==F] #remove v-min chord
    
    #V/iii
    borrow_V_iii_chord <- str_remove(scale_chords[7], 'o')
    borrow_V_iii_chord <- toupper(borrow_V_iii_chord)
    
    #V/VI
    #borrow_V_dom_VI_chord <- paste()
    
    #bII 
    borrow_bII_chord <- root_2b

    #ii min 
    borrow_ii_min_chord <-str_remove(borrow_min_scale[2], 'o')
    borrow_ii_min_chord <- paste0(borrow_ii_min_chord, 'm7(b5)')
    
    
    #   #IVb7/(bor) *Don't know what scale this is borrowed form
    sharp_4th_dom_7_chord <- paste0(reorder_chrom[7], '7')
    sharp_4th_maj_chord <- reorder_chrom[7]
    
    #VIb7/(bor)*
    flat_6_dom_7_chord <- paste0(borrow_min_scale[6], '7')
    
    #Null Scale
    borrow_maj_scale <- NULL
    borrow_ii_maj_scale <- NULL
    borrow_ii_maj_scale <- NULL
    borrow_IV_maj_scale <- NULL
    borrow_V_maj_scale <- NULL
    
    #NULL chords
    borrow_VI_maj_dom <- NULL
    borrow_maj_scale_dom_7 <- NULL
    borrow_lydian_II_chord  <- NULL
    borrow_loc_1 <- NULL
    mix_dom_7_chord <- NULL
    borrow_vii_dim_VII_chord <- NULL
    borrow_vi_chord <- NULL
    
  }else if(mode == 'minor'){
   # dim_idx <- 2 #Get diminished idx DON'T KNOW IF I NEED THIS
    
    borrow_key_maj <- paste0(root_1, 'maj')
    reorder_chrom_maj <- reorder_chrom_key(borrow_key_maj)
    borrow_maj_scale <-  get_chords_from_key(reorder_chrom_maj, 'major', F)

    borrow_key_dorian <- paste0(root_1, 'dor')
    reorder_chrom_dorian <- reorder_chrom_key(borrow_key_dorian )
    borrow_dor_scale <- get_chords_from_key( reorder_chrom_dorian , 'dor', F)
    
    borrow_key_2_maj <- paste0(root_2, 'maj')
    reorder_chrom_2 <- reorder_chrom_key( borrow_key_2_maj)
    borrow_ii_maj_scale <- get_chords_from_key(reorder_chrom_2, 'major', F)
  
    #Borrow IV(minor scale)
    borrow_key_4 <- paste0(root_4, 'min')
    reorder_chrom_4 <- reorder_chrom_key(borrow_key_4)
    borrow_IV_scale <- get_chords_from_key(reorder_chrom_4, 'minor', F)
 
    #Borrow IV(major scale)
    borrow_key_4_maj <- paste0(root_4, 'maj')
    reorder_chrom_4_maj <- reorder_chrom_key(borrow_key_4_maj)
    borrow_IV_maj_scale <- get_chords_from_key( reorder_chrom_4_maj , 'major', T)
    borrow_IV_maj_scale[borrow_IV_maj_scale %in% scale_chords] <- NA #Set chords to NA if in minor scale
    borrow_IV_maj_scale[borrow_IV_maj_scale %in%   borrow_maj_scale ] <- NA #set chords to NA if in borrowed major scale
    
    borrow_key_5 <- paste0(root_5, 'min')
    reorder_chrom_5 <- reorder_chrom_key(borrow_key_5)
    borrow_V_scale <- get_chords_from_key(reorder_chrom_5, 'minor', T)
    
    borrow_key_6_maj <- paste0(root_6, 'maj')
    reorder_chrom_6 <- reorder_chrom_key(borrow_key_6_maj)
    borrow_VI_maj_scale <- get_chords_from_key(reorder_chrom_6, 'major', F)

    # #Borrowed Chords 
    borrow_maj_scale_dom_7 <-  paste0(borrow_maj_scale[5], '7')
    borrow_loc_1 <- str_replace(scale_chords[1], 'm', 'o')
    borrow_VI_maj_dom_7 <- paste0(borrow_VI_maj_scale[5], '7')
    borrow_VI_maj_dom_9 <- paste0(borrow_VI_maj_scale[5], '9')
    borrow_VI_maj_dom <- paste0(borrow_VI_maj_dom_7, '|', borrow_VI_maj_dom_9 )
    borrow_lydian_II_chord <-  root_2
    borrow_vii_dim_VII_chord <- paste0(tolower(reorder_chrom[10]), 'm\\(b5\\)', '|', 'm7\\(b5\\)')
    
    #Borrow vi chord (i.e. Make VI minor)
    borrow_vi_chord <- paste0(tolower(scale_chords[6]), 'm')
      
    #Set NULL scales'
    borrow_II_scale <- NULL
    borrow_IV_IV_chord <- NULL
    borrow_V_maj_scale <- NULL
    borrow_IV_scale_dom_7 <- NULL
    borrow_V_scale_dom_7 <- NULL
    borrow_VI_scale <- NULL
    borrow_VI_dom <- NULL
    borrow_min_scale <- NULL
   
    #Set NULL Chords
    borrow_bII_chord <- NULL
    borrow_V_iii_chord <- NULL
    borrow_ii_min_chord <- NULL
    mix_dom_7_chord <- NULL
    lydian_dim <- NULL
    lydian_vii <- NULL
    lydian_half_dim <- NULL
    borrow_dor_half_dim_chord  <- NULL
    sharp_4th_dom_7_chord <- NULL
    sharp_4th_maj_chord  <- NULL
    flat_6_dom_7_chord <- NULL
    
  } else if(mode == 'mix'){
    
    #Borrow Dorian Scale
    borrow_key_dorian <- paste0(root_1, 'dor')
    reorder_chrom_dorian <- reorder_chrom_key( borrow_key_dorian )
    borrow_dor_scale <-  get_chords_from_key(reorder_chrom_dorian, 'dor', T)
    
    #Borrow Minor Scale
    borrow_minor_key <-  paste0(root_1, 'min') #Add mode to root to get key
    reorder_chrom_min <- reorder_chrom_key(borrow_minor_key) #Reorder chromatic scale according to key
    borrow_min_scale <- get_chords_from_key(reorder_chrom_min , 'minor', T)
    
    #Borrow Major Scale
    borrow_key_maj <- paste0(root_1, 'maj')
    reorder_chrom_maj <- reorder_chrom_key(borrow_key_maj)
    borrow_maj_scale <-  get_chords_from_key(reorder_chrom_maj, 'major', F)
    
    #Mixolydian Dominant 7th chord
    mix_dom_7_chord <- paste0(scale_chords[1], '7')
    
    
    
    #Set scales and chords to be NULL
    borrow_II_scale <- NULL
    borrow_ii_maj_scale <- NULL
    borrow_IV_IV_chord <- NULL
    borrow_IV_scale <- NULL
    borrow_IV_scale_dom_7 <- NULL
    borrow_IV_maj_scale <- NULL
    borrow_V_scale <- NULL
    borrow_V_scale_dom_7 <- NULL
    borrow_V_maj_scale <- NULL
    borrow_VI_scale <- NULL
    borrow_VI_dom <- NULL
    borrow_maj_scale_dom_7 <- NULL
    borrow_VI_maj_dom <- NULL
    
    #Set NULL Chords
    borrow_dor_half_dim_chord  <- NULL
    borrow_lydian_II_chord  <- NULL
    lydian_vii <- NULL
    lydian_dim <- NULL
    lydian_half_dim <- NULL
    borrow_loc_1 <- NULL
    borrow_bII_chord <- NULL
    borrow_ii_min_chord <- NULL
    borrow_V_iii_chord <- NULL
    borrow_vii_dim_VII_chord <- NULL
    sharp_4th_maj_chord  <- NULL
    flat_6_dom_7_chord <- NULL
    sharp_4th_dom_7_chord <- NULL
    borrow_vi_chord <- NULL
    
  } else if(mode == 'dor'){

    #Get Borrow from V major scale 
    borrow_key_5_maj <- paste0(root_5, 'maj')
    reorder_chrom_5_maj <- reorder_chrom_key(borrow_key_5_maj)
    borrow_V_maj_scale <- get_chords_from_key(reorder_chrom_5_maj, 'major', F)
    
    #Borrow Minor Scale
    borrow_minor_key <-  paste0(root_1, 'min') #Add mode to root to get key
    reorder_chrom_min <- reorder_chrom_key(borrow_minor_key) #Reorder chromatic scale according to key
    borrow_min_scale <- get_chords_from_key(reorder_chrom_min , 'minor', T)
    
    
    #Set NULL scales and chords
    borrow_ii_maj_scale <- NULL
    borrow_II_scale <- NULL
    borrow_lydian_II_chord  <- NULL
    borrow_IV_IV_chord <- NULL
    borrow_IV_scale <- NULL
    borrow_IV_scale_dom_7 <- NULL
    borrow_IV_maj_scale <- NULL
    borrow_V_scale <- NULL
    borrow_V_scale_dom_7 <- NULL
    borrow_VI_scale <- NULL
    borrow_VI_dom <- NULL
    borrow_VI_maj_dom <- NULL
    borrow_maj_scale <- NULL
    borrow_maj_scale_dom_7 <- NULL
    borrow_dor_scale <- NULL
    
    #Set NULL Chords
    borrow_bII_chord <- NULL
    borrow_ii_min_chord <- NULL
    borrow_V_iii_chord <- NULL
    mix_dom_7_chord <- NULL
    borrow_loc_1 <- NULL
    lydian_vii <- NULL
    lydian_dim <- NULL
    lydian_half_dim <- NULL
    borrow_dor_half_dim_chord  <- NULL
    sharp_4th_dom_7_chord <- NULL
    borrow_vii_dim_VII_chord <- NULL
    sharp_4th_maj_chord  <- NULL
    flat_6_dom_7_chord <- NULL
    borrow_vi_chord <- NULL
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
  
  
  #Find Chords Borrowed from Dorian Scale
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
        
        
        #NOTE: Minor scales have flat 3rd, 6th, and 7th notes
        #Example: Cmajor = C  D  E  F  G  A   B 
                # Cminor = C  D  Eb F  G  Ab  Bb
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
  
  
  
  #Borrow V7 from Major Sale 
  if(length(  borrow_maj_scale_dom_7 ) > 0 ){
    
      fill_idx <- grep(pattern =borrow_maj_scale_dom_7, x = chord_vec)
      roman_vec[fill_idx] <- paste('V', '(maj)', sep = '/')
      
    }
  
  
  if(length(borrow_IV_maj_scale) > 0 ){
    for(i in 1:length(borrow_IV_maj_scale)){
      borrow_idx <- match(borrow_IV_maj_scale[i], borrow_IV_maj_scale)
      fill_idx <- grep(pattern = borrow_IV_maj_scale[borrow_idx], x = chord_vec)
      roman_vec[fill_idx] <- paste(roman_major[borrow_idx], 'iv(maj)', sep = '/')
      
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

  if(length(lydian_half_dim) > 0){
    roman_vec[grep(lydian_half_dim, chord_vec)] <- '#iv/(lyd)'
  }
  if(length(lydian_dim) > 0){
    roman_vec[grep(lydian_dim, chord_vec)] <- 'iv/(lyd)'
  }
  if(length(borrow_VI_dom) > 0 ) {

    roman_vec[grep(borrow_VI_dom, chord_vec)] <- 'V/vi'
  }
  if(length(borrow_V_iii_chord) > 0){
    roman_vec[grep(borrow_V_iii_chord, chord_vec)] <- 'V/iii'
    
  }
  
  
  if(length( mix_dom_7_chord) > 0){
    roman_vec[grep(mix_dom_7_chord, chord_vec)] <- 'I'
    
  }
 
  
  #Borrow IV chord from IV
  if( length(borrow_IV_IV_chord) > 0 ){
    fill_idx <- which(chord_vec == borrow_IV_IV_chord)
    roman_vec[fill_idx] <- 'IV/IV'
    
  }
  
  if(length(borrow_bII_chord) > 0) {
    fill_idx <- which(chord_vec == borrow_bII_chord)
    roman_vec[fill_idx] <- 'bII/(loc)'
    
  }

  if(length(borrow_ii_min_chord) > 0){
    
    fill_idx <- which(chord_vec == borrow_ii_min_chord)
    roman_vec[fill_idx] <- 'ii/(min)'
  }

  if(length(borrow_VI_maj_dom) > 0){
    roman_vec[grep(borrow_VI_maj_dom, chord_vec)] <- 'V/VI(maj)'
  }
  
  
  if(length(borrow_lydian_II_chord) > 0){
    roman_vec[grep(borrow_lydian_II_chord, chord_vec)] <- 'II/(lydian)'
  }
  
  if(length(borrow_dor_half_dim_chord ) > 0 ){
    roman_vec[grep(borrow_dor_half_dim_chord, chord_vec)] <- 'vi/(dor)'
    
  }
  
  if(length(sharp_4th_dom_7_chord) > 0 ){
    roman_vec[grep(sharp_4th_dom_7_chord, chord_vec)] <- '#IV/(bor)'
    
  }
  if(!is.null(sharp_4th_maj_chord)){
    if(sharp_4th_maj_chord %in% unique_borrow_chords){
      roman_vec[grep(sharp_4th_maj_chord, chord_vec)] <- '#IV/(bor)'
    }
  }
  
  if( length(borrow_vii_dim_VII_chord ) > 0){
    
    roman_vec[grep(borrow_vii_dim_VII_chord, chord_vec)] <- 'vii/VII'
  }
  
  if(length(flat_6_dom_7_chord) > 0){
    roman_vec[grep(flat_6_dom_7_chord, chord_vec)] <- 'bVI/(bor)'
  }
  if(length(borrow_vi_chord) > 0){
    roman_vec[grep(borrow_vi_chord, chord_vec)] <- 'vi/(bor)'
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
  flat_mix_keys <- 'Fmix|Bbmix|Cmix'
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
  dor_mode_triads <- c('m', 'm', '', '', 'm', 'o', '')
  
  
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
    notes[mix_lowers] <- tolower(notes[mix_lowers] )
    notes <-  paste0(notes , mix_mode_triads)
    
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




check_roman <- function(x, song_name, song_part_name = NULL){
  
  require(dplyr)
  require(stringr)
  #Input: 
        #x a dataframe with columns song, song_parts, key, roman
        #song, songparts are strings  to select specific song and part
        #Default of song_part is null, so you can simply input song name if there is only one part listed in hooktheory
  #Output:
    #Data frame with two rows; one for chords and one with roman numerical analysis
  
  
  #Select specific row from dataframe based on song and song part if inputted
  if(is.null(song_part_name)){
    
    song_row <- x[x$song == song_name , ]

      } else {
    
            song_row <- x[x$song == song_name & x$song_parts ==  song_part_name , ]

      } 
  #Split X into dataframe: one with roman, one with chords
  roman <-   song_row %>%  
    select(song, song_parts, key, roman) %>% 
    rename(chords = roman) %>% 
    mutate(roman = 1)
  chords  <-   song_row %>%  
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

#Function to extract Features
extract_features <- function(chords, roman ){
  require(stringr)
  require(dplyr)

  
  #Test
  # i <- 174
  # chords <- beatles$chords[i]
  # roman <- beatles$roman[i]
  
  
  #Remove Flats
  features <- chords %>% 
                str_remove_all(pattern = '[A-Z]b') %>% 
                str_remove_all(pattern = '[A-Z]#') %>% 
                str_remove_all(pattern = '[A-Z]') 
  #Remove minor, keep m's
  features  <- features %>% 
                    str_remove_all(pattern = '[a-z]bm')  %>% 
                    str_remove_all(pattern ='[a-z]m') %>% 
                    str_remove_all(pattern = '[a-z]#m')
  
  #Remove dim
  features  <- features %>% 
                    str_replace_all(pattern ='[a-z]#o', 'o') %>% 
                    str_replace_all(pattern = '[a-z]b', 'o') %>% 
                    str_replace_all(pattern = '[a-z]o', 'o')  
              
  #Split into Vector 
  feature_vec <- features %>%  str_split(pattern = '-', simplify = T) %>%  as.vector
  
  #Split Roman into Vector 
  roman_vec <- roman %>%  str_split(pattern = '-') %>%  unlist
  
  #Find Borrowed Idx
  borrow_idx <- grep('/',   roman_vec)
  
  #Function to Paste features before slash in borrow chords
  paste_feature_borrow_chords <- function(x, feature){
    
    split_chord <- str_split(x, '\\/') %>%  unlist
    chord_plus_feature <- paste0(split_chord[1], feature, '/', split_chord[2])
    return(chord_plus_feature)
  }
  
  #Deal with borrowed Chords
  borrow_roman <- roman_vec[borrow_idx]
  borrow_features <- feature_vec[borrow_idx]
  borrow_roman_plus_features <- rep(NA, length(borrow_idx))
  for(i in seq_along(borrow_idx)){

    borrow_roman_plus_features[i] <-   paste_feature_borrow_chords(borrow_roman[i], borrow_features[i])

  }
  
  #Paste roman numerals and features and store into vector at proper idx 
  roman_plus_features <- rep(NA, length(feature_vec ))
  diatonic_idx <-   which(1:length(feature_vec) %in% borrow_idx == F) # Select idx from chords in scale
  roman_plus_features[diatonic_idx]  <- paste0(roman_vec[diatonic_idx], feature_vec[diatonic_idx])
                                             
  #Add roman plus features for borrowed chords
  roman_plus_features[borrow_idx] <-   borrow_roman_plus_features
  
  #Collapse vector into single string; each chord separated by hyphens
  roman_plus_features_string <- paste(roman_plus_features,  collapse = '-')

  return(roman_plus_features_string)
  
}


#### Plotting/Tabulation Functions


#### Functions

#Detects Minor Chords in a Song
detect_minor <- function(x){
                      x <- str_remove_all(x, "/.*") 
                      x<- str_detect(x, 'i|v')  == T &  str_detect(x, '7')   == F  
                      return(sum(x))
}


detect_minor_7 <- function(x){
                x <- str_remove_all(x, "/.*") 
                x <- str_detect(x, 'i7|v7')
                return(sum(x))
}

detect_dom_7 <- function(x){
                x <- str_remove_all(x, "/.*") 
                x <- str_detect(x, 'I7|V7') == T 
  return(sum(x))
}


#Detects Number of Unique Chords in a Song
sum_chords_unique <- function(chords){
  
  require(dplyr)
  
  chord_vec <- strsplit(chords, '-') %>%  unlist
  
  unique_chords <- unique(  chord_vec )
  
  return(length(unique_chords))
}


#Detects Perctange of chords matching a given string pattern
chord_pct <- function(chords, pattern){
  
  require(dplyr)
  require(stingr)
  chord_vec <- strsplit(chords, '-') %>%  unlist
  pct_chord <- sum(grepl(pattern,   chord_vec) ) / length(  chord_vec)
  
  return(pct_chord )
}



#Detects Borrowed Chord Percentages
borrow_chord_pct <- function(chords){
  
  require(dplyr)
  
  chord_vec <- strsplit(chords, '-') %>%  unlist
  pct_borrow <- sum(grepl('/',   chord_vec) ) / length(  chord_vec)
  
  return(pct_borrow)
}


remove_duplicates <- function(x){
  #Takes in a character string
  #Removes duplicate chords in a sequence
  #Returns character string
  require(dplyr)
  require(stringr)
  x <- x %>%   str_split(pattern = '-')  %>%  unlist
  x <- x[x != lag(x, default = 1)]
  x <- paste(x, collapse = '-')
  return(x)
}


#Merge Roman Numerical Anaylsis Chords from Song Parts into single string for each song
merge_song_parts <- function(df, features){
  
  require(dplyr)
  
  #Input: 
  # Dataframe with Factor column called song & string column called roman or roman_features
  #Process:
  #Splits data.frame into list of dataframes whose rows are the song parts of each song
  #Pastes roman string vectors into one long vector  
  #Bind rows
  #Output:
  #Data.frame where each row is a song, with roman 
  
  #Warnings 
  if(is.data.frame(df) == F){
    warning('The "df" arg must be a data.frame!',
            immediate. = T) 
    
  } 
  if(sum(names(beatles) == 'roman') != 1 | 
     sum(names(beatles) == 'roman_features') != 1){
    
    warning('"df" must contain a column called "roman" or "roman_features"!', 
            immediate. = T )
    
  }
  if(sum(names(beatles) == 'song') !=1){
    warning('"df" must contain a column called "song"!', immediate. = T )
    
  }
  if(is.logical(features) == F) {
    warning('"feature" must be TRUE or FALSE!', immediate. = T )
    
  }
  if(features & sum(names(beatles) == 'roman_features')!= 1 |
     !features & sum(names(beatles) == 'roman') != 1){
    warning('"features" arg must match col name', immediate. = T)
    
  }
  
  #Process
  l <- df %>%
    split(f = df$song)
  
  if(features){
            l <- l %>%
              lapply(function(x) data.frame(
                song = unique(x$song),
                roman_features= paste(x$roman_features, collapse = '-')))
  }else if(!features){

    l <- l %>%
      lapply(function(x) data.frame(artist = unique(x$artist),
                                    song = unique(x$song),
                                    roman = paste(x$roman, collapse = '-')))
  } 
  
  df_song_chords <- suppressWarnings(bind_rows(l)) 
  return(df_song_chords)
  
}

#Creates n_grams
convert_to_ngrams <- function(df, n, features){
  require(tm)
  require(dplyr)
  
  #Temporarily rename 'roman_features' col 
  if(features){
    df <- df %>% 
      rename(roman = roman_features)
     
  }
  
  #Replace /(Borrowed Scale) with "BORROWED"
  #Create ngrams
  df_ngrams <- df %>% 
    mutate(roman = str_replace_all(roman, '/', 'BORROWED'),
           roman = str_remove_all(roman, '\\('),
           roman= str_remove_all(roman, '\\)')) %>% 
    unnest_tokens(n_gram, roman, token = 'ngrams', n = n, to_lower = F)  

  #Restore Original Borrowed scale 
  df_ngrams$n_gram <-   df_ngrams$n_gram %>% 
    lapply(function(x) {
      x_chord <- str_split(x, pattern = '\\s', simplify = T)
      borrow_logical <- grepl('BORROWED', x_chord)
      x_chord[borrow_logical] <- paste0(x_chord[borrow_logical], ')')
      new_x <- paste(x_chord, collapse = ' ')
      new_x <- str_replace_all(new_x, "BORROWED", "/(")
      return( new_x)}) %>% 
    unlist

  
  #Rename n_gram field
  if(n== 2){
    df_ngrams <- df_ngrams %>%
      rename(bigram = n_gram)
      return(df_ngrams)
    
    
  } else if(n == 3){
    df_ngrams <- df_ngrams %>%
      rename(trigram = n_gram)
    
  } else {
    warning('"n" must equal 2 or 3!' )
  }
  
  
  return(df_ngrams)
  
}


#Convert from DTM into Tibble 
convert_to_tibble <- function(dtm){
  
  dtm_tibble <-  cbind(dtm$dimnames$Docs, 
                       as.matrix(dtm)) %>% 
    as.tibble()
  
  #Rename song column 
  dtm_tibble <- dtm_tibble %>% rename(song = V1) 
  
  #Convert n_gram columns to integer 
  n_gram_cols <- grepl('song',names(dtm_tibble)) == F
  dtm_tibble <- dtm_tibble %>%  mutate_if(.predicate = n_gram_cols, as.integer)
  return(dtm_tibble)
}



#### Themes ####

my_theme <- theme_bw() +
  theme(panel.grid = element_blank()) 

my_theme_tilt <- theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()) 


#### Color Schemes ####

#Red
pal_red <- brewer.pal(8,"Reds")
pal_red <- pal_red[-(1:3)]

#Blue
pal_blue <- brewer.pal(8,"Blues")
pal_blue <- pal_blue[-(1:3)]







