require(rvest)
require(XML)
require(tidyverse)
require(seleniumPipes)
require(RSelenium)
require(httr)
require(lubridate)
require(spotifyr)

#Get Spotify Token
my_token <- get_spotify_access_token(client_id = 'b7e786e6e51541e7b0e39a1c547e3434', 
                                     client_secret = '7164828c5b29439882806d654c6cfd73')


#Clean song_contents
clean_song_contents <- function(x){
  require(tidyverse)

    clean_x <- x %>% 
    str_replace_all('\\s+\\) ', ') ') %>% 
    str_replace_all('\\s+\\( ', '(') %>% 
    str_replace_all('\\s+2', '2') %>% 
    str_replace_all('\\s+4', '4') %>% 
    str_replace_all('\\s+6', '6') %>% 
    str_replace_all('\\s+7', '7') %>% 
    str_replace_all('\\s+9', '9') %>% 
    str_replace_all('A\\s+b', 'Ab') %>% 
    str_replace_all('B\\s+b', 'Bb') %>% 
    str_replace_all('C\\s+b', 'Cb') %>% 
    str_replace_all('D\\s+b', 'Db') %>% 
    str_replace_all('E\\s+b', 'Eb') %>% 
    str_replace_all('F\\s+b', 'Fb') %>% 
    str_replace_all('G\\s+b', 'Gb') %>% 
    str_replace_all('a\\s+b', 'ab') %>% 
    str_replace_all('b\\s+b', 'bb') %>% 
    str_replace_all('c\\s+b', 'cb') %>% 
    str_replace_all('d\\s+b', 'db') %>% 
    str_replace_all('e\\s+b', 'eb') %>% 
    str_replace_all('f\\s+b', 'fb') %>% 
    str_replace_all('g\\s+b', 'gb') %>% 
    str_replace_all('\\s+m', 'm') %>% 
    str_replace_all('\\s+#', '#') %>% 
    str_replace_all('\\s+o', 'o') %>% 
    str_replace_all('\\s+sus', 'sus') %>% 
    str_replace_all('\\s+sus2', 'sus2') %>% 
    str_replace_all('\\s_sus4', 'sus4') %>% 
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
    str_replace_all('bb\\s+b', 'b bb') 
  
  
  return(clean_x)
}



#Extract Song Parts
extract_song_parts <- function(txt) {
  
  require(tidyverse)
  
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
  } else if(is.na(page_vec)){
    artist_vec <- artist
    songs <- NA
    
  }
  d <- data.frame(Artist = artist_vec, 
                  Songs = songs, 
                  Links = href_vec)  
  d <- d %>%  
          mutate_all(as.character)
  return(d)
  
}