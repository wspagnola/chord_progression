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



#Takes the Name of an Artist
#Gets all the song names & links on Hooktheory for said Artists
#Returns dataframe with Artist, Song, Links
extract_song_links <- function(url){
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
    
    songs <- lapply(url,  function(url){
      url %>% 
        read_html %>% 
        html_nodes(xpath = '//p[@class ="song"]') %>% 
        html_text()})
    
    
    artist_list <- lapply(url,  function(url){
      url %>% 
        read_html %>% 
        html_nodes(xpath = '//p[@class ="artist"]') %>% 
        html_text()})
    
    songs <- unlist(songs)
    
    artist_list <- artist_list %>% lapply(function(x) str_remove(x, pattern = 'by '))
    artist_vec <- unlist(artist_list)
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