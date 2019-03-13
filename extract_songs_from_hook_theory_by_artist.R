require(tidyverse)
require(rvest)

#artist <- 'tom petty'
#artist <- 'the beatles'
#artist <- 'led zeppelin'
artist <- 'elvis presley'
#artist <- 'the pixies'
#artist <- 'ray charles'
baseURL <- 'https://www.hooktheory.com/theorytab/results/path/'

url <- paste0(baseURL, artist)
url <- url %>% str_replace(pattern = ' ', replacement = '+')

page_vec <- NA
page_vec <- url %>% 
  read_html %>%
  html_nodes(xpath = '//a[@class="button button-xs button-browse button-primary-open "]') %>% 
  html_text()
page_vec
if(length(page_vec) == 0) {
  
     songs <- NA
     artist_vec <- NA
     songs <-  url %>% 
        read_html %>% 
        html_nodes(xpath = '//p[@class ="song"]') %>% 
        html_text()
     
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


#Filter Out Cover Songs
## Watch out for Singers with Different Bands
d <- data.frame(Artist = artist_vec, Songs = songs)  
d[str_detect(tolower(d$Artist), pattern = artist) ,]
