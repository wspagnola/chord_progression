


baseURL <- 'https://www.hooktheory.com/theorytab/results/path/'

artist <- 'the beatles'
url <- paste0(baseURL, artist)
url <- url %>% str_replace(pattern = ' ', replacement = '+')

page_vec <- url %>% 
  read_html %>%
  html_nodes(xpath = '//a[@class="button button-xs button-browse button-primary-open "]') %>% 
  html_text()

if(length(page_vec) == 0) {
  
     songs <-  url %>% 
        read_html %>% 
        html_nodes(xpath = '//p[@class ="song"]') %>% 
        html_text()
  
} else if(length(page_vec) > 0){
  
  num_pages <- page_vec %>%  as.numeric %>%  max
  
  url <- url %>% paste0('/page/', 1:num_pages )
  
  songs <- lapply(url,  function(url){
                   url %>% 
                    read_html %>% 
                    html_nodes(xpath = '//p[@class ="song"]') %>% 
                    html_text()})
  songs <- unlist(songs)
}


data.frame(artist, songs)  

