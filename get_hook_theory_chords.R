require(tidyverse)
require(httr)
require(XML)
require(rvest)

#### Get Access to  HookTheory API ####

api_call <- 'https://api.hooktheory.com/v1/'

#Retrieve ID and activkey
pars <- list(
  username = 'wspagnola',
  password = 'Acadia125!'
)

#Request activkey 
POST(paste0(api_call, 'users/auth'), body = pars) %>%  content
id <- 187488
activkey <- "ceb222d144fc6b3b13f9e3dce2b2bd63"
authorization <- paste("Bearer", activkey) #Get authorization code

#EXAMPLE: Look up songs with 1-2-5 Chord progression 
# GET(paste0(api_call, 'trends/songs?cp=1,2,5'), 
#     add_headers(Authorization = authorization)) %>%  content %>% bind_rows


#### Function to Retrieve all songs for a given chord progression ####


retrieve_by_cp <- function(cp, page_num = 100){
  
  #Convert Chord Progression into a single string separated by commas
  cp_string <- cp %>%  as.character %>%  paste(collapse = ',')
  
  #Create Empty List
  song_request_list <-vector('list', page_num)
  
  #For Loop to retrieve songs; each page contains 20 songs
  #Breaks when new tibble in list is empty 
  #Sleeps 1 second after each request; limit = 10 requests / 10 seconds
  for (i in 1:page_num ) {
    url <- paste0('trends/songs?cp=', cp_string, '&page=', i)
    song_request_list[[i]] <-  GET(paste0(api_call, url), 
                            add_headers(Authorization = authorization)) %>%  
                        content %>% 
                        bind_rows
    
    if( song_request_list[[i]] %>%  nrow == 0) {
      break
    } else{
      Sys.sleep(1) 
    }
  }
  
  #Convert List into large tibble
  song_df <-song_request_list %>% 
                bind_rows
                  
  return(song_df)
  
}


#### Retrieve Common Chord Progressions ####
cp_125 <- retrieve_by_cp(cp = c(1,2,5)) 
cp_14 <- retrieve_by_cp(cp = c(1,4), page_num = 150) 
cp_14 %>%  View

cp_14 <- retrieve_by_cp(cp = c(1,4), page_num = 1) 
cp_14 %>%  View


cp_1464 <- retrieve_by_cp(cp = c(1,5, 6, 4), page_num = 11) 
cp_1464 %>%  as.data.frame %>% head



#### Retrieve One Page at a Time  ####

page_num <- 5
cp <- c(1,5, 6, 4)
cp_string <- cp %>%  as.character %>%  paste(collapse = ',')

url <- paste0('trends/songs?cp=', cp_string, '&page=', page_num )
GET(paste0(api_call, url), 
      add_headers(Authorization = authorization)) %>%  
  content %>% 
  bind_rows


#### Scrape Hook Theory Website (DOESN'T WORK) ####


#Step 1: Fire up Selenium

library('RSelenium')
rsDriver()
checkForServer() # search for and download Selenium Server java binary.  Only need to run once.
startServer() # run Selenium Server binary
remDr <- remoteDriver(browserName="firefox", port=4444) # instantiate remote driver to connect to Selenium Server
remDr$open(silent=T) # open web browser

<tspan alignment-baseline="middle">G</tspan>
  
  
  chords <- paste(url, artist, song, sep = '/')

'http://www.hooktheory.com/theorytab/view/3-doors-down/be-like-that#intro' %>% 
  read_html() %>%  class
  
  html_text() %>% 
  str_detect('tspan')
html_nodes(css = '.gotham') %>% 
  str_extract_all(pattern = 'tspan')
str_replace_all('\n', '')
require(stringr)  
  
  
#NOTE: This doesn't work

#Does anyone know how to scrape SVG?  
#Or are SVG's meant to prevent scraping?

url <- 'https://www.hooktheory.com/theorytab/view'

artist <- 'bj-thomas'
song <- 'raindrops-keep-fallin-on-my-head'

# Get Artists 
chords <- paste(url, artist, song, sep = '/') %>% 
  read_html() %>%    html_text() %>% 
  str_detect('tspan')
html_nodes(css = '.gotham') %>% 
  str_extract_all(pattern = 'tspan')
str_replace_all('\n', '')
require(stringr)

url <- 'https://www.hooktheory.com/theorytab/view/the-beatles/hey-jude'
read_html(url)  %>% 
  html_text() %>%  str_extract("[^svg]+")

read_json('https://www.hooktheory.com/theorytab/view/the-beatles/hey-jude') %>%  str_extract_all('chord')

?
  
  "/theorytab/view/the-beatles/hey-jude#verse"
<a href="/theorytab/view/the-beatles/hey-jude#verse" title="Jump to the verse"><span class="tight-top tight">Verse</span></a>
  
  