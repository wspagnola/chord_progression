
#### Work with HookTheory API ####

api_call <- 'https://api.hooktheory.com/v1/'

#Retrieve ID and activkey
pars <- list(
  username = 'wspagnola',
  password = 'Acadia125!'
)
POST(paste0(api_call, 'users/auth'), body = pars) %>%  content
id <- 187488
activkey <- "ceb222d144fc6b3b13f9e3dce2b2bd63"
authorization <- paste("Bearer", activkey)

#Look up songs with 1-2-5 Chord progression 
GET(paste0(api_call, 'trends/songs?cp=1,2,5'), 
    add_headers(Authorization = authorization)) %>%  content %>% bind_rows

GET(paste0(api_call, 'trends/songs?cp=1,2,5'), 
    add_headers(Authorization = authorization)) %>%  content %>% bind_rows





#### Scrape Hook Theory Website ####


#NOTE: This doesn't work

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
  
  