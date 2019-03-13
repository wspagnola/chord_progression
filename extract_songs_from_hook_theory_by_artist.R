source(file = 'source.R')

#SINGLE ARTIST
#artist <- 'michael jackson'
#artist <- 'tom petty'
#artist <- 'the beatles'
#artist <- 'led zeppelin'
#artist <- 'elvis presley'
#artist <- 'the pixies'
#artist <- 'ray charles'

#
artist <- c('michael jackson',  'tom petty', 
            'led zeppelin', 'elvis presley', 'ray charles')
baseURL <- 'https://www.hooktheory.com/theorytab/results/path/'
url <- paste0(baseURL, artist)
url <- url %>% 
          str_replace(pattern = ' ', replacement = '+')

#Create Data.frame of available songs
d <- url%>%  
        lapply(extract_song_links) %>%  
        bind_rows

#Filter Out Cover Songs
## Watch out for Singers with Different Bands
d[str_detect(tolower(d$Artist), pattern = artist) ,]
