source(file = 'source.R')

#Create data.frame 
#Cols: artists, songs = available songs on hooktheory, links = links to pages with chords for each song

#OBTAIN LINKS FOR VECTOR OF ARTISTS: artist %>% lapply(extract_song_links) %>%  bind_rows
#SEARCH FOR SINGLE ARTIST: extract_song_links(artist) 

#Create a vector of artists 
artist <- c('tom petty', 
            'led zeppelin', 'elvis presley', 'ray charles', 'the beatles')

#Create Data.frame of available songs
d <- artist %>%  
        lapply(extract_song_links) %>%  
        bind_rows


#Filter Out Cover Songs
## Watch out for Singers with Different Bands
d <- d[tolower(d$Artist) %in% artist ,]
