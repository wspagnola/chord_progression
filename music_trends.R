library(lubridate)
library(dplyr)
library(httr)
library(rvest)
library(XML)
library(xml2)
library(RCurl)
library(stringr)
first_bb_date <- as.Date('1958-08-04') 
num_weeks <- ceiling(as.numeric(today() - first_bb_date, 'weeks'))
as.du
?as.duration
as.Date('1958-08-04') 
dates <- as.Date('1958-08-04') + weeks(1:num_weeks)

baseURL <- "https://www.billboard.com/charts/hot-100/"

# Get Top Songs 
top_100_list <- lapply(paste0(baseURL, dates[1:5]), function(url) {
  
  
    # Get Artists 
     top_artist <- url %>% 
          read_html() %>%
          html_nodes(xpath = "//div[@class='chart-number-one__artist']") %>% 
          html_text() %>% 
          str_replace_all('\n', '')
    top_artists_2_to_100 <- url %>%          
          read_html() %>%
          html_nodes(xpath = "//div[@class='chart-list-item__artist']") %>% 
          html_text() %>% 
          str_replace_all('\n', '') %>% 
          str_replace_all('^ ', '')
     Artist <- c(top_artist, top_artists_2_to_100)
  
     # Get Song Titles
     top_song <- url %>% 
             read_html() %>%
             html_nodes(xpath = "//div[@class='chart-number-one__title']") %>% 
             html_text() %>% 
             str_replace_all('\n', '')
     top_songs_2_to_100 <- url %>%          
             read_html() %>%
             html_nodes(xpath = "//div[@class='chart-list-item__title']") %>% 
             html_text() %>% 
             str_replace_all('\n', '') %>% 
             str_replace_all('^ ', '')
      Song <- c(top_song,  top_songs_2_to_100)
     
      #Get Date  
      Week <- as.Date(str_sub(url, start = -10L))
      Year <- year(Week)
      df <- data.frame(Artist, Song, Week, Year)
     
     
  return(df)
}   )  
top_100_list[[1]]
top_artist_list[[1]]  %>%  head()            
x <- 'abcd'
str_sub(x, start = -2L)
top_artists_2_to_100 <- paste0(baseURL, '1958-08-04') %>%
 

top_artist_100 <- c(top_artist, top_artists_2_to_100)


div.chart-number-one__artist


#main > div.chart-detail-header > div.container.container--no-background.chart-number-one >
  



library(spotifyr)

get_artists()
get_album_tracks()

my_token <- get_spotify_access_token(client_id = 'b7e786e6e51541e7b0e39a1c547e3434', 
                         client_secret = '7164828c5b29439882806d654c6cfd73')



radiohead_uri <- get_artists('radiohead', access_token = access_token)[1,2]



GET("https://api.spotify.com/v1/search", query = list(q = 'beatles', 
                         type = "artist", access_token = my_token)) %>% content

library(purrr)
songs <- as.character(top_100_list[[1]]$Song)
get_track_uri(songs[1])
get_track_uri(track_name = '')
x <- songs[2]
songs <- str_remove_all(songs[2], pattern = ' \\(.*')
?str_remove_all
lapply(songs[3], get_track_uri)

songs[2]
get_track_uri <- function(track_name, access_token = my_token){

    require(purrr)
  
      
  
  
    #I don't Know what this does
    res <- GET("https://api.spotify.com/v1/search", 
               query = list(q = track_name, 
              type = "track", access_token = access_token)) %>% content
     content <- res$tracks %>%  .$items
  
     #This makes a dataframe of tracks containing the string in track_name
     tracks <- map_df(seq_len(length(content)), function(this_row) {
                  this_track <- content[[this_row]]
                  list(song_name = this_track$name,
                       track_uri = gsub("spotify:track:", "", this_track$uri)) }) %>%
                        dplyr::filter(!duplicated(tolower(track_name)))

    #This gets the URI for the exact match of track_name
     track_uri <- tracks %>% filter(song_name == track_name) %>%
     pull(track_uri)
     
      return(track_uri[1] )

}
get_track_uri(track_name = 'Poor Little Fool', 
              access_token = my_token )

get_track_audio_features("1ugZWl7RmEq95dea9hqorZ")
?get_track_audio_features
res <- GET("https://api.spotify.com/v1/search", 
           query = list(q="Ricky Nelson", 
                        type = "artist", access_token = my_token)) %>% content
content <- res$tracks %>%  .$items
res


df <- top_100_list[[1]]
df$Artist <- as.character(df$Artist)

x <- 'Tommy Cash with Johnny Cash'
df
x <- gsub(pattern = ' with.*', replacement = '', df$Artist, ignore.case = T)
x <-gsub(pattern = ' and.*', replacement = '', x, ignore.case = T)
gsub(pattern = ' with.*', replacement = '', x)
gsub(pattern = ' with.*', replacement = '', x)

artist_uri_df <- lapply(x, function(x) {
                    get_artists(artist_name = x , access_token = my_token)[1 ,]
  }) %>%  bind_rows() 

uris <- artist_uri_df$artist_uri
artist_uri_df[2]
lapply(uris[2], function(x) {
            if(is.na(x) ==F) {
               get_albums(artist_uri= x , access_token = my_token)
            } else(
              return(NA)
            )
           }) 
get_albums('')
get_albums(artist_uri_df$artist_uri[2], access_token = my_token)
missing <- is.na(artist_uri_df$artist_name)
df[missing ,]$Artist
get_artists('Duane Eddy', access_token = my_token)
get_artists('Johnny Otis', access_token = my_token)

df$Artist[8]
get_artists(artist_name = df$Artist[1],access_token = my_token )
df$Artist %>%  class()
[1,2] %>% 
  pull()
library(lubridate)
albums <- get_albums(artist_uri, access_token = my_token) %>%
                        mutate(year =  year(album_release_year)) %>% 
                        filter(year == 1958) 
get_album_tracks(albums = albums, access_token = my_token ) %>% 
filter(grepl('Poor Little Fool', track_name) ) %>% 
  pull(track_uri)


map_df(seq_len(length(content)), function(this_row) {
  this_track <- content[[this_row]]
  list(song_name = this_track$name, 
       track_uri = gsub("spotify:track:", "", this_track$uri)) }) %>% 
  dplyr::filter(!duplicated(tolower(song_name)))


content
