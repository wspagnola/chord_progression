library(lubridate)
library(dplyr)
library(httr)
library(rvest)
library(XML)
library(xml2)
library(RCurl)
library(stringr)
library(jsonlite)
#Create Vector of Dates for Each week of Top 100 
#Starts in 1958 and ends on current date
num_weeks <- ceiling(as.numeric(today() - first_bb_date, 'weeks'))
dates <- as.Date('1958-08-04') + weeks(1:num_weeks)

# Create vector of URls by year
baseURL <- "https://www.billboard.com/charts/hot-100/"
top_100_urls <- paste0(baseURL, dates )
                       
#Function to Scrape List for One Week
extra_hot_100 <- function(url) {
  
  
  'This function takes in a URL for the Billboard Hot 100 website,
  and then returns a dataframe with the Song, Artist, and Date.'
  
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
  top_song <- as.character(top_song)
  
  top_songs_2_to_100 <- url %>%          
    read_html() %>%
    html_nodes(xpath = "//div[@class='chart-list-item__title']") %>% 
    html_text() %>% 
    str_replace_all('\n', '') %>% 
    str_replace_all('^ ', '')
  top_songs_2_to_100 <- as.character(top_songs_2_to_100)
  
  Song <- as.character(c(top_song,  top_songs_2_to_100))
  
  #Get Date  
  Week <- as.Date(str_sub(url, start = -10L))
  
  #Store in Dataframe
  df <- data.frame(Artist, Song, Week,
                   stringsAsFactors = F)
  
  return(df)
}   


#Create List and then store each week's data as data.frame within list
top_100_list <- list()
for(i in seq_along(dates[year(dates) <=  max_date])){
  
  #Extract date of current iteration 
  week <- dates[i]

  top_100_list[[i]] <- extra_hot_100(top_100_urls[i])
  
  #lapply(top_100_urls[dates < = year])
  print(week)
}
  
#Bind Rows of list into Data.frame
hot_100_df <- bind_rows(top_100_list)
hot_100_df$Year <- year(hot_100_df$Week)

#Select year(s), and then sample 'n' songs from year range
sample_year <- 1958
n <- 1
sample_song <- hot_100_df %>% 
                  dplyr::filter(Year %in% 1958) %>%  
                  dplyr::sample_n(1)

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



#### Work with Spotify API  ####
<span class="js-tab-row" style="display: inline-block">  <span class="gt-chord js-tab-ch js-tapped" data-chord="B">B</span>             <span class="gt-chord js-tab-ch js-tapped" data-chord="Gbm">Gbm</span>                    <span class="gt-chord js-tab-ch js-tapped" data-chord="E">E</span>              <span class="gt-chord js-tab-ch js-tapped" data-chord="F#">F#</span></span>
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
