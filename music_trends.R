require(lubridate) # I like this because it does not reload the loaded package
require(dplyr)
require(httr)
require(rvest)
require(XML)
require(xml2)
require(RCurl)
require(stringr)
require(jsonlite)
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



#### Work with Spotify API  ####

get_artists()
get_album_tracks()


sample_df <- read.csv('data/sample_data_frame.csv')
sample_artists <- as.character(unique(sample_df$artist))

get_album_tracks()
?get_albums

options(access_token = my_token)

artist_uri_list <- list()
for(i in 1:length(sample_artists)){
  
  artist_uri_list[i] <- get_artists(sample_artists[i],
                                    access_token = my_token)[, 2]
  
  
}


length()
length(artist_uri_list)

artist_uri_list[[6]]


get_albums(artist_uri_list[[1]][1],
           access_token = my_token)

#Get List of Albums for Each Artist
album_uri_list <- list()
for(i in 1:length(artist_uri_list)){
  
  
     print(sample_artists[i])
  
       print('error 1')
      num_artist_names <- length(artist_uri_list[[i]]) #Get Number of Artist
      album_tibbles <- list()
  
      print('error 2')
     for(j in 1:num_artist_names){
        Sys.sleep(2)
        album_tibble <- get_albums(artist_uri_list[[i]][j],
                                    access_token = my_token)
        
        print('error 3')
        
        if(ncol(album_tibble) > 0){
              album_tibbles[j] <-    album_tibble[, 1]
            print('error 4')

        } else{
          print('error 5')
              album_tibbles[j] <- NULL
        }
        
     }
      
      print('error 6')
        album_tibbles <- compact(album_tibbles )
        album_num <- length(unlist(album_tibbles))
        
        album_uri_vec <- rep(NA, album_num)
      
        album_uri_vec <- unlist(album_tibbles)
        album_uri_vec <-  album_uri_vec[!is.na( album_uri_vec)]
        names(album_uri_vec) <- NULL
        
        #Creates data frame with album uris and artist name
        d <- data.frame(artist = sample_artists[i], album_uri = album_uri_vec )
  
    album_uri_list[[i]] <-  d
    
  
}


#Get Tracks Uris from Each album
track_uris <- list()
for(i in 1:length(  album_uri_list)){
    i <- 1
    albums <-  as.data.frame(album_uri_list[[i]])
    d <- albums %>%  select(album_uri)
    d$album_uri <- as.character(d$album_uri)
    for(j in 1:length(albums)){
        j <- 2
     get_album_tracks(d, access_token = my_token)
      
    }
  class(d$album_uri)
  
}

get_ar
albums[1]
?get_album_tracks()
artists <- get_artists('Tom Petty', access_token = my_token) 
albums <- get_albums(artists$artist_uri[1], access_token = my_token )
get_album_tracks(albums, access_token = my_token)
  pull(artist_uri) %>% 
  get_albums(access_token = my_token)
  get_albums()
get_albums('Tom Petty', access_token = my_token)
album_uri_list(arti)

album_tibble<- get_albums(artist_uri_list[[3]][1], access_token = my_token)
ncol(album_tibble)
album_tibble[, 1]
get_albums(artist_uri_list[[2]][2],
           access_token = my_token)[, 1]
artist_uri_list <- list()


get_artists(sample_artists[1], access_token = my_token)[, 2]
get_artists(sample_artists[2], access_token = my_token)[, 2]
get_artists(sample_artists[3], access_token = my_token)[, 2]
get_artists(sample_artists[4], access_token = my_token)[, 2]
get_artists(sample_artists[5], access_token = my_token)[, 2]

artist_uris <- sample_artists %>% 
                    sapply(function(x) get_albums(x, access_token = my_token))
) 

radiohead_uri <- get_artists('radiohead', access_token = access_token)[1,2]



GET("https://api.spotify.com/v1/search", query = list(q = 'beatles', 
                         type = "artist", access_token = my_token)) %>% content

require(purrr)
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
require(lubridate)
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
