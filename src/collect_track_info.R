### This file collects Beatles track info from Spotify's API.
### I also added some song info for songs that were not available on the API
#It outputs a file called beatles_track_info.csv, which is located in the data/input folder. 


### You will need to get your own api key.
###Then you can use the  get_spotify_access_token() function from the spotify.R package to get a token
###Store the token as an object called my_token using the code below. 
# my_token <-  get_spotify_access_token(client_id = ,    client_secret = )

source('src/source.R')

#source('src/api_token.R') #This is where I store my api_token 

#Get All Albums Before 1971 (While they were still together)
artist_name <- 'The Beatles'
artist_info <- get_artists(artist_name, access_token = my_token)[1, 1:2]
album_info <- get_albums(artist_info$artist_uri , access_token = my_token ) %>% 
  filter(year(album_release_date) < 1971 )

#Get Track info from each album
beatles_track_info <- split(album_info , album_info$album_name) %>%  
                              lapply(function(x) get_album_tracks(x, access_token = my_token)) %>% 
                              bind_rows



meet_the_beatles <- data.frame(album_name = 'Meet the Beatles',
                               artist_name = 'The Beatles',
                               album_release_date = '1964-01-20',
                               year = 1964,
                               track_name = 
                                 c("I Want To Hold Your Hand", "I Saw Her Standing There",
                                   "This Boy", "It Won't Be Long",
                                   "All I've Got to Do","All My Loving", 	
                                   "Don't Bother Me" , 	"Little Child",
                                   "Till There Was You" , "Hold Me Tight", 
                                   "I Wanna Be Your Man", 	"Not a Second Time"))

sgt_peppers_album <-  data.frame(album_name = 'Sgt. Peppers Lonely Hearts Club Band',
                                 artist_name = 'The Beatles',
                                 album_release_date = '1967-05-26',
                                 year = 1967, 
                                 track_name = 
                                   'Being For The Benefit Of Mr. Kite')


white_album <- data.frame(album_name = 'The Beatles', 
                          artist_name = 'The Beatles',
                          album_release_date = '1968-11-22',
                          year = 1968,
                          track_name = c('Lady Madonna', "Honey Pie", "Cry Baby Cry",
                                        "Revolution", "Ob-La-Di Ob-La-Da", "Sexy Sadie",
                                        "Good Night", "Everybody's Got Something to Hide"))

magical_mystery_tour <- data.frame(album_name = 'Magical Mystery Tour', 
                                   artist_name = 'The Beatles',
                                   album_release_date = '1967-11-27',
                                   year = 1967,
                                   track_name = 'Hello Goodbye')

singles <- data.frame(album_name = NA, 
                      artist_name = 'The Beatles',
                      album_release_date = NA,
                      year = c(1962, 1963, 1963, 1965, 1965, 1966, 1968, 1969),
                      track_name = c("Besame Mucho",
                                     "She Loves You", "From Me To You",  
                                     "Day Tripper", "We Can Work It Out",
                                     "Paperback Writer",
                                     "Hey Jude",
                                     "The Ballad of John and Yoko"))

rubber_soul <- data.frame(album_name = 'Rubber Soul', 
                          artist_name = 'The Beatles',
                          album_release_date = '1965-12-03',
                          year = 1965,
                          track_name = "Norwegian Wood")

revolver <- data.frame(album_name = 'Revolver', 
                       artist_name = 'The Beatles',
                       album_release_date = '1966-08-05',
                       year = 1966,
                       track_name = c("Here There And Everywhere", "Elenor Rigby" ))




#Clean Track Info
#Extract Year
beatles_track_info_clean <- beatles_track_info  %>% 
                          left_join(album_info, by = 'album_name') %>% 
                          mutate(album_name = str_remove_all(album_name, ' \\(Remastered\\)'),
                                 album_name = str_remove_all(album_name, ' \\(Super Deluxe Edition\\)'),
                                 track_name = str_remove_all(track_name, ' - Remastered 2009'),
                                 year = year(album_release_date),
                                 artist_name = artist_name) %>% 
                          select(artist_name,
                                 track_name, 
                                 album_name, 
                                 album_release_date,
                                 year) 

beatles_track_info_final <- beatles_track_info_clean %>% 
                               rbind(meet_the_beatles, rubber_soul,
                                     revolver, sgt_peppers_album,  magical_mystery_tour, 
                                     white_album, singles)


write.csv(beatles_track_info_final,
          file = 'data/input/beatles_track_info.csv',
          row.names = FALSE)
