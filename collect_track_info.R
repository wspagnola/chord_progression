source('source.R')
#### Get Songs from S


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
                                     revolver, magical_mystery_tour, 
                                     white_album, singles)


write.csv(beatles_track_info_final,
          file = 'data/input/beatles_track_info.csv',
          row.names = FALSE)

#### Old Code ####

#albums <- get_albums(artists$artist_uri[1], access_token = token )

#Get Artist Tracks
# tom_petty_tracks <- get_artist_tracks(artist_name ='Tom Petty', token = my_token)
# elvis_tracks <- get_artist_tracks(artist_name ='Elvis Presley', token = my_token)
# ray_tracks <- get_artist_tracks(artist_name ='Ray Charles', token = my_token)
# zeppelin_tracks <- get_artist_tracks(artist_name ='Led Zeppelin', token = my_token)
# beatles_tracks <- get_artist_tracks(artist_name ='The Beatles', token = my_token)
# 
# #Bind tracks with audiofeatures
# ray_track_info <- ray_tracks %>% 
#                   cbind(get_track_audio_features(tracks=ray_tracks, access_token = my_token))
# elvis_track_info <- elvis_tracks %>% 
#                     cbind(get_track_audio_features(elvis_tracks, access_token = my_token))
# tom_petty_track_info <-tom_petty_tracks %>% 
#                          cbind(get_track_audio_features(tom_petty_tracks, access_token = my_token))
# zeppelin_track_info <- zeppelin_tracks %>% 
#                         cbind(get_track_audio_features(zeppelin_tracks, access_token = my_token))
# beatles_track_info <- beatles_tracks %>% 
#                       cbind(get_track_audio_features(beatles_tracks, access_token = my_token))
# 
# #Bind Track Info Dfs Together
# track_info <- rbind(ray_track_info,
#                     elvis_track_info,
#                     tom_petty_track_info,
#                     zeppelin_track_info,
#                     beatles_track_info)
# 
# #Remove Remastered and Recording info from Track Name
# track_info$track_name <- track_info$track_name %>%  
#   str_remove_all(' - .*') 
# 
# track_info <- track_info[, -grep('track_uri', names(track_info))[2]]
# 
# ##### Merge Spotify Data with Sample Data Frame ####
# 
# sample_artists <- read.csv('data/sample_data_frame.csv')
# sample_artists <- sample_artists %>%  
#                         mutate_all(as.character) %>% 
#                         select(-X)
# sample_artists$artist
# sample_artists  <- sample_artists %>% 
#                       mutate(song = if_else(song == 'Georgia',
#                                             'Georgia On My Mind', song))
# sample_artists  <- sample_artists %>% filter(chords != "")
# sample_artists[grepl('Tom Petty and|Tom Petty And', sample_artists$artist) ,  'artist'] <- 'Tom Petty'
#        
# 
# d <-sample_artists %>% 
#        mutate(song_id = tolower(song)) %>% 
#        left_join(track_info %>% mutate(track_name = tolower(track_name)),
#             by =c('artist', 'song_id' = 'track_name')) %>% 
#        distinct(artist, song, song_parts, .keep_all = T) %>% 
#        select(-song_id)
# 
# d <- d %>%  mutate(artist_song = paste(artist, song, sep = "_"))
# d_list <- split(d, d$artist_song) 
# 
# d_keys <- d_list %>% lapply(extract_song_key) %>%  unlist
# 
# key_df <-str_split(names(d_keys), "_") %>%  bind_cols %>% t  %>% as.data.frame
# rownames(key_df) <- NULL
# colnames(key_df) <- c('artist', 'song')
# key_df$keys <- d_keys 
# 
# final_df <- d %>%  
#   left_join(key_df) %>% 
#   select(-artist_song)
