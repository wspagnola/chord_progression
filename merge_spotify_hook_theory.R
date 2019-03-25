source('source.R')
#### Get Songs from Spotify

#Get Artist Tracks
tom_petty_tracks <- get_artist_tracks(artist_name ='Tom Petty', token = my_token)
elvis_tracks <- get_artist_tracks(artist_name ='Elvis Presley', token = my_token)
ray_tracks <- get_artist_tracks(artist_name ='Ray Charles', token = my_token)
zeppelin_tracks <- get_artist_tracks(artist_name ='Led Zeppelin', token = my_token)
beatles_tracks <- get_artist_tracks(artist_name ='The Beatles', token = my_token)

#Bind tracks with audiofeatures
ray_track_info <- ray_tracks %>% 
                  cbind(get_track_audio_features(tracks=ray_tracks, access_token = my_token))
elvis_track_info <- elvis_tracks %>% 
                    cbind(get_track_audio_features(elvis_tracks, access_token = my_token))
tom_petty_track_info <-tom_petty_tracks %>% 
                         cbind(get_track_audio_features(tom_petty_tracks, access_token = my_token))
zeppelin_track_info <- zeppelin_tracks %>% 
                        cbind(get_track_audio_features(zeppelin_tracks, access_token = my_token))
beatles_track_info <- beatles_tracks %>% 
                      cbind(get_track_audio_features(beatles_tracks, access_token = my_token))

#Bind Track Info Dfs Together
track_info <- rbind(ray_track_info,
                    elvis_track_info,
                    tom_petty_track_info,
                    zeppelin_track_info,
                    beatles_track_info)

#Remove Remastered and Recording info from Track Name
track_info$track_name <- track_info$track_name %>%  
  str_remove_all(' - .*') 

track_info <- track_info[, -grep('track_uri', names(track_info))[2]]

##### Merge Spotify Data with Sample Data Frame ####

sample_artists <- read.csv('data/sample_data_frame.csv')
sample_artists <- sample_artists %>%  
                        mutate_all(as.character) %>% 
                        select(-X)
sample_artists$artist
sample_artists  <- sample_artists %>% 
                      mutate(song = if_else(song == 'Georgia',
                                            'Georgia On My Mind', song))
sample_artists  <- sample_artists %>% filter(chords != "")
sample_artists[grepl('Tom Petty and|Tom Petty And', sample_artists$artist) ,  'artist'] <- 'Tom Petty'
       

d <-sample_artists %>% 
       mutate(song_id = tolower(song)) %>% 
       left_join(track_info %>% mutate(track_name = tolower(track_name)),
            by =c('artist', 'song_id' = 'track_name')) %>% 
       distinct(artist, song, song_parts, .keep_all = T) %>% 
       select(-song_id)

d <- d %>%  mutate(artist_song = paste(artist, song, sep = "_"))
d_list <- split(d, d$artist_song) 

d_keys <- d_list %>% lapply(extract_song_key) %>%  unlist

key_df <-str_split(names(d_keys), "_") %>%  bind_cols %>% t  %>% as.data.frame
rownames(key_df) <- NULL
colnames(key_df) <- c('artist', 'song')
key_df$keys <- d_keys 

final_df <- d %>%  
  left_join(key_df) %>% 
  select(-artist_song)
