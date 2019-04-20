#### Process All 60s songs ####


song_df <- read.csv('Data/songs_60s.csv', stringsAsFactors =  F)

song_df$roman <- NA
for(i in 1:nrow(song_df)){
  print(i)
  song_df$roman[i] <- convert_to_roman(chords =song_df$chords[i], key = song_df$key[i])
  
  
}

#Check Discrepancy in Artist Names
unique(song_df$artist)


#### Create Beatles Csv #####

beatles_songs <- song_1960s_df %>% 
  filter(artist == 'Beatles' | artist == 'The Beatles') %>% 
  select(-X)
write.csv(beatles_songs, 'data/input/beatles_songs.csv', row.names = F)

  
  
  
#### Process Beatles songs ####
song_df <- read.csv('Data/songs_60s.csv', stringsAsFactors =  F)
beatles <- song_df[song_df$artist == 'The Beatles' , ]
vec <- 31:40
beatles$roman <- NA
for(i in vec){
  print(i)
  beatles$roman[i] <- convert_to_roman(chords =beatles$chords[i], key = beatles$key[i])
  
}

beatles[vec ,] %>%  View
i <- 3
convert_to_roman(chords =song_df$chords[i], key = song_df$key[i])
beatles[i ,]$song
beatles[i ,]$song_parts
beatles[i ,]$key
key <- beatles[i ,]$key
chords <- beatles[i ,]$chords
convert_to_roman(chords = beatles[i,]$chords, key = beatles[i,]$key  )

rbind(unlist(str_split(beatles[i ,]$chords, '-')),
      unlist(str_split(convert_to_roman(chords = beatles[i ,]$chords, key = beatles[i ,]$key  ), '-'))
) %>%  as.data.frame 


convert_to_roman(chords = blackbird$chords, key = blackbird$key  )
day_in_life$chord
beatles$roman <- NA
beatles[16 ,]
vec <-  c(93:100)


#### Merge Beatles and Track Info #####

beatles_songs <- read.csv('data/input/beatles_songs.csv', stringsAsFactors = F)

beatles_track_info <- read.csv('data/input/beatles_track_info.csv', stringsAsFactors =  F)

beatles_track_info <- beatles_track_info %>% 
                        mutate(track_name = str_remove(track_name, ' -.*'),
                               track_name = tolower(track_name),
                                track_name = str_remove_all(track_name,'\\.'),
                               track_name = str_remove(track_name, '\\!'))

#Missing Songs
missing_songs <-beatles_songs %>% 
                    mutate(song_join_code = tolower(song)) %>% 
                    anti_join(beatles_track_info,  by = c('song_join_code'= 'track_name')) 


  

#Plot Tracks
beatles <- beatles_songs %>% 
              mutate(song_join_code = tolower(song)) %>% 
              left_join(beatles_track_info,  by = c('song_join_code'= 'track_name'))

#Clean Album Names
beatles <- beatles %>% 
      mutate(album_name = str_remove(album_name, '\\s\\(.*'),
             album_name = str_replace(album_name, 'With The Beatles', 'With the Beatles'),
             album_name = str_replace(album_name, 'The Beatles', 'White Album')
) 


beatles %>%  
  group_by(song,album_name, year) %>% 
  count %>%  
  group_by(album_name, year) %>% 
  count %>% 
  arrange(year)

album_chron_levels <- beatles %>% 
                    group_by(album_name, album_release_date) %>% 
                    count %>% 
                    arrange(album_release_date) %>% 
                    drop_na %>% 
                    pull(album_name) 

beatles <- beatles %>% 
            mutate(phase = if_else(album_release_date <= '1966-06-21', 'Early', 'Late'),
                   phase = factor(phase, levels = c('Early', 'Late')),
                    album_name = factor(album_name, levels =album_chron_levels)
)      

#Plot Song Availability by Phase
beatles %>% 
  count(phase, song) %>% 
  count(phase) %>% 
  drop_na(phase) %>% 
  ggplot(aes(x = phase, y = nn, fill = phase)) +
  geom_col() +
  scale_fill_manual(values = c('red', 'blue' )) +
  ylab('Number of Songs') +
  ggtitle('Beatles Songs Available on Hook Theory')

#Plot Song Availability by Album
beatles %>% 
  count(album_name, year, song, phase) %>% 
  count(album_name, year, phase) %>% 
  drop_na(album_name) %>% 
  ggplot(aes(x = album_name, y = nn, fill = phase)) +
  geom_col() +
  scale_fill_manual(values = c('red', 'blue' )) +
  ylab('Number of Songs') +
  ggtitle('Beatles Songs Available on Hook Theory by Album') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### Merge with Hot100 CSV ? #####

hot_100 <- read.csv('data/Hot100.csv')
hot_100$Song <- tolower(hot_100$Song)
missing_songs %>% 
  left_join(hot_100, by = c('song_join_code' = 'Song', 'artist'='Performer')) %>% 
  distinct(artist, song, song_parts, key, bpm, chords, link, song_join_code, 
           WeekID) %>%  View
  


#### Tasks to Complete ####

#Task 1
#Look at borrowed Chords from these two songs
#Lovely Rita
#"I'm Looking Through You"

#Task 2
#Rescrape
#All across the Universe
#Michelle

#Task 3
#Allow scraper to work without song or artist info  (NA)


#Task 4
#Apply roman numerical analysis to all beatles songs
#Fix any problems as they come

#Task 5
#Double check songs (or sample of songs) with borrowed chords


#Task 6
#Add album/song year/Hot100 info to  Beatles songs

#Task 7
#Group by album and create visualizations


#Task 8
#If the scraper breaks down again, 
#...check if there is a way to keep scrolling until each song part is complete

#### Notes On Artist Names ####


#Notes on 60s Artists
#Rolling Stones vs. The Rolling Stones
#Beach Boys vs. The Beach Boys 
# Marvin Gaye and Tammi Terrell" "Marvin Gaye"  
#Hozier and Oleg Berg not from 1960s



#### Notes On Chords ####


#Blackbird
#A7 in Gmaj key listed as V/V

#Here Comes the Sun (Verse)
#B7/D# in Amaj ke listed as II(lyd)65


#Here Comes the Sun (Bridge)
#G in A maj listed as IV/IV


#While My Guitar Gently Weeps
#IV(maj) in A minor versus IV(dorian)


#Come together
#G7 in D minor key listed as IV(dor)b7 instead of vii/V