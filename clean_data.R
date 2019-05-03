source('source.R')




#### Clean Beatles Songs ####
beatles <- read.csv('data/output/beatles_fix.csv', stringsAsFactors = F)


#Help should be A major throughout;  
#the chords match a previous section that was listed as Amaj;
#In addition I have verified the key from several sources
beatles[beatles$song == 'Help' & beatles$song_parts == 'Chorus' , ]$key <- 'Amaj'
  

#Remove Songs that did not scrape chords properly
beatles <- beatles[!beatles$chords =='' ,]



#### Process Beatles songs ####

vec <-1:nrow(beatles)

beatles$roman <- NA
for(i in vec){
  print(i)
  beatles$roman[i] <- convert_to_roman(chords =beatles$chords[i], key = beatles$key[i])
  
}






beatles %>%  select(song, roman) %>%  View
#write.csv(beatles, 'data/output/beatles_roman_analysis.csv', row.names =F)

beatles[grep('NA', beatles$roman) , ] %>%  View


na_chords <- beatles[grep('NA', beatles$roman) , ]
na_chords_roman <- na_chords %>%  
                          select(song, song_parts, key, roman) %>% 
                          rename(chords = roman) %>% 
                          mutate(roman = 1)
na_chords_chords <- na_chords %>%  
                          select(song, song_parts, key, chords) %>% 
                          mutate(roman = 0)
na_df <- rbind(na_chords_roman, na_chords_chords )
na_df %>%  arrange(song, song_parts, roman) %>% View
nrow(beatles[grep('NA', beatles$roman) , ])



#View Scale Degree with Chords
check_roman( x = beatles, song_name ="You Never Give Me Your Money", song_part_name = 'Solo')



#Check individual Songs
which(beatles$song == "You Never Give Me Your Money"& beatles$song_parts ==  'Solo')
i <- 197 #Put Song Index number here
beatles[i ,]$song
beatles[i ,]$song_parts
beatles[i ,]$key
key <- beatles[i ,]$key
chords <- beatles[i ,]$chords
convert_to_roman(chords = beatles[i,]$chords, key = beatles[i,]$key  )

#Check Chords next to numerical anaylsis
rbind(unlist(str_split(beatles[vec ,]$chords, '-')),
      unlist(str_split(convert_to_roman(chords = beatles[vec ,]$chords, key = beatles[i ,]$key  ), '-'))
) %>%  as.list



#### Merge Beatles and Track Info #####

beatles_songs <- read.csv('data/input/beatles_songs.csv', stringsAsFactors = F)

beatles_track_info <- read.csv('data/input/beatles_track_info.csv', stringsAsFactors =  F)

#Standard Track Names to Facilitate with Merge
beatles_track_info <- beatles_track_info %>% 
                        mutate(track_name = str_remove(track_name, ' -.*'),
                               track_name = tolower(track_name),
                               track_name = str_remove_all(track_name,'\\.'),
                               track_name = str_remove(track_name, '\\!'))

beatles_track_info$album_name %>%  unique
beatles_track_info %>%  filter(album_name == 'Meet the Beatles!')


#Missing Songs: Not Available on Spotify API
missing_songs <-beatles_songs %>% 
                    mutate(song_join_code = tolower(song)) %>% 
                    anti_join(beatles_track_info,  by = c('song_join_code'= 'track_name')) 

#Plot Tracks
beatles <- beatles_songs %>% 
              mutate(song_join_code = tolower(song)) %>% 
              left_join(beatles_track_info,  by = c('song_join_code'= 'track_name')
)

#Clean Album Names
beatles <- beatles %>% 
      mutate(album_name = str_remove(album_name, '\\s\\(.*'),
             album_name = str_replace(album_name, '\\sThe Beatles', ' the Beatles'),
             album_name = str_replace(album_name, 'The Beatles', 'White Album')
) 



album_chron_levels <- beatles %>% 
                    group_by(album_name, album_release_date) %>% 
                    count %>% 
                    arrange(album_release_date) %>% 
                    drop_na %>% 
                    pull(album_name) 

beatles <- beatles %>% 
            mutate(phase = if_else(album_release_date <= '1966-06-21' |
                                     year < 1966, 'Early', 'Late'),
                   phase = factor(phase, levels = c('Early', 'Late')),
                    album_name = factor(album_name, levels =album_chron_levels)
)      





#### Process All 60s songs ####


songs_1960s_df$roman <- NA
for(i in 1:nrow(song_df)){
  print(i)
  song_df$roman[i] <- convert_to_roman(chords =song_df$chords[i], key = song_df$key[i])
  
  
}

#### Merge with Hot100 CSV ? #####

hot_100 <- read.csv('data/Hot100.csv')
hot_100$Song <- tolower(hot_100$Song)
missing_songs %>% 
  left_join(hot_100, by = c('song_join_code' = 'Song', 'artist'='Performer')) %>% 
  distinct(artist, song, song_parts, key, bpm, chords, link, song_join_code, 
           WeekID) %>%  View
  




#Check Discrepancy in Artist Names
unique(song_df$artist)



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