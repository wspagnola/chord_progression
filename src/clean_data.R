source('src/source.R')


#### Clean Beatles Songs ####
beatles <- read.csv('data/output/beatles_fix.csv', stringsAsFactors = F)


#Help should be A major throughout;  
#the chords match a previous section that was listed as Amaj;
#In addition I have verified the key from several sources
beatles[beatles$song == 'Help' & beatles$song_parts == 'Chorus' , ]$key <- 'Amaj'
  
#Note: Ebb = D, Bbb = A, Fb = E
#May create function later to process other songs 
beatles <- beatles %>% 
                mutate(chords = str_replace_all(chords,'Ebb', 'D'),
                       chords = str_replace_all(chords,'Bbb', 'A'),
                       chords = str_replace_all(chords,'fb', 'A')
) 

#Remove Songs that did not scrape chords properly
beatles <- beatles[!beatles$chords =='' ,]


#### Process Beatles songs ####

#Create new field for roman numerical analysis
#### Convert to Roman Numerical Analysis


#Create field without features (i.e. only distinguish between major and minor chords, not 7ths and other chords)
beatles$roman <- NA
for(i in 1:nrow(beatles)){
  print(i)
  beatles$roman[i] <- convert_to_roman(chords =beatles$chords[i], 
                                       key = beatles$key[i])
}

#Retain All Features
beatles$roman_features <- NA
for(i in 1:nrow(beatles)){
  print(i)
  beatles$roman_features[i] <- extract_features(chords =beatles$chords[i], roman = beatles$roman[i] )
}


#Uncomment this to create rewrite csv file
#write.csv(beatles, 'data/output/beatles_roman.csv', row.names =F)


#### Merge Beatles and Track Info #####

beatles_songs <- read.csv('data/output/beatles_roman.csv', stringsAsFactors = F)
beatles_track_info <- read.csv('data/input/beatles_track_info.csv', stringsAsFactors =  F)

beatles_track_info <- beatles_track_info %>% 
                          mutate(track_name = tolower(track_name),
                                 track_name = str_remove_all(track_name,'\\.'),
                                 track_name = str_remove(track_name, '\\!'))

#Join Hooktheory data (chords and roman numerical analysis) and  Hot100 data (Album/Song Date)
beatles <- beatles_songs %>% 
              mutate(song_join_code = tolower(song),
                     song_join_code = str_remove_all(song_join_code, '\\.')) %>% 
              left_join(beatles_track_info,  by = c('song_join_code'= 'track_name')
) 


#Clean Album Names Column
beatles <- beatles %>% 
      mutate(album_name = str_remove(album_name, '\\s\\(.*'),
             album_name = str_replace(album_name, '\\sThe Beatles', ' the Beatles'),
             album_name = str_replace(album_name, 'The Beatles', 'White Album')
) 


#Create A Vector of Chronological Levels for the Album Name column
album_chron_levels <- beatles %>% 
                    group_by(album_name, album_release_date) %>% 
                    count %>% 
                    arrange(album_release_date) %>% 
                    drop_na %>% 
                    pull(album_name) 

#Add column for phase (early vs. late ) and relevel the albums by chronological order

beatles <- beatles %>% 
  mutate(phase = if_else(album_release_date <= '1966-06-21' |
                           year < 1966, 'Early', 'Late'),
         phase = if_else(year > 1966, 'Late', phase),
         phase = if_else(song == 'Paperback Writer', 'Early', phase),
         phase = factor(phase, levels = c('Early', 'Late')),
         album_name = factor(album_name, levels =album_chron_levels)
)  
# beatles <- beatles %>% 
#             mutate(phase = if_else(album_release_date <= '1966-06-21' |
#                                      year < 1966, 'Early', 'Late'),
#                    phase = factor(phase, levels = c('Early', 'Late')),
#                     album_name = factor(album_name, levels =album_chron_levels)
# )  

#The song 'Real Love' was written by John Lennon after the Beatles broke up.  
#The song is included on Hooktheory as a Beatles song because the surviving members performed it after John's death
#However it doesn't belong to either phase, so we'll remove it from the dataset. 
beatles %>% 
  filter(is.na(phase))

#Remove 'Real Love' from Dataset
beatles <- beatles %>% 
            filter(!is.na(phase))


#Uncomment to rewrite CSV file
#write.csv(beatles, 'data/output/beatles_full.csv', row.names = F)

