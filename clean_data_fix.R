#Clean Data with Fixed Beatles DataSte
source('source.R')
beatles <- read.csv('data/output/beatles_fix.csv')
beatles$roman <- NA
for(i in 1:nrow(beatles)){
  print(i)
  beatles$roman[i] <- convert_to_roman(chords =beatles$chords[i], 
                                             key = beatles$key[i])
  
}
# View(beatles)
# 
i <- 128
beatles$song[i]

convert_to_roman(chords =beatles$chords[i],
                 key = beatles$key[i])

i <- 105
chords =beatles$chords[i]
key = beatles$key[i]
beatles$song[i]

####  Check NA chords ####

#Check number of song parts with missing chords

nrow(beatles[grep('NA', beatles$roman) , ])

#Check Roman Numerical Analysis on top of Chords 
na_chords <- beatles[grep('NA', beatles$roman) , ]
na_chords_roman <- na_chords %>%  
  select(song, song_parts, key, roman) %>% 
  rename(chords = roman) %>% 
  mutate(roman = 1)
na_chords_chords <- na_chords %>%  
  select(song, song_parts, key, chords) %>% 
  mutate(roman = 0)
na_df <- rbind(na_chords_roman, na_chords_chords )
#na_df %>%  arrange(song, song_parts, roman) %>% View


#### Check chords
check_roman(na_df)[[6]] %>%  View

check_roman <- function(x){
  
  require(dplyr)
  require(stringr)
  x <- na_df

  #Split X into dataframe: one with roman, one with chords
  roman <- x %>%  
    select(song, song_parts, key, roman) %>% 
    rename(chords = roman) %>% 
    mutate(roman = 1)
  chords  <- x %>%  
     select(song, song_parts, key, chords) %>% 
      mutate(roman = 0)
  
  #Bind dataframes together and rearrange
  df <- rbind(roman, chords) %>% 
              as.data.frame %>% 
              arrange(song, song_parts, roman)
  
  #Split dataframes
  list_df <- split(df,f = list(df$song, df$song_parts), drop =T)
  
  z <- list()
  for(i in 1:length(list_df)){

    x <- str_split(list_df[[i]][1 , 'chords'], '-') %>%  unlist
    y <- str_split(list_df[[i]][2 , 'chords'], '-') %>%  unlist
    w <- rbind(x,y)
    w <- as.data.frame(w)
    song_info <- list_df[[i]][1:2 , 1:3]
    row.names(song_info) <- NULL
    z[[i]] <- cbind(song_info, w)
    
    
  }
 
  return(z)
  
}
class(na_df)
View(z)



#### 

write.csv(beatles, 'data/output/beatles_roman_analysis.csv', row.names =F)



#### Merge Beatles and Track Info #####
beatles_songs <- read.csv('data/output/beatles_roman_analysis.csv', stringsAsFactors = F)


beatles_track_info <- read.csv('data/input/beatles_track_info.csv', stringsAsFactors =  F)

#Standard Track Names to Facilitate with Merge
beatles_track_info <- beatles_track_info %>% 
  mutate(track_name = str_remove(track_name, ' -.*'),
         track_name = tolower(track_name),
         track_name = str_remove_all(track_name,'\\.'),
         track_name = str_remove(track_name, '\\!'))


#Missing Songs: Not Available on Spotify API
missing_songs <-beatles_songs %>% 
  mutate(song_join_code = tolower(song)) %>% 
  anti_join(beatles_track_info,  by = c('song_join_code'= 'track_name')) 

#Merge
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

write.csv(beatles, 'data/output/beatles_full.csv', row.names = F)



#### Notes ####

#1) Fix 'Beatles' to 'The Beatles'
#2) Differentiate between Verses (Verse and Pre-chorus?)
#3) Something has a duplicate verse
#4) Deal with sus4/sus2 chords?
#5) cxo chord in Because


## Doesn't work
#### Drive My Car
#### The End
#### We can work it out (Bridge)



### Fix Data

#Good Night (chorus): change Csus#4  to Csus4
# Help (Chorus): change key from Bmin to  Amaj
#Michelle (Chorus): change dmb5 to dm7b5 
###OR recode so that dmb5 becomes equivalent

#the-continuing-story-of-bungalow-bill (bor) scale?


#Rescrape Because

