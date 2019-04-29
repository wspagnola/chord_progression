#Clean Data with Fixed Beatles DataSte
source('source.R')
beatles <- read.csv('data/output/beatles_fix.csv')
View(beatles)

beatles$roman <- NA
for(i in 1:nrow(beatles)){
  print(i)
  beatles$roman[i] <- convert_to_roman(chords =beatles$chords[i], 
                                             key = beatles$key[i])
  
}

View(beatles_clean)
#Note
## Doesn't work
#### Drive My Car
#### The End
#### We can work it out (Bridge)

#Rescrape Because

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
na_df %>%  arrange(song, song_parts, roman) %>% View
