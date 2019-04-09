

#### NOTES ON SPREADING ####

#Convert Chords to characters

#You may need to filter out duplicates and deal with those separate 
#Before spreading


parts <- rep(c('Verse', 'Chorus', 'Bridge'), 27)
songs <-rep(c('I', 'II', 'III', 'IV',
                   'V', 'VI', 'VII', 'VIII', 'IX'), each = 3)
artist <- rep(c('A', 'B', 'C'), each = 9)
id <- 1:length(parts)

chords <- replicate(27,paste(sample(c('a', 'b', 'c', 'd', 'e', 'f'), 4), collapse = '-'))
df <- data.frame(artist, songs,  parts, chords)
df %>%  unite('artist_song', c('artist', 'songs') ) %>% 
  mutate(id = 1:nrow(df)) %>% 
  spread(artist_song, chords)
?replicate

?spread
?separate



artist <- rep(c('A', 'B', 'C'), each = 3)
songs <- c('I', 'II', 'III', 'IV','V', 'VI', 'VII', 'VIII', 'IX')
chords <- replicate(27,paste(sample(c('a', 'b', 'c', 'd', 'e', 'f'), 4), collapse = '-'))
parts <- c('Verse', 'Chorus', 'Bridge')
chord_mat <- matrix(chords, byrow= T, ncol= length(parts))
chord_df <- as.data.frame(chord_mat)
names(chord_df) <- parts 

df_wide <- cbind(data.frame(artist, songs), chord_df)
df_wide <- df_wide %>% 
              mutate(Verse = as.character(Verse),
                     Chorus = as.character(Chorus),
                     Bridge = as.character(Bridge))

df_long <- df_wide %>%  gather(key = part, value =chord, -artist, -songs)
df_long <- df_long %>%  arrange(artist, songs)
df_long  %>%  spread(key = part, value =chord)


dup_songs <- songs_60s %>%  
  count(artist, song, song_parts) %>% 
  filter(n > 1) %>% 
  pull(song) %>% 
  unique %>% 
  as.character

dup_idx <- which(as.character(songs_60s$song) %in%   dup_songs)
View(songs_60s[dup_idx ,])

songs_60s %>% 
  slice(-dup_idx) %>% 
  select(-X, -link) %>%  
  mutate(chords = as.character(chords)) %>% 
  spread(song_parts, chords) %>% 
  View

songs_60s %>%  str
songs_60s[70:75 , ] %>% 
  select(-X, -link) %>%  
  mutate(chords = as.character(chords)) %>% 
  spread(song_parts, chords)







