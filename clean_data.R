
song_df <- read.csv('Data/songs_60s.csv', stringsAsFactors =  F)

song_df$roman <- NA
for(i in 1:nrow(song_df)){
  print(i)
  song_df$roman[i] <- convert_to_roman(chords =song_df$chords[i], key = song_df$key[i])
  
  
}
View(song_df)
