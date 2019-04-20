#### Process All 60s songs ####


song_df <- read.csv('Data/songs_60s.csv', stringsAsFactors =  F)

song_df$roman <- NA
for(i in 1:nrow(song_df)){
  print(i)
  song_df$roman[i] <- convert_to_roman(chords =song_df$chords[i], key = song_df$key[i])
  
  
}

#Check Discrepancy in Artist Names
unique(song_df$artist)
  
  
  
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