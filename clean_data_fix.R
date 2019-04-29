#Clean Data with Fixed Beatles DataSte
source('source.R')
beatles <- read.csv('data/output/beatles_fix.csv')
beatles$roman <- NA
for(i in 1:nrow(beatles)){
  print(i)
  beatles$roman[i] <- convert_to_roman(chords =beatles$chords[i], 
                                             key = beatles$key[i])
  
}

x <- c('A', 'B')
str_detect(x, "B")
str_detect(x, NA)
i <- 99
convert_to_roman(chords =beatles$chords[i], 
                 key = beatles$key[i])

i <- 99
chords =beatles$chords[i]
key = beatles$key[i]


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


#### Check chords

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
check_roman(na_df)[[3]] %>%  View
View(z)

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

#Rescrape Because

