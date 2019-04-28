#Clean Data with Fixed Beatles DataSte
source('source.R')
beatles_clean <- read.csv('data/output/beatles_fix.csv')
View(beatles_clean)

beatles_clean$roman <- NA
for(i in 1:nrow(beatles_clean)){
  print(i)
  beatles_clean$roman[i] <- convert_to_roman(chords =beatles_clean$chords[i], 
                                             key = beatles_clean$key[i])
  
}

View(beatles_clean)