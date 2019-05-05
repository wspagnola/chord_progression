'
For some of the reason, songs were swapped after being scraped form hook theory.
This script inputs file beatles_songs.csv and fixes these
The file beatles_songs.csv is an output of the collect_chords. script.
The fix_chords.R script outputs a file called beatles_fix.csv.
The file beatles_fix.csv will be cleaned by running clean_data.R script. 

'

source('src/source.R')


beatles_fix <- read.csv('data/output/beatles_songs.csv', stringsAsFactors = F)


#DELETE: Beatles , Day Tripper =hey-jude
beatles_fix <- beatles_fix %>% 
                    filter((beatles_fix$artist == 'Beatles' &
                              beatles_fix$song == 'Day Tripper') ==F)


#SWAP Links 
beatles_fix[grep('because', beatles_fix$link) ,]$song <- 'Because'
beatles_fix[grep('taxman', beatles_fix$link) ,]$song <- 'Taxman'
beatles_fix[grep('love-me-do', beatles_fix$link) ,]$song <- 'Love Me Do'
beatles_fix[grep('drive-my-car', beatles_fix$link) ,]$song <- 'Drive My Car'
beatles_fix[grep('being-for-the-benefit-of-mr-kite', 
                 beatles_fix$link) ,]$song <- 'Being For The Benefit Of Mr. Kite'
beatles_fix[grep('real-love', beatles_fix$link) ,]$song <- 'Real Love'
beatles_fix[grep('you-cant-do-that', beatles_fix$link) ,]$song <- "You Can't Do That"

# Real Love = Because
# Taxman = Daytripper
# You Can't Do That	= taxman
# Being For The Benefit Of Mr Kite =Love Me Do
# Because = Drive My Car
# Drive My Car = you-cant-do-that
# Love Me Do = real Love



write.csv(beatles_fix, 'data/output/beatles_fix.csv', stringsAsFactors= F)



