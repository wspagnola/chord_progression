sub_links <- links %>%
                  filter(Artist %in% c('the beatles', 'beatles', 'Beatles', 'The Beatles'))
sub_links$Songs <- as.character(sub_links$Songs)
sub_links$Artist <- as.character(sub_links$Artist)

sub_links <- sub_links%>%
                  slice(-grep('hard-days-night', sub_links$Links)) #Hard's Day Night Link Doesn't Work
url_stems <- sub_links %>%
  pull(Links) %>%
  as.character
song_urls <- paste0(baseURL, url_stems[!is.na(url_stems)])



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


#Rescrape: 
##Dear Prudence
##	I'm Looking Through You
## 	Nowhere Man
##Ticket to Ride
## When I'm 64
##
scrape_idx <- which(sub_links$Songs %in% c('Dear Prudence', 
                             "I'm Looking Through You",
                             "Nowhere Man",
                             "Ticket to Ride",
                             "When I'm Sixty-Four",
                             "We Can Work It Out"))

unique_ids <- song_urls[scrape_idx] %>% 
                   match(unique(song_urls[scrape_idx]) ) %>% 
                    unique

scrape_idx <- scrape_idx[unique_ids]
sub_links[scrape_idx ,]$Songs
song_urls[scrape_idx]

df <- data.frame(Artist = sub_links[scrape_idx ,]$Artist,
                 Songs = sub_links[scrape_idx ,]$Songs,
                 Links = song_urls[scrape_idx] )
fix_df <- scrape_hook_theory(song_urls = df$Links, 
                   artist = df$Artist,
                   songs = df$Songs,
                   remDr = remDr)
#Remove Songs from main df
beatles_remove <- beatles_fix %>%  
  anti_join(fix_df, by = c('artist', 'song', 'song_parts')) 

beatles_full <- beatles_remove %>% 
                      rbind(fix_df, by = c('artist', 'song', 'song_parts'))


beatles_full %>% View
beatles_full %>% 
  mutate(link =  str_remove(link, 'http://www.hooktheory.com/theorytab/view/') ) %>% 
  select(song, link) %>%  View
beatles_full <- beatles_full[-grep('song', beatles_full$song) ,]
nrow(beatles_full)
#write.csv(beatles_full, 'data/output/beatles_fix.csv', row.names =F)
