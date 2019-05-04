#### Set Up Docker ####

#STEPS to Load Dockter in Terminal
###docker pull selenium/standalone-firefox (only need to do this once)
###docker run -d -p 4445:4444 selenium/standalone-firefox

###docker pull selenium/standalone-chrome (only need to do this once)
###docker run -d -p 4445:4444 selenium/standalone-chrome

#LIST DOCKER CONTAINERS: docker container ls
#STOP DOCKER CONTAINER: docker stop test01

#### Import Source File ####

source(file = 'source.R')

#### Set Up Remote Driver ####

eCaps <- list(chromeOptions = list(
  args = list('--user-agent="music_fan"')
))
remDr <- remoteDriver(remoteServerAddr = "localhost",
                      extraCapabilities = eCaps,
                      port = 4445L, 
                      browserName = "chrome")



#### Import song urls ####
links <- read.csv('data/input/complete_links.csv') #Use Pre-made Data File 
baseURL <- 'http://www.hooktheory.com' #Save base URL

#### Scrape Each Decade  ####
# 
#1950s (21 songs)
# sub_links <- links %>%
#               filter(Decade == 1950)
# url_stems <- sub_links %>%
#                   pull(Links) %>%
#                   as.character
# song_urls <- paste0(baseURL, url_stems[!is.na(url_stems)])
# songs_50s <- scrape_hook_theory(song_urls = song_urls, remDr = remDr) #Scrape Songs
# #write.csv(songs_60s, file = 'Data/input/songs_50s.csv')
# 
# # 1960s (167 songs)
sub_links <- links %>%
              filter(Decade == 1960)
sub_links <- sub_links%>%
              slice(-grep('hard-days-night', sub_links$Links)) #Hard's Day Night Link Doesn't Work
url_stems <- sub_links %>%
                  pull(Links) %>%
                  as.character
song_urls <- paste0(baseURL, url_stems[!is.na(url_stems)])
songs_60s <- scrape_hook_theory(song_urls = song_urls, remDr = remDr,
                                artist = sub_links$Artist, 
                                songs = sub_links$Songs, start = 22, end = 22) #Scrape Songs

#write.csv(songs_60s, file = 'Data/input/songs_60s.csv')
# 
# ##1970s (131 songs)
# sub_links <- links %>%
#               filter(Decade == 1970)
# url_stems <- sub_links %>%
#                   pull(Links) %>%
#                   as.character
# song_urls <- paste0(baseURL, url_stems[!is.na(url_stems)])
# songs_70s <- scrape_hook_theory(song_urls = song_urls, 
#                                 remDr = remDr) 
# #write.csv(songs_70s, file = 'data/input/songs_70s.csv')

# #1980s (523 songs,  11h 20m)
sub_links <- links %>%
              filter(Decade == 1980)
url_stems <- sub_links %>%
                  pull(Links) %>%
                  as.character
half_1 <- round(nrow(sub_links) / 2)
song_urls <- paste0(baseURL, url_stems[!is.na(url_stems)])
songs_80s_1 <- scrape_hook_theory(song_urls = song_urls, 
                                  remDr = remDr, 
                                  end = half_1) 
#write.csv(songs_80s_1, file = 'data/input/songs_80s_1.csv')
songs_80s_2 <- scrape_hook_theory(song_urls = song_urls, 
                                  remDr = remDr, 
                                  start= half_1 +1) 
#write.csv(songs_80s_2, file = 'data/input/songs_80s_2.csv)

# 1990s
sub_links <- links %>%
              filter(Decade == 1990)
url_stems <- sub_links %>%
                  pull(Links) %>%
                  as.character
song_urls <- paste0(baseURL, url_stems[!is.na(url_stems)])
songs_90s <- scrape_hook_theory(song_urls = song_urls, 
                                remDr = remDr)
#write.csv(song_90s, file = 'data/input/songs_90s.csv)

# Scrape 2000s (252 Songs)
sub_links <- links %>%
              filter(Decade == 2000)
url_stems <- sub_links %>%
                  pull(Links) %>%
                  as.character
songs_2000s <- scrape_hook_theory(song_urls = song_urls, 
                                remDr = remDr)
#write.csv(song_2000s, file = 'data/input/songs_2000s.csv)

# 2010s (424 songs) (Est time 9h 12m)
sub_links <- links %>%
              filter(Decade == 2010)
url_stems <- sub_links %>%
                  pull(Links) %>%
                  as.character
songs_2000s <- scrape_hook_theory(song_urls = song_urls, 
                                  remDr = remDr)
#write.csv(song_2010s, file = 'data/input/songs_2010s.csv)





songs_1960s_df <- read.csv('Data/input/songs_60s.csv', stringsAsFactors =  F)
View(beatles)


#### Create Beatles Csv #####

beatles_songs <- songs_1960s_df %>% 
  filter(artist == 'Beatles' | artist == 'The Beatles') %>% 
  select(-X)
write.csv(beatles_songs, 'data/output/beatles_songs.csv', row.names = F)



#Remove Remote Driver
rm(remDr)




