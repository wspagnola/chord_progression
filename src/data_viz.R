#DATA VISUALIZATION

#This is a script for data visualization, processing, and exploratory data analysis. 
#It takes the clean file beatles_full, which is the output of clean_data.R.
#It produces graphs and table.
#There is also some data_processing; namely, objects are created with columns for bigrams and trigrams.
#Most of these steps are reproduced in RMD Final Presentation file for reproducibility purposes. 

source('src/source.R')
beatles <- read.csv( 'data/output/beatles_full.csv', stringsAsFactors = F)

#Relevel album_name factor to be in chronological order
album_chron_levels <- beatles %>% 
  group_by(album_name, album_release_date) %>% 
  count %>% 
  arrange(album_release_date) %>% 
  drop_na %>% 
  pull(album_name) 
beatles <- beatles %>% 
  mutate(album_name = factor(album_name, levels =album_chron_levels)
)



#### Plot Chords Per song ####

#Songs with Most Chords
beatles %>% 
  group_by(song, phase) %>% 
  summarize(num_chords = sum(sum_chords_unique(chords))) %>% 
  arrange(desc(num_chords)) %>% 
  ungroup %>% 
  slice(1:20) %>% 
  ggplot(aes(x = reorder(song, num_chords), y = num_chords, fill = phase)) +
  geom_col() +
  scale_y_continuous(limits = c(0,25), expand = c(0,0)) +
  ggtitle('Top 20 Beatles Songs by Number of Chords') +
  xlab('Songs') +
  ylab('Number of Chords') +
  my_theme_tilt





#Songs with Least Unique Chords
beatles %>%
    group_by(song, phase) %>% 
    summarize(num_chords = sum(sum_chords_unique(chords))) %>% 
    filter(num_chords > 0) %>% 
    arrange(num_chords) %>% 
    ungroup %>% 
    slice(1:20)%>% 
    ggplot(aes(x = reorder(song, num_chords), y = num_chords, fill = phase)) +
    geom_col() +
    my_theme_tilt

beatles %>%
  group_by(song, phase) %>% 
  summarize(num_chords = sum(sum_chords_unique(chords))) %>% 
  filter(num_chords > 0) %>% 
  arrange(num_chords) %>% 
  ggplot(aes(x = reorder(song, num_chords), y = num_chords, fill = phase)) +
  geom_col()
  
## Number of Unique Chords by Year (Scatter Plot )
beatles %>%
  group_by(song, year, phase) %>% 
  summarize(num_chords = sum(sum_chords_unique(chords))) %>% 
  filter(num_chords > 0) %>% 
  arrange(num_chords) %>% 
  ggplot() +
  geom_smooth(aes(x = year, y = num_chords))+
  geom_point(aes(x = year, y = num_chords, color= phase)) 


## Number of Unique Chords by Year (Scatter Plot )
beatles %>%
  group_by(song, year, phase) %>% 
  summarize(num_chords = sum(sum_chords_unique(chords))) %>% 
  filter(num_chords > 0) %>% 
  arrange(num_chords) %>% 
  ggplot() +
  geom_smooth(aes(x = year, y = num_chords))+
  geom_point(aes(x = year, y = num_chords, color= phase)) 


#### Mean Number of Unique Chords Per Song by Year (Grouped by Phase)'
beatles %>%
  group_by(song, year, phase) %>% 
  summarize(num_chords = sum_chords_unique(chords)) %>% 
  group_by(year, phase) %>% 
  summarize(mean_num_chords = mean(num_chords)) %>% 
  ggplot(aes(x = year, y = mean_num_chords, color = phase, group = phase)) +
  geom_line() +
  geom_point(aes(group = phase, color = phase)) +
  geom_vline(xintercept = 1966, lty = 'dashed', color = 'blue') +
  scale_x_discrete( limits =  1963:1970, breaks = 1963:1970) +
  scale_y_continuous(limits = c(0, 12), breaks = 0:12) +
  ggtitle('Mean Number of Unique Chords Per Song by Year (Grouped by Phase)') +
  xlab('Year') +
  ylab('Mean Chord Number') +
  my_theme

beatles %>%
  group_by(song, album_name, phase) %>% 
  summarize(num_chords = sum_chords_unique(chords)) %>% 
  group_by(album_name, phase) %>% 
  summarize(mean_num_chords = mean(num_chords)) %>% 
  drop_na(album_name) %>% 
  ggplot(aes(x = album_name, y = mean_num_chords, fill = phase)) +
  geom_col() +
  my_theme_tilt

  geom_line()





#### Plot Songs Over Time and by Album # ####
beatles %>%  
  group_by(song,album_name, year) %>% 
  count %>%  
  group_by(album_name, year) %>% 
  count %>% 
  arrange(year)

#### Plot Song Availability by Phase####
beatles %>% 
  count(phase, song) %>% 
  count(phase) %>% 
  drop_na(phase) %>% 
  ggplot(aes(x = phase, y = nn, fill = phase)) +
  geom_col() +
  scale_fill_manual(values = c('red', 'blue' )) +
  ylab('Number of Songs') +
  ggtitle('Beatles Songs Available on Hook Theory') +
  my_theme  

####Plot Song Availability by Album####
beatles %>% 
  count(album_name, year, song, phase) %>% 
  count(album_name, year, phase) %>% 
  drop_na(album_name) %>% 
  ggplot(aes(x = album_name, y = nn, fill = phase)) +
  geom_col() +
  scale_fill_manual(values = c('red', 'blue' )) +
  scale_y_continuous(expand = c(0,0), limits = c(0,20)) +
  ylab('Number of Songs') +
  geom_hline(yintercept = 12, lty = 'dashed', color = 'orange') +
  ggtitle('Beatles Songs Available on Hook Theory by Album') +
  my_theme



#### Plot Percent Borrowed Chords ####

beatles$roman <- as.character(beatles$roman)
beatles$borrowed_pct <- beatles$roman %>% 
                      lapply(borrow_chord_pct) %>%  unlist
beatles$num_chords <- beatles$roman %>% 
                              lapply(sum_chords) %>%  unlist




#### Plot Percent Borrowed Chords ####
pct_borrowed_chord_tab <-beatles %>% 
                              mutate(wt_pct_borrowed =  borrowed_pct*num_chords  )%>% 
                              drop_na(album_name) %>% 
                              group_by(album_name, phase) %>% 
                              summarize(pct_borrowed_sum = sum(wt_pct_borrowed) / sum(num_chords))

#Plot by album
pct_borrowed_chord_tab %>% 
  ggplot(aes(x = album_name, y= pct_borrowed_sum, fill = phase)) + 
  geom_col() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.5), breaks = seq(0, 0.4, .1))+
  my_theme_tilt


#This graph is pretty ugly
beatles %>% 
  mutate(wt_pct_borrowed =  borrowed_pct*num_chords  )%>% 
  group_by(year, phase) %>% 
  summarize(pct_borrowed_sum = sum(wt_pct_borrowed) / sum(num_chords)) %>% 
  #mutate(wt_pct_borrowed =  borrowed_pct*num_chords  )%>% 
  ggplot() +
  geom_line(aes(x = year, y= pct_borrowed_sum))


#Plot by year ()
# beatles %>% 
#   mutate(wt_pct_borrowed =  borrowed_pct*num_chords  )%>% 
#   drop_na(album_name) %>% 
#   group_by(year, song, phase) %>% 
#   summarize(pct_borrowed_sum = sum(wt_pct_borrowed) / sum(num_chords)) %>% 
#   ggplot() + 
#   geom_point(aes(x = year, y= pct_borrowed_sum, color = phase)) +
#   geom_smooth(method = 'loess', aes(x = year, y= pct_borrowed_sum)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Major 7th Chords ####
beatles$major_7_pct <- beatles$chords%>% 
  lapply(function(x)  chord_pct(x,pattern = 'maj7')) %>%  unlist

#Pct. Songs with Major Chords By Year
beatles %>% 
  mutate(num_maj_7 = major_7_pct*num_chords) %>% 
  group_by(phase, year, song ) %>% 
  summarize(maj_7 =  if_else(sum(num_maj_7 ) > 0, 1, 0) ) %>% 
  drop_na(maj_7) %>% 
  drop_na(year) %>% 
  group_by(year, phase) %>%  
  summarize(pct_songs_with_maj_7 = sum(maj_7) / n()) %>%
  ggplot(aes(x = year, y = pct_songs_with_maj_7, group = phase, color = phase)) +
  geom_line() +
  geom_point(pch = 'o')

#Pct. Songs with Major Chords By Album
beatles %>% 
  mutate(num_maj_7 = major_7_pct*num_chords) %>% 
  group_by(phase, album_name, song ) %>% 
  summarize(maj_7 =  if_else(sum(num_maj_7 ) > 0, 1, 0) ) %>% 
  drop_na(maj_7, album_name) %>% 
  group_by(album_name, phase) %>%  
  summarize(pct_songs_with_maj_7 = sum(maj_7) / n()) %>%
  ggplot(aes(x = album_name, y = pct_songs_with_maj_7, fill = phase)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### DTM #####

library(tm)
library(tidytext)
library(tidyverse)



beatles <- read.csv( 'data/output/beatles_full.csv', stringsAsFactors = F)


album_chron_levels <- beatles %>% 
  group_by(album_name, album_release_date) %>% 
  count %>% 
  arrange(album_release_date) %>% 
  drop_na %>% 
  pull(album_name) 

beatles <- beatles %>% 
  mutate(album_name = factor(album_name, levels =album_chron_levels),
         song = as.character(song),
         artist = as.character(artist)
)

l <- beatles %>%  
  split(f = beatles$song) %>% 
  lapply(function(x) data.frame(artist = unique(x$artist), 
                                song = unique(x$song), 
                                roman = paste(x$roman, collapse = '-')))
df <- l %>% 
  bind_rows 

bigrams <- df %>%  
            mutate(roman = str_replace_all(roman, '-', ' ')) %>% 
            mutate(roman = str_remove_all(roman, '\\(')) %>% 
            mutate(roman = str_remove_all(roman, '\\)')) %>% 
            mutate(roman = str_replace_all(roman, '/', 'BORROWED')) %>% 
            unnest_tokens(bigram, roman, token = 'ngrams', n = 2) %>%  
            count(song, bigram) %>% 
            mutate(bigram = str_replace_all(bigram, 'borrowed', '/')) %>% 
            select(song, bigram, n) 


#Remove bigrams with same chords
bigrams <- bigrams %>% 
              separate(bigram, c('chord_1', 'chord_2'), sep = " ") %>% 
              filter(chord_1 != chord_2) %>% 
              unite(bigram, chord_1, chord_2, sep = " ")

top_50_bigrams <- bigrams  %>%
                      count(bigram, sort = T) %>%
                      slice(1:50) %>%
                      select(bigram)
            
dtm <- bigrams %>% 
      inner_join(top_50_bigrams) %>% 
      cast_dtm(document = song, term = bigram,  value = n)
            

dtm_tibble <-  cbind(dtm$dimnames$Docs, as.matrix(dtm)) %>% as.tibble()

song_info <-beatles %>% 
              select(song, album_name, year, phase)

names(dtm_tibble)[grep('V1', names(dtm_tibble))] <- 'song'
dtm_song_info <- dtm_tibble %>% 
                    left_join(song_info)
mean_bigrams <- dtm_song_info %>% 
                    mutate_if((names(dtm_song_info) %in% 
                                   c("song","album_name","year", "phase"))== F,  as.numeric) %>% 
                    group_by(year) %>% 
                    summarize_if(names(dtm_song_info)[-grep('year', names(dtm_song_info))] %in%
                                   c("song","album_name", "phase")== F, mean) 

View(mean_bigrams)

mean_bigrams[1:10 ,]  %>% 
  gather(bigrams, value = mean, -year) %>% 
  drop_na(year) %>% 
  ggplot(aes(x = year, y = mean, group = bigrams,
             color = bigrams))  +
  geom_line()

mean_bigrams[c(1, 11:20)]  %>% 
  gather(bigrams, value = mean, -year) %>% 
  drop_na(year) %>% 
  ggplot(aes(x = year, y = mean, group = bigrams,
             color = bigrams))  +
  geom_line()

mean_bigrams[c(1, 21:30)]  %>% 
  gather(bigrams, value = mean, -year) %>% 
  drop_na(year) %>% 
  ggplot(aes(x = year, y = mean, group = bigrams,
             color = bigrams))  +
  geom_line()



mean_bigrams[c(1, 31:40)]  %>% 
  gather(bigrams, value = mean, -year) %>% 
  drop_na(year) %>% 
  ggplot(aes(x = year, y = mean, group = bigrams,
             color = bigrams))  +
  geom_line()



mean_bigrams[c(1, 41:50)]  %>% 
  gather(bigrams, value = mean, -year) %>% 
  drop_na(year) %>% 
  ggplot(aes(x = year, y = mean, group = bigrams,
             color = bigrams))  +
  geom_line()


mean_bigrams[c(1, 51:60)]  %>% 
  gather(bigrams, value = mean, -year) %>% 
  drop_na(year) %>% 
  ggplot(aes(x = year, y = mean, group = bigrams,
             color = bigrams))  +
  geom_line()


              
names(dtm_song_info)

View(dtm_song_info)
?summarize_if
