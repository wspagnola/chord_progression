
source('source.R')


beatles <- read.csv( 'data/output/beatles_full.csv', stringsAsFactors = F)
beatles %>% 
  filter(album_name ==  'Beatles For Sale') %>% 
   check_roman()

#### Functions

sum_chords <- function(chords){
  
  require(dplyr)
  
  chord_vec <- strsplit(chords, '-') %>%  unlist
  
  
  
  return(length(chord_vec))
}



borrow_chord_pct <- function(chords){
  
  require(dplyr)
  
  chord_vec <- strsplit(chords, '-') %>%  unlist
  pct_borrow <- sum(grepl('/',   chord_vec) ) / length(  chord_vec)
  
  
  
  return(pct_borrow)
}


#### Plot Songs Over Time and by Album # ####
beatles %>%  
  group_by(song,album_name, year) %>% 
  count %>%  
  group_by(album_name, year) %>% 
  count %>% 
  arrange(year)

#Plot Song Availability by Phase
beatles %>% 
  count(phase, song) %>% 
  count(phase) %>% 
  drop_na(phase) %>% 
  ggplot(aes(x = phase, y = nn, fill = phase)) +
  geom_col() +
  scale_fill_manual(values = c('red', 'blue' )) +
  ylab('Number of Songs') +
  ggtitle('Beatles Songs Available on Hook Theory')

#Plot Song Availability by Album
beatles %>% 
  count(album_name, year, song, phase) %>% 
  count(album_name, year, phase) %>% 
  drop_na(album_name) %>% 
  ggplot(aes(x = album_name, y = nn, fill = phase)) +
  geom_col() +
  scale_fill_manual(values = c('red', 'blue' )) +
  ylab('Number of Songs') +
  geom_hline(yintercept = 12, lty = 'dashed', color = 'orange') +
  ggtitle('Beatles Songs Available on Hook Theory by Album') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#### Plot 

beatles$roman <- as.character(beatles$roman)

beatles$borrowed_pct <- beatles$roman %>% 
                      lapply(borrow_chord_pct) %>%  unlist

beatles$num_chords <- beatles$roman %>% 
                              lapply(sum_chords) %>%  unlist


album_chron_levels <- beatles %>% 
  group_by(album_name, album_release_date) %>% 
  count %>% 
  arrange(album_release_date) %>% 
  drop_na %>% 
  pull(album_name) 

beatles <- beatles %>% 
  mutate(album_name = factor(album_name, levels =album_chron_levels)
  )      


beatles %>% 
  mutate(wt_pct_borrowed =  borrowed_pct*num_chords  )%>% 
  drop_na(album_name) %>% 
  group_by(album_name) %>% 
  summarize(pct_borrowed_sum = sum(wt_pct_borrowed) / sum(num_chords)) %>% 
  ggplot(aes(x = album_name, y= pct_borrowed_sum)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


beatles %>% 
  filter(album_name == 'Beatles For Sale') 
