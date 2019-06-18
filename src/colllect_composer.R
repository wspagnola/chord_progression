source('source.R')

url <- "http://www.beatlesarchive.net/composer-singer-beatles-songs.html"


composer_table <-url %>% 
                    read_html() %>%
                    html_nodes(xpath='//body//table') %>%
                    html_table %>% 
                    bind_rows %>% 
                    rename('song' = 'Song',
                           'composer' = 'Main composer') %>% 
                    select(song, composer) 

distinct_song_info <- song_info %>%  distinct(song, .keep_all = T)

write.csv(distinct_song_info, 'data/output/beatles_distinct_song_info.csv')
write.csv(composer_table,  'data/output/beatles_composer_info.csv')



left_song <- composer_table %>%  
                anti_join(distinct_song_info)  %>% 
                mutate(year = NA)
right_song <- distinct_song_info %>% left_join(composer_table)  %>% 
                    filter(is.na(composer)) %>% 
                    select(song, composer, year)

non_match <-  rbind(left_song, right_song) %>%  arrange(song)
View(non_match)

#Fix Composer Table
composer_table <- composer_table %>% 
                      mutate(song = str_replace_all(song, "´", "'"),
                             song = str_replace(song, "If I Need Someone", 
                                                          "If I Needed Someone"),
                      song = str_replace(song, "When I'm 64", "When I'm Sixty-Four"),
                      song = str_replace(song, "Only a Nothern Song", "Only a Northern Song"))


distinct_song_info$song_match <- distinct_song_info$song
distinct_song_info$song_match <- distinct_song_info$song_match %>% 
          str_replace("ALL I'VE GOT TO DO", "All I've Got to Do") %>% 
          str_replace("Back In The USSR", "Back in the USSR") %>% 
          str_replace("Everybody's Got Something to Hide", 
                      "Everybody's Got Something to Hide Except Me and My Monkey") %>% 
          str_replace("Hello Goodbye", "Hello, Goodbye") %>% 
          str_replace("Fixing A Hole", "Fixing a Hole") %>% 
          str_replace("Being For The Benefit Of Mr. Kite", "Being For the Benefit of Mr. Kite") %>% 
          str_replace("From Me To You", "From Me to You") %>% 
          str_replace("Help", "Help!") %>% 
          str_replace("I Want To Hold Your Hand", "I Want to Hold Your Hand") %>% 
          str_replace("Norwegian Wood", "Norwegian Wood (This Bird Has Flown)") %>% 
          str_replace("Oh Darling", "Oh! Darling") %>% 
          str_replace("Sgt Pepper's Lonely Hearts Club Band", 
                      "Sgt. Pepper's Lonely Hearts Club Band") %>% 
          str_replace("You've Got To Hide Your Love Away",
                      "You've Got to Hide Your Love Away") %>% 
          str_replace("With A Little Help! From My Friends",
                      "With a Little Help From My Friends") %>% 
          str_replace("Here There And Everywhere",
                          "Here, There and Everywhere") %>% 
          str_replace("I Don't Want To Spoil The Party", 
                          "I Don't Want to Spoil the Party") %>% 
          str_replace("Ob-La-Di Ob-La-Da", "Ob-la-di, Ob-la-da") %>% 
          str_replace("With A Little Help! From My Friends",
                          "With a Little Help From My Friends") %>% 
          str_replace("She Came in through the Bathroom Window", 
                        "She Came in Through the Bathroom Window") %>% 
          str_replace("I'm Happy Just To Dance With You", "I'm Happy Just to Dance With You") %>% 
          str_replace("I've Just Seen A Face", "I've Just Seen a Face") %>% 
          str_replace("Twist And Shout", "Twist and Shout") %>% 
          str_replace("Two Of Us", "Two of Us")

#Check Again 
left_song <- composer_table %>%  
  anti_join(distinct_song_info, c('song' = 'song_match'))  %>% 
  mutate(year = NA) %>%  
        rename('song_match' = 'song')
right_song <- distinct_song_info %>% 
                left_join(composer_table, by = c('song_match' = 'song'))  %>% 
  filter(is.na(composer)) %>% 
  select(song_match, composer, year)

non_match <-  rbind(left_song, right_song) %>%  arrange(song_match)
View(non_match)


full_distinct_song_info  <- distinct_song_info %>%  
                                distinct(song, .keep_all = T) %>% 
                                left_join(composer_table, by = c('song_match'= 'song')) %>% 
                                select(-song_match)

#Beatles COmposers
beatles_members <- c("Lennon/McCartney", "McCartney", "Lennon", "Starr", "Harrison",
                     "Lennon/McCartney/Harrison/Starr")

full_distinct_song_info <- full_distinct_song_info %>% 
                                mutate(composer = if_else(composer %in% beatles_members,
                                                          composer, 'Cover'))
full_distinct_song_info %>%  
  count(composer)

sum(is.na(full_distinct_song_info$composer))
View(full_distinct_song_info)
  

#Note: Remove 'Elenor Rigby'
#Note: Duplicate Michelle
#Note: Duplicate	Help!
non_match %>%  View
composer_table %>% 
    arrange(song) %>% 
    View
composer_table$song