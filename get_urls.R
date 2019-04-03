#Get Links 


source(file = 'source.R')

artist_list <- read.csv('data/artist_list.csv')


artist_list$Performer <- artist_list %>%  
                    pull(Performer) %>% 
                    str_remove_all(pattern = '\\.') %>% 
                    str_remove_all(pattern = 'the ') %>% 
                    str_remove_all(pattern = 'The ') %>% 
                    str_replace_all(pattern = '!', replacement = 'i') %>% 
                    str_replace_all(pattern = '&', replacement = 'and') %>% 
                    str_replace_all(pattern = 'twenty', replacement = '20') %>% 
                    str_replace_all(pattern = 'valens', replacement = 'valen')
decade_vec <- unique(artist_list$Decade)
link_list <- list()

for(i in 1:length(decade_vec)){

  #Get Decade from decade_vec
  current_decade_iter <- decade_vec[i]
  
  #filter artist list for current decade iter
  current_decade_df <-  artist_list %>%  
                        filter(Decade == current_decade_iter)
  
  #Get artist names for current decade iter
  names(artist_list)[grepl('Performer', names(artist_list))] <- 'Artist'
  artist_decade <- as.character(current_decade_df$Artist)
  
  
  print(paste0('Downloading song urls for artists from the ', 
              current_decade_iter, 's...'))
  
  #Create list to store links from current decade _iter
  decade_link_list <- list()
  
  #Create a sequencing vector to guide subsets in for loop
  seq_vec <- c(seq(1, length(artist_decade),5),  length(artist_decade)+1)
  loops <- length(seq_vec)-1
  #Get Links in given decade
  for(i in 1:loops){
   
    
    print(paste('Query number', i, 'of',  loops ))
    
    #Extract start start and end points from sequence vector
    start <- seq_vec[i]
    end <- seq_vec[i+1]-1

    decade_link_list[[i]] <- artist_decade[start:end] %>%  
      lapply(extract_song_links) %>%  
      bind_rows
    

  }
  
  # 
  # links_1 <-artist_decade[1:5] %>%  
  #   lapply(extract_song_links) %>%  
  #   bind_rows
  # links_2 <-artist_decade[6:10] %>%  
  #   lapply(extract_song_links) %>%  
  #   bind_rows
  # links_3 <-artist_decade[11:15] %>%  
  #   lapply(extract_song_links) %>%  
  #   bind_rows
  # links_4 <-artist_decade[16:20] %>%  
  #   lapply(extract_song_links) %>%  
  #   bind_rows
  # links_5 <-artist_decade[21:25] %>%  
  #   lapply(extract_song_links) %>%  
  #   bind_rows
  # links_6 <- artist_decade[25:30] %>%  
  #   lapply(extract_song_links) %>%  
  #   bind_rows
  
  #Combine links into one data.frame and 
  decade_link_df <-   decade_link_list %>% 
                                    bind_rows
  
  #Add col for Decade
  decade_link_df$Decade <-current_decade_iter 
  
  #Store in list
  link_list[[i]] <-  decade_link_df
}

links_df <- bind_rows(link_list)
links_df <- links_df %>%  dplyr::select(Artist, Songs, Decade, Links) 
View(links_df)

#

## I have commented this part out because I am afraid of overwriting it
#write.csv(links_df, file = 'data/links.csv')


#Create CSV of missing links 
#write.csv(links_df[is.na(links_df$Links) , ], file = 'data/missing_links.csv')

#### Notes on Missing Links ####

#1) Eliminate ampersands
#2) Eliminate the
#3) Eliminate .
#4) p!nk -> pink
#5) matchbox twenty -> matchbox 20
#6) daryl hall john oates - > hall and oates
#7) ritchie valens -> ritchie valen (SIC)

#These should work but don't
# ace of base ?
# 3 doors down ?
#electric light orchestra ?
# three dog night ?
# Boyz ii men?


#### Try Again ####
missing_links <- links_df[is.na(links_df$Links) , ] 
missing_links$Artist <- missing_links %>%  
            pull(Artist) %>% 
            str_remove_all(pattern = '\\.') %>% 
            str_remove_all(pattern = 'the ') %>% 
            str_replace_all(pattern = '!', replacement = 'i') %>% 
            str_replace_all(pattern = '&', replacement = 'and') %>% 
            str_replace_all(pattern = 'twenty', replacement = '20') %>% 
            str_replace_all(pattern = 'valens', replacement = 'valen')
missing_links <- missing_links %>%  dplyr::select(Artist, Decade)
missing_links <- missing_links %>%  arrange(Decade)
