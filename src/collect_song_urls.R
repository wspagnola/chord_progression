#Main Objective: Get links of songs that are available on Hooktheory.com

'
Input: This script takes artist_list.csv, which is a list of the top 30 artists for each decade that was the output
by the collect_top_30_artists.R script.

Process: Then the scrapes the available songs on Hooktheory for each artist.

Output: The script outputs a csv file with a list of artist, song, decade, and hooktheory links.

Next Process: These links can be used when running the collect_chords.R to scrape the chords for each song available on 
hooktheory.com

'

source('src/source.R')

artist_list <- read.csv('data/input/artist_list.csv')

#Clean artist list to match patterns from Hooktheory.com
artist_list$Performer <- artist_list %>%  
                    pull(Performer) %>% 
                    str_remove_all(pattern = ' With The Jordanaires') %>% 
                    str_remove_all(patter = ' His Twangy Guitar And The Rebels') %>% 
                    str_remove_all(pattern = '\\.') %>% 
                    str_remove_all(pattern = 'the ') %>% 
                    str_remove_all(pattern = 'The ') %>% 
                    str_replace_all(pattern = '!', replacement = 'i') %>% 
                    str_replace_all(pattern = '&', replacement = 'and') %>% 
                    str_replace_all(pattern = 'twenty', replacement = '20') %>% 
                    str_replace_all(pattern = 'Valens', replacement = 'Valen')  #Typo on Hook Theory Site
                  
decade_vec <- unique(artist_list$Decade)
link_list <- list()


for(i in 1:length(decade_vec)){

  #Get Decade from decade_vec
  current_decade_iter <- decade_vec[i]

  #filter artist list for current decade iter
  current_decade_df <-  artist_list %>%  
                            filter(Decade == current_decade_iter)

  #Get artist names for current decade iter
  names(  current_decade_df )[grepl('Performer', names(  current_decade_df ))] <- 'Artist'
  artist_decade <- as.character(current_decade_df$Artist)
  
  print(paste0('Downloading song urls for artists from the ', 
              current_decade_iter, 's...'))
  
  #Create list to store links from current decade _iter
  decade_link_list <- list()
  
  #Create a sequencing vector to guide subsets in for loop
  seq_vec <- c(seq(from = 1, to = length(artist_decade), by = 5),  length(artist_decade)+1)
  loops <- length(seq_vec)-1
  #Get Links in given decade
  for(j in 1:loops){
   
    
    print(paste('Query number', j, 'of',  loops ))
    
    #Extract start start and end points from sequence vector
    start <- seq_vec[j]
    end <- seq_vec[j+1]-1

    decade_link_list[[j]] <- artist_decade[start:end] %>%  
      lapply(extract_song_links) %>%  
      bind_rows
    

  }
  
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
complete_links <- links_df %>% 
                      filter(!is.na(Links)) 
complete_links <- complete_links %>% 
                        select(Decade, Artist, Songs, Links)

#Uncomment the code below to write over csv file
#write.csv(complete_links, file = 'data/complete_links.csv', row.names = F)
