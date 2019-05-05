
#Read in Hot100 data set
#Find Top 30 artists for each decade
#Defined by the number of weeks that she/he had a song in Hot100 in that decade
#The artist gets credit for each song she/he had in a given week.  
#So if in a given week, the artist had 3 songs in the Hot100, she/he would get credit for 3 weeks

source('src/source.R')
options(lubridate.verbose = TRUE)

#Load  in Hot100 csv.  
#This file was obtain from the internet from https://data.world/kcmillersean/billboard-hot-100-1958-2017
#Thank you Sean Miller for your contribution. 
hot100 <- read_csv('data/input/Hot100.csv')

#Extract Decade from Week IDs
hot100$WeekID <- mdy(hot100$WeekID) #Convert WeekID from Character to Date
hot100$Year <- year(hot100$WeekID ) #Extract Year
hot100$Decade <-  hot100$Year %>%  
                        str_sub(start= 1L, end = -2) %>%  
                        str_c('0') #Extract Decade

#Summarize Each Artist by the number of songs 
hot100_ord <- hot100 %>% 
                  group_by(Decade, Performer) %>% 
                  summarize(n = n()) %>% 
                  arrange(.by_group = TRUE, desc(n) ) 

#Split dataframe by decade into a list
hot100_split_list <- hot100_ord %>% 
                    split(f = 'Decade')

#Find the top 30 artists for each decade
hot100_sub <- hot100_split_list %>% 
                    lapply(function(x) slice(x, 1:30)) %>% 
                    bind_rows

#You can uncomment the code below to overwrite the current csv file
#write.csv(x = hot100_sub, file = 'data/input/artist_list.csv', row.names = F)


