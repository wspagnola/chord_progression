source('source.R')

hot100 <- read_csv('data/Hot100.csv')
options(lubridate.verbose = TRUE)

hot100$WeekID <- mdy(hot100$WeekID) 
hot100$Year <- year(hot100$WeekID )

hot100$Decade <-  hot100$Year %>%  
                      str_sub(start= -2, end = -2) %>%  
                      str_c('0s')

names(hot100)
hot100_ord <- hot100 %>% 
                  group_by(Decade, Performer) %>% 
                  summarize(n = n()) %>% 
                  arrange(.by_group = TRUE, desc(n) ) 
hot100_split_list <- hot100_ord %>% 
                    split(f = 'Decade')

hot100_sub <- hot100_split_list %>% 
                    lapply(function(x) slice(x, 1:30)) %>% 
                    bind_rows
