require(tuneR)
require(tidyverse)
require(rtracklayer)
d <- readMidi('Downloads/bach_branconc2F_I_145.mid')
notes <- getMidiNotes(d)

notes %>%  head
notes %>% arrange(track, channel) %>%  summarize(time = max(time)) / 4
list.files('Downloads')

384/4