install.packages("gutenbergr")

#loading the library in the r for the text analytics
library(gutenbergr)

#loading the mark twain dataset into the R
mark_twain <- gutenberg_download(c(76,74,3176,245))

install.packages("tidytext")
#the data is in very untidy form we need to tidy up the data in order to analyse
library(tidytext)

data("stop_words")

#now we need to start using the diply library for using tokenization
library(dplyr)
tidy_mark_twain <- mark_twain %>%
  unnest_tokens(word,text) %>% #tokenize
  anti_join(stop_words)#removing the unwanted words form the data frame

print(tidy_mark_twain)

#frequency distribution of the words
#visualizing the data 
library(ggplot2)


freq_hist <- tidy_mark_twain %>% count(word,sort = TRUE) %>% filter(n>400) %>% mutate(word = reorder(word,n)) %>% ggplot(aes(word,n))+
  geom_col(fill="lightgreen")+
  xlab(NULL)+
  coord_flip()
  
print(freq_hist)
