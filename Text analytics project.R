#loading the library 
library(tidyverse)
library(tidytext)
library(dsEssex)
#Preparing a draft of the osama project


require(dsEssex)

#aim of the project is to "present and compare word frequencies and sentiment analyses" Dan Ariely- Camille Seaman.
#speaker 1- Dan Ariely -His talks mostly focuses in the social science behaviours analysis. Those are very soft topics as per say very much related
#to behaviour economist and how our brain perceive the cognitive illusion and the mental barrier that we are assigned to ,to be honest it is one of the
#informative talks that i have heard in the quiet few times

#printing the head of the talk
data(ted_talks)

#filtering the data of the my speakers
My_data <- ted_talks %>%
  filter(speaker %in% c("Dan Ariely", "Camille Seaman"))

#loading the library
library(tidytext)
#unnesting the tokens and in order to find our which kind of words are being used by both the speaker and in which frequency according to their topics
tidy_data <- My_data %>%
  unnest_tokens(word,text)
#viewing the dataset
view(tidy_data)
#selecting only the speaker and the word, because these two are relevant and other are irrelevant
tidy_data <- tidy_data %>%
  select(speaker,word,headline)
#viewing the dataset with only speaker and the word
View(tidy_data)

#finding the missing data
length(which(!complete.cases(tidy_data))) #it is important to check if there are any missing vaues in the data frame

#now we need to remove the stop words form the data
#removing the stop words form the dataset
clean_tidy <- tidy_data %>%
  anti_join(get_stopwords()) 

#viewing the table after we get tidy data in the format that all the irrelevant words are out the dataframe 
count_tidy_data <- clean_tidy %>%
  count(word, sort = TRUE)

#viewing the tidy text data 
view(count_tidy_data)


library(ggplot2)
#plotting the graph for the above data and looking at the number of frequency of it
tidy_data %>%
  anti_join(get_stopwords())%>%
  count(word, sort = TRUE)%>%
  slice_max(n, n = 20) %>%
  mutate(word = reorder(word, n)) %>%     # To convert the `word` from character into factor to maintain the order. Otherwise, ggplot2 would plot them in an alphabetic order!
  ggplot(aes(n, word)) + geom_col()    # insert `n` on the x-axis and `word` on the y-axis

#plotting the more advanced and difficult graph in it to find out some more relation between the data
tidy_data %>%
  filter(speaker %in% c("Dan Ariely", "Camille Seaman")) %>%     # keep only the words spoken by Ken Robinson and Hans Rosling
  anti_join(get_stopwords()) %>%                                 # remove stop words
  count(speaker, word) %>%                                       # count with two arguments
  group_by(word) %>%                                             # Group data by word
  filter(sum(n) > 10) %>%                                        # filter by sum of the frequencies within each group (word)
  ungroup() %>%                                                  # Ungroup
  pivot_wider(names_from = "speaker", values_from = "n", values_fill = 0) # convert to wide format: get column names from the speaker variable
  

#now comaring the two spaker and findig out the relational and unrealtional similarities and comparison between them
library(ggrepel)

tidy_data %>%
  filter(speaker %in% c("Dan Ariely", "Camille Seaman")) %>%
  anti_join(get_stopwords()) %>%
  count(speaker, word) %>%
  group_by(word) %>%
  filter(sum(n) > 10) %>%
  ungroup() %>%
  pivot_wider(names_from = "speaker", values_from = "n", values_fill = 0) %>%
  ggplot(aes(`Dan Ariely`, `Camille Seaman`)) +
  geom_abline(color = "red", size = 3.4, alpha = 1, lty = 3) +
  # use the special ggrepel geom for nicer text plotting
  geom_text_repel(aes(label = word), max.overlaps = 15)



#now we are analyzing the sentiments of the speaker
# includes all rows in tidy_tweets and nrc.
speaker_sentiments <- 
    tidy_data %>%
    inner_join(get_sentiments("nrc"), by = "word")  
speaker_sentiments  %>% slice(1:10)

#counting the sentiment of the speaker , form here the visualization of the sentiment analysis starts
sentiments_count <-
  speaker_sentiments %>%
  count(timing, sentiment) %>%
  pivot_wider(names_from = timing, values_from = n, values_fill = 0) %>%
  mutate(OR = compute_OR(After, Before, correction = FALSE)) %>%
  arrange(desc(OR))

sentiments_count
