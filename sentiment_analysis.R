############################################################################
# ------ ------ --- --------- Sentiment Analysis --------- --- ------ ------ 
############################################################################
#' ---
#' title:   "Sentiment Analysis"
#' author:  "Kevin Hitt"
#' date:    "February 27th, 2020"
#' ---


#The fourth dictionary included with the tidytext package is the nrc dictionary. Let's start our exploration with sentiment counts.


# Load the tidyverse and tidytext packages
library(tidyverse)
library(tidytext)
library(textdata)

# Count the number of words associated with each sentiment in nrc
get_sentiments("nrc") %>% 
  count(sentiment) %>% 
  # Arrange the counts in descending order
  arrange(desc(n))
# A tibble: 10 x 2
#   sentiment        n
#   <chr>        <int>
# 1 negative      3324
# 2 positive      2312
# 3 fear          1476
# 4 anger         1247
# 5 trust         1231
# 6 sadness       1191
# 7 disgust       1058
# 8 anticipation   839
# 9 joy            689
#10 surprise       534


#This dictionary is interesting. It has ten different sentiments, from negative to surprise in descending count order.


#We've seen how visualizations can give us a better idea of patterns in data than counts alone. Let's visualize the sentiments from the nrc dictionary. I've loaded the tidyverse and tidytext packages for you already.

#Extract the nrc dictionary, count the sentiments and reorder them by count to create a new factor column, sentiment2.

#Visualize sentiment_counts using the new sentiment factor column.

#Change the title to "Sentiment Counts in NRC", x-axis to "Sentiment", and y-axis to "Counts".

# Pull in the nrc dictionary, count the sentiments and reorder them by count
sentiment_counts <- get_sentiments("nrc") %>% 
  count(sentiment) %>% 
  mutate(sentiment2 = fct_reorder(sentiment, n))

# Visualize sentiment_counts using the new sentiment factor column
ggplot(sentiment_counts, aes(sentiment2, n)) +
  geom_col() +
  coord_flip() +
  # Change the title to "Sentiment Counts in NRC", x-axis to "Sentiment", and y-axis to "Counts"
  labs(
    title = "Sentiment Counts in NRC",
    x = "Sentiment",
    y = "Counts"
  )