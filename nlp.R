############################################################################
# ------ ------------ ---- --------- NLP --------- ---- ------------ ------ 
############################################################################
#' ---
#' title:   "Natural Language Processing"
#' author:  "Kevin Hitt"
#' date:    "March 10th, 2020"
#' source:  "https://datascienceplus.com/introducing-udpipe-for-easy-natural-language-processing-in-r/"
#' ---

library(dplyr)
library(ggplot2)
library(udpipe)
library(stringr)
library(lattice)

# ABC news dataset from Kaggle: https://www.kaggle.com/therohk/million-headlines
news <- read.csv("abcnews-date-text.csv", header = T, stringsAsFactors = F)
# 1186018 observations of 2 variables

# Basic distribution
news %>% group_by(publish_date) %>% count() %>% arrange(desc(n))

# Split year, month, and date to see distribution of only years
# uses stringr
news$year <- str_sub(news$publish_date,1,4)
news$month <- str_sub(news$publish_date,5,6)
news$day <- str_sub(news$publish_date,7,8)

news %>% 
  group_by(year) %>% 
  count()  %>% 
  ggplot() + 
  geom_bar(aes(year,n), stat ='identity')


# Language Model

# during first time model download execute the below line too
model <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(file = model)

# Filter for easier analysis
news_2008 <- news %>% filter(year == 2008 & month == 10)

# Annotate input text for 2008
s <- udpipe_annotate(udmodel_english, news_2008$headline_text)
x <- data.frame(s)

# Plot part of speech tags
# uses lattice
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "yellow", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

# Find most common words
## NOUNS
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")

# Find most common adjectives
## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "purple", 
         main = "Most occurring adjectives", xlab = "Freq")

# Find most common verbs
## VERBS
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "gold", 
         main = "Most occurring Verbs", xlab = "Freq")

# MACHINE LEARNING - RAKE
## Using RAKE
# Unsupervised algorithms for extracting keywords in Information retrieval. 
# RAKE short for Rapid Automatic Keyword Extraction algorithm, is a domain 
# independent keyword extraction algorithm which tries to determine key 
# phrases in a body of text by analyzing the frequency of word appearance 
# and its co-occurrence with other words in the text.

stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "red", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")

# Find top noun-verb pairs
## Using a sequence of POS tags (noun phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 20), col = "magenta", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")



