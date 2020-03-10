############################################################################
# ------ ------------ ---- --- Class Proximity --- ---- ------------ ------ 
############################################################################
#' ---
#' title:   "Class Proximity Testing"
#' author:  "Kevin Hitt"
#' date:    "March 10th, 2020"
#' source:  "Canvas CTPE Reporting"
#' ---

library(dplyr)
library(stringr)

ctpe <- read.csv("Location Usage.csv", header=T, stringsAsFactors=F)
names(ctpe) <- c("id", "title", "starts", "ends", "location", "count")

# Split year, month, and date to see distribution of only years
# uses stringr
ctpe$s_date <- as.Date(str_sub(ctpe$starts,1,10), "%Y-%m-%d")
ctpe$e_date <- as.Date(str_sub(ctpe$ends,1,10), "%Y-%m-%d")

# Find only proximate offerings
ctpe_ended_recently <- ctpe %>% 
  filter(e_date < Sys.Date()) %>%
  arrange(desc(e_date))%>%
  head(10)

ctpe_begin_soon <- ctpe %>% 
  filter(s_date >= Sys.Date())%>%
  arrange(s_date) %>%
  head(10)
