library(httr)
library(xml2)
library(rvest)
library(magrittr)
library(jiebaR)
library(text2vec)
source('myfunctions.R')

# [0] Get raw data -------------------------------------------------------------------------

# input wiki url (eg : Donald John Trump)
url <- "https://zh.m.wikipedia.org/zh-hant/%E5%94%90%E7%B4%8D%C2%B7%E5%B7%9D%E6%99%AE"

# get content, and set encoding to UTF-8
doc <- GET(url) %>% content(., encoding="UTF-8")

# xpath setting
xpath <- "//*[@id='bodyContent']"

# get all text
wiki_content  <- xml_find_all(doc, xpath)  %>% xml_text()

wiki_content

# [1] First try ---------------------------------------------------

# do word segment, and label grammar
cutter = worker("tag") 
res <- cutter[wiki_content]

# show some nouns
get_noun(res) %>% head()

# we only need 'names', so we set grammar %in% c("nr","nr1","nr2","nrj","nrf","nrt")
name_table <- get_name(res) %>% table %>% as.data.frame()

# take a look at this name_table, we found that '上周四', '小丘', '小姐', '小鎮'... are not names, so we need to do some revise
name_table %>% head


# [2] Second try (do some revise to get higher accuracy) -----------------------

# you can add new words, or change grammer
new_user_word(cutter,'上周四',"Tg") 
new_user_word(cutter,'小丘',"n") 
new_user_word(cutter,'小姐',"r") 
new_user_word(cutter,'小鎮',"n") 

# NOW, we do it again
res <- cutter[wiki_content]
name_table <- get_name(res) %>% table %>% as.data.frame()

name_table %>% head

