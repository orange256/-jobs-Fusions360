#===========================================================================================
# output example

# [type] : para
# [txt] : 今天是8月2號
# [raw_date] : 8月2號
# [POSIXt_date] : 08-02     
# [POSIXt_date_NA] : NA-08-02 
#==========================================================================================

library(httr)
library(xml2)
library(rvest)
library(magrittr)
library(jiebaR)
library(text2vec)
library(stringr)
library(dplyr)

source('myfunctions.R')

# [0] Get raw data -------------------------------------------------------------------------

# input wiki url (eg : Donald John Trump)
url <- "https://zh.m.wikipedia.org/zh-hant/%E5%94%90%E7%B4%8D%C2%B7%E5%B7%9D%E6%99%AE"

# get content, and set encoding to UTF-8
doc <- GET(url) %>% content(., encoding="UTF-8")

# set Xpath
xpath_para  <- "//p"
xpath_vcard <- "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'vcard', ' ' ))]"
xpath_ref   <- "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'references', ' ' ))]"

# get text data from wiki
wiki_para  <- xml_find_all(doc, xpath_para)  %>% xml_text()
wiki_vcard <- xml_find_all(doc, xpath_vcard) %>% xml_text()
wiki_ref   <- xml_find_all(doc, xpath_ref)   %>% xml_text() %>% strsplit(., "\n") %>% unlist()

# split text by '\n', and save as dataframe
wiki_para  <- data.frame(type = "para"  , txt = wiki_para) 
wiki_vcard <- data.frame(type = "vcard" , txt = wiki_vcard)
wiki_ref   <- data.frame(type = "ref"   , txt = wiki_ref) 

wiki_raw <- rbind.data.frame(wiki_para, wiki_vcard, wiki_ref)
rm(wiki_para, wiki_vcard, wiki_ref)

# convert factor to charactor
wiki_raw$txt <- wiki_raw$txt %>% as.character()
wiki_raw$txt[4]

# remove nchar==0
wiki_raw <- subset(wiki_raw, nchar(wiki_raw$txt)>0)


# [1] Date extraction (Chinese) -----------------------------------------------------------
# eg : '2017年4月26日'
wiki_dat_zh <- data.frame()

for(k in 1:nrow(wiki_raw)) {

  tmp <- wiki_raw$txt[k]
  
  # 模糊字串格式: XXXX年XX月XX日
  # loc: 回傳字串的位置
  loc <- gregexpr("\\d{1,4}(年|月|日|號){1,1}\\d{0,2}(月|日|號){0,1}\\d{0,2}(日|號){0,1}", tmp)

  # obs: 發現幾個吻合的字串
  obs <- length(loc[[1]])
  
  # start: 位於第幾個
  start <- loc %>% unlist()
  len <- loc[[1]] %>% attributes %>% unlist(., use.names=FALSE)
  
  dates <- sapply(1:obs, function(i) substr(tmp, 
                                            start=start[i], 
                                            stop=(start[i]+len[i]-1) ))
  
  wiki_dat_zh <- data.frame(type = wiki_raw$type[k],
                         txt = wiki_raw$txt[k],
                         raw_date = dates) %>% rbind.data.frame(wiki_dat_zh, . )
}

# date format
wiki_dat_zh$year  <- sapply(1:nrow(wiki_dat_zh),function(j) get_year_zh(wiki_dat_zh$raw_date[j]))  %>% as.integer()
wiki_dat_zh$month <- sapply(1:nrow(wiki_dat_zh),function(j) get_month_zh(wiki_dat_zh$raw_date[j])) %>% as.integer()
wiki_dat_zh$day   <- sapply(1:nrow(wiki_dat_zh),function(j) get_day_zh(wiki_dat_zh$raw_date[j]))   %>% as.integer()


# remove NA
wiki_dat_zh$raw_date <- wiki_dat_zh$raw_date %>% as.character()
wiki_dat_zh <- subset(wiki_dat_zh, nchar(wiki_dat_zh$raw_date)>0)

# remove outlier (month ^[1:12], day ^[1:31])
outlier_idx <- c(which(wiki_dat_zh$month > 12 | wiki_dat_zh$month < 1),
                 which(wiki_dat_zh$day   > 31 | wiki_dat_zh$day   < 1 )) %>% unique()

if(length(outlier_idx) > 0){wiki_dat_zh <- wiki_dat_zh[-outlier_idx, ]}
rm(outlier_idx)

# POSIXt type
wiki_dat_zh$POSIXt_date <- sapply(1:nrow(wiki_dat_zh), function(j) sprintf("%02d-%02d-%02d", wiki_dat_zh$year[j], wiki_dat_zh$month[j], wiki_dat_zh$day[j]) ) %>% gsub('(-NA|NA-)','',.)
wiki_dat_zh$POSIXt_date_NA <- sapply(1:nrow(wiki_dat_zh), function(j) sprintf("%02d-%02d-%02d", wiki_dat_zh$year[j], wiki_dat_zh$month[j], wiki_dat_zh$day[j]) )


# [2] Date extraction (English) -----------------------------------------------------------
# eg : 'March 5, 2016'
wiki_dat_en <- data.frame()

for(k in 1:nrow(wiki_raw)) {
  
  tmp <- wiki_raw$txt[k]
  # 模糊字串格式: XXXX年XX月XX日
  # loc: 回傳字串的位置
  #loc <- gregexpr("(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec){1,1}(/|-|.|,| ){0,3}\\d{0,2}(/|-|.|,| ){0,3}\\d{1,4}", tmp)
  loc <- gregexpr("(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec){1,1}[a-zA-Z]{1,10}.\\d{0,2}.{1,3}\\d{1,4}", tmp)
  
  # obs: 發現幾個吻合的字串
  obs <- length(loc[[1]])
  
  # start: 位於第幾個
  start <- loc %>% unlist()
  len <- loc[[1]] %>% attributes %>% unlist(., use.names=FALSE)
  
  dates <- sapply(1:obs, function(i) substr(tmp, 
                                            start=start[i], 
                                            stop=(start[i]+len[i]-1) ))
  
  wiki_dat_en <- data.frame(type = wiki_raw$type[k],
                         txt = wiki_raw$txt[k],
                         raw_date = dates) %>% rbind.data.frame(wiki_dat_en, . )
}

# remove NA
wiki_dat_en$raw_date <- wiki_dat_en$raw_date %>% as.character()
wiki_dat_en <- subset(wiki_dat_en, nchar(wiki_dat_en$raw_date)>0)


# date format
wiki_dat_en$year  <- sapply(1:nrow(wiki_dat_en),function(j) get_year_en(wiki_dat_en$raw_date[j]))  %>% as.integer()
wiki_dat_en$month <- sapply(1:nrow(wiki_dat_en),function(j) get_month_en(wiki_dat_en$raw_date[j]) %>% month_en_to_num(.) )
wiki_dat_en$day   <- sapply(1:nrow(wiki_dat_en),function(j) get_day_en(wiki_dat_en$raw_date[j])) %>% as.integer()


# remove outlier (month ^[1:12], day ^[1:31])
outlier_idx <- c(which(wiki_dat_en$month > 12 | wiki_dat_en$month < 1),
                 which(wiki_dat_en$day   > 31 | wiki_dat_en$day   < 1 )) %>% unique() 

if(length(outlier_idx) > 0){wiki_dat_en <- wiki_dat_en[-outlier_idx, ]}
rm(outlier_idx)

# POSIXt type
wiki_dat_en$POSIXt_date    <- sapply(1:nrow(wiki_dat_en), function(j) sprintf("%02d-%02d-%02d", wiki_dat_en$year[j], wiki_dat_en$month[j], wiki_dat_en$day[j]) ) %>% gsub('(-NA|NA-)','',.)
wiki_dat_en$POSIXt_date_NA <- sapply(1:nrow(wiki_dat_en), function(j) sprintf("%02d-%02d-%02d", wiki_dat_en$year[j], wiki_dat_en$month[j], wiki_dat_en$day[j]) )


# [3] Date extraction (YMD) -----------------------------------------------------------
# eg : '2016-3-5', '2016.3.5', '2016/3/5'
wiki_dat_YMD <- data.frame()

for(k in 1:nrow(wiki_raw)) {
  
  tmp <- wiki_raw$txt[k]
  # 模糊字串格式: XXXX年XX月XX日
  # loc: 回傳字串的位置
  loc <- gregexpr("\\d{1,4}(`.`|,|-|/){1,2}\\d{1,2}(`.`|,|-|/){1,2}\\d{1,2}", tmp)
  
  # obs: 發現幾個吻合的字串
  obs <- length(loc[[1]])
  
  # start: 位於第幾個
  start <- loc %>% unlist()
  len <- loc[[1]] %>% attributes %>% unlist(., use.names=FALSE)
  
  dates <- sapply(1:obs, function(i) substr(tmp, 
                                            start=start[i], 
                                            stop=(start[i]+len[i]-1) ))
  
  wiki_dat_YMD <- data.frame(type = wiki_raw$type[k],
                            txt = wiki_raw$txt[k],
                            raw_date = dates) %>% rbind.data.frame(wiki_dat_YMD, . )
}

# remove NA
wiki_dat_YMD$raw_date <- wiki_dat_YMD$raw_date %>% as.character()
wiki_dat_YMD <- subset(wiki_dat_YMD, nchar(wiki_dat_YMD$raw_date)>0)


# POSIXt type
wiki_dat_YMD$POSIXt_date <- sapply(1:nrow(wiki_dat_YMD), function(j) get_date_YMD(wiki_dat_YMD$raw_date))
wiki_dat_YMD$POSIXt_date_NA <- sapply(1:nrow(wiki_dat_YMD), function(j) get_date_YMD(wiki_dat_YMD$raw_date))


# [4] output ---------------------------------------------------------------

final_output <- rbind.data.frame(wiki_dat_zh[ ,c(1:3,7:8)], wiki_dat_en[ ,c(1:3,7:8)], wiki_dat_YMD)

