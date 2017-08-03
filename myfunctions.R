# self-define functions 

# [Task 1]=====================================================
# get 'year', 'month', 'day' from Chinese format-------------------------------
# eg : '2017年4月26日'

get_year_zh <- function(dt_chr){
  
  # loc: 回傳字串的位置
  loc <- gregexpr("\\d{1,4}(年){1,1}", dt_chr)
  
  # start: 位於第幾個
  start <- loc %>% unlist()
  len <- loc[[1]] %>% attributes %>% unlist(., use.names=FALSE)
  
  year <- substr(dt_chr, start=start, stop=(start+len-2) )
  return(year)
}

get_month_zh <- function(dt_chr){
  
  # loc: 回傳字串的位置
  loc <- gregexpr("\\d{1,}(月){1,1}", dt_chr)
  
  # start: 位於第幾個
  start <- loc %>% unlist()
  len <- loc[[1]] %>% attributes %>% unlist(., use.names=FALSE)
  
  month <- substr(dt_chr, start=start, stop=(start+len-2))
  
  return(month)
}

get_day_zh <- function(dt_chr){
  
  # loc: 回傳字串的位置
  loc <- gregexpr("\\d{1,}(日|號){1,1}", dt_chr)
  
  # start: 位於第幾個
  start <- loc %>% unlist()
  len <- loc[[1]] %>% attributes %>% unlist(., use.names=FALSE)
  
  day <- substr(dt_chr, start=start, stop=(start+len-2) )
  return(day)
}


# get 'year', 'month', 'day' from English format -----------------------------------
# eg : 'March 5, 2016'

get_year_en <- function(dt_chr){
  
  # loc: 回傳字串的位置
  loc <- gregexpr("\\d{3,4}", dt_chr)
  
  # start: 位於第幾個
  start <- loc %>% unlist()
  len <- loc[[1]] %>% attributes %>% unlist(., use.names=FALSE)
  
  year <-substr(dt_chr, start=start, stop=(start+len-1) )
  return(year)
}

get_month_en <- function(dt_chr){
  
  # loc: 回傳字串的位置
  loc <- gregexpr("[[:alpha:]]{1,}", dt_chr)
  
  # start: 位於第幾個
  start <- loc %>% unlist()
  len <- loc[[1]] %>% attributes %>% unlist(., use.names=FALSE)
  
  month <- substr(dt_chr, start=start, stop=(start+len-1))
  
  return(month)
}

get_day_en <- function(dt_chr){
  
  # loc: 回傳字串的位置
  loc <- gregexpr("( |,|-){0,3}[1-3]{0,1}[0-9]{1,1}( |,|-){0,3}", dt_chr)
  
  # start: 位於第幾個
  start <- loc %>% unlist()
  len <- loc[[1]] %>% attributes %>% unlist(., use.names=FALSE)
  
  day <- substr(dt_chr, start=start, stop=(start+len-1) )
  
  loc <- gregexpr("[1-3]{0,1}[0-9]{1,1}", day)
  
  # start: 位於第幾個
  start <- loc %>% unlist()
  len <- loc[[1]] %>% attributes %>% unlist(., use.names=FALSE)
  
  day <- substr(day, start=start, stop=(start+len-1) )
  return(day)
}

# transfer enlish month name to number. eg: August -> 8
month_en_to_num <- function(en_month){
  bool_list <- sapply(1:12, function(x) grepl(month.abb[x], en_month))
  which(bool_list)
}

# get 'year', 'month', 'day' from YMD format -----------------------------------
# eg : '2016-3-5', '2016.3.5', '2016,3,5', '2016/3/5'
get_date_YMD <- function(dt_chr){
  fmts <- c("%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y,%m,%d")
  date_YMD <- outer(dt_chr, fmts, as.Date) %>% apply(., 1, na.omit) %>% as.Date(., "1970-01-01") %>% as.character()
  return(date_YMD)
}



# [Task 2]=====================================================

# select noun charater
get_noun = function(x){
  stopifnot(inherits(x,"character"))
  index = names(res) %in% c("n","nr","nr1","nr2","nrj","nrf","ns","nsf","nt","nz","nl","ng","x")
  x[index]
}

# select name charater
get_name = function(x){
  stopifnot(inherits(x,"character"))
  index = names(res) %in% c("nr","nr1","nr2","nrj","nrf","nrt")
  x[index]
}

