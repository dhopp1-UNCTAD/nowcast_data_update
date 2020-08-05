months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
non_na <- function(x, y) {
  if (!is.na(x)) {
    return (x)
  } else {
    return (y)
  }
}

get_rbi <- function (url, catalog, g, start_date, end_date, historical) {
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  url <- "https://www.rbi.org.in/Scripts/Pr_DataRelease.aspx?SectionID=352"
  regex_match <- "Monthly Data on India.s International Trade in Services"
  status <- tryCatch({
    html <- paste(readLines(url), collapse = "\n")
    matched <- str_match_all(html, "<a target='_blank' href='(.*?)'")[[1]][, 2]   # all links from page
    files <- matched[matched %>% sapply(function(x) str_sub(x, -3) == "PDF")] %>% unique  # keeping only PDFs
    for (url_i in files) {
      pdf <- pdf_text(url_i)
      raw_date <- str_locate_all(pattern = regex_match, pdf[1])[[1]][2]
      raw_date <- substr(pdf[1], raw_date + 1, raw_date + 120)
      raw_date <- trimws(gsub("for the Month of", "", raw_date))
      raw_date <- trimws(gsub("-", "", raw_date))
      t_end <- str_locate(pattern = "\n", raw_date)[[1]][1]
      raw_date <- substr(raw_date, 1, t_end - 1)
      rev_date <- as.Date(paste0(str_split(raw_date, " ")[[1]][2], "-", which(str_split(raw_date, " ")[[1]][1] == months), "-01"))
      raw_date1 <- paste(str_split(raw_date, " ")[[1]][1], str_split(raw_date, " ")[[1]][2], sep = "-")
      raw_date2 <- paste(str_split(raw_date, " ")[[1]][1], str_split(raw_date, " ")[[1]][2], sep = " - ")
      raw_date <- paste(raw_date1, raw_date2, sep = "|")
      location <- str_locate_all(pattern = raw_date, pdf[1])[[1]][2]
      value <- trimws(substr(pdf[1], location + 1, location + 100))
      value <- as.numeric(gsub("[^[:alnum:]]", "", value))  
      tmp[tmp$date == rev_date, 2] <- value
      Sys.sleep(1)
    }
    TRUE },
    error = function(e) {
      FALSE })
  
  data2 <- historical %>% 
    select(year, month, vars$code[1]) %>% 
    mutate(date = as.Date(paste0(year, "-", month, "-01"))) %>% 
    select(-year, -month) %>% 
    select(2, 1)
  if (status) {
    # Historical PDFs available online, but it'd be too slow to get them all. Only one page of files obtained online 
    # Previous data (2019 and before) pasted in "Others" file in NoScrap directory
    column_names <- colnames(tmp)
    tmp <- tmp %>% 
      left_join(data2, by="date")
    
    for (i in 1:nrow(tmp)) {
      tmp[i, 2] <- non_na(tmp[i, 2], tmp[i, 3])
    }
    tmp <- tmp[,c(1,2)]
    colnames(tmp) <- column_names
  } else {
    tmp <- data2
  }
    
  return(tmp %>% select(-1))
}