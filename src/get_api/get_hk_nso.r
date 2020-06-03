get_hk_nso <- function (url, catalog, g, start_date, end_date, historical) {
  httr::set_config(config(ssl_verifypeer = 0L))
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  historical_col <- vars %>% select(code) %>% pull
  
  # api call
  status <- tryCatch({
    which_date <- end_date
    try_url <- str_replace(url, "LAST_DATE", substr(which_date, 1, 7) %>% str_replace("-", ""))
    rawdata <- GET(try_url) %>% content %>% fromJSON
    while(rawdata$header$count$noOfRecords == 0) {
      try_url <- str_replace(url, "LAST_DATE", substr(which_date, 1, 7) %>% str_replace("-", ""))
      rawdata <- GET(try_url) %>% content %>% fromJSON
      which_date <- which_date - months(1)
    }
  TRUE }, error = function(e) {FALSE})
  
  if (status) {
    data1 <- rawdata$dataSet %>%
      as_tibble() %>%
      mutate(date = as.Date(paste(substr(period, 1, 4), substr(period, 5, 6), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      mutate(figure = as.numeric(figure) / 1000)
    # Online data only available from 2012. Previous data pasted in "historical.csv" file in output/ directory
    data2 <- historical %>% select(year, month, historical_col) %>%
      mutate(date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%m-%d")) %>%
      select(date, historical_col)
    starti <- which(pull(data2[1, "date"]) == tmp$date)
    tmp[starti:(starti + nrow(data2) - 1), 2] <- data2[,historical_col]
    starti <- which(grepl(pull(data1[1, "date"]), tmp$date))
    tmp[starti:(starti + nrow(data1) - 1), 2] <- data1$figure
    
    return(tmp %>% select(-1))
  }
}
