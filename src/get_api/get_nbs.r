get_nbs <- function(url, catalog, g, start_date, end_date) {
  start_year <- substr(start_date, 1, 4)
  url <- str_replace(url, "START_YEAR", start_year)
  filter_url <- strsplit(url, "-FILTER-")[[1]][2]
  url <- strsplit(url, "-FILTER-")[[1]][1]
  
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  # api call
  status <- tryCatch({
    rawdata <- fromJSON(url)
    TRUE },
    error = function(e) {
      FALSE })
  
  # processing api data
  if (status) {
    data <- cbind(rawdata$returndata$datanodes$data,
                  data.frame(matrix(unlist(rawdata$returndata$datanodes$wds), ncol = 4, byrow = T))) %>%
      as_tibble() %>%
      filter(X1 == filter_url) %>%
      rowwise() %>%
      mutate(date = as.Date(paste(substr(X2, 1, 4), substr(X2, 5, 6), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      arrange(date) %>%
      mutate(data = ifelse(hasdata == FALSE, NA, data))
    starti <- which(grepl(pull(data[1, "date"]), tmp$date))
    tmp[starti:(starti + nrow(data) - 1), 2] <- data$data
  }
  return(tmp %>% select(-1))
}

# nbs for series where the data is split in 2 places
get_nbs_double <- function (url1, url2, catalog, g, start_date, end_date) {
  take_non_na <- function (a, b) { if(is.na(a)) {b} else{a} }
  data1 <- get_nbs(url1, catalog, g, start_date, end_date)
  data2 <- get_nbs(url2, catalog, g, start_date, end_date)
  names <- colnames(data1)
  # keep the non-na one
  final <- data1 %>% 
    mutate(z=ifelse(is.na(.[[1]]), data2[,1], .[[1]])) %>%
    select(2)
  colnames(final) <- names
  return(final)
}
