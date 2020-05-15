get_banxico <- function (url, catalog, g, start_date, end_date) {
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  token <- str_split(url, ":")[[1]][2]
  series_name <- str_split(url, ":")[[1]][4]
  setToken(token)
  
  status <- tryCatch({
    rawdata <- getSeriesData("SE5830", start_date, end_date)
    TRUE },
    error = function(e) { FALSE })
  if (status) {
    data <- as.data.frame(rawdata) %>%
      as_tibble()
    colnames(data) <- c("date", "value")
    starti <- which(grepl(pull(data[1, "date"]), tmp$date))
    tmp[starti:(starti + nrow(data) - 1), 2] <- data$value
    
    return(tmp %>% select(-1))
  }
}