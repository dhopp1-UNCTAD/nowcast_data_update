get_nso_sg <- function (url, catalog, g, start_date, end_date) {
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  status <- tryCatch({
    rawdata <- fromJSON(url)
    TRUE },
    error = function(e) { FALSE })
  if (status) {
    data <- rawdata$Level1d %>%
      as_tibble() %>%
      mutate(date = as.Date(paste(month, "01", sep = "-"), format = "%Y-%m-%d")) %>%
      filter(date >= start_date) %>%
      mutate(value = as.numeric(value)) %>%
      arrange(date)
    starti <- which(grepl(pull(data[1, "date"]), tmp$date))
    tmp[starti:(starti + nrow(data) - 1), 2] <- data$value
    
    return(tmp %>% select(-1))
  }
}