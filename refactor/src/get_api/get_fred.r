get_fred <- function (url, catalog, g, start_date, end_date) {
  # which variables are being updated
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  # try api call
  status <- tryCatch({
    rawdata <- fromJSON(url)$observations %>% data.frame
    TRUE },
    error = function(e) {
      FALSE })
  
  if (status) {
    data <- rawdata %>%
      select(date, value) %>%
      mutate(date = as.Date(date, format = "%Y-%d-%m")) %>%
      filter(date >= start_date) %>%
      mutate(value = as.numeric(value))
    starti <- which(grepl(data[1, "date"], tmp$date))
    tmp[starti:(starti + nrow(data) - 1), 2] <- data$value
    
    return(tmp %>% select(-1))
  }
}
