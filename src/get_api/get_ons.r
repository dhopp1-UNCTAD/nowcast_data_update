get_ons <- function (url, catalog, g, start_date, end_date) {
  month_key <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  status <- tryCatch({
    tmps <- tempfile()
    download.file(url, tmps, quiet = T, mode = "wb")
    rawdata <- read_excel(path = tmps, skip = 8, col_names = F)
    unlink(tmps)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata
    colnames(data) <- c("date", "value")
    data <- data %>%
      filter(nchar(date) > 4)
    data$date <- sapply(data$date, function(x) as.Date(paste0(substr(x, 1, 4), "-", which(month_key == substr(x, 6, 8)), "-01")))
    data <- data %>% 
      mutate(date = as.Date(date, origin="1970-01-01"))
    
    tmp <- tmp %>% 
      left_join(data, by="date") %>% 
      mutate(is_uk=value) %>% 
      select(-value)
    
    return(tmp %>% select(-1))
  }
}