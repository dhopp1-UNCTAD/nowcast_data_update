get_ecb <- function (url, catalog, g, start_date, end_date) {
  # which variables are being updated
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  status <- tryCatch({
    rawdata <- readSDMX(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata) %>%
      as_tibble() %>%
      mutate(obsTime2 = as.Date(paste(obsTime, "01", sep = "-"), format = "%Y-%m-%d")) %>%
      filter(obsTime2 >= start_date & obsTime2 <= end_date)
    starti <- which(grepl(pull(data[1, "obsTime2"]), tmp$date))
    tmp[starti:(starti + nrow(data) - 1), 2] <- data$obsValue
    
    return (tmp %>% select(-1))
  }
}