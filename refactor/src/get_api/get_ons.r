get_ons <- function (url, catalog, g, start_date, end_date) {
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
      filter(nchar(date) > 4) %>%
      mutate(date = as.Date(paste(substr(date, 1, 4), substr(date, 6, 8), "01", sep = "-"), format = "%Y-%b-%d")) %>%
      filter(date >= start_date)
    starti <- which(grepl(pull(data[1, "date"]), tmp$date))
    tmp[starti:(starti + nrow(data) - 1), 2] <- data$value
    
    return(tmp %>% select(-1))
  }
}