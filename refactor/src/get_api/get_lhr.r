get_lhr <- function (url, catalog, g, start_date, end_date) {
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  status <- tryCatch({
    tmp_date <- end_date
    lastdate <- paste0(format(tmp_date, "%b"), "-", format(tmp_date, "%Y"))
    url <- str_replace(url, "LASTDATE", lastdate)
    while(GET(url)$status_code != 200) {
      tmp_date <- tmp_date %m-% months(1)
      old_date <- lastdate
      lastdate <- paste0(format(tmp_date, "%b"), "-", format(tmp_date, "%Y"))
      url <- str_replace(url, old_date, lastdate)
    }
    tmps <- tempfile()
    download.file(url, tmps, quiet = T, mode = "wb")
    rawdata <- read_excel(path = tmps, skip = 2, col_names = T, sheet = 3)
    unlink(tmps)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata %>%
      mutate(date = as.Date(Month, format = "%Y-%m-%d")) %>%
      mutate(International = Total - UK)
    starti <- which(grepl(pull(data[1, "date"]), tmp$date))
    tmp[starti:(starti + nrow(data) - 1), 2] <- data$Total
    tmp[starti:(starti + nrow(data) - 1), 3] <- data$International
    
    return(tmp %>% select(-1))
  }
}