get_hkg <- function (url, catalog, g, start_date, end_date) {
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  status <- tryCatch({
    tmps <- tempfile()
    download.file(url, tmps, quiet = T, mode = "wb")
    rawdata <- read_excel(path = tmps, skip = 7, col_names = T)
    unlink(tmps)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata
    for (i in 2:nrow(data)) {
      if(!is.na(data[i, "Year"])) next
      data[i, "Year"] <- data[i - 1, "Year"]
    }
    data <- data %>%
      filter(!is.na(Month) & Month != "*" & Month != "# ?") %>%
      rowwise() %>%
      mutate(date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d")) %>%
      filter(date >= start_date) %>%
      select(date, value = "Total...14") %>%
      arrange(date)
    starti <- which(grepl(pull(data[1, "date"]), tmp$date))
    tmp[starti:(starti + nrow(data) - 1), 2] <- data$value

    return(tmp %>% select(-1))
  }
}