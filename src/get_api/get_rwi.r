get_rwi <- function (url, catalog, g, start_date, end_date) {
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  url_start <- str_split(url, "container")[[1]][1] %>% substr(1, nchar(.)-1)
  webpage <- read_lines(url)
  row <- grep("Aktuelle Daten zum Index", webpage)
  url <- paste0(url_start,
                regmatches(webpage[row], gregexpr('(?<=\\").+?(?=\\")', webpage[row], perl = T))[[1]][1])
  status <- tryCatch({
    tmps <- tempfile()
    download.file(url, tmps, quiet = T, mode = "wb")
    rawdata <- read_excel(path = tmps, skip = 3)
    unlink(tmps)
    TRUE },
    error = function(e) { FALSE })
  if (status) {
    data <- rawdata %>%
      select(1, 3)
    colnames(data) <- c("date", "value")
    data <- data %>%
      drop_na(date, value) %>%
      mutate(date = as.Date(as.integer(date), origin = "1899-12-30")) %>%
      mutate(value = as.numeric(value))
    starti <- which(grepl(pull(data[1, "date"]), tmp$date))
    tmp[starti:(starti + nrow(data) - 1), 2] <- data$value
    
    return(tmp %>% select(-1))
  }
}