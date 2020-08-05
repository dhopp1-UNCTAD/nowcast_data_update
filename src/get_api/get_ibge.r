get_ibge <- function (url, catalog, g, start_date, end_date) {
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  if (vars$code[1] == "is_br") {
    sheet_num <- 2
    first_date <- as.Date("2011-01-01")
  } else {
    sheet_num <- 1
    first_date <- as.Date("2002-01-01")
  }
  
  status <- tryCatch({
    tmps <- tempfile()
    download.file(url, tmps, quiet = T, mode = "wb")
    rawdata <- read_excel(path = tmps, skip = 2, col_names = T, sheet = sheet_num)
    unlink(tmps)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata %>%
      slice(1, 3) %>%
      t(.) %>%
      as_tibble(.) %>%
      slice(-1)
    colnames(data) <- c("date", "value")
    data <- data %>%
      mutate(date = seq.Date(from = first_date, by = "month", length.out = nrow(.))) %>%
      mutate(value = as.numeric(value))
    starti <- which(grepl(pull(data[1, "date"]), tmp$date))
    tmp[starti:(starti + nrow(data) - 1), 2] <- data$value
    
    return(tmp %>% select(-1))
  }
}