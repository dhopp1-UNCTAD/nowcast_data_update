get_nso_tr <- function (url, catalog, g, start_date, end_date) {
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  status <- tryCatch({
    tmps <- tempfile()
    download.file(url, tmps, quiet = T, mode = "wb")
    rawdata <- read_excel(path = tmps, skip = 5, col_names = T, .name_repair = "unique")
    unlink(tmps)
    TRUE },
    error = function(e) { FALSE })
  if (status) {
    data <- rawdata %>%
      select(1, 2, "Toplam \nTotal...14")
    colnames(data) <- c("year", "quarter", "value")
    for (i in 2:nrow(data)) {
      if(!is.na(data[i, "year"])) next
      data[i, "year"] <- data[i - 1, "year"]
    }
    data <- data %>%
      mutate(quarter = recode(quarter, "I" = "03", "II" = "06", "III" = "09", "IV" = "12")) %>%
      rowwise() %>%
      mutate(date = as.Date(paste(year, quarter, "01", sep = "-"), format = "%Y-%m-%d")) %>%
      drop_na(date)
    starti <- which(grepl(pull(data[1, "date"]), tmp$date))
    tmp[seq(from = starti, to = starti + nrow(data)*3 - 1, by = 3), 2] <- data$value
    
    return(tmp %>% select(-1))
  }
}