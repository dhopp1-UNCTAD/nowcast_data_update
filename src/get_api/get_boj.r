get_boj <- function (url, catalog, g, start_date, end_date) {
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  status <- tryCatch({
    tmps <- tempfile()
    download.file(url, tmps, quiet = T)
    rawdata <- read.csv(unzip(tmps), header = F, stringsAsFactors = F)
    unlink(tmps)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    file.remove("bp_m_en.csv")
    rawdata[1, "V3"] <- "period"
    data <- rawdata %>%
      filter(V3 %in% c("period", "Services/Credit")) %>%
      select(-c(V1:V4)) %>%
      t(.) 
    colnames(data) <- c("period", "value")
    data <- data %>%
      as_tibble() %>%
      mutate(date = as.Date(paste(substr(period, 1, 4), substr(period, 5, 6), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      filter(date >= start_date & date <= end_date)
    starti <- which(grepl(pull(data[1, "date"]), tmp$date))
    tmp[starti:(starti + nrow(data) - 1), 2] <- data$value
    
    return (tmp %>% select(-1))
  }
}
