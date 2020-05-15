get_ec <- function (url, catalog, g, start_date, end_date) {
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  status <- tryCatch({
    tmps <- tempfile()
    download.file(url,
                  tmps, quiet = T)
    rawdata <- read_excel(unzip(tmps), sheet = "SERVICES MONTHLY")
    unlink(tmps)
    TRUE },
    error = function(e) { FALSE })
  if (status) {
    file.remove("services_total_sa_nace2.xlsx")
    data <- rawdata %>%
      select("...1", SERV.EU.TOT.COF.BS.M)
    colnames(data) <- c("date", "value")
    data <- data %>%
      mutate(date = as.Date(paste(substr(date, 1, 7), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      filter(date >= start_date) %>%
      mutate(value = as.numeric(value))
    starti <- which(grepl(pull(data[1, "date"]), tmp$date))
    tmp[starti:(starti + nrow(data) - 1), 2] <- data$value
    
    return(tmp %>% select(-1))
  }
}