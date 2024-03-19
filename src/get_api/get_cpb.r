get_cpb <- function (url, catalog, g, countries, start_date, end_date) {
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  month_key <- list()
  month_key[[1]] <- "january"
  month_key[[2]] <- "february"
  month_key[[3]] <- "march"
  month_key[[4]] <- "april"
  month_key[[5]] <- "may"
  month_key[[6]] <- "june"
  month_key[[7]] <- "july"
  month_key[[8]] <- "august"
  month_key[[9]] <- "september"
  month_key[[10]] <- "october"
  month_key[[11]] <- "november"
  month_key[[12]] <- "december"
  
  status <- tryCatch({
    # non linux: download.file("https://www.cpb.nl/sites/default/files/wtmonitor/cpb-data-wtm.xlsx", tmps, quiet = T, mode = "wb")
    # formerly https://www.cpb.nl/sites/default/files/wtmonitor/cpb-data-wtm.xlsx
    try_date <- end_date
    year <- as.numeric(substr(try_date, 1,4))
    stop_year <- year - 1
    
    success <- FALSE
    while (!success & year >= stop_year) { # keep looping while no success, but only try this year and last
      year <- as.numeric(substr(try_date, 1,4))
      month_num <- as.numeric(substr(try_date, 6, 7))
      
      tmps <- tempfile()
      interp_url <- str_replace(url, "MONTH", month_key[[month_num]]) %>%
        str_replace("YEAR", as.character(year))
      success <- tryCatch({
        download.file(interp_url, tmps, method="wget", quiet = T, mode = "wb", extra="--no-check-certificate")
      TRUE}, error = function(e) {FALSE})
      
      try_date <- seq(try_date, length = 2, by = "-1 month")[2]
    }
    
    rawdata <- read_excel(path = tmps, skip = 3, col_names = F)
    unlink(tmps)
    TRUE },
    error = function(e) { FALSE })
  
  if (status) {
    rawdata[1, 2] <- "date"
    data <- rawdata %>%
      select(variable = "...2", 5:ncol(.)) %>%
      drop_na(variable) %>%
      t(.) %>%
      as_tibble(., .name_repair = "unique") 
    colnames(data) <- data[1,]
    data <- data %>%
      slice(-1) %>%
      mutate_at(vars(-date), as.numeric) %>%
      mutate(date = as.Date(paste(substr(date, 1, 4), substr(date, 6, 7), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      filter(date >= start_date)
    for (i in 1:nrow(vars)) {
      if (substr(vars[i, "code"], 1, 5) == "x_vol") {
        var_name <- paste0("xgz_", pull(countries[countries$country == pull(vars[i, "country"]), "cpb"]), "_qnmi_sn")
      } else {
        var_name <- paste0("xgz_", pull(countries[countries$country == pull(vars[i, "country"]), "cpb"]), "_pdmi_sn")
      }
      datai <- data %>%
        select(date, var_name)
      starti <- which(grepl(pull(datai[1, "date"]), tmp$date))
      tmp[starti:(starti + nrow(data) - 1), i + 1] <- datai[, 2]
    }
    return(tmp %>% select(-1))
  }
}