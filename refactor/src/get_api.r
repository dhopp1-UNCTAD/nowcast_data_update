# transform a url with correct start date
gen_url <- function (url, start) {
  if (nchar(start) == 0) {
    return(url)
  } else if (nchar(start) == 4) {
    return(paste0(url,start))
  } else {
    return(paste0(url,format(start, "%Y-%m")))
  }
}

# generate the variables for this group
gen_vars <- function(cat, g) {
  cat %>% filter(download_group == g)
}

# generate empty tmp table
gen_tmp <- function(vars, start_date, end_date) {
  as.data.frame(
    matrix(NA, 
           ncol = (nrow(vars) + 1), 
           nrow = length(seq(from = start_date, to = end_date, by = "month"))
    )
  ) %>%
    rename_at(vars(colnames(.)), ~c("date", vars$code)) %>%
    mutate(date = seq(from = start_date, to = end_date, by = "month"))
}

# oecd and eurostat api
get_api <- function (url, cat, g, countries, which_time, data_source) {
  # differing things between data sources
  if (data_source == "oecd") {
    # Group 7: FDI inflows, source = OECD (quarterly) has a different matching key
    if (g == 7) {orig_country_col <- "COU"} else {orig_country_col <- "LOCATION"}
    match_country_col <- "oecd"
    orig_time_col <- "obsTime"
    orig_value_col <- "obsValue"
  } else if (data_source == "eurostat") {
    # Group 13: Maritime freight, source = Eurostat (quarterly) has a different matching key
    if (g == 13) {orig_country_col <- "REP_MAR"} else {orig_country_col <- "GEO"}
    match_country_col <- "eurostat"
    orig_time_col <- "obsTime"
    orig_value_col <- "obsValue"
  }
  # date needs to be converted if it's quarterly
  if (which_time == "quarterly") {
    date_transform <- function(x){as.Date(paste(substr(x, 1, 4), as.integer(substr(x, 7, 7)) * 3, "01", sep = "-"), format = "%Y-%m-%d")}
  } else {
    date_transform <- function(x){as.Date(paste0(x, "-01"), format = "%Y-%m-%d")}
  }
  
  # which variables are being updated
  vars <- gen_vars(cat, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  # try api call
  status <- tryCatch({
    rawdata <- readSDMX(url) 
    TRUE },
    error = function(e) {
      FALSE })
  
  if (status) {
    # set up api data
    data <- data.frame(rawdata) %>%
      merge(countries, by.x=orig_country_col, by.y=match_country_col, all.x=T)
    data <- data[sapply(data[,orig_time_col], nchar) > 4,]
    data[orig_time_col] <- lapply(data[orig_time_col], date_transform)
    # data expected oldest to newest
    data <- data[order(data[,orig_time_col], data$country),]
    
    # transform api data to database format
    for (i in 1:nrow(vars)) {
      datai <- data %>% 
        filter(country == as.character(vars[i, "country"]))
      starti <- which(grepl(datai[1, orig_time_col], tmp$date))
      if (which_time == "quarterly") {
        tmp[seq(from = starti, to = starti + nrow(datai)*3 - 1, by = 3), i + 1] <- datai[, orig_value_col]
      } else if (which_time == "monthly") {
        tmp[starti:(starti + nrow(datai) - 1), i + 1] <- datai[, orig_value_col]
      }
    }
    
    return(tmp %>% select(-1))
  }
}

# fred api
get_fred <- function (url, cat, g) {
  # which variables are being updated
  vars <- gen_vars(cat, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  # try api call
  status <- tryCatch({
    rawdata <- fromJSON(url)$observations %>% data.frame
    TRUE },
    error = function(e) {
      FALSE })
  
  if (status) {
    data <- rawdata %>%
      select(date, value) %>%
      mutate(date = as.Date(date, format = "%Y-%d-%m")) %>%
      filter(date >= start_date) %>%
      mutate(value = as.numeric(value))
    starti <- which(grepl(data[1, "date"], tmp$date))
    tmp[starti:(starti + nrow(data) - 1), 2] <- data$value
    
    return(tmp %>% select(-1))
  }
}