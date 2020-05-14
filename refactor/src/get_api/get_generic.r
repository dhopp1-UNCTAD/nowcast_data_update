# oecd, eurostat, imf api by country
get_api <- function (url, catalog, g, countries, which_time, data_source, start_date, end_date) {
  # getting necessary dates
  start_year <- format(start_date, "%Y")
  end_year <- format(end_date, "%Y")
  end_month <- format(end_date, "%m")
  start_quarter <- paste0(start_year, "-Q", max(1, floor(as.integer(format(start_date, "%m")) / 3)))
  if(as.integer(end_month) >= 3) {
    end_quarter <- paste0(end_year, "-Q", floor(as.integer(format(end_date, "%m")) / 3))
  } else {
    end_quarter <- paste0(format(end_date %m-% months(12), "%Y"), "-Q4")
  }
  if (data_source == "oecd") {
    start_url <- start_date
  } else if (data_source == "eurostat") {
    start_url <- start_year
  } else {
    start_url <- ""
  }
  url <- gen_url(url, start_url)
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
  } else if (data_source == "imf") {
    orig_country_col <- "X.REF_AREA"
    match_country_col <- "imf"
    orig_time_col <- "X.TIME_PERIOD"
    orig_value_col <- "X.OBS_VALUE"
  }
  # date needs to be converted if it's quarterly
  if (which_time == "q") {
    date_transform <- function(x){as.Date(paste(substr(x, 1, 4), as.integer(substr(x, 7, 7)) * 3, "01", sep = "-"), format = "%Y-%m-%d")}
  } else {
    date_transform <- function(x){as.Date(paste0(x, "-01"), format = "%Y-%m-%d")}
  }
  
  # which variables are being updated
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  # try api call
  status <- tryCatch({
    if (data_source %in% c("oecd", "eurostat")) {rawdata <- readSDMX(url)
    } else if (data_source == "imf") {rawdata <- CompactDataMethod("IFS", list(CL_FREQ = "Q", CL_AREA_IFS = c("CN", "SG"),CL_INDICATORS_IFS = url), startdate = start_quarter, enddate = end_quarter, verbose = F, tidy = T)}
    TRUE},
    error = function(e) {FALSE}
  )
  
  if (status) {
    # set up api data
    data <- data.frame(rawdata) %>%
      merge(countries, by.x=orig_country_col, by.y=match_country_col, all.x=T)
    data <- data[sapply(data[,orig_time_col], nchar) > 4,]
    data[orig_time_col] <- lapply(data[orig_time_col], date_transform)
    data[orig_value_col] <- lapply(data[orig_value_col], as.numeric)
    # data expected oldest to newest
    data <- data[order(data[,orig_time_col], data$country),]
    
    # transform api data to database format
    for (i in 1:nrow(vars)) {
      datai <- data %>% 
        filter(country == as.character(vars[i, "country"]))
      starti <- which(grepl(datai[1, orig_time_col], tmp$date))
      if (which_time == "q") {
        tmp[seq(from = starti, to = starti + nrow(datai)*3 - 1, by = 3), i + 1] <- datai[, orig_value_col]
      } else if (which_time == "m") {
        tmp[starti:(starti + nrow(datai) - 1), i + 1] <- datai[, orig_value_col]
      }
    }
    
    return(tmp %>% select(-1))
  }
}

# get result of an api that's not by country
get_single_api <- function (url, catalog, g, which_time, start_date, end_date, date_col, value_col) {
  # date needs to be converted if it's quarterly
  if (which_time == "q") {
    date_transform <- function(x){as.Date(paste(substr(x, 1, 4), as.integer(substr(x, 7, 7)) * 3, "01", sep = "-"), format = "%Y-%m-%d")}
  } else {
     date_transform <- function(x){as.Date(paste0(x, "-01"), format = "%Y-%m-%d")}
  }
  # which variables are being updated
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  status <- tryCatch({
    rawdata <- readSDMX(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata)
    data[date_col] <- lapply(data[date_col], date_transform)
    data <- data[data[,date_col] >= start_date & data[,date_col] <= end_date,]
    starti <- which(data[1, date_col] == tmp$date)
    i <- 1
    if (which_time == "q") {
      tmp[seq(from = starti, to = starti + nrow(data)*3 - 1, by = 3), i + 1] <- data[, value_col]
    } else if (which_time == "m") {
      tmp[starti:(starti + nrow(data) - 1), i + 1] <- data[, value_col]
    }
    
    return (tmp %>% select(-1))
  }
}