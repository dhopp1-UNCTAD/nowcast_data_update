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

# oecd, eurostat, imf api
get_api <- function (url, cat, g, countries, which_time, data_source, start_date, end_date) {
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
  vars <- gen_vars(cat, g)
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

# fred api
get_fred <- function (url, cat, g, start_date, end_date) {
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

# nbs api
get_nbs <- function(url, cat, g, start_date, end_date) {
  filter_url <- strsplit(url, "-FILTER-")[[1]][2]
  url <- strsplit(url, "-FILTER-")[[1]][1]
  
  vars <- gen_vars(cat, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  # api call
  status <- tryCatch({
    rawdata <- fromJSON(url)
    TRUE },
    error = function(e) {
      FALSE })
  
  # processing api data
  if (status) {
    data <- cbind(rawdata$returndata$datanodes$data,
                  data.frame(matrix(unlist(rawdata$returndata$datanodes$wds), ncol = 4, byrow = T))) %>%
      as_tibble() %>%
      filter(X1 == filter_url) %>%
      rowwise() %>%
      mutate(date = as.Date(paste(substr(X2, 1, 4), substr(X2, 5, 6), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      arrange(date) %>%
      mutate(data = ifelse(hasdata == FALSE, NA, data))
    starti <- which(grepl(pull(data[1, "date"]), tmp$date))
    tmp[starti:(starti + nrow(data) - 1), 2] <- data$data
  }
  return(tmp %>% select(-1))
}

# nbs for series where the data is split in 2 places
get_nbs_double <- function (url1, url2, cat, g, start_date, end_date) {
  take_non_na <- function (a, b) { if(is.na(a)) {b} else{a} }
  data1 <- get_nbs(url1, cat, g, start_date, end_date)
  data2 <- get_nbs(url2, cat, g, start_date, end_date)
  names <- colnames(data1)
  # keep the non-na one
  final <- data1 %>% 
    mutate(z=ifelse(is.na(.[[1]]), data2[,1], .[[1]])) %>%
    select(2)
  colnames(final) <- names
  return(final)
}