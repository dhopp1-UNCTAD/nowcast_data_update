# oecd, eurostat, imf api by country
get_api <- function (url, catalog, g, countries, which_time, data_source, start_date, end_date) {
  # which variables are being updated
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
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
    # Group FDI inflows, source = OECD (quarterly) has a different matching key
    if (vars$name[1] == "FDI financial flows, inwards, world") {orig_country_col <- "COU"} else {orig_country_col <- "LOCATION"}
    match_country_col <- "oecd"
    orig_time_col <- "obsTime"
    orig_value_col <- "obsValue"
  } else if (data_source == "eurostat") {
    # Maritime freight, source = Eurostat (quarterly) has a different matching key
    if (vars$name[1] == "Goods volume transported by main ports, Germany") {orig_country_col <- "REP_MAR"} else {orig_country_col <- "GEO"}
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
      merge(countries, by.x=orig_country_col, by.y=match_country_col, all.x=T) %>% 
      mutate(country = ifelse(is.na(country), "oecd", country))
    data <- data[sapply(data[,orig_time_col], nchar) > 4,]
    data[orig_time_col] <- lapply(data[orig_time_col], date_transform)
    data[orig_value_col] <- lapply(data[orig_value_col], as.numeric)
    # data expected oldest to newest
    data <- data[order(data[,orig_time_col], data$country),]
    
    # transform api data to database format
    bad_vars <- c()
    for (i in 1:nrow(vars)) {
      datai <- data %>% 
        filter(country == as.character(vars[i, "country"]))
      starti <- which(grepl(datai[1, orig_time_col], tmp$date))
      
      # delete variable if no data
      if (nrow(datai) == 0) {
        bad_vars <- c(bad_vars, vars$code[i])
      } else {
        if (which_time == "q") {
          tmp[seq(from = starti, to = starti + nrow(datai)*3 - 1, by = 3), i + 1] <- datai[, orig_value_col] 
        } else if (which_time == "m") {
          tmp[starti:(starti + nrow(datai) - 1), i + 1] <- datai[, orig_value_col] 
        }  
      }
    }
    
    tmp <- tmp %>% filter(date <= end_date)
    
    # remove bad variables
    for (bad_var in bad_vars) {
      tmp <- tmp %>% 
        select(-!!bad_var)
    }
    
    return(tmp %>% select(-1))
  }
}

# get result of an api that's not by country (ecb, wto)
get_single_api <- function (url, catalog, g, which_time, data_source, start_date, end_date, date_col, value_col) {
  if (data_source == "wto") {
    url <- str_replace(url, "START_YEAR", substr(start_date, 1, 4))
    url <- str_replace(url, "END_YEAR", substr(end_date, 1, 4))
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
  
  status <- tryCatch({
    if (data_source == "ecb") { rawdata <- readSDMX(url) 
    } else { rawdata <- fromJSON(url) }
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata)
    # WTO has different date structure
    if (data_source == "wto") {
      if (which_time == "m") {
        data[date_col] <- paste0(data$Dataset.Year, "-", substr(data$Dataset.PeriodCode, 2, 3), "-01") %>% as.Date 
      } else if (which_time == "q") {
        data[date_col] <- paste0(data$Dataset.Year, "-", as.numeric(substr(data$Dataset.PeriodCode, 2,2)) * 3, "-01") %>% as.Date 
      }
    } else {
      data[date_col] <- lapply(data[date_col], date_transform) 
    }
    data <- data[data[,date_col] >= start_date & data[,date_col] <= end_date,]
    data[value_col] <- lapply(data[value_col], as.numeric)
    # data expected oldest to newest
    data <- data[order(data[,date_col]),]
    
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


# function to convert a eurostat df that's having problems with readsdmx %>% data.frame
rdsmx_problem <- function (x) {
  tmp <- xmlToList(slot(x, "xmlObj")) %>% 
    .$DataSet %>% 
    .$Series %>% 
    data.frame %>% 
    gather(key="key", value="value") %>% 
    distinct(key, value) %>% 
    filter(grepl("Obs.Obs", key)) %>% 
    mutate(key = ifelse(nchar(value) == 7, "date", "value"))
  
  final <- tmp %>% 
    filter(key == "date") %>% 
    rename(date=value)
  final$value <- tmp %>% filter(key=="value") %>% select(value) %>% pull
  final <- final %>% select(-key)
  return (final)
}