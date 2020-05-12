collect_data <- function(start, end, cat, countries) {
  ###
  ### Function to collect data from sources, most of it downloaded directly from data providers through APIs or web scrapping
  ### There are some exceptions, including data that is sourced offline or from online sources that are too complex to scrap
  ### This are available in the "NoScrap" subdirectory
  ###
  ### Inputs:
  ###    - start: starting date (default = 2002-01-01) 
  ###    - end: ending date (default = last completed month)
  ###    - cat: catalogue of all variable to collect (defaults to file in same directory)
  ###    - contry_conv: conversion of country codes from different sources (defaults to file in same directory)
  ###
  ### Outputs:
  ###    - database: data file with all collected data, columns = variables, rows = dates
  ###                first column is the date
  ###    - log: log file with status of data downloads, 1 indicates an error during download
  ###
  ###
  ### This version: Fernando Cantu, 2020-04-28
  ###
  
  if (missing(start)) start <- "2002-01-01"
  if (missing(end)) end <- paste0(format(today() %m-% months(1), "%Y-%m"), "-01")
  if (missing(cat)) cat <- read_excel("Catalogue.xlsx")
  if (missing (countries)) countries <- read_excel("Country codes.xlsx")
  
  start_date <- as.Date(start, format = "%Y-%m-%d")
  start_year <- format(start_date, "%Y")
  start_quarter <- paste0(start_year, "-Q", max(1, floor(as.integer(format(start_date, "%m")) / 3)))
  start_month <- format(start_date, "%m")
  
  end_date <- as.Date(end, format = "%Y-%m-%d")
  end_year <- format(end_date, "%Y")
  end_month <- format(end_date, "%m")
  if(as.integer(end_month) >= 3) {
    end_quarter <- paste0(end_year, "-Q", floor(as.integer(format(end_date, "%m")) / 3))
  } else {
    end_quarter <- paste0(format(end_date %m-% months(12), "%Y"), "-Q4")
  }
  
  database <- as.data.frame(seq(from = start_date, to = end_date, by = "month"))
  colnames(database) <- "date"
  
  log <- as.data.frame(matrix(NA, ncol = 3, nrow = nrow(cat)))
  colnames(log) <- c("code", "download_group", "status")
  log$code <- cat$code
  log$download_group <- cat$download_group
  log$status <- 1
  
  ###
  ### Group 1: Merchandise exports, source = OECD (monthly)
  g <- 1
  vars <- cat %>% filter(download_group == g)
  url <- paste0("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/FRA+DEU+ITA+JPN+KOR+NLD+GBR+USA+OECD+CHN+IND+BRIICS.XTEXVA01.CXMLSA.M/all?startTime=",
                format(start_date, "%Y-%m"))
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- readSDMX(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata) %>%
      left_join(countries, by = c("LOCATION" = "oecd"))
    for (i in 1:nrow(vars)) {
      datai <- data %>% filter(country == as.character(vars[i, "country"]))
      starti <- which(grepl(as.Date(paste0(datai[1, "obsTime"], "-01"), format = "%Y-%m-%d"), temp$date))
      temp[starti:(starti + nrow(datai) - 1), i + 1] <- datai$obsValue
    }
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 2: Industrial production indices, source = OECD (monthly)
  g <- 2
  vars <- cat %>% filter(download_group == g)
  url <- paste0("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/FRA+DEU+ITA+JPN+KOR+MEX+GBR+USA+OECD.PRMNTO01.IXOBSA.M/all?startTime=",
                format(start_date, "%Y-%m"))
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- readSDMX(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata) %>%
      left_join(countries, by = c("LOCATION" = "oecd"))
    for (i in 1:nrow(vars)) {
      datai <- data %>% filter(country == as.character(vars[i, "country"]))
      starti <- which(grepl(as.Date(paste0(datai[1, "obsTime"], "-01"), format = "%Y-%m-%d"), temp$date))
      temp[starti:(starti + nrow(datai) - 1), i + 1] <- datai$obsValue
    }
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 3: Retail trade indices in value, source = OECD (monthly)
  g <- 3
  vars <- cat %>% filter(download_group == g)
  url <- paste0("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/CAN+FRA+DEU+ITA+JPN+GBR+USA.SLRTTO02.IXOBSA.M/all?startTime=",
                format(start_date, "%Y-%m"))
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- readSDMX(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata) %>%
      left_join(countries, by = c("LOCATION" = "oecd"))
    for (i in 1:nrow(vars)) {
      datai <- data %>% filter(country == as.character(vars[i, "country"]))
      starti <- which(grepl(as.Date(paste0(datai[1, "obsTime"], "-01"), format = "%Y-%m-%d"), temp$date))
      temp[starti:(starti + nrow(datai) - 1), i + 1] <- datai$obsValue
    }
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 4: Retail trade indices in volume, source = OECD (monthly)
  g <- 4
  vars <- cat %>% filter(download_group == g)
  url <- paste0("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/CAN+FRA+DEU+ITA+JPN+GBR+USA+OECD+BRA.SLRTTO01.IXOBSA.M/all?startTime=",
                format(start_date, "%Y-%m"))
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- readSDMX(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata) %>%
      left_join(countries, by = c("LOCATION" = "oecd"))
    for (i in 1:nrow(vars)) {
      datai <- data %>% filter(country == as.character(vars[i, "country"]))
      starti <- which(grepl(as.Date(paste0(datai[1, "obsTime"], "-01"), format = "%Y-%m-%d"), temp$date))
      temp[starti:(starti + nrow(datai) - 1), i + 1] <- datai$obsValue
    }
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 5: Construction indices, source = OECD (monthly)
  g <- 5
  vars <- cat %>% filter(download_group == g)
  url <- paste0("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/USA+OECD.PRCNTO01.IXOBSA.M/all?startTime=",
                format(start_date, "%Y-%m"))
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- readSDMX(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata) %>%
      left_join(countries, by = c("LOCATION" = "oecd"))
    for (i in 1:nrow(vars)) {
      datai <- data %>% filter(country == as.character(vars[i, "country"]))
      starti <- which(grepl(as.Date(paste0(datai[1, "obsTime"], "-01"), format = "%Y-%m-%d"), temp$date))
      temp[starti:(starti + nrow(datai) - 1), i + 1] <- datai$obsValue
    }
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 6: Exports of services, source = OECD (quarterly)
  g <- 6
  vars <- cat %>% filter(download_group == g)
  url <- paste0("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI_BOP6/B6CRSE01.OECD.CXCUSA.Q/all?startTime=",
                start_quarter)
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- readSDMX(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata) %>%
      left_join(countries, by = c("LOCATION" = "oecd"))
    for (i in 1:nrow(vars)) {
      datai <- data %>%
        filter(country == as.character(vars[i, "country"])) %>%
        rowwise() %>%
        mutate(obsTime2 = as.Date(paste(substr(obsTime, 1, 4), as.integer(substr(obsTime, 7, 7)) * 3, "01", sep = "-"),
                                  format = "%Y-%m-%d"))
      starti <- which(grepl(pull(datai[1, "obsTime2"]), temp$date))
      temp[seq(from = starti, to = starti + nrow(datai)*3 - 1, by = 3), i + 1] <- datai$obsValue
    }
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 7: FDI inflows, source = OECD (quarterly)
  g <- 7
  vars <- cat %>% filter(download_group == g)
  url <- paste0("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/FDI_AGGR_SUMM/OECD+WLD.USD.DI.T_FA_F/all?startTime=",
                start_quarter)
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- readSDMX(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata) %>%
      left_join(countries, by = c("COU" = "oecd"))
    for (i in 1:nrow(vars)) {
      datai <- data %>%
        filter(country == as.character(vars[i, "country"])) %>%
        filter(nchar(obsTime) > 4) %>%
        rowwise() %>%
        mutate(obsTime2 = as.Date(paste(substr(obsTime, 1, 4), as.integer(substr(obsTime, 7, 7)) * 3, "01", sep = "-"),
                                  format = "%Y-%m-%d"))
      starti <- which(grepl(pull(datai[1, "obsTime2"]), temp$date))
      temp[seq(from = starti, to = starti + nrow(datai)*3 - 1, by = 3), i + 1] <- datai$obsValue
    }
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 8: Consumer confidence indices, source = OECD (monthly)
  g <- 8
  vars <- cat %>% filter(download_group == g)
  url <- paste0("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/DEU+JPN+GBR+USA+OECD+CHN.CSCICP03.IXNSA.M/all?startTime=",
                format(start_date, "%Y-%m"))
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- readSDMX(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata) %>%
      left_join(countries, by = c("LOCATION" = "oecd"))
    for (i in 1:nrow(vars)) {
      datai <- data %>% filter(country == as.character(vars[i, "country"]))
      starti <- which(grepl(as.Date(paste0(datai[1, "obsTime"], "-01"), format = "%Y-%m-%d"), temp$date))
      temp[starti:(starti + nrow(datai) - 1), i + 1] <- datai$obsValue
    }
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 9: Business confidence indices, source = OECD (monthly)
  g <- 9
  vars <- cat %>% filter(download_group == g)
  url <- paste0("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/DEU+JPN+NLD+GBR+USA+OECD+CHN.BSCICP03.IXNSA.M/all?startTime=",
                format(start_date, "%Y-%m"))
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- readSDMX(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata) %>%
      left_join(countries, by = c("LOCATION" = "oecd"))
    for (i in 1:nrow(vars)) {
      datai <- data %>% filter(country == as.character(vars[i, "country"]))
      starti <- which(grepl(as.Date(paste0(datai[1, "obsTime"], "-01"), format = "%Y-%m-%d"), temp$date))
      temp[starti:(starti + nrow(datai) - 1), i + 1] <- datai$obsValue
    }
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 10: Order books, source = OECD (monthly)
  g <- 10
  vars <- cat %>% filter(download_group == g)
  url <- paste0("https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI/DEU+NLD+GBR+USA.BSOBLV02.STSA.M/all?startTime=",
                format(start_date, "%Y-%m"))
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- readSDMX(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata) %>%
      left_join(countries, by = c("LOCATION" = "oecd"))
    for (i in 1:nrow(vars)) {
      datai <- data %>% filter(country == as.character(vars[i, "country"]))
      starti <- which(grepl(as.Date(paste0(datai[1, "obsTime"], "-01"), format = "%Y-%m-%d"), temp$date))
      temp[starti:(starti + nrow(datai) - 1), i + 1] <- datai$obsValue
    }
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 11: Exports of services, source = Eurostat (monthly)
  g <- 11
  vars <- cat %>% filter(download_group == g)
  url <- paste0("http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data/bop_c6_m/.MIO_EUR.S.S1.S1.CRE.WRL_REST.DE+FR.?startPeriod=",
                start_year)
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- readSDMX(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata) %>%
      left_join(countries, by = c("GEO" = "eurostat"))
    for (i in 1:nrow(vars)) {
      datai <- data %>%
        filter(country == as.character(vars[i, "country"])) %>%
        rowwise() %>%
        mutate(obsTime2 = as.Date(paste(obsTime, "01", sep = "-"), format = "%Y-%m-%d")) %>%
        arrange(obsTime2)
      starti <- which(grepl(pull(datai[1, "obsTime2"]), temp$date))
      temp[starti:(starti + nrow(datai) - 1), i + 1] <- datai$obsValue
    }
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 12: Exports of services, source = Eurostat (quarterly)
  g <- 12
  vars <- cat %>% filter(download_group == g)
  url <- paste0("http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data/bop_c6_q/.MIO_EUR.S.S1.S1.CRE.WRL_REST.IE+NL+UK.?startPeriod=",
                start_year)
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- readSDMX(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata) %>%
      left_join(countries, by = c("GEO" = "eurostat"))
    for (i in 1:nrow(vars)) {
      datai <- data %>%
        filter(country == as.character(vars[i, "country"])) %>%
        rowwise() %>%
        mutate(obsTime2 = as.Date(paste(substr(obsTime, 1, 4), as.integer(substr(obsTime, 7, 7)) * 3, "01", sep = "-"),
                                  format = "%Y-%m-%d")) %>%
        arrange(obsTime2)
      starti <- which(grepl(pull(datai[1, "obsTime2"]), temp$date))
      temp[seq(from = starti, to = starti + nrow(datai)*3 - 1, by = 3), i + 1] <- datai$obsValue
    }
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 13: Maritime freight, source = Eurostat (quarterly)
  g <- 13
  vars <- cat %>% filter(download_group == g)
  url <- paste0("http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data/mar_go_qm/.TOTAL.TOTAL.THS_T.DE+NL?startPeriod=",
                start_year)
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- readSDMX(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata) %>%
      left_join(countries, by = c("REP_MAR" = "eurostat"))
    for (i in 1:nrow(vars)) {
      datai <- data %>%
        filter(country == as.character(vars[i, "country"])) %>%
        rowwise() %>%
        mutate(obsTime2 = as.Date(paste(substr(obsTime, 1, 4), as.integer(substr(obsTime, 7, 7)) * 3, "01", sep = "-"),
                                  format = "%Y-%m-%d")) %>%
        arrange(obsTime2)
      starti <- which(grepl(pull(datai[1, "obsTime2"]), temp$date))
      temp[seq(from = starti, to = starti + nrow(datai)*3 - 1, by = 3), i + 1] <- datai$obsValue
    }
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 14: Industrial production index, source = Eurostat (monthly)
  g <- 14
  vars <- cat %>% filter(download_group == g)
  url <- paste0("http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data/sts_inpr_m/.PROD.C.SCA.I15.EU27_2020?startPeriod=",
                start_year)
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- readSDMX(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata) %>%
      left_join(countries, by = c("GEO" = "eurostat"))
    for (i in 1:nrow(vars)) {
      datai <- data %>%
        filter(country == as.character(vars[i, "country"])) %>%
        rowwise() %>%
        mutate(obsTime2 = as.Date(paste(obsTime, "01", sep = "-"), format = "%Y-%m-%d")) %>%
        arrange(obsTime2)
      starti <- which(grepl(pull(datai[1, "obsTime2"]), temp$date))
      temp[starti:(starti + nrow(datai) - 1), i + 1] <- datai$obsValue
    }
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 15: Services production index, source = Eurostat (monthly)
  g <- 15
  vars <- cat %>% filter(download_group == g)
  url <- paste0("http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data/sts_sepr_m/.PROD.G-N_STS.SCA.I15.FR?startPeriod=",
                start_year)
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- readSDMX(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata) %>%
      left_join(countries, by = c("GEO" = "eurostat"))
    for (i in 1:nrow(vars)) {
      datai <- data %>%
        filter(country == as.character(vars[i, "country"])) %>%
        rowwise() %>%
        mutate(obsTime2 = as.Date(paste(obsTime, "01", sep = "-"), format = "%Y-%m-%d")) %>%
        arrange(obsTime2)
      starti <- which(grepl(pull(datai[1, "obsTime2"]), temp$date))
      temp[starti:(starti + nrow(datai) - 1), i + 1] <- datai$obsValue
    }
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 16: Tourist arrivals, source = Eurostat (monthly)
  g <- 16
  vars <- cat %>% filter(download_group == g)
  url <- paste0("http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data/tour_occ_arm/.FOR.NR.I551-I553.DE+ES+FR+IT+UK?startPeriod=",
                start_year)
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- readSDMX(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata) %>%
      left_join(countries, by = c("GEO" = "eurostat"))
    for (i in 1:nrow(vars)) {
      datai <- data %>%
        filter(country == as.character(vars[i, "country"])) %>%
        rowwise() %>%
        mutate(obsTime2 = as.Date(paste(obsTime, "01", sep = "-"), format = "%Y-%m-%d")) %>%
        arrange(obsTime2)
      starti <- which(grepl(pull(datai[1, "obsTime2"]), temp$date))
      temp[starti:(starti + nrow(datai) - 1), i + 1] <- datai$obsValue
    }
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 17: Exports of services, source = FRED (monthly)
  g <- 17
  vars <- cat %>% filter(download_group == g)
  url <- "https://api.stlouisfed.org/fred/series/observations?series_id=BOPSEXP&api_key=2de1403493a03c96c5af253fca05abd2&file_type=json"
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- fromJSON(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata$observations %>%
      as_tibble() %>%
      select(date, value) %>%
      mutate(date = as.Date(date, format = "%Y-%d-%m")) %>%
      filter(date >= start_date) %>%
      mutate(value = as.numeric(value))
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2] <- data$value
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 18: Manufacturers' new orders, source = FRED (monthly)
  g <- 18
  vars <- cat %>% filter(download_group == g)
  url <- "https://api.stlouisfed.org/fred/series/observations?series_id=AMXDNO&api_key=2de1403493a03c96c5af253fca05abd2&file_type=json"
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- fromJSON(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata$observations %>%
      as_tibble() %>%
      select(date, value) %>%
      mutate(date = as.Date(date, format = "%Y-%d-%m")) %>%
      filter(date >= start_date) %>%
      mutate(value = as.numeric(value))
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2] <- data$value
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 19: Exports of services, source = IMF (quarterly)
  g <- 19
  vars <- cat %>% filter(download_group == g)
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- CompactDataMethod("IFS", list(CL_FREQ = "Q",
                                             CL_AREA_IFS = c("CN", "SG"),
                                             CL_INDICATORS_IFS = "BXS_BP6_USD"),
                                 startdate = start_quarter, enddate = end_quarter, verbose = F, tidy = T)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata %>%
      as_tibble() %>%
      rename_all(function(x) str_replace(x, "@", "")) %>%
      mutate(OBS_VALUE = as.numeric(OBS_VALUE)) %>%
      left_join(countries, by = c("REF_AREA" = "imf"))
    for (i in 1:nrow(vars)) {
      datai <- data %>%
        filter(country == as.character(vars[i, "country"])) %>%
        rowwise() %>%
        mutate(obsTime2 = as.Date(paste(substr(TIME_PERIOD, 1, 4),
                                        as.integer(substr(TIME_PERIOD, 7, 7)) * 3, "01", sep = "-"),
                                  format = "%Y-%m-%d")) %>%
        arrange(obsTime2)
      starti <- which(grepl(pull(datai[1, "obsTime2"]), temp$date))
      temp[seq(from = starti, to = starti + nrow(datai) * 3 - 1, by = 3), i + 1] <- datai$OBS_VALUE
    }
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 20: Merchandise exports, source = NSBC (monthly)
  g <- 20
  vars <- cat %>% filter(download_group == g)
  url <- paste0('http://data.stats.gov.cn/english/easyquery.htm?m=QueryData&dbcode=hgyd&rowcode=zb&colcode=sj&wds=[]&dfwds=[{"wdcode":"sj","valuecode":"',
                start_year, '-2030"},{"wdcode":"zb","valuecode":"A0801"}]')
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- fromJSON(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- cbind(rawdata$returndata$datanodes$data,
                  data.frame(matrix(unlist(rawdata$returndata$datanodes$wds), ncol = 4, byrow = T))) %>%
      as_tibble() %>%
      filter(X1 == "A080105") %>%
      rowwise() %>%
      mutate(date = as.Date(paste(substr(X2, 1, 4), substr(X2, 5, 6), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      arrange(date) %>%
      mutate(data = ifelse(hasdata == FALSE, NA, data))
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2] <- data$data
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 21: Development and sale of real estate, source = NSBC (monthly)
  g <- 21
  vars <- cat %>% filter(download_group == g)
  url <- paste0('http://data.stats.gov.cn/english/easyquery.htm?m=QueryData&dbcode=hgyd&rowcode=zb&colcode=sj&wds=[]&dfwds=[{"wdcode":"sj","valuecode":"',
                start_year, '-2030"},{"wdcode":"zb","valuecode":"A0603"}]')
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- fromJSON(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- cbind(rawdata$returndata$datanodes$data,
                  data.frame(matrix(unlist(rawdata$returndata$datanodes$wds), ncol = 4, byrow = T))) %>%
      as_tibble() %>%
      filter(X1 == "A060301") %>%
      rowwise() %>%
      mutate(date = as.Date(paste(substr(X2, 1, 4), substr(X2, 5, 6), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      arrange(date) %>%
      mutate(data = ifelse(hasdata == FALSE, NA, data))
    temp <- as.data.frame(matrix(NA, ncol = 2, nrow = length(seq(from = start_date, to = end_date, by = "month"))))
    colnames(temp) <- c("date", as.character(vars[1, "code"]))
    temp$date <- seq(from = start_date, to = end_date, by = "month")
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2] <- data$data
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 22: Volume of freight handled by main coastal ports, source = NSBC (monthly)
  g <- 22
  vars <- cat %>% filter(download_group == g)
  url <- paste0('http://data.stats.gov.cn/english/easyquery.htm?m=QueryData&dbcode=hgyd&rowcode=zb&colcode=sj&wds=[]&dfwds=[{"wdcode":"sj","valuecode":"',
                start_year, '-2030"},{"wdcode":"zb","valuecode":"A0906"}]')
  url2 <- paste0('http://data.stats.gov.cn/english/easyquery.htm?m=QueryData&dbcode=hgyd&rowcode=zb&colcode=sj&wds=[]&dfwds=[{"wdcode":"sj","valuecode":"',
                 start_year, '-2030"},{"wdcode":"zb","valuecode":"A0905"}]')
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- fromJSON(url)
    rawdata2 <- fromJSON(url2)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data1 <- cbind(rawdata$returndata$datanodes$data,
                   data.frame(matrix(unlist(rawdata$returndata$datanodes$wds), ncol = 4, byrow = T))) %>%
      as_tibble() %>%
      filter(X1 == "A09060A") %>%
      rowwise() %>%
      mutate(date = as.Date(paste(substr(X2, 1, 4), substr(X2, 5, 6), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      arrange(date) %>%
      mutate(data = ifelse(hasdata == FALSE, NA, data)) %>%
      filter(date >= "2019-01-01")
    # Sometimes the cumulative series has NAs, we can fill them with the series for current period
    data1b <- cbind(rawdata$returndata$datanodes$data,
                    data.frame(matrix(unlist(rawdata$returndata$datanodes$wds), ncol = 4, byrow = T))) %>%
      as_tibble() %>%
      filter(X1 == "A090609") %>%
      rowwise() %>%
      mutate(date = as.Date(paste(substr(X2, 1, 4), substr(X2, 5, 6), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      arrange(date) %>%
      mutate(data = ifelse(hasdata == FALSE, NA, data)) %>%
      filter(date >= "2019-01-01")
    if (nrow(data1[is.na(data1$data) & format(data1$date, "%m") == "01", "data"]) > 0) {
      data1[is.na(data1$data) & format(data1$date, "%m") == "01", "data"] <-
        data1[data1$date == pull(data1[is.na(data1$data) & format(data1$date, "%m") == "01", "date"]) %m+% months(1), "data"] -
        data1b[data1b$date == pull(data1[is.na(data1$data) & format(data1$date, "%m") == "01", "date"]) %m+% months(1), "data"]
    }
    # This series is split in two tables. This is the historical data (2018 and before)
    data2 <- cbind(rawdata2$returndata$datanodes$data,
                   data.frame(matrix(unlist(rawdata2$returndata$datanodes$wds), ncol = 4, byrow = T))) %>%
      as_tibble() %>%
      filter(X1 == "A090502") %>%
      rowwise() %>%
      mutate(date = as.Date(paste(substr(X2, 1, 4), substr(X2, 5, 6), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      arrange(date) %>%
      mutate(data = ifelse(hasdata == FALSE, NA, data))
    starti <- which(grepl(pull(data2[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data2) - 1), 2] <- data2$data
    starti <- which(grepl(pull(data1[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data1) - 1), 2] <- data1$data
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 23: Industrial production index, source = NSBC (monthly)
  g <- 23
  vars <- cat %>% filter(download_group == g)
  url <- paste0('http://data.stats.gov.cn/english/easyquery.htm?m=QueryData&dbcode=hgyd&rowcode=zb&colcode=sj&wds=[]&dfwds=[{"wdcode":"sj","valuecode":"',
                start_year, '-2030"},{"wdcode":"zb","valuecode":"A0201"}]')
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- fromJSON(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- cbind(rawdata$returndata$datanodes$data,
                  data.frame(matrix(unlist(rawdata$returndata$datanodes$wds), ncol = 4, byrow = T))) %>%
      as_tibble() %>%
      filter(X1 == "A020101") %>%
      rowwise() %>%
      mutate(date = as.Date(paste(substr(X2, 1, 4), substr(X2, 5, 6), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      arrange(date) %>%
      mutate(data = ifelse(hasdata == FALSE, NA, data)) %>%
      mutate(data = ifelse(format(date, "%m") == "02", NA, data)) %>%
      mutate(data = data / 100)
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2] <- data$data
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 24: Retails sales index, source = NSBC (monthly)
  g <- 24
  vars <- cat %>% filter(download_group == g)
  url <- paste0('http://data.stats.gov.cn/english/easyquery.htm?m=QueryData&dbcode=hgyd&rowcode=zb&colcode=sj&wds=[]&dfwds=[{"wdcode":"sj","valuecode":"',
                start_year, '-2030"},{"wdcode":"zb","valuecode":"A0701"}]')
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- fromJSON(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- cbind(rawdata$returndata$datanodes$data,
                  data.frame(matrix(unlist(rawdata$returndata$datanodes$wds), ncol = 4, byrow = T))) %>%
      as_tibble() %>%
      filter(X1 == "A070103") %>%
      rowwise() %>%
      mutate(date = as.Date(paste(substr(X2, 1, 4), substr(X2, 5, 6), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      arrange(date) %>%
      mutate(data = ifelse(hasdata == FALSE, NA, data)) %>%
      mutate(data = ifelse(format(date, "%m") %in% c("01", "02"), NA, data)) %>%
      mutate(data = data / 100)
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2] <- data$data
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 25: Manufacturing PMI, source = NSBC (monthly)
  g <- 25
  vars <- cat %>% filter(download_group == g)
  url <- paste0('http://data.stats.gov.cn/english/easyquery.htm?m=QueryData&dbcode=hgyd&rowcode=zb&colcode=sj&wds=[]&dfwds=[{"wdcode":"sj","valuecode":"',
                start_year, '-2030"},{"wdcode":"zb","valuecode":"A0B01"}]')
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- fromJSON(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data1 <- cbind(rawdata$returndata$datanodes$data,
                   data.frame(matrix(unlist(rawdata$returndata$datanodes$wds), ncol = 4, byrow = T))) %>%
      as_tibble() %>%
      filter(X1 == "A0B0101") %>%
      rowwise() %>%
      mutate(date = as.Date(paste(substr(X2, 1, 4), substr(X2, 5, 6), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      arrange(date) %>%
      mutate(data = ifelse(hasdata == FALSE, NA, data))
    data2 <- cbind(rawdata$returndata$datanodes$data,
                   data.frame(matrix(unlist(rawdata$returndata$datanodes$wds), ncol = 4, byrow = T))) %>%
      as_tibble() %>%
      filter(X1 == "A0B0104") %>%
      rowwise() %>%
      mutate(date = as.Date(paste(substr(X2, 1, 4), substr(X2, 5, 6), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      arrange(date) %>%
      mutate(data = ifelse(hasdata == FALSE, NA, data))
    starti <- which(grepl(pull(data1[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data1) - 1), 2] <- data1$data
    starti <- which(grepl(pull(data2[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data2) - 1), 3] <- data2$data
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 26: Non-manufacturing PMI, source = NSBC (monthly)
  g <- 26
  vars <- cat %>% filter(download_group == g)
  url <- paste0('http://data.stats.gov.cn/english/easyquery.htm?m=QueryData&dbcode=hgyd&rowcode=zb&colcode=sj&wds=[]&dfwds=[{"wdcode":"sj","valuecode":"',
                start_year, '-2030"},{"wdcode":"zb","valuecode":"A0B02"}]')
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- fromJSON(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- cbind(rawdata$returndata$datanodes$data,
                  data.frame(matrix(unlist(rawdata$returndata$datanodes$wds), ncol = 4, byrow = T))) %>%
      as_tibble() %>%
      filter(X1 == "A0B0201") %>%
      rowwise() %>%
      mutate(date = as.Date(paste(substr(X2, 1, 4), substr(X2, 5, 6), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      arrange(date) %>%
      mutate(data = ifelse(hasdata == FALSE, NA, data))
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2] <- data$data
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 27: Merchandise exports, source = HK NSO (monthly)
  g <- 27
  vars <- cat %>% filter(download_group == g)
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    temp_date <- end_date
    lastdata <- paste0(format(temp_date, "%Y"), format(temp_date, "%m"))
    rawdata <- NULL
    while(is.null(rawdata$dataSet)) {
      url <- paste0("https://tradeidds.censtatd.gov.hk/api/get?lang=EN&sv=VCM&freq=M&period=201201,", lastdata, "&ttype=4")
      rawdata <- fromJSON(url)
      temp_date <- temp_date %m-% months(1)
      lastdata <- paste0(format(temp_date, "%Y"), format(temp_date, "%m"))
    }
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data1 <- rawdata$dataSet %>%
      as_tibble() %>%
      mutate(date = as.Date(paste(substr(period, 1, 4), substr(period, 5, 6), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      mutate(figure = as.numeric(figure) / 1000)
    # Online data only available from 2012. Previous data pasted in "Others" file in NoScrap directory
    data2 <- read_excel("NoScrap/Others.xlsx", sheet = 1, skip = 0) %>%
      mutate(date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%m-%d")) %>%
      select(date, x_hk)
    starti <- which(grepl(pull(data2[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data2) - 1), 2] <- data2$x_hk
    starti <- which(grepl(pull(data1[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data1) - 1), 2] <- data1$figure
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 28: Exports of services, source = BOJ (monthly)
  g <- 28
  vars <- cat %>% filter(download_group == g)
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    temps <- tempfile()
    download.file("https://www.stat-search.boj.or.jp/info/bp_m_en.zip", temps, quiet = T)
    rawdata <- read.csv(unzip(temps), header = F, stringsAsFactors = F)
    unlink(temps)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    file.remove("bp_m_en.csv")
    rawdata[1, "V3"] <- "period"
    data <- rawdata %>%
      filter(V3 %in% c("period", "Services/Credit")) %>%
      select(-c(V1:V4)) %>%
      t(.) %>%
      set_colnames(c("period", "value")) %>%
      as_tibble() %>%
      mutate(date = as.Date(paste(substr(period, 1, 4), substr(period, 5, 6), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      filter(date >= start_date & date <= end_date)
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2] <- data$value
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 29: Industrial new orders, source = ECB (monthly)
  g <- 29
  vars <- cat %>% filter(download_group == g)
  url <- "https://sdw-wsrest.ecb.europa.eu/service/data/STS/M.I7.Y.ORDT.NSC002.3.000?format=genericdata"
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- readSDMX(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata) %>%
      as_tibble() %>%
      mutate(obsTime2 = as.Date(paste(obsTime, "01", sep = "-"), format = "%Y-%m-%d")) %>%
      filter(obsTime2 >= start_date & obsTime2 <= end_date)
    starti <- which(grepl(pull(data[1, "obsTime2"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2] <- data$obsValue
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 30: Export prices of manufactures, source = WTO (monthly)
  g <- 30
  vars <- cat %>% filter(download_group == g)
  url <- paste0("https://api.wto.org/timeseries/v1/data?i=ITS_MTP_MXPM&r=000&pc=MA&ps=",
                start_year, "-", end_year, "&subscription-key=3af1b3ff4ff04e4c9646ce6e80141a62")
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- fromJSON(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata$Dataset %>%
      as_tibble() %>%
      mutate(date = as.Date(paste(Year, substr(PeriodCode, 2, 3), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      arrange(date)
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2] <- data$Value
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 31: Total merchandise exports, source = WTO (quarterly)
  g <- 31
  vars <- cat %>% filter(download_group == g)
  url <- paste0("https://api.wto.org/timeseries/v1/data?i=ITS_MTV_QX&r=000&p=000&pc=TO&ps=",
                start_year, "-", end_year, "&subscription-key=3af1b3ff4ff04e4c9646ce6e80141a62")
  Sys.sleep(3)
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- fromJSON(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata$Dataset %>%
      as_tibble() %>%
      rowwise() %>%
      mutate(date = as.Date(paste(Year, as.integer(substr(PeriodCode, 2, 2)) * 3, "01", sep = "-"), format = "%Y-%m-%d")) %>%
      arrange(date)
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[seq(from = starti, to = starti + nrow(data)*3 - 1, by = 3), 2] <- data$Value
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 32: Export volumes and UVI, source = CPB (monthly)
  g <- 32
  vars <- cat %>% filter(download_group == g)
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    temps <- tempfile()
    download.file("https://www.cpb.nl/sites/default/files/wtmonitor/cpb-data-wtm.xlsx", temps, quiet = T, mode = "wb")
    rawdata <- read_excel(path = temps, skip = 3, col_names = F)
    unlink(temps)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    rawdata[1, 2] <- "date"
    data <- rawdata %>%
      select(variable = "...2", 5:ncol(.)) %>%
      drop_na(variable) %>%
      t(.)%>%
      as_tibble(., .name_repair = "unique") %>%
      set_colnames(slice(., 1)) %>%
      slice(-1) %>%
      mutate_at(vars(-date), as.numeric) %>%
      mutate(date = as.Date(paste(substr(date, 1, 4), substr(date, 6, 7), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      filter(date >= start_date)
    for (i in 1:nrow(vars)) {
      if (substr(vars[i, "code"], 1, 5) == "x_vol") {
        var_name <- paste0("xgz_", pull(countries[countries$country == pull(vars[i, "country"]), "cpb"]), "_qnmi_sn")
      }
      else {
        var_name <- paste0("xgz_", pull(countries[countries$country == pull(vars[i, "country"]), "cpb"]), "_pdmi_sn")
      }
      datai <- data %>%
        select(date, var_name)
      starti <- which(grepl(pull(datai[1, "date"]), temp$date))
      temp[starti:(starti + nrow(data) - 1), i + 1] <- datai[, 2]
    }
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 33: International tourist arrivals, source = Banxico (monthly)
  g <- 33
  vars <- cat %>% filter(download_group == g)
  setToken("3a03947310e12f4de4cc5ac5f42e41e445bbdbec7825a4b84f1610d5200e1743")
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- getSeriesData("SE5830", start_date, end_date)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- as.data.frame(rawdata) %>%
      as_tibble() %>%
      set_colnames(c("date", "value"))
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2] <- data$value
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 34: Business confidence, services sector, source = European Commission (monthly)
  g <- 34
  vars <- cat %>% filter(download_group == g)
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    temps <- tempfile()
    download.file("https://ec.europa.eu/economy_finance/db_indicators/surveys/documents/series/nace2_ecfin_2003/services_total_sa_nace2.zip",
                  temps, quiet = T)
    rawdata <- read_excel(unzip(temps), sheet = "SERVICES MONTHLY")
    unlink(temps)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    file.remove("services_total_sa_nace2.xlsx")
    data <- rawdata %>%
      select("...1", SERV.EU.TOT.COF.BS.M) %>%
      set_colnames(c("date", "value")) %>%
      mutate(date = as.Date(paste(substr(date, 1, 7), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      filter(date >= start_date) %>%
      mutate(value = as.numeric(value))
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2] <- data$value
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 35: Container throughput indicator, source = RWI/ISL (monthly)
  g <- 35
  vars <- cat %>% filter(download_group == g)
  url <- paste("http://www.rwi-essen.de/containerindex")
  webpage <- read_lines(url)
  row <- grep("Aktuelle Daten zum Index", webpage)
  url <- paste0("http://www.rwi-essen.de",
                regmatches(webpage[row], gregexpr('(?<=\\").+?(?=\\")', webpage[row], perl = T))[[1]][1])
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    temps <- tempfile()
    download.file(url, temps, quiet = T, mode = "wb")
    rawdata <- read_excel(path = temps, skip = 3)
    unlink(temps)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata %>%
      select(1, 3) %>%
      set_colnames(c("date", "value")) %>%
      drop_na(date, value) %>%
      mutate(date = as.Date(as.integer(date), origin = "1899-12-30")) %>%
      mutate(value = as.numeric(value))
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2] <- data$value
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 36: Index of services production, source = METI (monthly)
  g <- 36
  vars <- cat %>% filter(download_group == g)
  url <- "https://www.meti.go.jp/english/statistics/tyo/zenkatu/excel/b2010_zsme.xls"
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    temps <- tempfile()
    download.file(url, temps, quiet = T, mode = "wb")
    rawdata <- read_excel(path = temps, skip = 2, col_names = F)
    unlink(temps)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata %>%
      filter(...1 %in% c("Item_Number", "KAC00000I")) %>%
      t(.) %>%
      as_tibble(., .name_repair = "unique") %>%
      slice(-1:-3) %>%
      set_colnames(c("date", "value")) %>%
      mutate(date = as.Date(paste(substr(date, 1, 4), substr(date, 5, 6), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      mutate(value = as.numeric(value))
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2] <- data$value
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 37: Index of services production, source = ONS (monthly)
  g <- 37
  vars <- cat %>% filter(download_group == g)
  url <- "https://www.ons.gov.uk/generator?format=xls&uri=/economy/economicoutputandproductivity/output/timeseries/s2ku/ios1"
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    temps <- tempfile()
    download.file(url, temps, quiet = T, mode = "wb")
    rawdata <- read_excel(path = temps, skip = 8, col_names = F)
    unlink(temps)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata %>%
      set_colnames(c("date", "value")) %>%
      filter(nchar(date) > 4) %>%
      mutate(date = as.Date(paste(substr(date, 1, 4), substr(date, 6, 8), "01", sep = "-"), format = "%Y-%b-%d")) %>%
      filter(date >= start_date)
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2] <- data$value
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 38: Index of services production, source = IBGE (monthly)
  g <- 38
  vars <- cat %>% filter(download_group == g)
  url <- "https://sidra.ibge.gov.br/geratabela?format=xlsx&name=tabela6442.xlsx&terr=N&rank=-&query=t/6442/n1/all/v/all/p/all/c11046/40312/d/v8676%201,v8677%201/l/v,p%2Bc11046,t"
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    temps <- tempfile()
    download.file(url, temps, quiet = T, mode = "wb")
    rawdata <- read_excel(path = temps, skip = 2, col_names = T, sheet = 2)
    unlink(temps)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata %>%
      slice(1, 3) %>%
      t(.) %>%
      as_tibble(.) %>%
      slice(-1) %>%
      set_colnames(c("date", "value")) %>%
      mutate(date = seq.Date(from = as.Date("2011-01-01"), by = "month", length.out = nrow(.))) %>%
      mutate(value = as.numeric(value))
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2] <- data$value
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 39: Departing foreign visitors, source = NSO TR (quarterly)
  g <- 39
  vars <- cat %>% filter(download_group == g)
  url <- "http://www.turkstat.gov.tr/PreIstatistikTablo.do?istab_id=324"
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    temps <- tempfile()
    download.file(url, temps, quiet = T, mode = "wb")
    rawdata <- read_excel(path = temps, skip = 5, col_names = T, .name_repair = "unique")
    unlink(temps)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata %>%
      select(1, 2, "Toplam \nTotal...14") %>%
      set_colnames(c("year", "quarter", "value"))
    for (i in 2:nrow(data)) {
      if(!is.na(data[i, "year"])) next
      data[i, "year"] <- data[i - 1, "year"]
    }
    data <- data %>%
      mutate(quarter = recode(quarter, "I" = "03", "II" = "06", "III" = "09", "IV" = "12")) %>%
      rowwise() %>%
      mutate(date = as.Date(paste(year, quarter, "01", sep = "-"), format = "%Y-%m-%d")) %>%
      drop_na(date)
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[seq(from = starti, to = starti + nrow(data)*3 - 1, by = 3), 2] <- data$value
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 40: Total container throughput, source = NSO SG (monthly)
  g <- 40
  vars <- cat %>% filter(download_group == g)
  url <- "https://www.tablebuilder.singstat.gov.sg/publicfacing/api/json/title/15317.json"
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- fromJSON(url)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata$Level1d %>%
      as_tibble() %>%
      mutate(date = as.Date(paste(month, "01", sep = "-"), format = "%Y-%m-%d")) %>%
      filter(date >= start_date) %>%
      mutate(value = as.numeric(value)) %>%
      arrange(date)
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2] <- data$value
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 41: Air freight, Heathrow airport, source = LHR (monthly)
  g <- 41
  vars <- cat %>% filter(download_group == g)
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    temp_date <- end_date
    lastdata <- paste0(format(temp_date, "%b"), "-", format(temp_date, "%Y"))
    url <- paste0("https://www.heathrow.com/content/dam/heathrow/web/common/documents/company/investor/reports-and-presentations/traffic-statistics/Heathrow-Monthly-Traffic-Statistics-Jan-2005-",
                  lastdata, ".xlsx")
    while(GET(url)$status_code != 200) {
      temp_date <- temp_date %m-% months(1)
      lastdata <- paste0(format(temp_date, "%b"), "-", format(temp_date, "%Y"))
      url <- paste0("https://www.heathrow.com/content/dam/heathrow/web/common/documents/company/investor/reports-and-presentations/traffic-statistics/Heathrow-Monthly-Traffic-Statistics-Jan-2005-",
                    lastdata, ".xlsx")
    }
    temps <- tempfile()
    download.file(url, temps, quiet = T, mode = "wb")
    rawdata <- read_excel(path = temps, skip = 2, col_names = T, sheet = 3)
    unlink(temps)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata %>%
      mutate(date = as.Date(Month, format = "%Y-%m-%d")) %>%
      mutate(International = Total - UK)
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2] <- data$Total
    temp[starti:(starti + nrow(data) - 1), 3] <- data$International
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 42: Air freight Hong Kong airport, source = HKG (monthly)
  g <- 42
  vars <- cat %>% filter(download_group == g)
  url <- "https://www.cad.gov.hk/english/pdf/Stat%20Webpage.xlsx"
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    temps <- tempfile()
    download.file(url, temps, quiet = T, mode = "wb")
    rawdata <- read_excel(path = temps, skip = 7, col_names = T)
    unlink(temps)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata
    for (i in 2:nrow(data)) {
      if(!is.na(data[i, "Year"])) next
      data[i, "Year"] <- data[i - 1, "Year"]
    }
    data <- data %>%
      filter(!is.na(Month) & Month != "*" & Month != "# ¶") %>%
      rowwise() %>%
      mutate(date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%b-%d")) %>%
      filter(date >= start_date) %>%
      select(date, value = "Total...14") %>%
      arrange(date)
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2] <- data$value
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 43: Free market commodity price index, source = UNCTAD (monthly)
  g <- 43
  vars <- cat %>% filter(download_group == g)
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    temps <- tempfile(fileext = ".7z")
    download.file("https://unctadstat.unctad.org/7zip/US_CommodityPriceIndices_M.csv.7z", temps, quiet = T, mode = "wb")
    rawdata <- read.csv(archive_read(temps), header = T, stringsAsFactors = F)
    unlink(temps)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata %>%
      as_tibble() %>%
      select(Period = "ï..Period", commodity = CommodityProduct.Label, value = Index.Base.2015) %>%
      filter(commodity == "All groups") %>%
      mutate(date = as.Date(paste(substr(Period, 1, 4), substr(Period, 6, 7), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      filter(date >= start_date) %>%
      arrange(date)
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2] <- data$value
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 44: Export volumes, source = UNCTAD (quarterly)
  g <- 44
  vars <- cat %>% filter(download_group == g)
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    temps <- tempfile(fileext = ".7z")
    download.file("https://unctadstat.unctad.org/7zip/US_MerchVolumeQuarterly.csv.7z", temps, quiet = T, mode = "wb")
    rawdata <- read.csv(archive_read(temps), header = T, stringsAsFactors = F)
    unlink(temps)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata %>%
      as_tibble() %>%
      select(Period = "ï..Quarter", country = Economy.Label, flow = Flow.Label, value = Volume.Index..2005.100.) %>%
      filter(country == "World" & flow == "Exports") %>%
      mutate(date = as.Date(paste(substr(Period, 1, 4), as.integer(substr(Period, 7, 7)) * 3, "01", sep = "-"), 
                            format = "%Y-%m-%d")) %>%
      filter(date >= start_date) %>%
      arrange(date)
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[seq(from = starti, to = starti + nrow(data)*3 - 1, by = 3), 2] <- data$value
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 45: Various indicators, source = Eikon (monthly)
  g <- 45
  vars <- cat %>% filter(download_group == g)
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- read_excel("NoScrap/Eikon.xlsx", skip = 1)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    for(i in 1:nrow(vars)){
      datai <- rawdata %>%
        select(((i - 1) * 4 + 1):((i * 4) - 1)) %>%
        select(2, 3) %>%
        set_colnames(c("date", "value")) %>%
        slice(-1, -2) %>%
        drop_na(value) %>%
        mutate(value = as.numeric(value), date = as.Date(as.integer(date), origin = "1899-12-30")) %>%
        filter(date <= end_date) %>%
        mutate(date = as.Date(paste(substr(date, 1, 7), "01", sep = "-"), format = "%Y-%m-%d")) %>%
        arrange(date) %>%
        complete(date = seq.Date(min(date), max(date), by = "month"))
      starti <- which(grepl(pull(datai[1, "date"]), temp$date))
      temp[starti:(starti + nrow(datai) - 1), i + 1] <- datai$value
    }
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 46: Tourist arrivals, source = UNWTO (monthly)
  g <- 46
  vars <- cat %>% filter(download_group == g)
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- read_excel("NoScrap/Tourism_UNWTO.xlsx", skip = 0)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata %>%
      mutate(date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%m-%d")) %>%
      select(-year, -month) %>%
      mutate_at(vars(-date), function(x) x / 100)
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2:ncol(temp)] <- data %>% select(-date)
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  ###
  ### Group 47: Non-scrappable indicators, source = Various (monthly & quarterly)
  g <- 47
  vars <- cat %>% filter(download_group == g)
  temp <- as.data.frame(matrix(NA, ncol = (nrow(vars) + 1), 
                               nrow = length(seq(from = start_date, to = end_date, by = "month"))))
  colnames(temp) <- c("date", pull(vars[, "code"]))
  temp$date <- seq(from = start_date, to = end_date, by = "month")
  status <- tryCatch({
    rawdata <- read_excel("NoScrap/Others.xlsx", sheet = 1, skip = 0)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata %>%
      mutate(date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%m-%d")) %>%
      select(-year, -month)
    starti <- which(grepl(pull(data[1, "date"]), temp$date))
    temp[starti:(starti + nrow(data) - 1), 2:ncol(temp)] <- data %>% select(-date, -x_hk)
    log[log$download_group == g, "status"] <- 0
  }
  database <- cbind(database, temp %>% select(-1))
  
  return(list(database = database, log = log))
  
}