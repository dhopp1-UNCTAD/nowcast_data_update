transform_data <- function (run_date, output_directory) {
  month <- function (x) {sapply(x, function (x) as.numeric(substr(x, 6, 7)))}
  year <- function (x) {sapply(x, function (x) as.numeric(substr(x, 1, 4)))}  
  
  catalog <- read_csv(paste0(helper_directory,"catalog.csv"), col_types=cols())
  data <- read_csv(paste0(output_directory, run_date, "_database.csv"))
  if ("tmp" %in% colnames(data)) {
    data <- data %>% 
      select(-tmp)
  }  
  
  # For data expressed in cumulative terms, transform to current period data
  for (i in 2:ncol(data)){
    datai <- data %>% select(1, i) %>% mutate(date = as.Date(date)) %>% data.frame
    cati <- catalog %>% filter(code == colnames(datai)[2])
    if (cati$cumulative == "n") next
    datai$tmp <- datai[, 2] - lag(datai[, 2])
    datai[month(datai$date) == 1, "tmp"] <- datai[month(datai$date) == 1, 2]
    data[, i] <- datai$tmp
  }
  
  # For all data that has not been seasonally adjusted, remove seasonal component
  data_sa <- data
  cat_sa <- catalog
  for (i in 2:ncol(data)) {
    datai <- data %>% select(1, i) %>% mutate(date = as.Date(date)) %>% data.frame
    cati <- catalog %>% filter(code == colnames(datai)[2])
    if (cati$sa == "y") next
    varname.sa <- colnames(datai)[2] # paste0(colnames(datai)[2], ".sa")
    for (j in 1:nrow(datai)) if (!is.na(datai[j, 2])) break; starti <- datai[j, "date"]
    for (j in nrow(datai):1) if (!is.na(datai[j, 2])) break; endi <- datai[j, "date"]
    datai$obs <- ifelse(datai$date >= starti & datai$date <= endi, 1, 0)
    if(cati$frequency == "m") {
      datai.ts <- ts(datai[, 2], frequency = 12, start = 2002)
      datai.sa <- seas(window(datai.ts, start = c(year(starti), month(starti)), end = c(year(endi), month(endi))), 
                       na.action = na.x13)
      datai[datai$obs == 1, varname.sa] <- datai.sa$series$s11
    } else {
      datai$obs <- datai$obs * !is.na(datai[, 2])
      datai.ts <- datai[datai$date >= starti & datai$date <= endi, 2]
      datai.ts <- datai.ts[seq(from = 1, to = length(datai.ts), by = 3)]
      datai.ts <- ts(datai.ts, frequency = 4, start = c(year(starti), month(starti)/3), end = c(year(endi), month(endi)/3))
      # x11 for unctad global services, x13 for everything else
      datai[, varname.sa] <- NA
      if (varname.sa == "x_servs_world"){
        datai.sa <- seas(datai.ts, regression.aictest="easter", x11="", na.action = na.x13)
        datai[datai$obs == 1, varname.sa] <- datai.sa$series$d11
      } else {
        datai.sa <- seas(datai.ts, na.action = na.x13)
        datai[datai$obs == 1, varname.sa] <- datai.sa$series$s11
      }
    }
    data_sa[, i] <- datai[, varname.sa]
    colnames(data_sa)[i] <- varname.sa
    cat_sa[cat_sa$code == colnames(datai)[2], "code"] <- varname.sa
  }
  
  # Calculate required transformation
  data_tf <- data_sa
  for (i in 2:ncol(data_sa)) {
    datai <- data_sa %>% select(1, i) %>% mutate(date = as.Date(date)) %>% data.frame
    cati <- cat_sa %>% filter(code == colnames(datai)[2])
    for (j in 1:nrow(datai)) if (!is.na(datai[j, 2])) break; starti <- datai[j, "date"]
    for (j in nrow(datai):1) if (!is.na(datai[j, 2])) break; endi <- datai[j, "date"]
    if (cati$frequency == "q") {
      step = 3
    } else if (cati$frequency == "m") {
      step = 1
    }
    if (cati$transformation == "linear") {
      temp <- data_tf[,i] / 100
    } else if (cati$transformation == "growthr") {
      temp <- datai[, 2] / lag(datai[, 2], step) - 1
      # max decrease of -0.99
      #for (x in 1:nrow(datai)) {
      #  temp[x] <- max(temp[x], -0.99)
      #}
    } else if (cati$transformation == "growthr-yoy") {
      temp <- datai[, 2] / lag(datai[, 2], 12) - 1
    } else if (cati$transformation == "chg") {
      temp <- (datai[, 2] - lag(datai[, 2], step)) / 100
    } else if (cati$transformation == "50_chg") {
      datai$transf50 <- datai[, 2] / 2 + 50
      temp <- (datai[, 3] - lag(datai[, 3], step)) / 100
    }
    data_tf[, i] <- temp
  }
  
  write_csv(data_sa, paste0(output_directory, run_date, "_database_sa.csv"))
  write_csv(data_tf, paste0(output_directory, run_date, "_database_tf.csv"))
}
