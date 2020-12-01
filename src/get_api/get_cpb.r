get_cpb <- function (url, catalog, g, countries, start_date, end_date) {
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  status <- tryCatch({
    tmps <- tempfile()
    # non linux: download.file("https://www.cpb.nl/sites/default/files/wtmonitor/cpb-data-wtm.xlsx", tmps, quiet = T, mode = "wb")
    download.file("https://www.cpb.nl/sites/default/files/wtmonitor/cpb-data-wtm.xlsx", tmps, method="wget", quiet = T, mode = "wb", extra="--no-check-certificate")
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