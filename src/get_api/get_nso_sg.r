get_nso_sg <- function (url, catalog, g, start_date, end_date) {
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  month_key <- list()
  month_key[["Jan"]] <- 1
  month_key[["Feb"]] <- 2
  month_key[["Mar"]] <- 3
  month_key[["Apr"]] <- 4
  month_key[["May"]] <- 5
  month_key[["Jun"]] <- 6
  month_key[["Jul"]] <- 7
  month_key[["Aug"]] <- 8
  month_key[["Sep"]] <- 9
  month_key[["Oct"]] <- 10
  month_key[["Nov"]] <- 11
  month_key[["Dec"]] <- 12
  
  status <- tryCatch({
      rawdata <- fromJSON(url)
    TRUE },
    error = function(e) { FALSE })
  if (status) {
    if (vars$code[1] == "container_sg") {
      data <- rawdata$Data$row[5][[1]][8] %>% 
        data.frame()
    } else if (vars$code[1] == "x_sg") {
      data <- rawdata$Data$row[5][[1]][1] %>% 
        data.frame()
    }
    data[, "month_num"] <- substr(data$key, 6, 8) %>%
      sapply(function (x) month_key[[x]]) %>%
      unname()
    data <- data %>%
      mutate(date = as.Date(paste0(substr(key, 1, 4), "-", as.character(month_num), "-01"))) %>%
      arrange(date) %>%
      select(date, value)
    tmp <- tmp %>%
      select(date) %>%
      left_join(data, by="date") %>%
      rename(!!vars$code[1] := value)
    return (tmp %>% select(-1))
  }
}