get_eikon <- function (catalog, g, start_date, end_date, eikon) {
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  rawdata <- eikon
  counter <- 1
  for (i in 1:length(colnames(rawdata))) {
    if (colnames(rawdata)[i] == "") {
      colnames(rawdata)[i] <- paste0("x", as.character(counter)) 
    }
    counter <- counter + 1
  }
  for(i in 1:nrow(vars)){
    datai <- rawdata %>%
      select(((i - 1) * 4 + 1):((i * 4) - 1)) %>%
      select(2, 3)
    colnames(datai) <- c("date", "value")
    datai <- datai %>%
      slice(-1, -2) %>%
      drop_na(value) %>%
      mutate(value = as.numeric(value), date = as.Date(as.integer(date), origin = "1899-12-30")) %>%
      filter(date <= end_date) %>%
      mutate(date = as.Date(paste(substr(date, 1, 7), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      arrange(date) %>%
      complete(date = seq.Date(min(date), max(date), by = "month"))
    starti <- which(grepl(pull(datai[1, "date"]), tmp$date))
    tmp[starti:(starti + nrow(datai) - 1), i + 1] <- datai$value
  }
  
  return(tmp %>% select(-1))
}