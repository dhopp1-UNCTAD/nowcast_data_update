get_hkg <- function (url, catalog, g, start_date, end_date) {
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  status <- tryCatch({
    tmps <- tempfile()
    download.file(url, tmps, quiet = T, mode = "wb")
    rawdata <- read_excel(path = tmps, skip = 7, col_names = T, n_max=304)
    unlink(tmps)
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
      filter(!is.na(Month) & Month != "*" & Month != "# ?") %>%
      rowwise() %>%
      # drop the first row that's not a dat
      slice(2:n()) %>% 
      filter(!is.na(Month)) %>% 
      mutate(date = as.Date(paste(Year, which(months == Month), "01", sep = "-"))) %>%
      filter(date >= start_date) %>%
      select(date, value = "Total...14") %>%
      arrange(date)
    
    column_names <- colnames(tmp)
    tmp <- tmp %>% select(-2) %>%
      left_join(data, by="date")
    colnames(tmp) <- column_names

    return(tmp %>% select(-1))
  }
}