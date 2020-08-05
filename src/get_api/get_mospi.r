get_mospi <- function (url, catalog, g, start_date, end_date) {
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date) %>% select(date)
  
  status <- tryCatch({
    tmps <- tempfile()
    download.file(url, tmps, quiet = T, mode = "wb")
    rawdata <- read_excel(tmps)
    unlink(tmps)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata
    date_row <- 5
    general_row <- 32
    first_col <- 4 # apr 2012
    
    dates <- data[date_row, first_col:ncol(data)] %>% as.numeric %>% as.Date(origin="1899-12-30")
    index <- data[general_row, first_col:ncol(data)] %>% as.numeric
    final_data <- data.frame(date=dates, index=index)
    colnames(final_data) <- c("date", vars$code[1])
    final_data <- final_data %>% 
      mutate(date = as.Date(paste0(substr(date, 1, 4), "-", substr(date, 6, 7), "-01")))
    
    tmp <- tmp %>% left_join(final_data, by="date")
    
    return(tmp %>% select(-1))
  }
}