get_ibge <- function (url, catalog, g, start_date, end_date) {
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  months <- seq(as.Date(start_date), as.Date(end_date), by = "month") %>%
    sapply(function (x) paste0(substr(x, 1, 4), substr(x, 6, 7))) %>%
    unname() %>%
    paste(collapse="|")
  url <- str_replace(url,"MONTHS", months)
  
  status <- tryCatch({
    rawdata <- GET(url) %>% 
      .$content %>% 
      rawToChar() %>% 
      fromJSON()
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata$resultados[[1]][[2]] %>% 
      data.frame() %>%
      select(starts_with("serie")) %>%
      t() %>%
      data.frame()
    data$date <- rownames(data)
    rownames(data) <- 1:nrow(data)
    data <- data %>%
      mutate(date = as.Date(paste0(substr(date, 7, 10), "-", substr(date, 11, 12), "-01"))) %>%
      select(date, X1) %>%
      rename(!!vars$code[1] := X1)
    
    tmp <- tmp %>%
      select(date) %>%
      left_join(data, by="date")
    
    return(tmp %>% select(-1))
  }
}