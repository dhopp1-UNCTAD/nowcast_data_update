get_hk_ports <- function (url, catalog, g, start_date, end_date, historical) {
  # info on the API: https://data.gov.hk/en/help/api-spec#historicalAPI, https://data.gov.hk/en-data/dataset/hk-thb-thb-mpb/resource/a1c00d18-25b4-4f36-8724-a112fd1373e3
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  # want the "Total ( '000 TEUs)" column
  total_teu_col_num <- 5
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  history <- historical[,c("year", "month", vars$code)]
  history["date"] <- as.Date(paste(history$year, history$month, "01",sep="-"))
  tmp <- tmp %>% 
    left_join(history, by="date") %>%
    select(c(1, 5))
  colnames(tmp) <- c("date", vars$code)
  
  status <- tryCatch({
    rawdata <- GET(url) %>% content %>% as_tibble
    TRUE },
    error = function(e) { FALSE })
  if (status) {
    data <- rawdata %>%
      filter(month != "All") %>%
      select(c(1:2, total_teu_col_num)) %>%
      rename(value=3, year=Year) %>%
      mutate(month = sapply(month, function(x) which(x == months)))
    data["date"] <- as.Date(paste(data$year, data$month, "01",sep="-"))
    starti <- which(grepl(pull(data[1, "date"]), tmp$date))
    tmp[starti:(starti + nrow(data) - 1), 2] <- data$value
    
    return(tmp %>% select(-1))
  } 
}