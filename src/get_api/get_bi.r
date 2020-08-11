get_bi <- function (url, catalog, g, start_date, end_date) {
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date) %>% select(date)
  
  status <- tryCatch({
    uastring <- "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.22 (KHTML, like Gecko) Ubuntu Chromium/25.0.1364.160 Chrome/25.0.1364.160 Safari/537.22"
    tmps <- tempfile()
    withr::with_options(list(HTTPUserAgent=uastring), download.file(url, tmps))
    unzip(tmps)
    data <- read_excel("SK.xlsx")
    unlink(tmps)
    file.remove("SK.xlsx")
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    first_row <- 6
    first_col <- 5
    last_col <- ncol(data) - 4 # last 4 cols don't have data
    first_date <- as.Date("2012-01-01")
    add.months= function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]
    series <- data[first_row, first_col:last_col] %>% as.numeric
    dates <- c()
    for (i in 1:length(series)) {
      dates <- append(dates, add.months(first_date, i-1))
    }
    final_data <- data.frame(date=dates, series=series)
    colnames(final_data) <- c("date", vars$code[1])
    tmp <- tmp %>% 
      left_join(final_data, by="date")
    
    return(tmp %>% select(-1))
  }
}