get_la_port <- function (url, catalog, g, start_date, end_date, historical) {
  months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  total_exports_col <- 7
  total_teus_col <- 8
  year <- substr(as.Date(end_date), 1, 4) %>% as.numeric
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  history <- historical[,c("year", "month", vars$code)]
  history["date"] <- as.Date(paste(history$year, history$month, "01",sep="-"))
  tmp <- tmp %>% 
    left_join(history, by="date") %>%
    select(c(1, 6, 7))
  colnames(tmp) <- c("date", vars$code)
  
  final_url <- str_interp("${url}${year}")
  
  file <- read_html(final_url)
  tables <- html_nodes(file, "table")
  data <- html_table(tables[1], fill = TRUE)
  data <- data[[1]] %>% tibble
  data <- data[[1]]
  
  all_data <- data %>%
    select(1, total_exports_col, total_teus_col) %>%
    rename(month=1, container_exports_la=2, container_total_la=3) %>%
    slice(2:(n()-2)) %>%
    filter(month %in% months) %>%
    mutate(
      month = sapply(month, function(x) which(x == months)),
      container_exports_la = as.numeric(str_replace_all(container_exports_la, ",", "")),
      container_total_la = as.numeric(str_replace_all(container_total_la, ",", ""))
    )
  
  all_data["date"] <- as.Date(paste(year, all_data$month, "01",sep="-"))
  for (row_date in all_data$date) {
    tmp[tmp$date == row_date,vars$code[1]] <- all_data[all_data$date == row_date, vars$code[1]]
    tmp[tmp$date == row_date,vars$code[2]] <- all_data[all_data$date == row_date, vars$code[2]]
  }
  
  return(tmp %>% select(-1))
}