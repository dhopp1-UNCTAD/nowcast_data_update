get_manual <- function (catalog, g, start_date, end_date, historical) {
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  history <- historical[,c("year", "month", vars$code)]
  history["date"] <- as.Date(paste(history$year, history$month, "01",sep="-"))
  tmp <- tmp %>% select(date) %>%
    left_join(history %>% select(-year, -month), by="date")
  
  return(tmp %>% select(-1))
}