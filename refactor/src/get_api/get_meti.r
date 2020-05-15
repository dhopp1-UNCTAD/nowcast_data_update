get_meti <- function (url, catalog, g, start_date, end_date) {
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  
  item_number <- str_split(url, "item_number")[[1]][2] %>% str_replace(":", "")
  url <- str_split(url, "item_number")[[1]][1] %>% substr(1, nchar(.)-1)
  
  status <- tryCatch({
    tmps <- tempfile()
    download.file(url, tmps, quiet = T, mode = "wb")
    rawdata <- read_excel(path = tmps, skip = 2, col_names = F)
    unlink(tmps)
    TRUE },
    error = function(e) {
      FALSE })
  if (status) {
    data <- rawdata %>%
      filter(...1 %in% c("Item_Number", item_number)) %>%
      t(.) %>%
      as_tibble(., .name_repair = "unique") %>%
      slice(-1:-3)
    colnames(data) <- c("date", "value")
    data <- data %>%
      mutate(date = as.Date(paste(substr(date, 1, 4), substr(date, 5, 6), "01", sep = "-"), format = "%Y-%m-%d")) %>%
      mutate(value = as.numeric(value))
    starti <- which(grepl(pull(data[1, "date"]), tmp$date))
    tmp[starti:(starti + nrow(data) - 1), 2] <- data$value
    
    return(tmp %>% select(-1))
  }
}