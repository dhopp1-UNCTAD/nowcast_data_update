# transform a url with correct start date
gen_url <- function (url, start) {
  if (nchar(start) == 0) {
    return(url)
  } else if (nchar(start) == 4) {
    return(paste0(url,start))
  } else {
    return(paste0(url,format(start, "%Y-%m")))
  }
}

# generate the data hash file from the catalog
# only do below # for now (where i have done)
gen_data_hash <- function (catalog) {
  data_hash <- hash()
  for (i in unique(catalog$download_group)) {
    if (i == "22b" | as.numeric(i) <= 33) {
      which_time <- catalog %>% filter(download_group == i) %>% select(frequency) %>% slice(1) %>% pull
      source <- catalog %>% filter(download_group == i) %>% select(source) %>% slice(1) %>% pull
      url <- catalog %>% filter(download_group == i) %>% select(url) %>% slice(1) %>% pull
      data_hash[[i]] <- c(url, which_time, source)
    }
  }
  return(data_hash)
}

# generate the variables for this group
gen_vars <- function(catalog, g) {
  catalog %>% filter(download_group == g)
}

# generate empty tmp table
gen_tmp <- function(vars, start_date, end_date) {
  as.data.frame(
    matrix(NA, 
           ncol = (nrow(vars) + 1), 
           nrow = length(seq(from = start_date, to = end_date, by = "month"))
    )
  ) %>%
    rename_at(vars(colnames(.)), ~c("date", vars$code)) %>%
    mutate(date = seq(from = start_date, to = end_date, by = "month"))
}
