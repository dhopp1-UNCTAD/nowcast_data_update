compare_value <- function (old, new, sensitivity) {
  if (is.na(old) & is.na(new)) {
    return (NA)
  } else if (is.na(old) & !is.na(new)) {
    return (2)
  } else if (!is.na(old) & is.na(new)) {
    return (3)
  } else if (old == 0 & new == 0) {
    return (0)
  } else if(old == 0 & new != 0) {
    return (1)
  } else if (abs(new - old) / old <= sensitivity) {
    return (0)
  } else if (abs(new - old) / old > sensitivity) {
    return (1)
  }
}

compare_dfs <- function (df1, df2, exclude_cols) {
  comparison <- df2
  for (col in (colnames(df2))) {
    if (!(col %in% exclude_cols)) {
      if (!(col %in% colnames(df1))) {
        comparison[col] <- 2
      } else {
        comparison[col] <- mapply(function (x, y) compare_value(x, y, sensitivity), df1[,col], df2[,col])  
      } 
    }
  }
  return (comparison)
}

compare_data <- function (end_date, output_directory, sensitivity=0.2) {
  # writes comparison file with 0 for no change or change <= sensitivity, 1 for existed in old data but revised, 2 for new data, 3 for existed in old now NA, NA for doesn't exist in either
  most_recent <- read.csv(paste0(output_directory, "most_recent_database.csv")) %>% 
    mutate(date = as.Date(date))
  # database file from two ago (i.e. not most recent) is file with _database -2, one for "most_recent", one for dated most recent
  two_ago_file <- list.files(output_directory) %>%
    .[sapply(., function (x) grepl("_database", x))] %>%
    sort() %>%
    .[length(.)-2]
  two_ago <- read.csv(paste0(output_directory, two_ago_file)) %>% mutate(date=as.Date(date))
  two_ago <- data.frame(date=most_recent$date) %>%
    left_join(two_ago, on="date")
  comparison <- compare_dfs(two_ago, most_recent, c("date"))
  
  write.csv(comparison, paste0(output_directory,end_date,"_comparison.csv"), row.names=F)
  # success message
  output_text <- str_interp("Success! Compared data ending on ${end_date}. New comparison file written to: ${paste0(output_directory,end_date,'_comparison.csv')}.")
  cat(str_interp("\033[0;32m${output_text}\033[0m\n"))
}

mapply(function (x, y) compare_value(x, y, sensitivity), two_ago["fdi_world"], most_recent["fdi_world"])
colnames(most_recent)[21]