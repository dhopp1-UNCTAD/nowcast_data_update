get_suez_canal <- function (url, catalog, g, start_date, end_date, historical) {

  months <- c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec.")
  year <- substr(as.Date(end_date), 1, 4) %>% as.numeric
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  history <- historical[,c("year", "month", vars$code)]
  history["date"] <- as.Date(paste(history$year, history$month, "01",sep="-"))
  tmp <- tmp %>% 
    left_join(history, by="date") %>%
    select(c(1, 5))
  colnames(tmp) <- c("date", vars$code)
  
  total_col <- 6 # which column the desired total is in in the pdf
  url <- str_interp("${url}${year}")
  
  all_data <- data.frame(year=2000, month=1, transit_suez_canal=0)
  for (month in months) {
    skip <- FALSE
    tryCatch({
      filename <- str_interp("${month} ${year}.pdf")
      final_url <- str_interp("${url}/${filename}") %>% str_replace_all(" ", "%20")
      pdf <- pdf_text(final_url)
      total_start <- str_locate(pattern="Total", pdf[2])[[2]]
      total <- substr(pdf[2], total_start+1, total_start + 100) %>% str_replace_all(" ", "\\|")
      # splitting into columns (replacing differing number of spaces with just one)
      total <- gsub("([\\|])\\1{1,}", "\\1", total) %>% strsplit("\\|")
      value <- total[[1]][total_col] %>% as.numeric
      
      data <- data.frame(year=year, month=which(month == months), transit_suez_canal=value)
      all_data <- all_data %>%
        rbind(data)
      
      print(month)
    }, error = function(e) {skip <<- TRUE}
    )
    if (skip) {next}
  }
  
  all_data <- all_data %>% slice(2:n())
  all_data["date"] <- as.Date(paste(all_data$year, all_data$month, "01",sep="-"))
  for (row_date in all_data$date) {
    tmp[tmp$date == row_date,2] <- all_data[all_data$date == row_date, vars$code]
  }
  
  return(tmp %>% select(-1))
}