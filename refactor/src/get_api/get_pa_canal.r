get_pa_canal <- function (url, catalog, g, start_date, end_date, historical) {

  months <- c("JANUARY","FEBRUARY","MARCH","APRIL","MAY","JUNE","JULY","AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER")
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  history <- historical[,c("year", "month", vars$code)]
  history["date"] <- as.Date(paste(history$year, history$month, "01",sep="-"))
  tmp <- tmp %>% 
    left_join(history, by="date") %>%
    select(c(1, 5))
  colnames(tmp) <- c("date", vars$code)
  
  n_files <- 20 # number of files to reget
  index_url <- str_interp("${url}index.html")
  html <- paste(readLines(index_url), collapse="\n")
  # all links from page
  matched <- str_match_all(html, "<a href=\"(.*?)\"")
  files <- matched[[1]][,2]
  # keeping only PDFs
  files <- files[files %>% sapply(function(x) str_sub(x, -3) == "pdf")] %>% unique
  
  all_data <- data.frame(year=2000, month=1, transit_pa_canal=0)
  # loop to get info from pdfs
  for (file_name in files[1:n_files]) {
    final_url <- paste(url, file_name, sep="")
    pdf <- pdf_text(final_url)
    
    necessary_text <- "Monthly Canal Operations Summary"
    if (grepl(necessary_text, pdf[1])) {
      raw_month <- str_locate_all(pattern =necessary_text, pdf[1])[[1]][2]
      raw_month <- substr(pdf[1], raw_month, raw_month + 100)
      month_start <- str_locate(pattern="–", raw_month)[[1]][1]
      month_end <- str_locate(pattern="\n", raw_month)[[1]][1]
      month <- substr(raw_month, month_start+2, month_end-1)
      
      year <- str_sub(month, -4) %>% as.numeric
      month <- which(str_sub(month, 1, nchar(month)-5) == months)
    
      location <- str_locate_all(pattern ='Total:', pdf[1])[[1]][2]
      figure_string <- substr(pdf[1], location, location + 100)
      figure <- as.numeric(gsub("[^[:alnum:]]","",figure_string))  
      
      data <- data.frame(year=year, month=month, transit_pa_canal=figure)
      all_data <- rbind(all_data, data)
    }
    print(file_name)
    Sys.sleep(2)
  }
  all_data <- all_data %>% slice(2:n()) %>% distinct
  all_data["date"] <- as.Date(paste(all_data$year, all_data$month, "01",sep="-"))
  for (row_date in all_data$date) {
    tmp[tmp$date == row_date,2] <- all_data[all_data$date == row_date,"transit_pa_canal"]
  }
  
  return(tmp %>% select(-1))
}