get_ita <- function (url, catalog, g, start_date, end_date) {
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date) %>% select(date)
  
  tmps <- tempfile()
  download.file(url,tmps)
  
  years <- c(2006:as.numeric(substr(end_date, 1, 4)))
  complete_string <- " COR Month-QTR Volume"
  latest_string <- " COR Month Volume"
  
  final_data <- data.frame(date=as.Date(character()), visits=integer())
  for (year in years) {
    if (year == years[length(years)]) {
      data <- read_excel(tmps, str_interp("${year}${latest_string}"))
    } else {
      # in case it has a trailing space
      tryCatch({
        data <- read_excel(tmps, str_interp("${year}${complete_string}"))  
      }, error = function(e) { error <<- TRUE })
      tryCatch({
        data <- read_excel(tmps, str_interp("${year}${complete_string} ")) 
      }, error = function(e) { error <<- TRUE })
    }
    
    which_row <- which(data[,2] == "TOTAL OVERSEAS")
    start_col <- 3 # jan, same for all tabs
    # last year has YTD, other dont
    if (year == years[length(years)]) {
      last_col <- which(grepl("YTD", data[1,]))[2] - 1 # second occurence of YTD in string
    } else {
      last_col <- 17
    }
    visits <- data[which_row, start_col:last_col][!grepl("Q", data[1, start_col:last_col])] %>% as.numeric # skip Q columns
    dates <- data[1, start_col:last_col][!grepl("Q", data[1, start_col:last_col])] %>% as.numeric %>% as.Date(origin="1899-12-30")
    tmp_final_data <- data.frame(date=dates,visits=visits)
    final_data <- rbind(final_data, tmp_final_data)
  }
  
  unlink(tmps)
  
  colnames(final_data) <- c("date", vars$code[1])
  tmp <- tmp %>% 
    left_join(final_data, by="date")
    
  return(tmp %>% select(-1))
}