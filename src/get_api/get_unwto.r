get_unwto <- function (url, catalog, g, start_date, end_date, historical) {
  non_na <- function(x, y) {
    if (!is.na(x)) {
      return (x)
    } else {
      return (y)
    }
  }
  months <- c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec.")
  pdf_regions <- c("North-East Asia", "South-East Asia", "World")
  
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date) %>% select(date)
  persistant_tmp <- tmp
  
  var_names <- vars$code[1:length(vars$code)] # to coincide with pdf_regions
  
  # edition numbers go by year, 2020=18, 2019=17, etc.
  offset <- 2002
  year <- as.numeric(substr(end_date, 1, 4))
  edition <- year - offset
  # count back from this to find the most recent barometer for the year
  n <- 10
  success <- FALSE
  while (!success & n > 0) {
    test_url <- str_interp("${url}${substr(end_date, 1, 4)}.${edition}.1.${n}?download=true")
    success <- tryCatch({
      tmps <- tempfile()
      download.file(test_url, tmps, quiet = T, mode = "wb")
      rawdata <- pdf_text(tmps)
      tables <- extract_tables(tmps)
      unlink(tmps)
      TRUE },
      error = function(e) {
        FALSE })
    n <- n - 1
  }
  
  if (success) {
    for (region in pdf_regions) {
      # 1st page where arrivals data is
      arriv_page <- rawdata[which(sapply(rawdata, function(x) grepl("International Tourist Arrivals by (Sub)region", x, fixed=TRUE)) == TRUE)[1]]
      location <- str_locate_all(arriv_page, region)[[1]][1]
      data_row <- substr(arriv_page, location, location + 300)
      data_row <- str_split(data_row, "\n")[[1]][1]
      data_values <- str_split(data_row, " ")
      validation_sum <- sum(sapply(data_values[[1]], function(x) as.numeric(str_replace(x, ",", ""))), na.rm=TRUE) # want to make sure the extracted table is the right one
      
      # table info
      for (table in tables) {
        df <- data.frame(table)
        sub_df <- df[df$X1 == region,]
        if (
            (nrow(sub_df) > 0) & # the region exists in the table
            ((sub_df[,2:ncol(sub_df)] %>% t %>% array() %>% str_replace(",", "") %>% as.numeric %>% sum()) == validation_sum) # has the same values as the validation row
        )  {
          df <- cbind(df[,1], df[,sapply(df[1,], function(x) x %in% months)])
          vals <- df[df[,1] == region,2:ncol(df)] %>% t %>% as.numeric
          dates <- df[1,2:ncol(df)] %>% t %>% array %>% sapply(function(x) which(months == x)) %>% array
          region_df <- data.frame(date=dates, value=vals) %>% 
            mutate(date = as.Date(paste0(year, "-", date, "-01")))
          colnames(region_df) <- c("date", var_names[which(region == pdf_regions)])
          tmp_historical <- historical %>% mutate(date=as.Date(paste0(year, "-", month, "-01"))) %>% select(date, var_names[which(region == pdf_regions)])
          tmp_i <- persistant_tmp %>% 
            left_join(region_df, by="date") %>% 
            left_join(tmp_historical, by="date")
          for (i in 1:nrow(tmp_i)) {
            tmp_i[i,2] <- non_na(tmp_i[i,2], tmp_i[i,3])
          }
          tmp_i <- tmp_i %>% select(1, 2)
          colnames(tmp_i) <- c("date", var_names[which(region == pdf_regions)])
          
          tmp <- tmp %>% 
            left_join(tmp_i, by="date")
          
          break
        }
      }
    }
    return (tmp %>% select(-1))
  }
}