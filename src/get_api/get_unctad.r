open_7z <- function (series_name) {
  version <- str_interp("https://unctadstat-api.unctad.org/api/reportMetadata/${series_name}/bulkfile/") %>% 
    GET() %>% 
    .$content %>% 
    rawToChar() %>% 
    fromJSON() %>% 
    .$version
  file_id <- str_interp("https://unctadstat-api.unctad.org/api/reportMetadata/${series_name}/${version}/bulkfiles/en") %>% 
    GET() %>% 
    .$content %>% 
    rawToChar() %>% 
    fromJSON() %>% 
    .$fileId
  url <- str_interp("https://unctadstat-api.unctad.org/api/reportMetadata/${series_name}/${version}/bulkfile/${file_id}/en")
  
  file_name_like <- str_replace(series_name, "\\." , "_")
  
  tmps <- tempfile(fileext = ".7z")
  download.file(url, tmps, quiet = T, mode = "wb")
  tmps_path <- str_split(tmps, "/")
  tmps_file <- str_split(tmps, "/")[[1]][length(tmps_path[[1]])]
  tmps_path <- paste0(paste(tmps_path[[1]][1:length(tmps_path[[1]])-1], collapse="/"), "/")
  # unzip the file
  system(str_interp('cd ${tmps_path} && 7z x ${tmps_file} -aoa > nul && rm nul'))
  for (csv_file in list.files(tmps_path)) {
    if (grepl(file_name_like, csv_file)) {
      rawdata <- read.csv(paste0(tmps_path, csv_file), header=T, stringsAsFactors=F)
      system(str_interp('rm ${paste0(tmps_path, csv_file)}'))
    }
  }
  unlink(tmps)
  return (rawdata)
}

get_unctad <- function (url, catalog, g, which_time, start_date, end_date) {
  vars <- gen_vars(catalog, g)
  tmp <- gen_tmp(vars, start_date, end_date)
  file_name_like <- url
  
  if (which_time == "m") {
    date_transform <- function (x) { as.Date(paste(substr(x, 1, 4), substr(x, 6, 7), "01", sep = "-"), format = "%Y-%m-%d") }
  } else {
    date_transform <- function (x) { as.Date(paste(substr(x, 1, 4), as.integer(substr(x, 7, 7)) * 3, "01", sep = "-"), format = "%Y-%m-%d") }
  }
  
  status <- tryCatch({
    # if jimhester/archive library is able to be installed
    if (F) {
      tmps <- tempfile(fileext = ".7z")
      download.file(url, tmps, quiet = T, mode = "wb")
      rawdata <- read.csv(archive_read(tmps), header = T, stringsAsFactors = F)
      unlink(tmps) 
    } else {
      rawdata <- open_7z(file_name_like)
    }
    TRUE },
    error = function(e) { FALSE })
  if (status) {
    if (which_time == "m") {
      data <- rawdata %>%
        as_tibble() %>%
        select(Period, commodity = CommodityProduct.Label, value = Index.Base.2015) %>%
        filter(commodity == "All groups") %>%
        mutate(date = date_transform(Period)) %>%
        filter(date >= start_date) %>%
        arrange(date)
      starti <- which(grepl(pull(data[1, "date"]), tmp$date))
      tmp[starti:(starti + nrow(data) - 1), 2] <- data$value  
    } else {
      data <- rawdata %>%
        as_tibble() %>%
        select(Period = Quarter, country = Economy.Label, flow = Flow.Label, value = Volume.Index..seasonally.adjusted..2005.100.) %>%
        filter(country == "World" & flow == "Exports") %>%
        mutate(date = date_transform(Period)) %>%
        filter(date >= start_date) %>%
        arrange(date)
      starti <- which(grepl(pull(data[1, "date"]), tmp$date))
      tmp[seq(from = starti, to = starti + nrow(data)*3 - 1, by = 3), 2] <- data$value
    }
    
    return(tmp %>% select(-1))
  }
}