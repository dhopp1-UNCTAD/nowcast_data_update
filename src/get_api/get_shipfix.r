get_shipfix <- function (url, catalog, g, start_date, end_date) {
  # shipfix specific stuff
  # getting token
  shipfix_updated <- tryCatch({
    header <- "content-type:application/json"
    pword <- Sys.getenv("SHIPFIXPASS")
    data <- str_interp("'{\"email\":\"daniel.hopp@unctad.org\",\"password\":\"${pword}\"}'")
    auth_url <- "https://unctad.shipfix.com/api/rest/1.0/auth/login"
    token <- fromJSON(system(str_interp("curl --request POST -k --url ${auth_url} --header 'content-type:application/json' --data ${data}"), intern=T))$token
    
    
    # getting data
    # url <- "https://unctad.shipfix.com/api/rest/1.0/insights/files"
    header <- str_interp("authorization: Bearer ${token}")
    subdirectory <- "shipfix/"
    filename <- "data"
    system(str_interp("mkdir ${subdirectory}"))
    
    api_url <- "https://unctad.shipfix.com/api/rest/1.0/insights/files"
    response <- fromJSON(getURL(url=api_url, .opts = list(httpheader = header)))
    
    
    # has many days, take the latest
    file_url <- response$url[length(response$url)]
    
    system(str_interp("curl --request GET -k --url '${file_url}' --output ${paste0(subdirectory, filename)}.tar.gz"))
    system(str_interp("tar -C ${subdirectory} -zxvf ${paste0(subdirectory, filename)}.tar.gz"))
    system(str_interp("rm -r ${subdirectory}"))
    #system(str_interp("rm ${paste0(subdirectory, filename)}.tar.gz"))
    TRUE
  },
  error = function(e) {FALSE}
  )
  
  # normal data processing
  if (shipfix_updated) {
    vars <- gen_vars(catalog, g)
    tmp <- gen_tmp(vars, start_date, end_date) %>% select(date)
    
    all_files <- list.files(str_interp("${subdirectory}/history/"))
    which_file <- all_files[sapply(all_files, function(x) grepl("History_Tonnage_Daily_Aggregates", x))][1]
    file_path <- str_interp("${subdirectory}history/${which_file}")
    shipfix <- read_csv(file_path) %>% 
      rename(date=`Email date (first email)`, region_short=Region, region_long = `Region name`, tonnage = `Total Deadweight tonnage`) %>% 
      select(date, region_short, tonnage)
    for (region in c("DE", "NL", "CN", "HK", "US", "FR", "UK", "JP", "SG", "AE")) {
      if (region == "UK") {
        correction <- "GB" 
      } else {
        correction <- region
      }
      data <- shipfix %>% 
        filter(region_short == correction) %>% 
        mutate(date = as.Date(paste(substr(date, 1, 4), substr(date, 6, 7), "01", sep="-"))) %>% 
        group_by(date) %>% 
        summarise(tonnage = sum(tonnage)) %>% 
        data.frame
      colnames(data) <- c("date", paste0("shipfix_", tolower(region)))
      tmp <- tmp %>% left_join(data, by="date")
    }
    system(str_interp("rm -r ${subdirectory}"))
    
    return (tmp %>% select(-1))
  } else {
    system(str_interp("rm -r ${subdirectory}"))
  }
}