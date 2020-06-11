get_data <- function (start_date, end_date, helper_directory, output_directory, get_api_directory, groups="All") {
  catalog <- read_csv(paste0(helper_directory,"catalog.csv"), col_types=cols())
  countries <- read_csv(paste0(helper_directory, "country_codes.csv"), col_types=cols())
  historical <- read_csv(paste0(helper_directory, "historical.csv"), col_types=cols())
  eikon <- read_excel(paste0(helper_directory, "Eikon.xlsx"), skip=1, .name_repair = "minimal")
  most_recent_database <- read_csv(paste0(output_directory, "most_recent_database.csv"), col_types=cols())
  
  # initializing the database
  database <- seq(from = start_date, to = end_date, by = "month") %>%
    data.frame %>%
    rename(date=1)
  
  # initializing a log to keep track of download status
  log <- catalog %>%
    select(code, download_group) %>%
    mutate(status="Not run")
  
  ###
  ### Getting data
  begin_path <- get_api_directory
  files <- list.files(begin_path)
  files <- sapply(files, function(x) paste0(begin_path, x))
  sapply(files, source)
  # generating dictionary from catalog of api calls
  data_hash <- gen_data_hash(catalog)
  
  # function to generate the table of a g
  get_group <- function (g) {
    print(paste("Fetching group", g))
    url <-data_hash[[as.character(g)]][1]
    which_time <-data_hash[[as.character(g)]][2]
    data_source <- data_hash[[as.character(g)]][3]
    
    # getting api data
    if (data_source %in% c("oecd", "eurostat", "imf")) {
      tmp <- get_api(url, catalog, g, countries, which_time, data_source, start_date, end_date)
    } else if (data_source == "fred") {
      tmp <- get_fred(url, catalog, g, start_date, end_date)
    } else if (data_source == "nbs") {
      # if nbs is split in 2 tables, get the 2nd too
      if (has.key(paste0(as.character(g),"b"), data_hash)) {
        url2 <- data_hash[[paste0(as.character(g),"b")]][1]
        tmp <- get_nbs_double(url, url2, catalog, g, start_date, end_date)
      } else {
        tmp <- get_nbs(url, catalog, g, start_date, end_date)
      }
    } else if (data_source == "hk nso") {
      tmp <- get_hk_nso(url, catalog, g, start_date, end_date, historical)
    } else if (data_source == "boj") {
      tmp <- get_boj(url, catalog, g, start_date, end_date)
    } else if (data_source == "ecb") {
      tmp <- get_single_api(url, catalog, g, which_time, data_source, start_date, end_date, "obsTime", "obsValue")
    } else if (data_source == "wto") {
      tmp <- get_single_api(url, catalog, g, which_time, data_source, start_date, end_date, "date", "Dataset.Value")
    } else if (data_source == "cpb") {
      tmp <- get_cpb(url, catalog, g, countries, start_date, end_date)
    } else if (data_source == "banxico")  {
      tmp <- get_banxico(url, catalog, g, start_date, end_date)
    } else if (data_source == "ec") {
      tmp <- get_ec(url, catalog, g, start_date, end_date)
    } else if (data_source == "rwi/isl") {
      tmp <- get_rwi(url, catalog, g, start_date, end_date)
    } else if (data_source == "meti") {
      tmp <- get_meti(url, catalog, g, start_date, end_date)
    } else if (data_source == "ons") {
      tmp <- get_ons(url, catalog, g, start_date, end_date)
    } else if (data_source == "ibge") {
      tmp <- get_ibge(url, catalog, g, start_date, end_date)
    } else if (data_source == "nso_tr") {
      tmp <- get_nso_tr(url, catalog, g, start_date, end_date)
    } else if (data_source == "nso_sg") {
      tmp <- get_nso_sg(url, catalog, g, start_date, end_date)
    } else if (data_source == "lhr") {
      tmp <- get_lhr(url, catalog, g, start_date, end_date)
    } else if (data_source == "hkg") {
      tmp <- get_hkg(url, catalog, g, start_date, end_date)
    } else if (data_source == "unctad") {
      tmp <- get_unctad(url, catalog, g, which_time, start_date, end_date)
    } else if (data_source == "eikon") {
      tmp <- get_eikon(catalog, g, start_date, end_date, eikon)
    } else if (data_source == "hk_ports") {
      tmp <- get_hk_ports(url, catalog, g, start_date, end_date, historical)
    } else if (data_source == "pa_canal") {
      tmp <- get_pa_canal(url, catalog, g, start_date, end_date, historical)
    } else if (data_source == "suez_canal") {
      tmp <- get_suez_canal(url, catalog, g, start_date, end_date, historical)
    } else if (data_source == "mem") {
      tmp <- get_mem(url, catalog, g, start_date, end_date, historical)
    } else if (data_source == "la_port") {
      tmp <- get_la_port(url, catalog, g, start_date, end_date, historical)
    } else {
      tmp <- get_manual(catalog, g, start_date, end_date, historical)
    }
      
    return(tmp)
  }
  # -1 for the nbs double entry, try multiple times
  if ((groups == "All")[1]) {
    groups <- 1:(length(data_hash)-1)
  }
  n_tries <- 1
  while (n_tries <= 5) {
    for (g in groups) {
      skip <- FALSE
      # only try if not already updated
      if (log %>% filter(download_group == g) %>% select(status) %>% slice(1) %>% pull == "Not run"){
        tryCatch({
          start_time <- Sys.time()
          
          tmp <- get_group(g)
          database <- cbind(database,tmp)
          
          end_time <- Sys.time()
          time_took <- difftime(end_time, start_time, units='mins')[[1]]
          log[log$download_group == g, "status"] <- str_interp("Took ${round(time_took, 2)} minutes for ${end_date}")
        }, error = function(e) {
          skip <<- TRUE
          print(paste0("Error getting group ", g, ", try ", n_tries))
        })
        if (skip) {next}
      }
    }
    n_tries <- n_tries + 1
  }
  
  # merging with most_recent_database in case some columns weren't successfuly gotten, or only a subset is gotten
  if (ncol(database) > 1) {
    cols_in_current_db <- colnames(database)[2:length(colnames(database))]
    cols_in_most_recent_db <- colnames(most_recent_database)
    final_database <- database %>%
    left_join(most_recent_database[, !cols_in_most_recent_db %in% cols_in_current_db], by="date")
  } else {
    return("No data was updated")
  }
  
  # writing final new database
  write.csv(final_database, paste0(output_directory,"most_recent_database.csv"), row.names=F)
  write.csv(final_database, paste0(output_directory,end_date,"_database.csv"), row.names=F)
  write.csv(log, paste0(output_directory,end_date,"_log.csv"), row.names=F)
  
  # writing to history too to see what variables were in the past
  write.csv(final_database, paste0(output_directory, "comparison_history/", Sys.Date(),"_database.csv"), row.names=F)
  
  # udpating historical.csv for necessary series
  historical_cols <- colnames(historical)[3:length(colnames(historical))]
  tmp <- data.frame(year=as.numeric(substr(final_database$date, 1, 4)), month=as.numeric(substr(final_database$date, 6, 7)))
  tmp <- cbind(tmp, final_database[,historical_cols])
  write.csv(tmp, paste0(helper_directory, "historical.csv"), row.names=F)
  
  # success message
  output_text <- str_interp("Success! Got new data ending on ${end_date}. New database file written to: ${paste0(output_directory,end_date,'_database.csv')}. View log at: ${paste0(output_directory,end_date,'_log.csv')}.")
  cat(str_interp("\033[0;32m${output_text}\033[0m\n"))
  failures <- read_csv(paste0(output_directory, end_date, "_log.csv")) %>% filter(status == "Not run") %>% select(download_group) %>% unique %>% as.character %>% str_replace('[c|"]', "")
  failures <- paste0("Groups ", failures, " were not successfully updated.")
  cat(str_interp("\033[0;33m${failures}\033[0m\n"))  
}
