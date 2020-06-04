prepare_individual <- function (data, vars) {
  db <- data %>%
    select(vars) %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= start_date)
  if (db[nrow(db), "date"] < end_date) {
    db <- rbind(db, matrix(NA, ncol = ncol(db), 
                           nrow = length(seq.Date(from = db[nrow(db), "date"], to = end, by = "month")) - 1, 
                           dimnames = list(NULL, names(db))))
    db$date <- seq.Date(from = start, to = end, by = "month")
  }
  db <- db %>%
    replace(is.na(.), -99999)
  return (db)
}

prepare_data <- function(start_date, end_date, helper_directoryo, output_directory) {
  ###
  ### Function to prepare final databases for use in nowcasting program in Octave
  ### Current models used for variable selection
  ###
  ### Inputs:
  ###    - data: current update of the database, seasonally adjusted and transformed as required (required, no default)
  ###    - start: starting data for estimation purposes
  ###    - end: ending date for estimation purposes
  ###
  ### Outputs:
  ###    - data_nowcast_goods_value: database for nowcast model of global merchandise trade in values
  ###    - data_nowcast_goods_volume: database for nowcast model of global merchandise trade in volumes
  ###    - data_nowcast_services: database for nowcast model of global trade in services
  ###
  ### Note: this function is only used for the current configuration of the model. Once the new one is implemented,
  ###       it won't be necessary since it will be implemented directly in Octave
  ###
  ### This version: Fernando Cantu, 2020-04-28
  ###
  catalog <- read_csv(paste0(helper_directory, "catalog.csv"))
  data <- read_csv(paste0(output_directory, end_date, "_database_tf.csv"))
  
  # Global merchandise trade in value
  vars <- catalog %>% select(octave_value) %>% na.omit %>% unique %>% pull %>% prepend("date")
  data_nowcast_goods_value <- prepare_individual(data, vars)
  
  # Global merchandise trade in volume
  vars <- catalog %>% select(octave_volume) %>% na.omit %>% unique %>% pull %>% prepend("date")
  data_nowcast_goods_volume <- prepare_individual(data, vars)
  
  # Global trade in services
  vars <- catalog %>% select(octave_services) %>% na.omit %>% unique %>% pull %>% prepend("date")
  data_nowcast_services <- prepare_individual(data, vars)
  
  write_csv(data_nowcast_goods_value, paste0(output_directory, end_date, "_octave_value.csv"))
  write_csv(data_nowcast_goods_volume, paste0(output_directory, end_date, "_octave_volume.csv"))
  write_csv(data_nowcast_services, paste0(output_directory, end_date, "_octave_services.csv"))
}