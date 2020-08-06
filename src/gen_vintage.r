library(tidyverse)
library(seasonal)

# generate historical vintage datasets based on publication lag
helper_directory <- "../helper/"
output_directory <- "../output/"
source("transform_data.r")
catalog <- read_csv(paste0(helper_directory, "catalog.csv"))
data <- read_csv(paste0(output_directory, "most_recent_database.csv"))

lag_date <- as.Date("2020-07-01")

lag_series <- function(data, col_name, which_date, catalog) {
   n_lag <- catalog %>% 
    filter(code == col_name) %>% 
    select(publication_lag) %>% pull
   last_row <- which(data$date == which_date) - n_lag
   series <- data[,col_name] %>% pull
   series[last_row:nrow(data)] <- NA
   return (series)
}

for (col in colnames(data)[2:ncol(data)]) {
  data[,col] <- lag_series(data, col, lag_date, catalog)
}
data <- data %>% 
  filter(date <= lag_date)
write_csv(data, paste0(output_directory, lag_date, "_database.csv"))

transform_data(lag_date, output_directory)