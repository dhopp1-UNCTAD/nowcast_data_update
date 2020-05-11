library(tidyverse)
library(rvest)

months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
year <- 2020
url <- "https://www.portoflosangeles.org/business/statistics/container-statistics/historical-teu-statistics-"
total_exports_col <- 7
total_teus_col <- 8

final_url <- str_interp("${url}${year}")

file <- read_html(final_url)
tables <- html_nodes(file, "table")
data <- html_table(tables[1], fill = TRUE)
data <- data[[1]] %>% tibble
data <- data[[1]]

all_data <- data %>%
  select(1, total_exports_col, total_teus_col) %>%
  rename(month=1, container_exports_la=2, container_total_la=3) %>%
  slice(2:(n()-2)) %>%
  filter(month %in% months) %>%
  mutate(
    month = sapply(month, function(x) which(x == months)),
    container_exports_la = as.numeric(str_replace_all(container_exports_la, ",", "")),
    container_total_la = as.numeric(str_replace_all(container_total_la, ",", ""))
    )