library(tidyverse)
library(stringr)
library(pdftools)

months <- c(
  "JANUARY",
  "FEBRUARY", 
  "MARCH",
  "APRIL",
  "MAY",
  "JUNE",
  "JULY",
  "AUGUST",
  "SEPTEMBER",
  "OCTOBER",
  "NOVEMBER",
  "DECEMBER"
)
prefix <- "https://www.pancanal.com/common/maritime/advisories/"
url <- str_interp("${prefix}/index.html")
html <- paste(readLines(url), collapse="\n")
# all links from page
matched <- str_match_all(html, "<a href=\"(.*?)\"")
files <- matched[[1]][,2]
# keeping only PDFs
files <- files[files %>% sapply(function(x) str_sub(x, -3) == "pdf")]

all_data <- data.frame(year=2000, month=1, transit_pa_canal=0)
# testing
for (url in files) {
  test_one <- url
  test_url <- paste(prefix, test_one, sep="")
  pdf <- pdf_text(test_url)
  
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
  print(url)
}

all_data <- all_data %>% slice(2:n())
