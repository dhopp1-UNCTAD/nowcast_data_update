library(tidyverse)
library(stringr)
library(pdftools)

total_col <- 6 # whic column the desired total is in in the pdf
year <- 2020
months <- c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec.")
url <- str_interp("https://www.suezcanal.gov.eg/English/Downloads/DownloadsDocLibrary/Navigation Reports/Monthly Reports/${year}")

all_data <- data.frame(year=2000, month=1, transit_suez_canal=0)
for (month in months) {
  skip <- FALSE
  tryCatch({
    filename <- str_interp("${month} ${year}.pdf")
    final_url <- str_interp("${url}/${filename}") %>% str_replace_all(" ", "%20")
    pdf <- pdf_text(final_url)
    total_start <- str_locate(pattern="Total", pdf[2])[[2]]
    total <- substr(pdf[2], total_start+1, total_start + 100) %>% str_replace_all(" ", "-")
    # splitting into columns (replacing differing number of spaces with just one)
    total <- gsub("([-])\\1{1,}", "\\1", total) %>% strsplit("-")
    value <- total[[1]][total_col] %>% as.numeric
    
    data <- data.frame(year=year, month=which(month == months), transit_suez_canal=value)
    all_data <- all_data %>%
      rbind(data)
    
    print(month)
  }, error = function(e) {skip <<- TRUE}
  )
  if (skip) {next}
}

all_data <- all_data %>% slice(2:n())