library(tidyverse)
library(nowcasting)

data <- read_csv("/home/danhopp/dhopp1/UNCTAD/nowcasts/output/2020-06-01_database_tf.csv") %>% 
  slice(2:nrow(.))
catalog <- read_csv("/home/danhopp/dhopp1/UNCTAD/nowcasts/helper/catalog.csv")
target <- "x_world.sa"

start_list <- c(2002, 1)
end_list <- c(2020, 6)
tmp <- data
if (target == "x_world.sa") {
  target_col <- "octave_value"  
} else if (target == "x_vol_world2.sa") {
  target_col <- "octave_volume"
} else {
  target_col <- "octave_services"
}
targets <- c()

xs <- catalog %>%
  filter(!is.na(!!sym(target_col))) %>%
  select(!!sym(target_col)) %>%
  unique %>%
  pull
frequency <- catalog %>% 
  slice(lapply(xs, function (r) which(r==catalog[,target_col])[1]) %>% unlist) %>% 
  select(frequency) %>%
  pull
frequency <- lapply(frequency, function (r) if (r == "m") {12} else {4}) %>% unlist
blocks <- cbind(as.matrix(rep(1, length(frequency))), as.matrix(rep(1, length(frequency))))
x <- tmp[,xs]

x <- Bpanel(x, trans=rep(0, ncol(x)), na.prop=1, NA.replace=F)

# forecast
nowEM <- nowcast(formula = as.formula(paste0(target, "~ .")), data=x, r=2, p=2, method="EM", blocks=blocks, frequency=frequency)

forecast <- nowEM$yfcst %>% as.tibble