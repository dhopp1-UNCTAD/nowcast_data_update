compare_data <- function(old_data, new_data, sensitivity) {
  ###
  ### Function to compare collected data with previous update
  ###
  ### Inputs:
  ###    - old_data: previous update (required, no default)
  ###    - new_data: new update (required, no default)
  ###    - sensitivity: threshold of what is considered a change, in percentage terms with respect to old value
  ###
  ### Outputs:
  ###    - compare: data file of the same size as new_data, with each cell taking one of four values
  ###               0 = no change with respect to old_data, or relative change < sensitivity
  ###               1 = data existed in old_data but it was revised (relative change >= sensitivity)
  ###               2 = new data
  ###               NA = data doesn't exist in new_data
  ###
  ### This version: Fernando Cantu, 2020-04-27
  ###
  
  if(nrow(old_data) < nrow(new_data)) {
    old_data <- rbind(old_data, matrix(NA, ncol = ncol(new_data), nrow = nrow(new_data) - nrow(old_data), 
                                       dimnames = list(NULL, names(new_data))))
    old_data$date <- new_data$date
  }
  log <- NULL
  
  compare <- new_data
  # For loop not the preferred solution, but couldn't find another way to do it
  for (j in 2:ncol(new_data)) {
    for (i in 1:nrow(new_data)) {
      new <- new_data[i, j]
      old <- old_data[i, j]
      compare[i, j] <- ifelse(is.na(old) & is.na(new), NA, 
                              ifelse(is.na(old) & !is.na(new), 2,
                                     ifelse(abs(new/old - 1) < sensitivity, 0, 
                                            ifelse(abs(new/old - 1) >= sensitivity, 1, 99999))))
      if (!is.na(compare[i, j]) & compare[i, j] > 0) {
        log <- rbind(log, c(colnames(compare)[j], as.character(compare[i, 1]), compare[i, j], old, new))
      }
    }
  }
  colnames(log) <- c("variable", "date", "change", "old value", "new value")
  
  return(list(compare = compare, changelog = log))
  
}
