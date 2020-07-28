compare_value <- function (old, new, sensitivity) {
  if (is.na(old) & is.na(new)) {
    return (NA)
  } else if (is.na(old) & !is.na(new)) {
    return (2)
  } else if (!is.na(old) & is.na(new)) {
    return (3)
  } else if (old == 0 & new == 0) {
    return (0)
  } else if(old == 0 & new != 0) {
    return (1)
  } else if (abs(new - old) / old <= sensitivity) {
    return (0)
  } else if (abs(new - old) / old > sensitivity) {
    return (1)
  }
}

compare_dfs <- function (df1, df2, exclude_cols=c(), sensitivity=0.02) {
  comparison <- df2
  for (col in (colnames(df2))) {
    if (!(col %in% exclude_cols)) {
      if (!(col %in% colnames(df1))) {
        comparison[col] <- 2
      } else {
        comparison[col] <- mapply(function (x, y) compare_value(x, y, sensitivity), df1[,col], df2[,col])  
      } 
    }
  }
  return (comparison)
}