# Function to find outliers
outlier <- function(x) {
  return(
    x < quantile(as.numeric(x), 0.25, na.rm = TRUE) - 1.5 * IQR(as.numeric(x), na.rm = TRUE) | 
      x > quantile(as.numeric(x), 0.75, na.rm = TRUE) + 1.5 * IQR(as.numeric(x), na.rm = TRUE))
}