# Helper function to reshape wide format results back to long
reshape_to_long <- function(dat, format_info) {
  if (is.null(format_info)) return(dat)

  # If input was a single vector, convert to data frame
  if (!is.data.frame(dat)) {
    dat <- data.frame(value = dat)
  }

  # Reshape to long format
  long_dat <- data.frame(
    value = unlist(dat),
    group = rep(format_info$groups, each = nrow(dat))
  )
  names(long_dat) <- c(format_info$value_col, format_info$grp_var)

  return(long_dat)
}
