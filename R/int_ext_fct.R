#' Identify and compute internal/external graph characteristics
#'
#' @param x value
#' @param r intern list of variables
#' @param pattern default pattern to identify r code in dashboardr dataframe
#' @return real value (internal or external)

int_ext_fct <- function(x, r = NULL, pattern = "^%r%") {
  y <- as.character(x[1])
  # Evaluation of r code
  if (grepl(pattern, y)) {
    x <- tryCatch(
      {
        value <- trimws(substr(y, nchar(pattern), 100000)[1])
        value <- eval(parse(text = value))
        return(value)
      },
      error = function(cond) {
        message("R code in dashboardr dataframe is not valid")
        message("Original error message:")
        message(cond)
        return(NA)
      }
    )
  }
  # get it from c() pattern
  else if (grepl("(^c\\(.*\\))", y)) {
    x <- tryCatch(
      {
        value <- eval(parse(text = y[1]))
      },
      error = function(cond) {
        message("c() specification in dashboardr dataframe is not valid")
        message("Original error message:")
        message(cond)
        return(NA)
      }
    )
  }
  # get from local env with r
  else if (grepl("^r\\$", y)) {
    if (is.null(r)) {
      stop("Internal variable(s) but r is NULL")
    }
    if (gsub("^r\\$", "", y) %in% names(r)) {
      x <- eval(parse(text = y[1]))
    } else {
      stop(paste0("argument ", gsub("^r\\$", "", y), " does not exist in list r"))
    }
  }
  return(x)
}
