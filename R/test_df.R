#' Test the conformity of necessary variables in dashboardr dataframe
#'
#' @param check_var Vector of variables to test
#' @param df Dashboardr dataframe
#' @param r r internal value
#'
#' @return several error messages (if any)

test_df <- function(df, r = NULL, check_var) {
  # Test necessary var
  missing_var <- NULL
  lapply(check_var, function(i) {
    if (!i %in% colnames(df)) {
      missing_var <- c(missing_var, i)
    }
  })
  # error message for necessary var
  if (is.null(missing_var) == FALSE) {
    label <- ifelse(
      length(missing_var) > 1,
      "are necessary columns in dashboardr dataframe",
      "is a necessary column in dashboardr dataframe"
    )
    label <- sprintf("%s %s", missing_var, label)
    stop(label)
  }
  # control type unity by id
  unicity <- unique(df[, c("id", "type")])
  unicity <- names(table(unicity$id)[table(unicity$id) > 1])
  if (length(unicity) > 0) {
    stop(sprintf("Several types for id : %s", paste(unique(unicity), collapse = " & ")))
  }
  # control in check_var
  lapply(check_var, function(i) {
    if (TRUE %in% is.na(df[, i])) {
      stop(sprintf("NA not allowed in %s variable", i))
    }
  })
  # # control existing type
  exist_type <- df[, c("id", "type")]
  exist_type$exist <- exist_type$type %in% c("plot", "table", "module", "indicator", "infobox")
  exist_type <- exist_type$exist[exist_type$exist == FALSE]
  if (length(exist_type) > 0) {
    stop(sprintf("Unvalid type %s", paste(unique(exist_type$type), collapse = " & ")))
  }
}
