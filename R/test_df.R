#' Test the conformity of necessary variables in dashboardr dataframe
#'
#' @param df Dashboardr dataframe
#'
#' @return several error messages (if any)

test_df <- function(df) {
  # test ui
  if (TRUE %in% is.na(df$ui$id)) {
    stop("id can't be NA (ui variables)")
  }
  if (TRUE %in% is.na(df$ui$element)) {
    stop("element can't be NA (ui variables)")
  }
  if (TRUE %in% is.na(df$ui$type)) {
    stop("type can't be NA (ui variables)")
  }
}
