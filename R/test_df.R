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

  for (k in check_var) {
    if (! k %in% colnames(df)) { missing_var <- c(missing_var, k) }
  }
  # error message for necessary var
  if (is.null(missing_var) == FALSE){
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
  # control NA in necessary var
  if (TRUE %in% is.na(df$id)) {stop("NA not allowed in id variable")}
  if (TRUE %in% is.na(df$type)) {stop("NA not allowed in type variable")}
  if (TRUE %in% is.na(df$data)) {stop("NA not allowed in data variable")}

  # # control existing type
  test1 <- df[, c("id", "type")]
  test1$test <- test1$type %in% c("plot", "table")
  test1 <- test1[test1$test == FALSE, ]
  if (dim(test1)[1] > 0) {
    stop(sprintf("Unvalid type %s", paste(unique(test1$type), collapse = " & ")))}
}
