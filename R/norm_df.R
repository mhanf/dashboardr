
norm_df <- function(df, grp_var = "ui") {
  # Data frame formatting
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df$order <- 1:dim(df)[1]
  # build of complete dataframe
  grp_var <- extract_var_name()[[grp_var]]
  # default value
  unique_id <- unique(df$id)
  default_df <- data.frame("id" = unique_id, stringsAsFactors = FALSE)
  for (k in grp_var) {
    default_df[, k] <- NA
  }
  # keep absent columns
  var_keep <- c("id", grp_var[!grp_var %in% colnames(df)])
  # merging
  if (length(var_keep) != 1) {
    default_df <- default_df[, var_keep]
    df <- merge(df, default_df, all.x = TRUE, by = "id")
  }
  #  ordering id in the final dataframe
  df <- df[order(df$order), ]
  df <- df[, colnames(df) != "order"]
  # automatic conversion of factor var in character var (necessary for R version < 4)
  df[sapply(df, is.factor)] <- lapply(df[sapply(df, is.factor)], as.character)
  # transform all columns in character
  df[] <- lapply(df, as.character)
  # return
  return(df)
}
