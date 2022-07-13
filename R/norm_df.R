#' Function to normalize a dashboardr dataframe
#'
#' @param r r internal list (advanced use)
#' @param df A dashboardr dataframe
#'
#' @return A normalized dashboardr dataframe
#' @export

norm_df <- function(df, r) {

  # Data frame formatting
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df$order <- 1:dim(df)[1]
  # Construction of complete dataframe
  all_var <- c(
    # essential
    "id","type","data","row","card",
    # section
    "sect_width", "sect_width_sm", "sect_width_md",
    "sect_width_lg", "sect_width_xl", "sect_title",
    "sect_footer", "sect_title_align", "sect_footer_align",
    # plot
    "x",
    "y"
  )
  # default value
  unique_id <- unique(df$id)
  default_df<-data.frame("id" = unique_id, stringsAsFactors=FALSE)
  for (k in all_var){default_df[, k] <- NA }
  default_df$row <- 1:dim(default_df)[1]
  default_df$card <- 1
  # keep absent columns
  var_keep <- c("id", all_var[! all_var %in% colnames(df)])
  # merging
  if (length(var_keep) != 1) {
    default_df <- default_df[,var_keep]
    df <- merge(df, default_df, all.x = TRUE, by = "id")
  }
  #  ordering id in the final dataframe
  df <- df[order(df$order), ]
  df <- df[,colnames(df) != "order"]
  # automatic conversion of factor var in character var (necessary for R version < 4)
  df[sapply(df, is.factor)] <- lapply(df[sapply(df, is.factor)], as.character)
  # transform all columns in character
  df[] <- lapply(df, as.character)
  # return
  return(df)
}
