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
    "id", "type", "data", "row", "section",
    # row
    "row_title",
    "row_title_align",
    "row_title_color",
    # section
    "sect_width", "sect_width_sm", "sect_width_md",
    "sect_width_lg", "sect_width_xl", "sect_title",
    "sect_footer", "sect_title_align", "sect_footer_align",
    "sect_title_color", "sect_title_bgcolor", "sect_footer_bgcolor",
    "sect_tlp_msg", "sect_tlp_color", "sect_tlp_position",
    # nav
    "nav_title", "nav_icon_name", "nav_icon_lib",
    # el
    "el_height",
    # indicator
    "indic_value", "indic_value_color", "indic_value_prefix",
    "indic_value_suffix", "indic_ref", "indic_ref_relative",
    "indic_ref_invcolor", "indic_ref_format", "indic_ref_pos",
    "indic_gauge_color", "indic_gauge_min", "indic_gauge_max",
    "indic_gauge_step_val", "indic_gauge_step_color",
    "indic_gauge_prefix", "indic_gauge_suffix",
    # module
    "mod_ui", "mod_server",
    # plot
    "x",
    "y"
  )
  # default value
  unique_id <- unique(df$id)
  default_df <- data.frame("id" = unique_id, stringsAsFactors = FALSE)
  for (k in all_var) {
    default_df[, k] <- NA
  }
  default_df$row <- 1:dim(default_df)[1]
  default_df$section <- 1
  # keep absent columns
  var_keep <- c("id", all_var[!all_var %in% colnames(df)])
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
