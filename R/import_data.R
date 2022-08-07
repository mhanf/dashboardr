#' Function to import data from excel spreadsheet
#' @param path path to the excel spreadsheet
#' @importFrom readxl read_excel
#' @importFrom gridlayout get_element_ids new_gridlayout
#' @return a dataframe of data from excel spreadsheet
#' @export

import_data <- function(path) {
  # init
  df <- list()
  # layout
  layout <- readxl::read_excel(path, sheet = "layout") |>
    dplyr::mutate_at(1, ~ tidyr::replace_na(., " ")) |>
    tidyr::unite("col_layout", 1:5, remove = TRUE, sep = " ")
  df$layout <- gridlayout::new_gridlayout(layout$col_layout)

  df$element <- gridlayout::get_element_ids(df$layout)
  df$ui <- readxl::read_excel(path, sheet = "ui")
  df$ui$order <- 1:dim(df$ui)[1]
  df$infobox <- readxl::read_excel(path, sheet = "infobox")
  df$table <- readxl::read_excel(path, sheet = "table")
  df$module <- readxl::read_excel(path, sheet = "module")
  df$gauge <- readxl::read_excel(path, sheet = "gauge")
  # norm df
  df$ui <- norm_df(df = df$ui, grp_var = "ui")
  # test df
  test_df(df = df)
  # return
  return(df)
}
