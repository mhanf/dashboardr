
#' dashboard ui part
#'
#' @param id module id
#' @param df A dashboardr dataframe
#' @param r r internal list (advanced use)
#' @param pattern default pattern to identify r code in dashboardr dataframe
#' @importFrom shiny NS
#' @importFrom gridlayout grid_container
#' @return dashboard ui part

mod_dashboard_ui <- function(id,
                             df,
                             r = NULL,
                             pattern = "^%r%") {
  # ns
  ns <- shiny::NS(id)

  gridlayout::grid_container(
    flag_mismatches = FALSE,
    layout = df$layout,
    lapply(df$ui$element, function(i) {
      # element df
      df_el <- df$ui[df$ui$element == i, ]
      # parameter
      val <- def_ui_val(
        df = df_el,
        r = r,
        pattern = pattern
      )
      # grid card
      if (val$card == TRUE) {
        dash_card(
          area = i,
          val = val,
          r = r,
          pattern = pattern
        )
      } else {
        gridlayout::grid_place(
          area = i,
          element = paste("card", i)
        )
      }
    })
  )
}
