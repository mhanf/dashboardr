
ui <- bootstrapPage(
  theme = theme,
  shinyjs::useShinyjs(),
  actionButton("click", "Click"),
# dashboard ui
  mod_dashboard_ui("dashboard",
                   df = df,
                   r = NULL,
                   pattern = "^%r%"
                   )


)





# ui <- shiny::bootstrapPage(
#   theme = theme,
#   shinyjs::useShinyjs(),
#   actionButton("click", "Click"),
#
#   div(class = "d-flex d-row justify-content-center w-100",
#     div(class = "col-12 col-md-11 bg-light", style = "height:200px",
#         # mod_gauge_ui(id = "gauge",
#         #                df,
#         #                r = r,
#         #                pattern = "^%r%"
#         # )
#         mod_infobox_ui(id = "infobox",
#                        df,
#                        r = r,
#                        pattern = "^%r%"
#                        )
#       )
#   )
# )
