ui <- shiny::bootstrapPage(
  theme = theme,
  actionButton("click","Click"),
  # mod ui
  mod_dashboardr_ui(id = "dashboard1", df = df, r = NULL)
)
