ui <- shiny::bootstrapPage(
  theme = theme,
  actionButton("click","Click"),

  div(class = "row justify-content-center",
      div(class="col-12 col-md-10",
  # mod ui
  mod_dashboardr_ui(id = "dashboard1", df = df)
  ))
)
