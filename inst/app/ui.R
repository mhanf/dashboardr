ui <- shiny::bootstrapPage(
  theme = theme,

  #waiter::autoWaiter(),
  actionButton("click", "Click"),
  div(
    class = "d-flex justify-content-center",
    div(
      class = "col-12 col-md-11",
      # mod ui
      mod_dashboardr_ui(id = "dashboard1", df = df)
    )
  )
)
