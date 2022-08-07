# library
library(shiny)
library(bslib)
library(dashboardr)
library(readxl)
# dashboardr dataframe

library(readxl)
layout_dashboardr <- read_excel("www/model_dashboardr.xlsx", sheet = "layout")

layout_dashboardr$order <- 1:dim(layout_dashboardr)[1]

infobox_dashboardr <- read_excel("www/model_dashboardr.xlsx", sheet = "infobox")

table_dashboardr <- read_excel("www/model_dashboardr.xlsx", sheet = "table")

module_dashboardr <- read_excel("www/model_dashboardr.xlsx", sheet = "module")


df <-merge(
  layout_dashboardr,
  infobox_dashboardr,
  by.x="id",
  by.y="id",
  all.x=TRUE)

df <-merge(
  df,
  table_dashboardr,
  by.x="id",
  by.y="id",
  all.x=TRUE)

df <-merge(
  df,
  module_dashboardr,
  by.x="id",
  by.y="id",
  all.x=TRUE)

df <- df[order(df$order),]


# theme
theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "pulse",
)

# example module part ui
# counterButton <- function(id, label = "Counter") {
#   ns <- NS(id)
#   tagList(
#     actionButton(ns("button"), label = label, class = "btn-danger"),
#     p(textOutput(ns("out"), inline = FALSE))
#   )
# }

counterButton <- function(id, label = "Counter") {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("essai"))
  )
}


# example module part server
# counterServer <- function(id) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       count <- reactiveVal(0)
#       observeEvent(input$button, {
#         count(count() + 1)
#       })
#       output$out <- renderText({
#         count()
#       })
#       count
#     }
#   )
# }

counterServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$essai <- DT::renderDataTable({
        print("test")
    mtcars}
      )
    }
  )
}

