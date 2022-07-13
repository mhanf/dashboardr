library(shiny)
library(bslib)
library(dashboardr)
library(readxl)

df <- read_excel("www/data_dashboardr.xlsx")

theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "pulse"
)
