# library
library(shiny)
library(bslib)
library(dashboardr)
library(readxl)
library(bsutils)
library(shinyjs)
library(gridlayout)
library(readxl)


df <- import_data(path = "www/model_dashboardr.xlsx")

# theme
theme <- bslib::bs_theme(version = 5, bootswatch = "flatly")
