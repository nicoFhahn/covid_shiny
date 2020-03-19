library(leaflet)
library(shinyWidgets)
library(plotly)
library(dotenv)
library(shiny)
library(fresh)
library(readr)
library(sf)
library(plotly)
# load the data
source("server/load_data.r")
# load the api key for mapbox
try(load_dot_env("key.env"), silent = TRUE)
key <- Sys.getenv("MAPBOX_KEY")
ui <- source(file.path("ui", "ui.R"), local = TRUE)$value
# load the server
server <- function(input, output, session) {
  source(file.path("server", "server.R"), local = TRUE)$value
}

shinyApp(ui, server)