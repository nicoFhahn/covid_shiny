library(leaflet)
library(plotly)
library(dotenv)
library(shiny)
library(fresh)
library(readr)
library(sf)
library(stringr)
library(highcharter)
library(htmltools)
library(quantmod)
library(leaflet.extras)
library(shinyanimate)
library(shinyWidgets)
# load the data
source(file.path("server", "load_data.R"), local = TRUE)
source(file.path("server", "create_story.R"), local = TRUE)
# load the api key for mapbox
try(load_dot_env("key.env"), silent = TRUE)
key <- Sys.getenv("MAPBOX_KEY")
# load the interface
ui <- source(file.path("ui", "ui.R"), local = TRUE)$value
# load the server
server <- function(input, output, session) {
  source(file.path("server", "leaflet_maps.R"), local = TRUE)$value
  source(file.path("server", "event_observer.R"), local = TRUE)$value
  source(file.path("server", "reactive_events.R"), local = TRUE)$value
  source(file.path("server", "text_outputs.R"), local = TRUE)$value
  source(file.path("server", "ui_outputs.R"), local = TRUE)$value
  source(file.path("server", "plotly_outputs.R"), local = TRUE)$value
  source(file.path("server", "highcharter_outputs.R"), local = TRUE)$value
  source(file.path("server", "observer.R"), local = TRUE)$value
}
shinyApp(ui, server)
