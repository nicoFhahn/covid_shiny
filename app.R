library(dotenv)
library(fresh)
library(highcharter)
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(quantmod)
library(readr)
library(sass)
library(sf)
library(shiny)
library(shinyanimate)
library(shinyWidgets)
library(stringr)
# load the data
source(file.path("server", "load_data.R"), local = TRUE)
source(file.path("server", "create_story.R"), local = TRUE)
# load the api key for mapbox
try(load_dot_env("key.env"), silent = TRUE)
key <- Sys.getenv("MAPBOX_KEY")
css <- sass(sass_file("styles.scss"))
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
