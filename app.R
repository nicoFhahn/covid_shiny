library(leaflet)
library(plotly)
library(dotenv)
library(shiny)
library(fresh)
library(readr)
library(sf)
library(plotly)
library(stringr)
library(highcharter)
library(htmltools)
library(quantmod)
library(leaflet.extras)
library(shinyanimate)
library(shinyjs)
# load the data
source(file.path("server","load_data.R"), local = TRUE)
source(file.path("server","create_story.R"), local = TRUE)
# load the api key for mapbox
try(load_dot_env("key.env"), silent = TRUE)
key <- Sys.getenv("MAPBOX_KEY")
# load the interface
ui <- source(file.path("ui", "ui.R"), local = TRUE)$value
# load the server
server <- function(input, output, session) {
  source(file.path("server", "server.R"), local = TRUE)$value
}
shinyApp(ui, server)