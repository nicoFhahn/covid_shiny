library(leaflet)
library(shiny)
library(fresh)
library(readr)
library(sf)
library(stringr)
library(plotly)
fillPage(
  title = "SARS-CoV-2 Outbreak 2020",
  tags$head(
    includeCSS("styles.css"),
    includeScript("gomap.js")
    ),
  use_googlefont("Oswald"),
  use_theme(create_theme(
    theme = "default",
    bs_vars_font(
      family_sans_serif = "'Oswald', cursive"
    )
  )),
  leafletOutput(
    "mymap",
    width = "100%",
    height = "100vh"
    ),
  absolutePanel(
    id = "controls",
    class = "panel panel-default",
    fixed = TRUE,
    draggable = TRUE,
    top = 40,
    left = "auto",
    right = 60,
    bottom = "auto",
    width = "18%",
    height = "auto",
    h2(
      "SARS-CoV-2 Outbreak 2020",
      style = "font-size:2em;"
    ),
    uiOutput("total_cases_ui"),
    br(),
    fluidRow(
      column(
        width = 4,
        uiOutput("cases_all_ui")
      ),
      column(
        width = 4,
        uiOutput("cases_recovered_ui")
      ),
      column(
        width = 4,
        htmlOutput("cases_death_ui")
      )
    ),
    br(),
    
    htmlOutput("show_everything"),
    br(),
    uiOutput("date_ui"),
    h2(
      "Cumulative numbers:",
      style = "font-size:1.25em;"
    ),
    plotlyOutput("everything_plot", height = "20em"),
    
    h2(
      "Daily numbers:",
      style = "font-size:1.25em;"
    ),
    plotlyOutput("daily_plot", height = "15em"),
    br(),
    h2(
      "Created by Nico Hahn with data from Johns Hopkins University",
      style = "font-size:0.75em;"
    ),
    h2(
      "Works best in Firefox",
      style = "font-size:0.75em;"
    ),
  )
)