fluidPage(
  tags$style(
  HTML("
  .tabbable > .nav > li > a {
  background-color: #161616;  color:#00bb8b
  }
  .tabbable > .nav > li > a[data-value='Timeline of the outbreak'] {
  background-color: #00bb8b;   color:#161616
  }
  .tabbable > .nav > li > a[data-value='Worldwide cases'] {
  background-color: #00bb8b;  color:#161616
  }
  .tabbable > .nav > li[class=active]    > a {
  background-color: #161616; color:#00bb8b
  }
  ")
  ),
  withAnim(),
  tabsetPanel(
    id = "tabset",
    tabPanel(
      "Timeline of the outbreak",
      HTML('<a name="top">'),
      absolutePanel(
        draggable = FALSE,
        fixed = TRUE,
        top = 900,
        left = 10,
        HTML(
        '<a href = "https://github.com/nicoFhahn/covid_shiny"
        target="_blank">
        <button id="github"
        title="GitHub repository">GitHub</button></a>'),
        HTML(
          '<a href = "#top"><button id="myBtn"
        title="jump to top">jump to top</button></a>'),
        style = "z-index: 420;"
      ),
      fluidRow(
        includeHTML("html_files/landing.html")
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_1.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "lf1", leafletOutput("map_wuhan", height = "80vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_2.html")
        )
      ),
      fluidRow(
        column(
          width = 2
        ),
        column(
          width = 8,
          div(id = "lf2", leafletOutput("map_emergency", height = "80vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_3.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc1", highchartOutput("highcharter_1", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_4.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc2", highchartOutput("highcharter_2", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_5.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc3", highchartOutput("highcharter_3", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_6.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc4", highchartOutput("highcharter_4", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_7.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc5", highchartOutput("highcharter_5", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_8.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc6", highchartOutput("highcharter_6", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_9.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc7", highchartOutput("highcharter_7", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_10.html")
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/today_1.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc8", highchartOutput("highcharter_8", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(class = "story_2"),
          div(id = "hc9", highchartOutput("highcharter_9", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          div(class = "story_2"),
          includeHTML("html_files/today_2.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc8", highchartOutput("highcharter_10", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(class = "story_2"),
          div(id = "hc9", highchartOutput("highcharter_11", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          div(class = "story_2"),
          includeHTML("html_files/today_3.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc8", highchartOutput("highcharter_12", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(class = "story_2"),
          div(id = "hc9", highchartOutput("highcharter_13", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          div(class = "story_2"),
          includeHTML("html_files/today.html")
        )
      )
    ),
    tabPanel(
      "Worldwide cases",
      icon = icon("map"),
      tags$head(
        shiny::includeCSS("styles.css"),
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
        id = "controls_panel",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = TRUE,
        top = 70,
        left = "auto",
        right = 70,
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
            width = 6,
            uiOutput("cases_all_ui")
          ),
          column(
            width = 6,
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
        plotlyOutput("everything_plot", height = "15em"),
        h2(
          "Daily numbers:",
          style = "font-size:1.25em;"
        ),
        plotlyOutput("daily_plot_confirmed", height = "10em"),
        br(),
        plotlyOutput("daily_plot_deaths", height = "10em"),
        br(),
        h2(
          "Created by Nico Hahn with data from Johns Hopkins University",
          style = "font-size:0.75em;"
        ),
        h2(
          "Works best in Firefox",
          style = "font-size:0.75em;"
        ),
        style = "z-index: 420;"
      )
    )
  ),
  setBackgroundColor("#161616")
)
