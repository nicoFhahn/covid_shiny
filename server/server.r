output$mymap <- renderLeaflet({
  # if no mapbox key exists use cartodb dark matter
  if (key == "") {
    map <- leaflet() %>%
      addProviderTiles(
        "CartoDB.DarkMatter"
      ) %>%
      setView(
        lng = 40,
        lat = 30.45,
        zoom = 3
      ) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomleft' }).addTo(this)
    }")
  } else {
    map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(
        "MapBox",
        options = providerTileOptions(
          id = "mapbox.dark", noWrap = FALSE,
          accessToken = key
        )
      ) %>%
      setView(
        lng = 40,
        lat = 30.45,
        zoom = 3
      ) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }")
  }
})

# update card on every click or changed date
observeEvent(list(
  input$date,
  input$mymap_click
), {
  if (!is.null(input$date)) {
    # get the selected dates
    daterange <- get_date()
    # try and get the country
    country <- try(get_country(), silent = TRUE)
    corona_frame <- corona_sf
    if (class(country) != "try-error") {
      # if a country was clicked, select the subset of corona data
      if (country != "world") {
        country_df <- countries[countries$ADMIN == country, ]
        corona_frame <- corona_sf[unlist(st_contains(country_df, corona_sf)), ]
        if (nrow(corona_frame) == 0) {
          corona_frame <- corona_sf[corona_sf$`Province/State` == country, ]
        }
        if (nrow(corona_frame) == 0) {
          corona_frame <- corona_sf[corona_sf$`Country/Region` == country, ]
        }
        if (nrow(corona_frame) == 0) {
          corona_frame[1:length(unique(corona_sf$date)), 1:2] <- country
          corona_frame[1:length(unique(corona_sf$date)), c(3, 5)] <- 0
          corona_frame[, ]$date <- unique(corona_sf$date)
          corona_frame$geometry <- country_df$geometry
        }
      }
    }
    # get a frame between the two dates
    corona_frame <- corona_frame[corona_frame$date >= daterange[1] - 1 & corona_frame$date <= daterange[2], ]
    # get data for the last day
    corona_frame1 <- corona_frame[corona_frame$date == max(corona_frame$date), ]
    # get data for the first day
    corona_frame2 <- corona_frame[corona_frame$date == min(corona_frame$date), ]
    # calculate the number of caes in the timeframe
    corona_frame1$confirmed <- corona_frame1$confirmed - corona_frame2$confirmed
    # corona_frame1$recovered <- corona_frame1$recovered - corona_frame2$recovered
    corona_frame1$deaths <- corona_frame1$deaths - corona_frame2$deaths
    corona_frame <- corona_frame1
    # create the map
    if (class(country) != "try-error") {
      if (country != "world") {
        country_df2 <- countries[countries$ADMIN != country, ]
        leafletProxy("mymap") %>%
          # remove stuff from the old map
          clearControls() %>%
          clearMarkers() %>%
          clearShapes() %>%
          clearPopups() %>%
          # add the country shapes
          addPolygons(
            data = country_df2,
            weight = 0,
            color = "#000000",
            fillOpacity = 0,
            # highlight the outer lines
            highlightOptions = highlightOptions(
              color = "#ffffff", opacity = 1, weight = 2, fillOpacity = 0,
              sendToBack = TRUE
            )
          ) %>%
          addPolylines(
            data = st_cast(country_df, "MULTILINESTRING"),
            color = "#ff80ed",
            weight = 2
          ) %>%
          # add the corona data
          addCircles(
            data = st_centroid(corona_frame),
            fillOpacity = 0.5,
            radius = ~ sqrt(confirmed) * 1250,
            color = "#ffb733",
            stroke = FALSE,
            # add labels
            label = paste(
              corona_frame$`Province/State`, ":<br>",
              "Confirmed cases: ", corona_frame$confirmed, "<br>",
              # "Recovered cases: ", corona_frame$recovered, "<br>",
              "Deceased cases: ", corona_frame$deaths,
              sep = ""
            ) %>% lapply(htmltools::HTML),
            # style the labels
            labelOptions = labelOptions(
              style = list(
                "font-family" = "Oswald",
                "font-style" = "sans-serif",
                "font-size" = "14px",
                "border-color" = "rgba(0,0,0,0.5)"
              )
            )
          ) %>%
          # do the same again
          # addCircles(
          #   data = st_centroid(corona_frame),
          #   fillOpacity = 0.5,
          #   radius = ~ sqrt(recovered) * 1250,
          #   color = "#5ac18e",
          #   stroke = FALSE,
          #   label = paste(
          #     corona_frame$`Province/State`, ":<br>",
          #     "Confirmed cases: ", corona_frame$confirmed, "<br>",
          #     "Recovered cases: ", corona_frame$recovered, "<br>",
          #     "Deceased cases: ", corona_frame$deaths,
          #     sep = ""
          #   ) %>% lapply(htmltools::HTML),
          #   labelOptions = labelOptions(
          #     style = list(
          #       "font-family" = "Oswald",
          #       "font-style" = "sans-serif",
          #       "font-size" = "14px",
          #       "border-color" = "rgba(0,0,0,0.5)"
          #     )
          #   )
          # ) %>%
          # and again
          addCircles(
            data = st_centroid(corona_frame),
            fillOpacity = 0.5,
            radius = ~ sqrt(deaths) * 1250,
            color = "#ff7373",
            stroke = FALSE,
            label = paste(
              corona_frame$`Province/State`, ":<br>",
              "Confirmed cases: ", corona_frame$confirmed, "<br>",
              # "Recovered cases: ", corona_frame$recovered, "<br>",
              "Deceased cases: ", corona_frame$deaths,
              sep = ""
            ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(
              style = list(
                "font-family" = "Oswald",
                "font-style" = "sans-serif",
                "font-size" = "14px",
                "border-color" = "rgba(0,0,0,0.5)"
              )
            )
          )
      } else {
        leafletProxy("mymap") %>%
          # remove stuff from the old map
          clearControls() %>%
          clearMarkers() %>%
          clearShapes() %>%
          clearPopups() %>%
          # add the country shapes
          addPolygons(
            data = countries,
            weight = 0,
            color = "#000000",
            fillOpacity = 0,
            # highlight the outer lines
            highlightOptions = highlightOptions(
              color = "#ffffff", opacity = 1, weight = 2, fillOpacity = 0,
              sendToBack = TRUE
            )
          ) %>%
          # add the corona data
          addCircles(
            data = corona_frame,
            fillOpacity = 0.5,
            radius = ~ sqrt(confirmed) * 1250,
            color = "#ffb733",
            stroke = FALSE,
            # add labels
            label = paste(
              corona_frame$`Province/State`, ":<br>",
              "Confirmed cases: ", corona_frame$confirmed, "<br>",
              # "Recovered cases: ", corona_frame$recovered, "<br>",
              "Deceased cases: ", corona_frame$deaths,
              sep = ""
            ) %>% lapply(htmltools::HTML),
            # style the labels
            labelOptions = labelOptions(
              style = list(
                "font-family" = "Oswald",
                "font-style" = "sans-serif",
                "font-size" = "14px",
                "border-color" = "rgba(0,0,0,0.5)"
              )
            )
          ) %>%
          # do the same again
          # addCircles(
          #   data = corona_frame,
          #   fillOpacity = 0.5,
          #   radius = ~ sqrt(recovered) * 1250,
          #   color = "#5ac18e",
          #   stroke = FALSE,
          #   label = paste(
          #     corona_frame$`Province/State`, ":<br>",
          #     "Confirmed cases: ", corona_frame$confirmed, "<br>",
          #     "Recovered cases: ", corona_frame$recovered, "<br>",
          #     "Deceased cases: ", corona_frame$deaths,
          #     sep = ""
          #   ) %>% lapply(htmltools::HTML),
          #   labelOptions = labelOptions(
          #     style = list(
          #       "font-family" = "Oswald",
          #       "font-style" = "sans-serif",
          #       "font-size" = "14px",
          #       "border-color" = "rgba(0,0,0,0.5)"
          #     )
          #   )
          # ) %>%
          # and again
          addCircles(
            data = corona_frame,
            fillOpacity = 0.5,
            radius = ~ sqrt(deaths) * 1250,
            color = "#ff7373",
            stroke = FALSE,
            label = paste(
              corona_frame$`Province/State`, ":<br>",
              "Confirmed cases: ", corona_frame$confirmed, "<br>",
              # "Recovered cases: ", corona_frame$recovered, "<br>",
              "Deceased cases: ", corona_frame$deaths,
              sep = ""
            ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(
              style = list(
                "font-family" = "Oswald",
                "font-style" = "sans-serif",
                "font-size" = "14px",
                "border-color" = "rgba(0,0,0,0.5)"
              )
            )
          )
      }
    } else {
      leafletProxy("mymap") %>%
        # remove stuff from the old map
        clearControls() %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearPopups() %>%
        # add the country shapes
        addPolygons(
          data = countries,
          weight = 0,
          color = "#000000",
          fillOpacity = 0,
          # highlight the outer lines
          highlightOptions = highlightOptions(
            color = "#ffffff", opacity = 1, weight = 2, fillOpacity = 0,
            sendToBack = TRUE
          )
        ) %>%
        # add the corona data
        addCircles(
          data = corona_frame,
          fillOpacity = 0.5,
          radius = ~ sqrt(confirmed) * 1250,
          color = "#ffb733",
          stroke = FALSE,
          # add labels
          label = paste(
            corona_frame$`Province/State`, ":<br>",
            "Confirmed cases: ", corona_frame$confirmed, "<br>",
            # "Recovered cases: ", corona_frame$recovered, "<br>",
            "Deceased cases: ", corona_frame$deaths,
            sep = ""
          ) %>% lapply(htmltools::HTML),
          # style the labels
          labelOptions = labelOptions(
            style = list(
              "font-family" = "Oswald",
              "font-style" = "sans-serif",
              "font-size" = "14px",
              "border-color" = "rgba(0,0,0,0.5)"
            )
          )
        ) %>%
        # do the same again
        # addCircles(
        #   data = corona_frame,
        #   fillOpacity = 0.5,
        #   radius = ~ sqrt(recovered) * 1250,
        #   color = "#5ac18e",
        #   stroke = FALSE,
        #   label = paste(
        #     corona_frame$`Province/State`, ":<br>",
        #     "Confirmed cases: ", corona_frame$confirmed, "<br>",
        #     "Recovered cases: ", corona_frame$recovered, "<br>",
        #     "Deceased cases: ", corona_frame$deaths,
        #     sep = ""
        #   ) %>% lapply(htmltools::HTML),
        #   labelOptions = labelOptions(
        #     style = list(
        #       "font-family" = "Oswald",
        #       "font-style" = "sans-serif",
        #       "font-size" = "14px",
        #       "border-color" = "rgba(0,0,0,0.5)"
        #     )
        #   )
        # ) %>%
        # and again
        addCircles(
          data = corona_frame,
          fillOpacity = 0.5,
          radius = ~ sqrt(deaths) * 1250,
          color = "#ff7373",
          stroke = FALSE,
          label = paste(
            corona_frame$`Province/State`, ":<br>",
            "Confirmed cases: ", corona_frame$confirmed, "<br>",
            # "Recovered cases: ", corona_frame$recovered, "<br>",
            "Deceased cases: ", corona_frame$deaths,
            sep = ""
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list(
              "font-family" = "Oswald",
              "font-style" = "sans-serif",
              "font-size" = "14px",
              "border-color" = "rgba(0,0,0,0.5)"
            )
          )
        ) 
    }
  }
})

# get the country that was clicked
get_country <- eventReactive(input$mymap_click, {
  # get the coordinates of the click
  coords <- data.frame(lng = input$mymap_click$lng, lat = input$mymap_click$lat)
  # turn into points
  coords <- st_as_sf(coords, coords = c("lng", "lat"), crs = 4326)
  # match with the country shapes
  country <- countries[unlist(st_intersects(coords, countries)), ]
  # either return the country or world
  if (nrow(country) == 0) {
    country <- "world"
  } else {
    country <- country$ADMIN
  }
})

# get the date
get_date <- eventReactive(input$date, {
  input$date
})

output$total_cases <- renderText({
  a <- get_country()
  # display custom text based on what was clicked
  if (a != "world") {
    text <- paste("Coronavirus cases in ", a, ":", sep = "")
  } else {
    text <- "Worldwide Coronavirus cases:"
  }
  text
})

# either show default text or click based
output$total_cases_ui <- renderUI({
  if (is.null(input$mymap_click)) {
    htmlOutput("text_default")
  } else {
    htmlOutput("total_cases")
  }
})

# default text
output$text_default <- renderText({
  "Worldwide Coronavirus cases:"
})

# either show default text or click based
output$cases_all_ui <- renderUI({
  if (is.null(input$mymap_click)) {
    htmlOutput("all_default")
  } else {
    htmlOutput("all_country")
  }
})

output$all_default <- renderText({
  # get the daterange
  daterange <- get_date()
  # check if the first date changed
  check1 <- daterange[1] == (min(corona_sf$date) + 1)
  # check if the second date changed
  check2 <- daterange[2] == max(corona_sf$date)
  # calculate the number of cases
  if (all(c(check1, check2))) {
    paste("<font size = 3em> confirmed:</font><br>", sum(corona_sf[corona_sf$date == max(corona_sf$date), ]$confirmed, na.rm = TRUE))
  } else {
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(corona_sf[corona_sf$date == daterange[1], ]$confirmed, na.rm = TRUE)
    cases_end <- sum(corona_sf[corona_sf$date == daterange[2], ]$confirmed, na.rm = TRUE)
    paste("<font size = 3em> confirmed:</font><br>", cases_end - cases_beginning)
  }
})

output$all_country <- renderText({
  # get the country
  country <- get_country()
  daterange <- input$date
  # calculate cases based on whether a country was clicked
  if (country != "world") {
    country_df <- countries[countries$ADMIN == country, ]
    corona_frame <- corona_sf[unlist(st_contains(country_df, corona_sf)), ]
    if (nrow(corona_frame) == 0) {
      corona_frame <- corona_sf[corona_sf$`Province/State` == country, ]
    }
    if (nrow(corona_frame) == 0) {
      corona_frame <- corona_sf[corona_sf$`Country/Region` == country, ]
    }
    if (nrow(corona_frame) == 0) {
      corona_frame[1:length(unique(corona_sf$date)), 1:2] <- country
      corona_frame[1:length(unique(corona_sf$date)), c(3, 5)] <- 0
      corona_frame[, ]$date <- unique(corona_sf$date)
      corona_frame$geometry <- country_df$geometry
    }
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(corona_frame[corona_frame$date == daterange[1], ]$confirmed, na.rm = TRUE)
    cases_end <- sum(corona_frame[corona_frame$date == daterange[2], ]$confirmed, na.rm = TRUE)
    paste("<font size = 3em> confirmed:</font><br>", cases_end - cases_beginning)
  } else {
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(corona_sf[corona_sf$date == daterange[1], ]$confirmed, na.rm = TRUE)
    cases_end <- sum(corona_sf[corona_sf$date == daterange[2], ]$confirmed, na.rm = TRUE)
    paste("<font size = 3em> confirmed:</font><br>", cases_end - cases_beginning)
  }
})

# either show default text or click based
# output$cases_recovered_ui <- renderUI({
#   if (is.null(input$mymap_click)) {
#     htmlOutput("recovered_default")
#   } else {
#     htmlOutput("recovered_country")
#   }
# })
# 
# 
# output$recovered_default <- renderText({
#   # get the daterange
#   daterange <- get_date()
#   # check if the first date changed
#   check1 <- daterange[1] == (min(corona_sf$date) + 1)
#   # check if the second date changed
#   check2 <- daterange[2] == max(corona_sf$date)
#   # calculate the number of cases
#   if (all(c(check1, check2))) {
#     paste("<font size = 3em> recovered:</font><br>", sum(corona_sf[corona_sf$date == max(corona_sf$date), ]$recovered, na.rm = TRUE))
#   } else {
#     daterange[1] <- daterange[1] - 1
#     cases_beginning <- sum(corona_sf[corona_sf$date == daterange[1], ]$recovered, na.rm = TRUE)
#     cases_end <- sum(corona_sf[corona_sf$date == daterange[2], ]$recovered, na.rm = TRUE)
#     paste("<font size = 3em> recovered:</font><br>", cases_end - cases_beginning)
#   }
# })
# 
# output$recovered_country <- renderText({
#   country <- get_country()
#   daterange <- input$date
#   # calculate the number of cases based on whether a country was clicked
#   if (country != "world") {
#     country_df <- countries[countries$ADMIN == country, ]
#     corona_frame <- corona_sf[unlist(st_contains(country_df, corona_sf)), ]
#     if (nrow(corona_frame) == 0) {
#       corona_frame <- corona_sf[corona_sf$`Province/State` == country, ]
#     }
#     if (nrow(corona_frame) == 0) {
#       corona_frame <- corona_sf[corona_sf$`Country/Region` == country, ]
#     }
#     if (nrow(corona_frame) == 0) {
#       corona_frame[1:length(unique(corona_sf$date)), 1:2] <- country
#       corona_frame[1:length(unique(corona_sf$date)), c(3, 5)] <- 0
#       corona_frame[, ]$date <- unique(corona_sf$date)
#       corona_frame$geometry <- country_df$geometry
#     }
#     daterange[1] <- daterange[1] - 1
#     cases_beginning <- sum(corona_frame[corona_frame$date == daterange[1], ]$recovered, na.rm = TRUE)
#     cases_end <- sum(corona_frame[corona_frame$date == daterange[2], ]$recovered, na.rm = TRUE)
#     paste("<font size = 3em> recovered:</font><br>", cases_end - cases_beginning)
#   } else {
#     daterange[1] <- daterange[1] - 1
#     cases_beginning <- sum(corona_sf[corona_sf$date == daterange[1], ]$recovered, na.rm = TRUE)
#     cases_end <- sum(corona_sf[corona_sf$date == daterange[2], ]$recovered, na.rm = TRUE)
#     paste("<font size = 3em> recovered:</font><br>", cases_end - cases_beginning)
#   }
# })

# either show default text or click based
output$cases_death_ui <- renderUI({
  b <- get_date()
  if (is.null(input$mymap_click)) {
    htmlOutput("death_default")
  } else {
    htmlOutput("death_country")
  }
})

output$death_default <- renderText({
  # get the daterange
  daterange <- get_date()
  # check if the first date changed
  check1 <- daterange[1] == (min(corona_sf$date) + 1)
  # check if the second date changed
  check2 <- daterange[2] == max(corona_sf$date)
  # calculate the number of cases
  if (all(c(check1, check2))) {
    paste("<font size = 3em> deceased:</font><br>", sum(corona_sf[corona_sf$date == max(corona_sf$date), ]$deaths, na.rm = TRUE))
  } else {
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(corona_sf[corona_sf$date == daterange[1], ]$deaths, na.rm = TRUE)
    cases_end <- sum(corona_sf[corona_sf$date == daterange[2], ]$deaths, na.rm = TRUE)
    paste("<font size = 3em> deceased:</font><br>", cases_end - cases_beginning)
  }
})

output$death_country <- renderText({
  country <- get_country()
  daterange <- input$date
  # calculate the number of cases based on whether a country was clicked
  if (country != "world") {
    country_df <- countries[countries$ADMIN == country, ]
    corona_frame <- corona_sf[unlist(st_contains(country_df, corona_sf)), ]
    if (nrow(corona_frame) == 0) {
      corona_frame <- corona_sf[corona_sf$`Province/State` == country, ]
    }
    if (nrow(corona_frame) == 0) {
      corona_frame <- corona_sf[corona_sf$`Country/Region` == country, ]
    }
    if (nrow(corona_frame) == 0) {
      corona_frame[1:length(unique(corona_sf$date)), 1:2] <- country
      corona_frame[1:length(unique(corona_sf$date)), c(3, 5)] <- 0
      corona_frame[, ]$date <- unique(corona_sf$date)
      corona_frame$geometry <- country_df$geometry
    }
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(corona_frame[corona_frame$date == daterange[1], ]$deaths, na.rm = TRUE)
    cases_end <- sum(corona_frame[corona_frame$date == daterange[2], ]$deaths, na.rm = TRUE)
    paste("<font size = 3em> deceased:</font><br>", cases_end - cases_beginning)
  } else {
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(corona_sf[corona_sf$date == daterange[1], ]$deaths, na.rm = TRUE)
    cases_end <- sum(corona_sf[corona_sf$date == daterange[2], ]$deaths, na.rm = TRUE)
    paste("<font size = 3em> deceased:</font><br>", cases_end - cases_beginning)
  }
})

# default
output$show_everything <- renderText({
  "To show all cases, simply click anywhere in the ocean"
})

# dateinput based on dates available
output$date_ui <- renderUI({
  dateRangeInput("date", "Choose a date range:",
    start = min(corona_sf$date) + 1, end = max(corona_sf$date),
    min = min(corona_sf$date) + 1, max = max(corona_sf$date),
    width = "100%"
  )
})

# empty plotly without modebar
output$everything_plot <- renderPlotly({
  config(plotly_empty(), displayModeBar = FALSE)
})

# update plotly on click or date
observeEvent(list(
  input$date,
  input$mymap_click
), {
  if (!is.null(input$date)) {
    # get the daterange
    daterange <- get_date()
    # get the country
    country <- try(get_country(), silent = TRUE)
    corona_frame <- corona_sf
    if (class(country) != "try-error") {
      # select subset
      if (country != "world") {
        country_df <- countries[countries$ADMIN == country, ]
        corona_frame <- corona_sf[unlist(st_contains(country_df, corona_sf)), ]
        if (nrow(corona_frame) == 0) {
          corona_frame <- corona_sf[corona_sf$`Province/State` == country, ]
        }
        if (nrow(corona_frame) == 0) {
          corona_frame <- corona_sf[corona_sf$`Country/Region` == country, ]
        }
        if (nrow(corona_frame) == 0) {
          corona_frame[1:length(unique(corona_sf$date)), 1:2] <- country
          corona_frame[1:length(unique(corona_sf$date)), c(3, 5)] <- 0
          corona_frame[, ]$date <- unique(corona_sf$date)
          corona_frame$geometry <- country_df$geometry
        }
      }
    }
    corona_frame <- corona_frame[corona_frame$date >= daterange[1] & corona_frame$date <= daterange[2], ]
    # remove geometry col
    corona_frame$geometry <- NULL
    # group the data
    corona_grouped <- corona_frame %>%
      group_by(date) %>%
      summarise(
        confirmed = sum(confirmed),
        deaths = sum(deaths)# ,
        # recovered = sum(recovered)
      )
    # colnames(corona_grouped) <- c("Date", "Confirmed", "Deaths", "Recovered")
    colnames(corona_grouped) <- c("Date", "Confirmed", "Deaths")
    # update the plot
    plotlyProxy("everything_plot", session) %>%
      # delete the old traces
      plotlyProxyInvoke("deleteTraces", list(0)) %>%
      plotlyProxyInvoke("deleteTraces", list(0)) %>%
      # plotlyProxyInvoke("deleteTraces", list(0)) %>%
      # add new traces
      plotlyProxyInvoke("addTraces", list(
        x = corona_grouped$Date,
        y = corona_grouped$Deaths,
        name = "Deceased",
        type = "scatter",
        mode = "none",
        stackgroup = "one",
        fillcolor = "rgba(255, 115, 115, 0.5)",
        hoverinfo = "text",
        text = paste(
          "Confirmed cases:", corona_grouped$Confirmed, "<br>",
          # "Recovered cases:", corona_grouped$Recovered, "<br>",
          "Deceased cases:", corona_grouped$Deaths
        ),
        line = list(color = "rgba(255, 115, 115, 1)")
      )) %>%
      # plotlyProxyInvoke("addTraces", list(
      #   x = corona_grouped$Date,
      #   y = corona_grouped$Recovered - corona_grouped$Deaths,
      #   name = "Recovered",
      #   type = "scatter",
      #   mode = "none",
      #   stackgroup = "one",
      #   fillcolor = "rgba(90, 193, 142, 0.5)",
      #   hoverinfo = "text",
      #   text = paste(
      #     "Confirmed cases:", corona_grouped$Confirmed, "<br>",
      #     "Recovered cases:", corona_grouped$Recovered, "<br>",
      #     "Deceased cases:", corona_grouped$Deaths
      #   ),
      #   line = list(color = "rgba(90, 193, 142, 1)")
      # )) %>%
      plotlyProxyInvoke("addTraces", list(
        x = corona_grouped$Date,
        y = corona_grouped$Confirmed - corona_grouped$Deaths, #- corona_grouped$Recovered,
        name = "Confirmed",
        type = "scatter",
        mode = "none",
        stackgroup = "one",
        fillcolor = "rgba(255, 183, 51, 0.5)",
        hoverinfo = "text",
        text = paste(
          "Confirmed cases:", corona_grouped$Confirmed, "<br>",
          # "Recovered cases:", corona_grouped$Recovered, "<br>",
          "Deceased cases:", corona_grouped$Deaths
        ),
        line = list(color = "rgba(255, 183, 51, 1)")
      )) %>%
      # define the layout
      plotlyProxyInvoke("relayout",
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          y = 100000
        ),
        yaxis = list(
          title = "Total cases",
          fixedrange = TRUE
        ),
        xaxis = list(fixedrange = TRUE, showspikes = TRUE)
      )
  }
})

# empty plot with no modebar
output$daily_plot <- renderPlotly({
  config(plotly_empty(), displayModeBar = FALSE)
})

observeEvent(list(
  input$date,
  input$mymap_click
), {
  if (!is.null(input$date)) {
    # first get the specifiy dataset again
    daterange <- get_date()
    country <- try(get_country(), silent = TRUE)
    corona_frame <- corona_sf
    if (class(country) != "try-error") {
      if (country != "world") {
        country_df <- countries[countries$ADMIN == country, ]
        corona_frame <- corona_sf[unlist(st_contains(country_df, corona_sf)), ]
        if (nrow(corona_frame) == 0) {
          corona_frame <- corona_sf[corona_sf$`Province/State` == country, ]
        }
        if (nrow(corona_frame) == 0) {
          corona_frame <- corona_sf[corona_sf$`Country/Region` == country, ]
        }
        if (nrow(corona_frame) == 0) {
          corona_frame[1:length(unique(corona_sf$date)), 1:2] <- country
          corona_frame[1:length(unique(corona_sf$date)), c(3, 5)] <- 0
          corona_frame[, ]$date <- unique(corona_sf$date)
          corona_frame$geometry <- country_df$geometry
        }
      }
    }
    corona_frame <- corona_frame[corona_frame$date >= daterange[1] & corona_frame$date <= daterange[2], ]
    corona_frame$geometry <- NULL
    # group it
    corona_grouped <- corona_frame %>%
      group_by(date) %>%
      summarise(
        confirmed = sum(confirmed),
        deaths = sum(deaths)# ,
        # recovered = sum(recovered)
      )
    # colnames(corona_grouped) <- c("Date", "Confirmed", "Deaths", "Recovered")
    colnames(corona_grouped) <- c("Date", "Confirmed", "Deaths")
    corona_grouped2 <- corona_grouped
    # count the number of cases for each specific day
    for (i in nrow(corona_grouped2):2) {
      corona_grouped2[i, ]$Confirmed <- corona_grouped2[i, ]$Confirmed - corona_grouped2[i - 1, ]$Confirmed
      corona_grouped2[i, ]$Deaths <- corona_grouped2[i, ]$Deaths - corona_grouped2[i - 1, ]$Deaths
      # corona_grouped2[i, ]$Recovered <- corona_grouped2[i, ]$Recovered - corona_grouped2[i - 1, ]$Recovered
    }
    # update the plot
    plotlyProxy("daily_plot", session) %>%
      # remove old traces
      plotlyProxyInvoke("deleteTraces", list(0)) %>%
      plotlyProxyInvoke("deleteTraces", list(0)) %>%
      # plotlyProxyInvoke("deleteTraces", list(0)) %>%
      # add new traces
      plotlyProxyInvoke("addTraces", list(
        x = corona_grouped2$Date,
        y = corona_grouped2$Confirmed,
        name = "Deceased",
        type = "bar",
        marker = list(color = "rgba(255, 183, 51, 0.7)")
      )) %>%
      # plotlyProxyInvoke("addTraces", list(
      #   x = corona_grouped2$Date,
      #   y = corona_grouped2$Recovered,
      #   name = "Deceased",
      #   type = "bar",
      #   marker = list(color = "rgba(90, 193, 142, 0.7)"),
      #   visible = FALSE
      # )) %>%
      plotlyProxyInvoke("addTraces", list(
        x = corona_grouped2$Date,
        y = corona_grouped2$Deaths,
        name = "Deceased",
        type = "bar",
        marker = list(color = "rgba(255, 115, 115, 0.7)"),
        visible = FALSE
      )) %>%
      # set the layout
      plotlyProxyInvoke(
        "relayout",
        # add dropdown menu
        updatemenus = list(
          list(
            y = 1.1,
            x = 0.43,
            buttons = list(
              list(
                method = "restyle",
                # args = list("visible", list(TRUE, FALSE, FALSE)),
                args = list("visible", list(TRUE, FALSE)),
                label = "Confirmed"
              ),
              # list(
              #   method = "restyle",
              #   args = list("visible", list(FALSE, TRUE, FALSE)),
              #   label = "Recovered"
              # ),
              list(
                method = "restyle",
                # args = list("visible", list(FALSE, FALSE, TRUE)),
                args = list("visible", list(FALSE, TRUE)),
                label = "Deceased"
              )
            )
          )
        ),
        showlegend = FALSE,
        yaxis = list(
          title = "Daily cases",
          fixedrange = TRUE
        ),
        xaxis = list(fixedrange = TRUE)
      )
  }
})

output$map_wuhan <- renderLeaflet({
  wuhan <- data.frame(
    lon = 114.283,
    lat = 30.583
  )
  coords <- st_as_sf(wuhan, coords = c("lon", "lat"), crs = 4326)
  china <- countries[countries$ADMIN == "China",]
  leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 4, maxZoom = 4, scrollWheelZoom = FALSE,
                                   dragging = FALSE, touchZoom = FALSE, doubleClickZoom = FALSE,
                                   boxZoom = FALSE, attributionControl = FALSE)) %>%
    addPolygons(
      data = china,
      fillOpacity = 0,
      color = "#00bb8b",
      weight = 1
    ) %>%
    addPulseMarkers(
      lng = wuhan$lon,
      lat = wuhan$lat,
      icon = makePulseIcon(heartbeat = 0.5, color = "#fb5a19"),
      
    ) %>%
    setView(
      lat = wuhan$lat + 7,
      lng = wuhan$lon,
      zoom = 1
    )
})


output$map_emergency <- renderLeaflet({
  leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 2, scrollWheelZoom = FALSE,
                                   dragging = FALSE, touchZoom = FALSE, doubleClickZoom = FALSE,
                                   boxZoom = FALSE, attributionControl = FALSE)) %>%
    addPolygons(
      data = infected,
      fillOpacity = 0,
      color = "#00bb8b",
      weight = 1
    ) %>%
    addPulseMarkers(
      data = coords_inf,
      icon = makePulseIcon(heartbeat = 0.5, color = "#fb5a19", iconSize = 3)
    ) %>%
    setView(
      lat = 40,
      lng = 0,
      zoom = 2
    )
  
})


output$highcharter_1 <- renderHighchart({
  highchart() %>%
    hc_yAxis_multiples(
      list(title = list(text = "Number of cases in China")),
      list(opposite = TRUE, title = list(text = "Number of cases outside China"))
    ) %>%
    hc_xAxis(categories = january$date, title = list(text = "Date")) %>%
    hc_add_series(name = "Confirmed cases in China  ", data = january$confirmed[january$`Country/Region` == "China"], color = "#EF476F") %>%
    hc_add_series(name = "Confirmed cases outside China", data = january$confirmed[january$`Country/Region` != "China"], yAxis = 1, color = "#FFD166") %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Number of confirmed COVID-19 cases in January 2020") %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(align = "left")
 })

output$highcharter_2 <- renderHighchart({
  highchart() %>%
    hc_xAxis(categories = unique(february$date), title = list(text = "Date")) %>%
    hc_yAxis(title = list(text = "Confirmed cases")) %>%
    hc_add_series(name = "Italy", data = february$confirmed[february$`Country/Region` == "Italy"], color = "#DD6E42") %>%
    hc_add_series(name = "Iran", data = february$confirmed[february$`Country/Region` == "Iran"], color = "#E8DAB2") %>%
    hc_add_series(name = "South Korea", data = february$confirmed[february$`Country/Region` == "Korea, South"], color = "#4F6D7A") %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Number of confirmed COVID-19 cases at the end of February 2020") %>%
    hc_legend(align = "left") %>%
    hc_chart(backgroundColor = "#161616")
  
})

output$highcharter_3 <- renderHighchart({
  getSymbols("^GSPC")
  GSPC <- as.data.frame(GSPC)
  GSPC$date <- as.Date(rownames(GSPC))
  highchart() %>%
    hc_xAxis(categories = GSPC$date, title = list(text = "Date")) %>%
    hc_yAxis(title = list(text = "Closing value")) %>%
    hc_add_series(name = "Closing value", data = GSPC$GSPC.Close, color = "#FB5A19") %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Closing value of the S&P 500") %>%
    hc_legend(align = "left") %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
  
})

output$highcharter_4 <- renderHighchart({
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(categories = feb_euro$Country.Region) %>%
    hc_add_series(data = feb_euro$confirmed, name = "Confirmed cases", color = "#00bb8b") %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Number of confirmed COVID-19 cases at the end of February 2020") %>%
    hc_xAxis(title = list(text = "Country")) %>%
    hc_yAxis(title = list(text = "Confirmed cases")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_5 <- renderHighchart({
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(categories = top10feb$`Country/Region`) %>%
    hc_add_series(data = top10feb$confirmed, name = "Confirmed cases", color = "#00bb8b") %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Number of confirmed COVID-19 cases at the end of February 2020") %>%
    hc_xAxis(title = list(text = "Country")) %>%
    hc_yAxis(title = list(text = "Confirmed cases")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_6 <- renderHighchart({
  highchart() %>%
    hc_xAxis(categories = pandemic$date, 
             type = "date",
             title = list(text = "Date"),
             plotLines = list(
               list(
                 label = list(text = "2 weeks prior"),
                 color = "#00bb8b",
                 width = 2,
                 value = 13
               ),
               list(
                 label = list(text = "Declared a pandemic"),
                 color = "#00bb8b",
                 width = 2,
                 value = 27
               )
             )
    ) %>%
    hc_yAxis(title= list(text = "Confirmed cases")) %>%
    hc_add_series(data = pandemic$confirmed, name = "Confirmed cases", color = "#fb5a19") %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Number of confirmed COVID-19 cases outside China over the last 28 days") %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_7 <- renderHighchart({
  highchart() %>%
    hc_xAxis(categories = germany$date)%>%
    hc_yAxis(title = list(text = "Confirmed cases"))%>%
    hc_add_series(data = germany$confirmed, name = "Confirmed cases", color = "#fb5a19") %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Number of confirmed COVID-19 cases in Germany") %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE) %>%
    hc_xAxis(title = list(text = "Date"),
             plotLines = list(
               list(
                 label = list(text = "Contact ban", verticalAlign = "middle"),
                 color = "#00bb8b",
                 width = 2,
                 value = 61
               )
             )
    )
})

output$highcharter_8 <- renderHighchart({
  data_pre <- corona_sf
  data_pre$geometry <- NULL
  daily_cases <- data_pre %>%
    group_by(date) %>%
    summarise(
      confirmed = sum(confirmed),
      deaths = sum(deaths)# ,
      # recovered = sum(recovered)
    )
  daily_cases$increase <- c(0, daily_cases$confirmed[-1] - daily_cases$confirmed)[seq_len(nrow(daily_cases))]
  highchart() %>%
    hc_xAxis(categories = daily_cases$date, title = list(text = "Date")) %>%
    hc_yAxis(title = list(text = "Confirmed cases")) %>%
    hc_add_series(data = daily_cases$confirmed, name = "Confirmed cases", color = "#fb5a19") %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Number of confirmed COVID-19 cases") %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_9 <- renderHighchart({
  data_pre <- corona_sf
  data_pre$geometry <- NULL
  daily_cases <- data_pre %>%
    group_by(date) %>%
    summarise(
      confirmed = sum(confirmed),
      deaths = sum(deaths)# ,
      # recovered = sum(recovered)
    )
  daily_cases$increase <- c(0, daily_cases$confirmed[-1] - daily_cases$confirmed)[seq_len(nrow(daily_cases))]
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(categories = daily_cases$date) %>%
    hc_add_series(data = daily_cases$increase, name = "New infections", color = "#00bb8b") %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Daily number of new infections") %>%
    hc_xAxis(title = list(text = "Date")) %>%
    hc_yAxis(title = list(text = "Confirmed infections")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_10 <- renderHighchart({
  today <- daily_cases2[daily_cases2$date == max(daily_cases2$date), ]
  most_infected <- daily_cases2[daily_cases2$`Country/Region` == as.character(today[today$confirmed == max(today$confirmed), 2]), ]
  most_infected$increase <- c(0, most_infected$confirmed[-1] - most_infected$confirmed)[seq_len(nrow(most_infected))]
  highchart() %>%
    hc_xAxis(categories = most_infected$date, title = list(text = "Date")) %>%
    hc_yAxis(title = list(text = "Confirmed cases")) %>%
    hc_add_series(data = most_infected$confirmed, name = "New infections", color = "#fb5a19") %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = paste("Number of confirmed COVID-19 cases in", unique(most_infected$`Country/Region`))) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})


output$highcharter_11 <- renderHighchart({
  today <- daily_cases2[daily_cases2$date == max(daily_cases2$date), ]
  most_infected <- daily_cases2[daily_cases2$`Country/Region` == as.character(today[today$confirmed == max(today$confirmed), 2]), ]
  most_infected$increase <- c(0, most_infected$confirmed[-1] - most_infected$confirmed)[seq_len(nrow(most_infected))]
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(categories = most_infected$date) %>%
    hc_add_series(data = most_infected$increase, name = "Confirmed cases", color = "#00bb8b") %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = paste("Daily number of new infections in", unique(most_infected$`Country/Region`))) %>%
    hc_xAxis(title = list(text = "Date")) %>%
    hc_yAxis(title = list(text = "Confirmed infections")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_12 <- renderHighchart({
  most_death <- daily_cases2[daily_cases2$`Country/Region` == as.character(today[today$deaths == max(today$deaths), 2]), ]
  most_death$increase <- c(0, most_death$deaths[-1] - most_death$deaths)[seq_len(nrow(most_death))]
  
  highchart() %>%
    hc_xAxis(categories = most_death$date, title = list(text = "Date")) %>%
    hc_yAxis(title = list(text = "Deaths")) %>%
    hc_add_series(data = most_death$deaths, name = "Deaths", color = "#fb5a19") %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = paste("Number of deaths in", unique(most_death$`Country/Region`))) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_13 <- renderHighchart({
  most_death <- daily_cases2[daily_cases2$`Country/Region` == as.character(today[today$deaths == max(today$deaths), 2]), ]
  most_death$increase <- c(0, most_death$deaths[-1] - most_death$deaths)[seq_len(nrow(most_death))]
  
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(categories = most_death$date) %>%
    hc_add_series(data = most_death$increase, name = "Deaths", color = "#00bb8b") %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = paste("Daily number of new deaths in", unique(most_death$`Country/Region`))) %>%
    hc_xAxis(title = list(text = "Date")) %>%
    hc_yAxis(title = list(text = "Deaths")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

observe(addScrollAnim(session, 'hc1', 'bounceInRight'))
observe(addScrollAnim(session, 'hc2', 'bounceInRight'))
observe(addScrollAnim(session, 'hc3', 'bounceInRight'))
observe(addScrollAnim(session, 'hc4', 'bounceInRight'))
observe(addScrollAnim(session, 'hc5', 'bounceInRight'))
observe(addScrollAnim(session, 'hc6', 'bounceInRight'))
observe(addScrollAnim(session, 'hc7', 'bounceInRight'))
observe(addScrollAnim(session, 'hc8', 'bounceInRight'))
observe(addScrollAnim(session, 'hc9', 'bounceInRight'))
observe(addScrollAnim(session, 'hc10', 'bounceInRight'))
observe(addScrollAnim(session, 'hc11', 'bounceInRight'))
observe(addScrollAnim(session, 'hc12', 'bounceInRight'))
observe(addScrollAnim(session, 'hc13', 'bounceInRight'))
observe(addScrollAnim(session, 'lf3', 'bounceInRight'))

observeEvent(input$toTop, {
  js$toTop();
})