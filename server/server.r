output$mymap <- renderLeaflet({
  # if no mapbox key exists use cartodb dark matter
  if (key == "") {
    map <- leaflet() %>%
      addProviderTiles(
        "CartoDB.DarkMatter"
      ) %>%
      setView(
        lng = 20,
        lat = 30.45,
        zoom = 2.5
      )
  } else {
    map <- leaflet() %>%
      addProviderTiles(
        "MapBox",
        options = providerTileOptions(
          id = "mapbox.dark", noWrap = FALSE,
          accessToken = key
        )
      ) %>%
      setView(
        lng = 20,
        lat = 30.45,
        zoom = 2.5
      )
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
        country <- countries[countries$ADMIN == country, ]
        corona_frame <- corona_sf[unlist(st_contains(country, corona_sf)), ]
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
    corona_frame1$recovered <- corona_frame1$recovered - corona_frame2$recovered
    corona_frame1$deaths <- corona_frame1$deaths - corona_frame2$deaths
    corona_frame <- corona_frame1
    # create the map
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
          "Recovered cases: ", corona_frame$recovered, "<br>",
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
      addCircles(
        data = corona_frame,
        fillOpacity = 0.5,
        radius = ~ sqrt(recovered) * 1250,
        color = "#5ac18e",
        stroke = FALSE,
        label = paste(
          corona_frame$`Province/State`, ":<br>",
          "Confirmed cases: ", corona_frame$confirmed, "<br>",
          "Recovered cases: ", corona_frame$recovered, "<br>",
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
      ) %>%
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
          "Recovered cases: ", corona_frame$recovered, "<br>",
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
    paste("<font size = 3em> confirmed:</font><br>", sum(corona_sf[corona_sf$date == max(corona_sf$date), ]$confirmed))
  } else {
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(corona_sf[corona_sf$date == daterange[1], ]$confirmed)
    cases_end <- sum(corona_sf[corona_sf$date == daterange[2], ]$confirmed)
    paste("<font size = 3em> confirmed:</font><br>", cases_end - cases_beginning)
  }
})

output$all_country <- renderText({
  # get the country
  a <- get_country()
  daterange <- input$date
  # calculate cases based on whether a country was clicked
  if (a != "world") {
    country <- countries[countries$ADMIN == a, ]
    corona_frame <- corona_sf[unlist(st_contains(country, corona_sf)), ]
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(corona_frame[corona_frame$date == daterange[1], ]$confirmed)
    cases_end <- sum(corona_frame[corona_frame$date == daterange[2], ]$confirmed)
    paste("<font size = 3em> confirmed:</font><br>", cases_end - cases_beginning)
  } else {
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(corona_sf[corona_sf$date == daterange[1], ]$confirmed)
    cases_end <- sum(corona_sf[corona_sf$date == daterange[2], ]$confirmed)
    paste("<font size = 3em> confirmed:</font><br>", cases_end - cases_beginning)
  }
})

# either show default text or click based
output$cases_recovered_ui <- renderUI({
  if (is.null(input$mymap_click)) {
    htmlOutput("recovered_default")
  } else {
    htmlOutput("recovered_country")
  }
})


output$recovered_default <- renderText({
  # get the daterange
  daterange <- get_date()
  # check if the first date changed
  check1 <- daterange[1] == (min(corona_sf$date) + 1)
  # check if the second date changed
  check2 <- daterange[2] == max(corona_sf$date)
  # calculate the number of cases
  if (all(c(check1, check2))) {
    paste("<font size = 3em> recovered:</font><br>", sum(corona_sf[corona_sf$date == max(corona_sf$date), ]$recovered))
  } else {
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(corona_sf[corona_sf$date == daterange[1], ]$recovered)
    cases_end <- sum(corona_sf[corona_sf$date == daterange[2], ]$recovered)
    paste("<font size = 3em> recovered:</font><br>", cases_end - cases_beginning)
  }
})

output$recovered_country <- renderText({
  a <- get_country()
  daterange <- input$date
  # calculate the number of cases based on whether a country was clicked
  if (a != "world") {
    country <- countries[countries$ADMIN == a, ]
    corona_frame <- corona_sf[unlist(st_contains(country, corona_sf)), ]
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(corona_frame[corona_frame$date == daterange[1], ]$recovered)
    cases_end <- sum(corona_frame[corona_frame$date == daterange[2], ]$recovered)
    paste("<font size = 3em> recovered:</font><br>", cases_end - cases_beginning)
  } else {
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(corona_sf[corona_sf$date == daterange[1], ]$recovered)
    cases_end <- sum(corona_sf[corona_sf$date == daterange[2], ]$recovered)
    paste("<font size = 3em> recovered:</font><br>", cases_end - cases_beginning)
  }
})

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
    paste("<font size = 3em> deceased:</font><br>", sum(corona_sf[corona_sf$date == max(corona_sf$date), ]$deaths))
  } else {
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(corona_sf[corona_sf$date == daterange[1], ]$deaths)
    cases_end <- sum(corona_sf[corona_sf$date == daterange[2], ]$deaths)
    paste("<font size = 3em> deceased:</font><br>", cases_end - cases_beginning)
  }
})

output$death_country <- renderText({
  a <- get_country()
  daterange <- input$date
  # calculate the number of cases based on whether a country was clicked
  if (a != "world") {
    country <- countries[countries$ADMIN == a, ]
    corona_frame <- corona_sf[unlist(st_contains(country, corona_sf)), ]
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(corona_frame[corona_frame$date == daterange[1], ]$deaths)
    cases_end <- sum(corona_frame[corona_frame$date == daterange[2], ]$deaths)
    paste("<font size = 3em> deceased:</font><br>", cases_end - cases_beginning)
  } else {
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(corona_sf[corona_sf$date == daterange[1], ]$deaths)
    cases_end <- sum(corona_sf[corona_sf$date == daterange[2], ]$deaths)
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
        country <- countries[countries$ADMIN == country, ]
        corona_frame <- corona_sf[unlist(st_contains(country, corona_sf)), ]
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
        deaths = sum(deaths),
        recovered = sum(recovered)
      )
    colnames(corona_grouped) <- c("Date", "Confirmed", "Deaths", "Recovered")
    # update the plot
    plotlyProxy("everything_plot", session) %>%
      # delete the old traces
      plotlyProxyInvoke("deleteTraces", list(0)) %>%
      plotlyProxyInvoke("deleteTraces", list(0)) %>%
      plotlyProxyInvoke("deleteTraces", list(0)) %>%
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
          "Recovered cases:", corona_grouped$Recovered, "<br>",
          "Deceased cases:", corona_grouped$Deaths
        ),
        line = list(color = "rgba(255, 115, 115, 1)")
      )) %>%
      plotlyProxyInvoke("addTraces", list(
        x = corona_grouped$Date,
        y = corona_grouped$Recovered - corona_grouped$Deaths,
        name = "Recovered",
        type = "scatter",
        mode = "none",
        stackgroup = "one",
        fillcolor = "rgba(90, 193, 142, 0.5)",
        hoverinfo = "text",
        text = paste(
          "Confirmed cases:", corona_grouped$Confirmed, "<br>",
          "Recovered cases:", corona_grouped$Recovered, "<br>",
          "Deceased cases:", corona_grouped$Deaths
        ),
        line = list(color = "rgba(90, 193, 142, 1)")
      )) %>%
      plotlyProxyInvoke("addTraces", list(
        x = corona_grouped$Date,
        y = corona_grouped$Confirmed - corona_grouped$Deaths - corona_grouped$Recovered,
        name = "Confirmed",
        type = "scatter",
        mode = "none",
        stackgroup = "one",
        fillcolor = "rgba(255, 183, 51, 0.5)",
        hoverinfo = "text",
        text = paste(
          "Confirmed cases:", corona_grouped$Confirmed, "<br>",
          "Recovered cases:", corona_grouped$Recovered, "<br>",
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
        country <- countries[countries$ADMIN == country, ]
        corona_frame <- corona_sf[unlist(st_contains(country, corona_sf)), ]
      }
    }
    corona_frame <- corona_frame[corona_frame$date >= daterange[1] & corona_frame$date <= daterange[2], ]
    corona_frame$geometry <- NULL
    # group it
    corona_grouped <- corona_frame %>%
      group_by(date) %>%
      summarise(
        confirmed = sum(confirmed),
        deaths = sum(deaths),
        recovered = sum(recovered)
      )
    colnames(corona_grouped) <- c("Date", "Confirmed", "Deaths", "Recovered")
    corona_grouped2 <- corona_grouped
    # count the number of cases for each specific day
    for (i in nrow(corona_grouped2):2) {
      corona_grouped2[i, ]$Confirmed <- corona_grouped2[i, ]$Confirmed - corona_grouped2[i - 1, ]$Confirmed
      corona_grouped2[i, ]$Deaths <- corona_grouped2[i, ]$Deaths - corona_grouped2[i - 1, ]$Deaths
      corona_grouped2[i, ]$Recovered <- corona_grouped2[i, ]$Recovered - corona_grouped2[i - 1, ]$Recovered
    }
    # update the plot
    plotlyProxy("daily_plot", session) %>%
      # remove old traces
      plotlyProxyInvoke("deleteTraces", list(0)) %>%
      plotlyProxyInvoke("deleteTraces", list(0)) %>%
      plotlyProxyInvoke("deleteTraces", list(0)) %>%
      # add new traces
      plotlyProxyInvoke("addTraces", list(
        x = corona_grouped2$Date,
        y = corona_grouped2$Confirmed,
        name = "Deceased",
        type = "bar",
        marker = list(color = "rgba(255, 183, 51, 0.7)")
      )) %>%
      plotlyProxyInvoke("addTraces", list(
        x = corona_grouped2$Date,
        y = corona_grouped2$Recovered,
        name = "Deceased",
        type = "bar",
        marker = list(color = "rgba(90, 193, 142, 0.7)"),
        visible = FALSE
      )) %>%
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
            x = 0.33,
            buttons = list(
              list(
                method = "restyle",
                args = list("visible", list(TRUE, FALSE, FALSE)),
                label = "Confirmed"
              ),
              list(
                method = "restyle",
                args = list("visible", list(FALSE, TRUE, FALSE)),
                label = "Recovered"
              ),
              list(
                method = "restyle",
                args = list("visible", list(FALSE, FALSE, TRUE)),
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
