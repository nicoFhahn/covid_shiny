output$mymap <- renderLeaflet({
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
          )) %>%
        setView(
          lng = 20,
          lat = 30.45,
          zoom = 2.5
        )
    }
  })
  
  observeEvent(list(
    input$date,
    input$mymap_click
  ), {
    if (!is.null(input$date)) {
      daterange <- get_date()
      country <- try(get_country(), silent = TRUE)
      corona_frame <- corona_sf
      if (class(country) != "try-error") {
        if (country != "world") {
          country <- countries[countries$ADMIN == country, ]
          corona_frame <- corona_sf[unlist(st_contains(country, corona_sf)), ]
        }
      }
      corona_frame <- corona_frame[corona_frame$date >= daterange[1] - 1 & corona_frame$date <= daterange[2], ]
      corona_frame1 <- corona_frame[corona_frame$date == max(corona_frame$date), ]
      corona_frame2 <- corona_frame[corona_frame$date == min(corona_frame$date), ]
      corona_frame1$confirmed <- corona_frame1$confirmed - corona_frame2$confirmed
      corona_frame1$recovered <- corona_frame1$recovered - corona_frame2$recovered
      corona_frame1$deaths <- corona_frame1$deaths - corona_frame2$deaths
      corona_frame <- corona_frame1
      leafletProxy("mymap") %>%
        clearControls() %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearPopups() %>%
        addPolygons(
          data = countries,
          weight = 0,
          color = "#000000",
          fillOpacity = 0,
          highlightOptions = highlightOptions(
            color='#ffffff', opacity = 1, weight = 2, fillOpacity = 0,
            sendToBack = TRUE)
        ) %>%
        addCircles(
          data = corona_frame,
          fillOpacity = 0.5,
          radius = ~sqrt(confirmed) * 1250,
          color = "#ffb733",
          stroke = FALSE,
          label = paste(
            corona_frame$`Province/State`, ":<br>",
            "Confirmed cases:", corona_frame$confirmed, "<br>",
            "Recovered cases:", corona_frame$recovered, "<br>",
            "Deceased cases:", corona_frame$deaths) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions(
          style = list(
            "font-family" = "Oswald",
            "font-style" = "sans-serif",
            "font-size" = "14px",
            "border-color" = "rgba(0,0,0,0.5)"
          ))
        ) %>%
        addCircles(
          data = corona_frame,
          fillOpacity = 0.5,
          radius = ~sqrt(recovered) * 1250,
          color = "#5ac18e",
          stroke = FALSE,
          label = paste(
            corona_frame$`Province/State`, ":<br>",
            "Confirmed cases:", corona_frame$confirmed, "<br>",
            "Recovered cases:", corona_frame$recovered, "<br>",
            "Deceased cases:", corona_frame$deaths) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list(
              "font-family" = "Oswald",
              "font-style" = "sans-serif",
              "font-size" = "14px",
              "border-color" = "rgba(0,0,0,0.5)"
            ))
        ) %>%
        addCircles(
          data = corona_frame,
          fillOpacity = 0.5,
          radius = ~sqrt(deaths) * 1250,
          color = "#ff7373",
          stroke = FALSE,
          label = paste(
            corona_frame$`Province/State`, ":<br>",
            "Confirmed cases:", corona_frame$confirmed, "<br>",
            "Recovered cases:", corona_frame$recovered, "<br>",
            "Deceased cases:", corona_frame$deaths) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list(
              "font-family" = "Oswald",
              "font-style" = "sans-serif",
              "font-size" = "14px",
              "border-color" = "rgba(0,0,0,0.5)"
            ))
        )
    }
  })
  
  get_country <- eventReactive(input$mymap_click, {
    coords <- data.frame(lng = input$mymap_click$lng, lat = input$mymap_click$lat)
    coords <- st_as_sf(coords, coords = c("lng", "lat"), crs = 4326)
    country <- countries[unlist(st_intersects(coords, countries)), ]
    if (nrow(country) == 0) {
      country <- "world"
    } else {
      country <- country$ADMIN
    }
  })
  
  get_date <- eventReactive(input$date, {
    input$date
  })
  
  output$total_cases <- renderText({
    a <- get_country()
    b <- get_date()
    if (a != "world") {
      text = paste("Coronavirus cases in ", a, ":", sep = "")
    } else {
      text = "Worldwide Coronavirus cases:"
    }
    text
  })
  
  output$total_cases_ui <- renderUI({
    if (is.null(input$mymap_click)) {
      htmlOutput("text_default")
    } else {
      htmlOutput("total_cases")
    }
  })
  
  output$text_default <- renderText({
    "Worldwide Coronavirus cases:"
  })
  
  output$cases_all_ui <- renderUI({
    if (is.null(input$mymap_click)) {
      htmlOutput("all_default")
    } else {
      htmlOutput("all_country")
    }
  })
  
  output$all_default <- renderText({
    daterange <- get_date()
    check1 <- daterange[1] == (min(corona_sf$date) + 1)
    check2 <- daterange[2] == max(corona_sf$date)
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
    a <- get_country()
    daterange <- input$date
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
  
  output$cases_recovered_ui <- renderUI({
    if (is.null(input$mymap_click)) {
      htmlOutput("recovered_default")
    } else {
      htmlOutput("recovered_country")
    }
  })
  
  
  output$recovered_default <- renderText({
    daterange <- get_date()
    check1 <- daterange[1] == (min(corona_sf$date) + 1)
    check2 <- daterange[2] == max(corona_sf$date)
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
  
  output$cases_death_ui <- renderUI({
    b <- get_date()
    if (is.null(input$mymap_click)) {
      htmlOutput("death_default")
    } else {
      htmlOutput("death_country")
    }
  })
  
  output$death_default <- renderText({
    daterange <- get_date()
    check1 <- daterange[1] == (min(corona_sf$date) + 1)
    check2 <- daterange[2] == max(corona_sf$date)
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
  
  output$show_everything <- renderText({
    "To show all cases, simply click anywhere in the ocean"
  })
  
  output$date_ui <- renderUI({
    dateRangeInput("date", "Choose a date range:",
                   start = min(corona_sf$date) + 1, end = max(corona_sf$date),
                   min = min(corona_sf$date) + 1, max = max(corona_sf$date),
                   width = "100%")
  })
  
  output$everything_plot <- renderPlotly({
    config(plotly_empty(), displayModeBar = FALSE)
  })
  
  observeEvent(list(
    input$date,
    input$mymap_click
  ), {
    if (!is.null(input$date)) {
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
      corona_grouped <- corona_frame %>%
        group_by(date) %>%
        summarise(confirmed = sum(confirmed),
                  deaths = sum(deaths),
                  recovered = sum(recovered))
      colnames(corona_grouped) <- c("Date", "Confirmed", "Deaths", "Recovered")
      plotlyProxy("everything_plot", session) %>%
        plotlyProxyInvoke("deleteTraces", list(0)) %>%
        plotlyProxyInvoke("deleteTraces", list(0)) %>%
        plotlyProxyInvoke("deleteTraces", list(0)) %>%
        plotlyProxyInvoke("addTraces", list(
          x = corona_grouped$Date,
          y = corona_grouped$Deaths,
          name = "Deceased",
          type = "scatter",
          mode = "none",
          stackgroup = "one",
          fillcolor = "rgba(255, 115, 115, 0.5)",
          hoverinfo = "text",
          text = paste("Confirmed cases:", corona_grouped$Confirmed, "<br>",
                       "Recovered cases:", corona_grouped$Recovered, "<br>",
                       "Deceased cases:", corona_grouped$Deaths),
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
          text = paste("Confirmed cases:", corona_grouped$Confirmed, "<br>",
                       "Recovered cases:", corona_grouped$Recovered, "<br>",
                       "Deceased cases:", corona_grouped$Deaths),
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
          text = paste("Confirmed cases:", corona_grouped$Confirmed, "<br>",
                       "Recovered cases:", corona_grouped$Recovered, "<br>",
                       "Deceased cases:", corona_grouped$Deaths),
          line = list(color = "rgba(255, 183, 51, 1)")
        )) %>%
        plotlyProxyInvoke("relayout",
                          legend = list(
                            orientation = "h",
                            xanchor = "center",
                            x = 0.5,
                            y = 100000
                          ),
                          yaxis = list(
                            title = "Total cases",
                            fixedrange = TRUE),
                          xaxis = list(fixedrange = TRUE, showspikes = TRUE))
    }
  })
  
  output$daily_plot <- renderPlotly({
    config(plotly_empty(), displayModeBar = FALSE)
  })
  
  observeEvent(list(
    input$date,
    input$mymap_click
  ), {
    if (!is.null(input$date)) {
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
      corona_grouped <- corona_frame %>%
        group_by(date) %>%
        summarise(confirmed = sum(confirmed),
                  deaths = sum(deaths),
                  recovered = sum(recovered))
      colnames(corona_grouped) <- c("Date", "Confirmed", "Deaths", "Recovered")
      corona_grouped2 <- corona_grouped
      for(i in nrow(corona_grouped2):2) {
        corona_grouped2[i, ]$Confirmed <- corona_grouped2[i, ]$Confirmed - corona_grouped2[i - 1, ]$Confirmed
        corona_grouped2[i, ]$Deaths <- corona_grouped2[i, ]$Deaths - corona_grouped2[i - 1, ]$Deaths
        corona_grouped2[i, ]$Recovered <- corona_grouped2[i, ]$Recovered - corona_grouped2[i - 1, ]$Recovered
      }
      plotlyProxy("daily_plot", session) %>%
        plotlyProxyInvoke("deleteTraces", list(0)) %>%
        plotlyProxyInvoke("deleteTraces", list(0)) %>%
        plotlyProxyInvoke("deleteTraces", list(0)) %>%
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
      plotlyProxyInvoke("relayout", updatemenus = list(
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
        fixedrange = TRUE),
      xaxis = list(fixedrange = TRUE))
    }
  })
