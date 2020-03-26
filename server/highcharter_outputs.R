output$highcharter_1 <- renderHighchart({
  highchart() %>%
    hc_yAxis_multiples(
      list(title = list(text = "Number of cases in China")),
      list(opposite = TRUE,
           title = list(text = "Number of cases outside China"))
    ) %>%
    hc_xAxis(categories = january$date, title = list(text = "Date")) %>%
    hc_add_series(
      name = "Confirmed cases in China  ",
      data = january$confirmed[january$`Country/Region` == "China"],
      color = "#EF476F"
      ) %>%
    hc_add_series(
      name = "Confirmed cases outside China",
      data = january$confirmed[january$`Country/Region` != "China"],
      yAxis = 1,
      color = "#FFD166"
      ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Number of confirmed COVID-19 cases in January 2020") %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(align = "left")
})

output$highcharter_2 <- renderHighchart({
  highchart() %>%
    hc_xAxis(
      categories = unique(february$date),
      title = list(text = "Date")
      ) %>%
    hc_yAxis(title = list(text = "Confirmed cases")) %>%
    hc_add_series(
      name = "Italy",
      data = february$confirmed[february$`Country/Region` == "Italy"],
      color = "#DD6E42"
      ) %>%
    hc_add_series(
      name = "Iran",
      data = february$confirmed[february$`Country/Region` == "Iran"],
      color = "#E8DAB2"
      ) %>%
    hc_add_series(
      name = "South Korea",
      data = february$confirmed[february$`Country/Region` == "Korea, South"],
      color = "#4F6D7A"
      ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(
      text = "Number of confirmed COVID-19
      cases at the end of February 2020"
      ) %>%
    hc_legend(align = "left") %>%
    hc_chart(backgroundColor = "#161616")
})

output$highcharter_3 <- renderHighchart({
  getSymbols("^GSPC")
  gspc <- as.data.frame(GSPC)
  gspc$date <- as.Date(rownames(gspc))
  highchart() %>%
    hc_xAxis(categories = gspc$date, title = list(text = "Date")) %>%
    hc_yAxis(title = list(text = "Closing value")) %>%
    hc_add_series(
      name = "Closing value",
      data = gspc$GSPC.Close,
      color = "#FB5A19"
      ) %>%
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
    hc_add_series(
      data = feb_euro$confirmed,
      name = "Confirmed cases",
      color = "#00bb8b"
      ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(
      text = "Number of confirmed COVID-19
      cases at the end of February 2020"
      ) %>%
    hc_xAxis(title = list(text = "Country")) %>%
    hc_yAxis(title = list(text = "Confirmed cases")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_5 <- renderHighchart({
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(categories = top10feb$`Country/Region`) %>%
    hc_add_series(
      data = top10feb$confirmed,
      name = "Confirmed cases",
      color = "#00bb8b"
      ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(
      text = "Number of confirmed COVID-19 cases
      at the end of February 2020"
      ) %>%
    hc_xAxis(title = list(text = "Country")) %>%
    hc_yAxis(title = list(text = "Confirmed cases")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_6 <- renderHighchart({
  highchart() %>%
    hc_xAxis(
      categories = pandemic$date,
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
    hc_yAxis(title = list(text = "Confirmed cases")) %>%
    hc_add_series(
      data = pandemic$confirmed,
      name = "Confirmed cases",
      color = "#fb5a19"
      ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(
      text = "Number of confirmed COVID-19 cases
      outside China over the last 28 days"
      ) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_7 <- renderHighchart({
  highchart() %>%
    hc_xAxis(categories = germany$date) %>%
    hc_yAxis(title = list(text = "Confirmed cases")) %>%
    hc_add_series(
      data = germany$confirmed,
      name = "Confirmed cases",
      color = "#fb5a19"
      ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Number of confirmed COVID-19 cases in Germany") %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE) %>%
    hc_xAxis(
      title = list(text = "Date"),
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
      deaths = sum(deaths)
    )
  daily_cases$increase <- c(
    0, daily_cases$confirmed[-1] -
      daily_cases$confirmed)[seq_len(nrow(daily_cases))]
  highchart() %>%
    hc_xAxis(categories = daily_cases$date, title = list(text = "Date")) %>%
    hc_yAxis(title = list(text = "Confirmed cases")) %>%
    hc_add_series(
      data = daily_cases$confirmed,
      name = "Confirmed cases",
      color = "#fb5a19"
      ) %>%
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
      deaths = sum(deaths)
    )
  daily_cases$increase <- c(
    0, daily_cases$confirmed[-1] -
      daily_cases$confirmed)[seq_len(nrow(daily_cases))]
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(categories = daily_cases$date) %>%
    hc_add_series(
      data = daily_cases$increase,
      name = "New infections",
      color = "#00bb8b"
      ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Daily number of new infections") %>%
    hc_xAxis(title = list(text = "Date")) %>%
    hc_yAxis(title = list(text = "Confirmed infections")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_10 <- renderHighchart({
  today <- daily_cases2[daily_cases2$date == max(daily_cases2$date), ]
  most_infected <- daily_cases2[
    daily_cases2$`Country/Region` == as.character(
      today[today$confirmed == max(today$confirmed), 2]), ]
  most_infected$increase <- c(
    0, most_infected$confirmed[-1] -
      most_infected$confirmed)[seq_len(nrow(most_infected))]
  highchart() %>%
    hc_xAxis(categories = most_infected$date, title = list(text = "Date")) %>%
    hc_yAxis(title = list(text = "Confirmed cases")) %>%
    hc_add_series(
      data = most_infected$confirmed,
      name = "New infections",
      color = "#fb5a19"
      ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(
      text = paste(
        "Number of confirmed COVID-19 cases in",
        unique(most_infected$`Country/Region`)
        )
      ) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_11 <- renderHighchart({
  today <- daily_cases2[daily_cases2$date == max(daily_cases2$date), ]
  most_infected <- daily_cases2[
    daily_cases2$`Country/Region` == as.character(
      today[today$confirmed == max(today$confirmed), 2]), ]
  most_infected$increase <- c(
    0, most_infected$confirmed[-1] -
      most_infected$confirmed)[seq_len(nrow(most_infected))]
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(categories = most_infected$date) %>%
    hc_add_series(
      data = most_infected$increase,
      name = "Confirmed cases",
      color = "#00bb8b"
      ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(
      text = paste(
        "Daily number of new infections in",
        unique(most_infected$`Country/Region`)
        )
      ) %>%
    hc_xAxis(title = list(text = "Date")) %>%
    hc_yAxis(title = list(text = "Confirmed infections")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_12 <- renderHighchart({
  most_death <- daily_cases2[
    daily_cases2$`Country/Region` == as.character(
      today[today$deaths == max(today$deaths), 2]), ]
  most_death$increase <- c(
    0, most_death$deaths[-1] - most_death$deaths)[seq_len(nrow(most_death))]
  highchart() %>%
    hc_xAxis(categories = most_death$date, title = list(text = "Date")) %>%
    hc_yAxis(title = list(text = "Deaths")) %>%
    hc_add_series(
      data = most_death$deaths,
      name = "Deaths",
      color = "#fb5a19"
      ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(
      text = paste(
        "Number of deaths in",
        unique(most_death$`Country/Region`)
        )
      ) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_13 <- renderHighchart({
  most_death <- daily_cases2[
    daily_cases2$`Country/Region` == as.character(
      today[today$deaths == max(today$deaths), 2]), ]
  most_death$increase <- c(
    0, most_death$deaths[-1] - most_death$deaths)[seq_len(nrow(most_death))]
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(categories = most_death$date) %>%
    hc_add_series(
      data = most_death$increase,
      name = "Deaths",
      color = "#00bb8b"
      ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(
      text = paste(
        "Daily number of deaths in",
        unique(most_death$`Country/Region`)
        )
      ) %>%
    hc_xAxis(title = list(text = "Date")) %>%
    hc_yAxis(title = list(text = "Deaths")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})
