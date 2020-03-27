output$highcharter_1 <- renderHighchart({
  highchart() %>%
    hc_yAxis_multiples(
      list(title = list(text = "Number of cases in China")),
      list(
        opposite = TRUE,
        title = list(text = "Number of cases outside China")
      )
    ) %>%
    hc_xAxis(categories = january$date, title = list(text = "Date")) %>%
    hc_add_series(
      name = "Confirmed cases in China  ",
      data = january$confirmed[january$`Country/Region` == "China"],
      color = "#5BC0EB"
    ) %>%
    hc_add_series(
      name = "Confirmed cases outside China",
      data = january$confirmed[january$`Country/Region` != "China"],
      yAxis = 1,
      color = "#FDE74C"
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
      color = "#F8333C"
    ) %>%
    hc_add_series(
      name = "Iran",
      data = february$confirmed[february$`Country/Region` == "Iran"],
      color = "#44AF69"
    ) %>%
    hc_add_series(
      name = "South Korea",
      data = february$confirmed[february$`Country/Region` == "Korea, South"],
      color = "#88CCF1"
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
      daily_cases$confirmed
  )[seq_len(nrow(daily_cases))]
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
      daily_cases$confirmed
  )[seq_len(nrow(daily_cases))]
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
      today[today$confirmed == max(today$confirmed), 2]
    ),
  ]
  most_infected$increase <- c(
    0, most_infected$confirmed[-1] -
      most_infected$confirmed
  )[seq_len(nrow(most_infected))]
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
      today[today$confirmed == max(today$confirmed), 2]
    ),
  ]
  most_infected$increase <- c(
    0, most_infected$confirmed[-1] -
      most_infected$confirmed
  )[seq_len(nrow(most_infected))]
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
      today[today$deaths == max(today$deaths), 2]
    ),
  ]
  most_death$increase <- c(
    0, most_death$deaths[-1] - most_death$deaths
  )[seq_len(nrow(most_death))]
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
      today[today$deaths == max(today$deaths), 2]
    ),
  ]
  most_death$increase <- c(
    0, most_death$deaths[-1] - most_death$deaths
  )[seq_len(nrow(most_death))]
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

output$highcharter_14 <- renderHighchart({
  highchart() %>%
    hc_xAxis(
      categories = unique(daily_cases2$date),
      title = list(text = "Date"),
      plotLines = list(
        list(
          label = list(text = "U.S. leads in confirmed cases"),
          color = "#00bb8b",
          width = 2,
          value = 65
        )
      )
    ) %>%
    hc_yAxis(title = list(text = "Confirmed cases")) %>%
    hc_add_series(
      name = "China",
      data = daily_cases2$confirmed[daily_cases2$`Country/Region` == "China"],
      color = "#5BC0EB"
    ) %>%
    hc_add_series(
      name = "Italy",
      data = daily_cases2$confirmed[daily_cases2$`Country/Region` == "Italy"],
      color = "#F8333C"
    ) %>%
    hc_add_series(
      name = "United States",
      data = daily_cases2$confirmed[daily_cases2$`Country/Region` == "US"],
      color = "#FAFAFA"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(
      text = "Number of confirmed COVID-19
      cases"
    ) %>%
    hc_legend(align = "left") %>%
    hc_chart(backgroundColor = "#161616")
})
