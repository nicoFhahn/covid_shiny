daily_cases <- corona_sf %>%
  group_by(date) %>%
  summarise(
    confirmed = sum(confirmed),
    deaths = sum(deaths)
  )


corona_no_sf <- corona_sf
corona_no_sf$geometry <- NULL
daily_cases2 <- corona_no_sf %>%
  group_by(date, `Country/Region`) %>%
  summarise(
    confirmed = sum(confirmed),
    deaths = sum(deaths)
  )
daily_cases2 <- daily_cases2[order(daily_cases2$date, daily_cases2$`Country/Region`), ]
a <- corona_sf[!duplicated(corona_sf$`Country/Region`), ]
a <- a[order(a$`Country/Region`), ]
daily_cases2$geometry <- rep(a$geometry, length(unique(daily_cases2$date)))
daily_cases2 <- st_as_sf(daily_cases2)
