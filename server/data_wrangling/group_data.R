daily_cases <- corona_sf %>%
  group_by(date) %>%
  summarise(
    confirmed = sum(confirmed),
    deaths = sum(deaths)
  )

daily_cases2 <- corona_sf %>%
  group_by(date, `Country/Region`) %>%
  summarise(
    confirmed = sum(confirmed),
    deaths = sum(deaths)
  )

