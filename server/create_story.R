data_pre <- corona_sf
data_pre$geometry <- NULL
daily_cases <- data_pre %>%
  group_by(date) %>%
  summarise(
    confirmed = sum(confirmed),
    deaths = sum(deaths)
  )
daily_cases2 <- data_pre %>%
  group_by(date, `Country/Region`) %>%
  summarise(
    confirmed = sum(confirmed),
    deaths = sum(deaths)
  )

infected_world <- daily_cases[daily_cases$date == max(daily_cases$date), ]$confirmed
died <- daily_cases[daily_cases$date == max(daily_cases$date), ]$deaths
file_1[14] <- paste("To date", infected_world, "people have been infected with the virus and", died, "have died.")
today <- daily_cases2[daily_cases2$date == max(daily_cases$date), ]
country <- as.character(today[today$confirmed == max(today$confirmed), 2])
if (country == "US") {
  country <- "the United States"
}
file_2[8] <- paste(
  "In ",
  country,
  ", ",
  as.character(today[today$confirmed == max(today$confirmed), 3]),
  " people are infected with the virus, making it the country with the most infections worldwide.",
  sep = ""
)
country <- as.character(today[today$deaths == max(today$deaths), 2])
if (country == "US") {
  country <- "the United States"
}
file_3[8] <- paste(
  as.character(today[today$deaths == max(today$deaths), 4]),
  "people have died in",
  country,
  "as a result of the coronavirus. <br>"
)

file_3[9] <- paste(
  "This makes",
  country,
  "the country with the most deaths attributable to the virus."
)

writeLines(file_1, "html_files/today_1.html")
writeLines(file_2, "html_files/today_2.html")
writeLines(file_3, "html_files/today_3.html")
