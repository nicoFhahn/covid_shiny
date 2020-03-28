# download the data
confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
# now a whole bunch of preprocessing
# get the cases for the first day
confirmed_long <- confirmed[, 1:5]
# i hate dates and found no easier way to do this even though i know there is
date <- str_split(colnames(confirmed)[5], "/")[[1]]
date_new <- paste(ifelse(nchar(date[1]) == 2, date[1], paste(0, date[1], sep = "")),
  ifelse(nchar(date[2]) == 2, date[2], paste(0, date[2], sep = "")),
  paste("20", date[3], sep = ""),
  sep = "/"
)
confirmed_long$date <- as.Date(date_new, format = "%m/%d/%Y")
colnames(confirmed_long)[5] <- "confirmed"

# "slowly" transform into a long data set. once again better ways exist but yeah
confirmed_long2 <- lapply(6:ncol(confirmed), function(i, ...) {
  confirmed_new <- confirmed[, c(1:4, i)]
  colnames(confirmed_new)[5] <- "confirmed"
  date <- str_split(colnames(confirmed)[i], "/")[[1]]
  date_new <- paste(ifelse(nchar(date[1]) == 2, date[1], paste(0, date[1], sep = "")),
    ifelse(nchar(date[2]) == 2, date[2], paste(0, date[2], sep = "")),
    paste("20", date[3], sep = ""),
    sep = "/"
  )
  confirmed_new$date <- as.Date(date_new, format = "%m/%d/%Y")
  confirmed_new
})

confirmed_long <- rbind(confirmed_long, Reduce(rbind, confirmed_long2))

# old death
deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
if (any(!confirmed$`Country/Region` %in% deaths$`Country/Region`)) {
  frames <- deaths[1:sum(!confirmed$`Country/Region` %in% deaths$`Country/Region`), ]
  frames$`Province/State` <- confirmed$`Province/State`[
    !confirmed$`Country/Region` %in% deaths$`Country/Region`
  ]
  frames$`Country/Region` <- confirmed$`Country/Region`[
    !confirmed$`Country/Region` %in% deaths$`Country/Region`
  ]
  frames$Lat <- confirmed$Lat[
    !confirmed$`Country/Region` %in% deaths$`Country/Region`
  ]
  frames$Long <- confirmed$Long[
    !confirmed$`Country/Region` %in% deaths$`Country/Region`
  ]
  frames[
    seq_len(
      sum(!confirmed$`Country/Region` %in% deaths$`Country/Region`)
    ),
    5:ncol(frames)
  ] <- 0
  deaths <- rbind(deaths, frames)
}

deaths_long <- deaths[, 1:5]
colnames(deaths_long)[5] <- "deaths"

deaths_long2 <- lapply(6:ncol(deaths), function(i, ...) {
  deaths_new <- deaths[, c(1:4, i)]
  colnames(deaths_new)[5] <- "deaths"
  deaths_new
})
deaths_long <- rbind(deaths_long, Reduce(rbind, deaths_long2))

# bind together to one data.frame
corona <- cbind(confirmed_long, deaths_long$deaths)
# nicer colnames
colnames(corona)[7] <- c("deaths")
# add a day 0 frame
corona_0 <- corona[seq_len(nrow(confirmed)), ]
corona_0$confirmed <- 0
corona_0$deaths <- 0
corona_0$date <- min(corona$date) - 1
corona <- rbind(corona_0, corona)
# save the first and last date for later
dates <- c(min(corona$date), max(corona$date))
# turn into a geo data.frame
corona_sf <- st_as_sf(corona, coords = c("Long", "Lat"), crs = 4326)
# replace empty province with the country/region
corona_sf$`Province/State`[is.na(corona_sf$`Province/State`)] <- corona_sf$`Country/Region`[is.na(corona_sf$`Province/State`)]
# load the country shapes
countries <- read_sf("data/ne_50m_admin_0_countries.shp")

infected <- read_sf("data/infected.shp")
coords_inf <- read_sf("data/coords.shp")
january <- read_csv("data/january.csv")
february <- read_csv("data/february.csv")
feb_euro <- read_csv("data/february_europe.csv")
top10feb <- read_csv("data/top10_february.csv")
pandemic <- read_csv("data/pandemic.csv")
germany <- read_csv("data/germany.csv")
file_1 <- readLines("html_files/today_1.html")
file_2 <- readLines("html_files/today_2.html")
file_3 <- readLines("html_files/today_3.html")
file_4 <- readLines("html_files/leaders_4.html")
file_5 <- readLines("html_files/leaders_5.html")
file_6 <- readLines("html_files/leaders_6.html")
file_7 <- readLines("html_files/leaders_7.html")
file_8 <- readLines("html_files/leaders_8.html")
file_9 <- readLines("html_files/text_12.html")
file_10 <- readLines("html_files/today.html")
data_pre <- corona_sf
data_pre$geometry <- NULL
daily_cases2 <- data_pre %>%
  group_by(date, `Country/Region`) %>%
  summarise(
    confirmed = sum(confirmed),
    deaths = sum(deaths)
  )
today <- daily_cases2[daily_cases2$date == max(daily_cases2$date), ]
top15_confirmed <- today[today$confirmed %in% sort(today$confirmed, decreasing = TRUE)[1:15], ]
top15_confirmed <- top15_confirmed[order(top15_confirmed$confirmed, decreasing = TRUE), ]
top15_deaths <- today[today$confirmed %in% sort(today$confirmed, decreasing = TRUE)[1:15], ]
top15_deaths <- top15_deaths[order(top15_deaths$deaths, decreasing = TRUE), ]
today_inter <- st_intersection(countries, today)
per_capita <- data.frame(
  country = today_inter$Country.Region,
  confirmed = today_inter$confirmed,
  confirmed_per_capita = round(today_inter$confirmed / today_inter$POP_EST, 5) * 1000,
  deaths = today_inter$deaths,
  deaths_per_capita = round(today_inter$deaths / today_inter$POP_EST, 5) * 1000
)
top15_confirmed_per_capita <- per_capita[per_capita$confirmed_per_capita %in% sort(per_capita$confirmed_per_capita, decreasing = TRUE)[1:15], ]
top15_deaths_per_capita <- per_capita[per_capita$deaths_per_capita %in% sort(per_capita$deaths_per_capita, decreasing = TRUE)[1:15], ]
top15_confirmed_per_capita <- top15_confirmed_per_capita[order(top15_confirmed_per_capita$confirmed_per_capita, decreasing = TRUE), ]
top15_deaths_per_capita <- top15_deaths_per_capita[order(top15_deaths_per_capita$deaths_per_capita, decreasing = TRUE), ]
daily_cases3 <- daily_cases2[daily_cases2$date %in% c(max(daily_cases2$date), max(daily_cases2$date) - 14), ]
daily_cases3 <- daily_cases3[daily_cases3$`Country/Region` != "Diamond Princess", ]
splitted <- split(daily_cases3, daily_cases3$date)
df <- data.frame(
  country = unique(daily_cases3$`Country/Region`),
  increase = (splitted[[2]]$confirmed / splitted[[1]]$confirmed) * 100 - 100,
  old = splitted[[1]]$confirmed,
  new = splitted[[2]]$confirmed
)

df <- df[!is.infinite(df$increase), ]
df$increase <- round(df$increase)
df_most <- df[df$increase %in% df[order(df$increase, decreasing = TRUE), ][1:15, ]$increase, ]
df_most <- df_most[order(df_most$increase, decreasing = TRUE), ]
if (any(df_most$old[1:3] < 25)) {
  limit <- 250
} else if (any(df_most$old[1:3] < 50)) {
  limit <- 500
} else {
  limit <- mean(df_most$old) * 100
}
df_limit <- df[df$old >= limit, ]
df_limit_most <- df_limit[df_limit$increase %in% df_limit[order(df_limit$increase, decreasing = TRUE), ][1:15, ]$increase, ]
df_limit_most <- df_limit_most[order(df_limit_most$increase, decreasing = TRUE), ]
df_limit_few <- df_limit[df_limit$increase %in% df_limit[order(df_limit$increase), ][1:15, ]$increase, ]
df_limit_few <- df_limit_few[order(df_limit_few$increase), ]
df_few <- df[df$increase %in% df[order(df$increase), ][1:15, ]$increase, ]
df_few <- df_few[order(df_few$increase), ]

daily_cases <- data_pre %>%
  group_by(date) %>%
  summarise(
    confirmed = sum(confirmed),
    deaths = sum(deaths)
  )
inds <- daily_cases$date

## Create a time series object

myts_conf <- ts(daily_cases$confirmed, # random data
  start = c(2020, as.numeric(format(inds[1], "%j"))),
  frequency = 365
)
myts_death <- ts(daily_cases$deaths, # random data
  start = c(2020, as.numeric(format(inds[1], "%j"))),
  frequency = 365
)

forecast_conf <- forecast(auto.arima(myts_conf), h = 30, level = 95)
forecast_conf$lower <- round(forecast_conf$lower)
forecast_conf$upper <- round(forecast_conf$upper)
forecast_conf$mean <- round(forecast_conf$mean)
forecast_death <- forecast(auto.arima(myts_death), h = 30, level = 95)
forecast_death$lower <- round(forecast_death$lower)
forecast_death$upper <- round(forecast_death$upper)
forecast_death$mean <- round(forecast_death$mean)
dates_ts <- seq(from = daily_cases$date[1], to = max(daily_cases$date) + 30, by = 1)
forecast_df <- data.frame(
  date = dates_ts,
  confirmed = c(daily_cases$confirmed, rep(NA, length(dates_ts) - length(daily_cases$confirmed))),
  fitted_conf = c(rep(NA, length(dates_ts) - length(forecast_conf$mean)), forecast_conf$mean),
  upper95_conf = c(rep(NA, length(dates_ts) - length(forecast_conf$mean)), forecast_conf$upper),
  lower95_conf = c(rep(NA, length(dates_ts) - length(forecast_conf$mean)), forecast_conf$lower),
  deaths = c(daily_cases$deaths, rep(NA, length(dates_ts) - length(daily_cases$deaths))),
  fitted_death = c(rep(NA, length(dates_ts) - length(forecast_death$mean)), forecast_death$mean),
  upper95_death = c(rep(NA, length(dates_ts) - length(forecast_death$mean)), forecast_death$upper),
  lower95_death = c(rep(NA, length(dates_ts) - length(forecast_death$mean)), forecast_death$lower)
)


a <- try(getSymbols("^GSPC"), silent = TRUE)
if (a != "^GSPC") {
  gspc <- read_csv("data/gspc_backup.csv")
} else {
  gspc <- as.data.frame(GSPC)
  gspc$date <- as.Date(rownames(gspc))
}
