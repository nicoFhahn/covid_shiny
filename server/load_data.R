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
data_pre <- corona_sf
data_pre$geometry <- NULL
daily_cases2 <- data_pre %>%
  group_by(date, `Country/Region`) %>%
  summarise(
    confirmed = sum(confirmed),
    deaths = sum(deaths)
  )
