confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
confirmed_long <- confirmed[, 1:5]
date <- str_split(colnames(confirmed)[5], "/")[[1]]
date_new <- paste(ifelse(nchar(date[1]) == 2, date[1], paste(0, date[1], sep = "")),
                  ifelse(nchar(date[2]) == 2, date[2], paste(0, date[2], sep = "")),
                  paste("20", date[3], sep = ""), sep = "/")
confirmed_long$date <- as.Date(date_new, format = "%m/%d/%Y")
colnames(confirmed_long)[5] <- "confirmed"

for (i in 6:ncol(confirmed)) {
  confirmed_new <- confirmed[, c(1:4, i)]
  colnames(confirmed_new)[5] <- "confirmed"
  date <- str_split(colnames(confirmed)[i], "/")[[1]]
  date_new <- paste(ifelse(nchar(date[1]) == 2, date[1], paste(0, date[1], sep = "")),
                    ifelse(nchar(date[2]) == 2, date[2], paste(0, date[2], sep = "")),
                    paste("20", date[3], sep = ""), sep = "/")
  confirmed_new$date <- as.Date(date_new, format = "%m/%d/%Y")
  confirmed_long <- rbind(confirmed_long, confirmed_new)
}

recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
recovered_long <- recovered[, 1:5]
colnames(recovered_long)[5] <- "recovered"

for (i in 6:ncol(recovered)) {
  recovered_new <- recovered[, c(1:4, i)]
  colnames(recovered_new)[5] <- "recovered"
  recovered_long <- rbind(recovered_long, recovered_new)
}

deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
deaths_long <- deaths[, 1:5]
colnames(deaths_long)[5] <- "deaths"

for (i in 6:ncol(deaths)) {
  deaths_new <- deaths[, c(1:4, i)]
  colnames(deaths_new)[5] <- "deaths"
  deaths_long <- rbind(deaths_long, deaths_new)
}


corona <- cbind(confirmed_long, recovered_long$recovered, deaths_long$deaths)
colnames(corona)[7:8] <- c("recovered", "deaths")
corona_0 <- corona[1:nrow(confirmed), ]
corona_0$confirmed <- 0
corona_0$recovered <- 0
corona_0$deaths <- 0
corona_0$date <- min(corona$date) - 1
corona <- rbind(corona_0, corona)
dates <- c(min(corona$date), max(corona$date))
corona_sf <- st_as_sf(corona, coords = c("Long", "Lat"), crs = 4326)
countries <- read_sf("data/ne_50m_admin_0_countries.shp")
corona_sf$`Province/State`[is.na(corona_sf$`Province/State`)] <- corona_sf$`Country/Region`[is.na(corona_sf$`Province/State`)]