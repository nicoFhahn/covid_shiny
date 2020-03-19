# download the data
confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
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

# now do the same for the recovered and deceased cases
recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
recovered_long <- recovered[, 1:5]
colnames(recovered_long)[5] <- "recovered"

recovered_long2 <- lapply(6:ncol(recovered), function(i, ...) {
  recovered_new <- recovered[, c(1:4, i)]
  colnames(recovered_new)[5] <- "recovered"
  recovered_new
})

recovered_long <- rbind(recovered_long, Reduce(rbind, recovered_long2))

deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
deaths_long <- deaths[, 1:5]
colnames(deaths_long)[5] <- "deaths"

deaths_long2 <- lapply(6:ncol(deaths), function(i, ...) {
  deaths_new <- deaths[, c(1:4, i)]
  colnames(deaths_new)[5] <- "deaths"
  deaths_new
})

deaths_long <- rbind(deaths_long, Reduce(rbind, deaths_long2))

# bind together to one data.frame
corona <- cbind(confirmed_long, recovered_long$recovered, deaths_long$deaths)
# nicer colnames
colnames(corona)[7:8] <- c("recovered", "deaths")
# add a day 0 frame
corona_0 <- corona[1:nrow(confirmed), ]
corona_0$confirmed <- 0
corona_0$recovered <- 0
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
