a <- Sys.time()
# load johns hopkins data
source(file.path("server/data_wrangling", "jhu.R"), local = TRUE)

# load the country shapes
countries <- read_sf("data/ne_50m_admin_0_countries.shp")
# group the countries by their sovereign state
countries <- countries %>%
  group_by(SOVEREIGNT) %>%
  summarise(
    POP_EST = sum(POP_EST),
    ADMIN = unique(SOVEREIGNT),
    geometry = st_union(geometry)
  )

# load data for highcharter plots
source(file.path("server/data_wrangling", "highcharter_data.R"), local = TRUE)

# load html files
source(file.path("server/data_wrangling", "html_files.R"), local = TRUE)

# load grouped data
source(file.path("server/data_wrangling", "group_data.R"), local = TRUE)

today <- daily_cases2[daily_cases2$date == max(daily_cases2$date), ]
daily_cases2$geometry <- NULL

# load top_15 data
source(file.path("server/data_wrangling", "top_15.R"), local = TRUE)

# load increase data
source(file.path("server/data_wrangling", "increase_data.R"), local = TRUE)

# load forecasting data
source(file.path("server/data_wrangling", "forecasting.R"), local = TRUE)

# load s&p 500 data
source(file.path("server/data_wrangling", "s_n_p.R"), local = TRUE)

# load lockdown data
source(file.path("server/data_wrangling", "lockdown_data.R"), local = TRUE)
