# get the country that was clicked
get_country <- eventReactive(input$mymap_click, {
  # get the coordinates of the click
  coords <- data.frame(lng = input$mymap_click$lng, lat = input$mymap_click$lat)
  # turn into points
  coords <- st_as_sf(coords, coords = c("lng", "lat"), crs = 4326)
  # match with the country shapes
  country <- countries[unlist(st_intersects(coords, countries)), ]
  # either return the country or world
  if (nrow(country) == 0) {
    country <- "world"
  } else {
    country <- country$ADMIN
  }
})

# get the date
get_date <- eventReactive(input$date, {
  input$date
})
