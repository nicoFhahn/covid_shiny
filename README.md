# Covid-19: Interactive Coronavirus Map with R and Shiny
Covid-19 uses the data from Johns Hopkins University to visualize the outbreak of the novel coronavirus. The data is available from [this repository](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data).

The app uses the [mapbox API](https://docs.mapbox.com/api/). If you have an API key, save it in a **key.env** file that looks like this:
```{bash}
MAPBOX_KEY=YOUR_SECRET_API_KEY
```
If you do not have such a key, the app will display a dark basemap from carto.
# Run it on your machine
You can run Covid-19 on your own machine using the following code:
```R
packages = c("dotenv", "fresh", "leaflet", "plotly", "readr", "sf", "shiny", "stringr")
install.packages(packages, repos = "https://cran.rstudio.com/")
library(shiny)
runGitHub("covid_shiny", "nicoFhahn")
```
# Screenshot
![alt text](https://i.imgur.com/LXsXsbY.png "Logo Title Text 1")

### Acknowledgments
The folks over at RStudio with the [SuperZIP demo](https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example) which heavily inspired the layout of this app.

### Questions/issuses/contacts
nico@f-hahn.de, or open a GitHub issue
