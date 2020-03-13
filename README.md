# Corona-19-shiny

Small visualization of confirmed and recovered Covid-19 cases, for World region and selected countries. Data is based on https://github.com/CSSEGISandData/COVID-19 and the Shiny-app (found here: https://r-kurs.shinyapps.io/Corona-19/) will update the latest data on (re-)load of the webpage.

Required files to build own Shiny-app:

* `global.R` - downloads and prepares data, initialize data frame
* `ui.R` - the user interface, defining check boxes and plot region
* `server.R` - defining interactive reactions and building ggplot2-output.
