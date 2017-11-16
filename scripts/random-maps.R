# Adapted from "An Introduction to R for Spatial Analysis and Mapping" by Chris
# Brusdon and Lex Comber

library(rgdal)
library(spdep)
library(GISTools)

data.dir <- './wec-gis/data'
setwd(data.dir)
wi.counties <- readOGR(dsn = '.', layer = 'wi-counties-with-data')

plot.random.maps <- function(variable,colors) {
  # drawn six plots - one will contain the actual data, the other five will be
  # randomizations

  if (variable == 'Flood') {
    var <- 'flod_evt'
  } else if (variable == 'Tornado') {
    var <- 'torn_evt'
  } else if (variable == 'Hail') {
    var <- 'hail_evt'
  }

  par(mfrow=c(3,2),mar=c(1,1,1,1)/2) # set up the plotting area
  real.data.i <- sample(1:6,1) # select a random value between 1 and 6
  shades <- auto.shading(wi.counties[[var]], n = 9, cols = brewer.pal(9, colors)) # set up shades of colors

  for (i in 1:6) {
    if (i == real.data.i) {
      choropleth(wi.counties, wi.counties[[var]], shades)
    } else
    {
      choropleth(wi.counties, sample(wi.counties[[var]]), shades)
    }
  }
  return(real.data.i)
}

real.data <- plot.random.maps("Flood", "Greens")

# Try different values for both function arguments
# Variable 1:
#   Flood
#   Tornado
#   Hail
# Variable 2:
#   Greens
#   Reds
#   Blues
