library(rgdal)

data.dir <- '/home/matt/git-repos/wec-gis/data'
setwd(data.dir)
wi.counties <- readOGR(dsn = data.dir, layer = 'wi-counties')
df <- data.frame(read.csv('severe-weather.csv'))
wi.counties.join <- merge(wi.counties, df, by = 'GEOID')

writeOGR(wi.counties.join,
         dsn = data.dir,
         layer = 'wi-counties-with-data',
         driver = 'ESRI Shapefile',
         overwrite_layer = TRUE)

