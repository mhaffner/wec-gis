library(rgdal)

## subset wisconsin counties from entire u.s.
us.counties <- readOGR(dsn = '/home/matt/Downloads', layer = 'tl_2017_us_county')
wi.counties <- us.counties[us.counties$STATEFP == '55',]

## save wisconsin counties in data directory
writeOGR(obj = wi.counties, dsn = '/home/matt/git-repos/wec-gis/data', layer = 'wi-counties', driver = 'ESRI Shapefile')
