# Adapted from "An Introduction to R for Spatial Analysis and Mapping" by Chris
# Brusdon and Lex Comber

library(rgdal)
library(spdep)

if (Sys.info()["nodename"] == "panopticon" | Sys.info()["nodename"] == "manjaro") {
  setwd('/home/matt/git-repos/')
}
wi.counties <- readOGR(dsn = './wec-gis/data', layer = 'wi-counties-with-data')

moran.iter.neigh <- function(var,min.neigh,max.neigh,interval){
  coords <- coordinates(wi.counties)
  moran.results <- data.frame(matrix(NA, nrow=length(seq(min.neigh,max.neigh,interval)),ncol=4)) # prepare dataframe
  colnames(moran.results) <- c("neighbors", "morans_i", "variance", "p.value") # rename columns
  neigh.list <- seq(min.neigh,max.neigh,interval) # list of no. of neighbors to use

  if (var == 'Flood') {
    var <- 'flod_evt'
  } else if (var == 'Tornado') {
    var <- 'torn_evt'
  } else if (var == 'Hail') {
    var <- 'hail_evt'
  }

  for (i in 1:length(neigh.list)){
    nb.obj <- make.sym.nb(knn2nb(knearneigh(coords, k=neigh.list[i], longlat=TRUE)))
    result <- moran.test(wi.counties[[var]],nb2listw(nb.obj))
    moran.results$neighbors[i] <- neigh.list[i] # number of neighbors
    moran.results$morans_i[i] <- result$estimate[1] # moran's i stat
    moran.results$variance[i] <- result$estimate[3] # variance
    moran.results$p.value[i] <- result$p.value # p.value
  }
  return(moran.results)
}

# calculate Moran's I on tornadoes in Wisconsin counties
moran.iter.neigh('Flood', # variable
                 2, # number of neighbors to start with
                 16, # number of neighbors to end with
                 2) # increment of neighbors
