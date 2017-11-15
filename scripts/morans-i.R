library(rgdal)
library(spdep)

data.dir <- '/home/matt/git-repos/wec-gis/data'
setwd(data.dir)
wi.counties <- readOGR(dsn = data.dir, layer = 'wi-counties-with-data')

moran.iter.neigh <- function(var,coords,min.neigh,max.neigh,interval){
  moran.results <- data.frame(matrix(NA, nrow=length(seq(min.neigh,max.neigh,interval)),ncol=4)) # prepare dataframe
  colnames(moran.results) <- c("neighbors", "morans_i", "variance", "p.value") # rename columns
  neigh.list <- seq(min.neigh,max.neigh,interval) # list of no. of neighbors to use

  for (i in 1:length(neigh.list)){
    nb.obj <- make.sym.nb(knn2nb(knearneigh(coords, k=neigh.list[i], longlat=TRUE)))
    result <- moran.test(var,nb2listw(nb.obj))
    moran.results$neighbors[i] <- neigh.list[i] # number of neighbors
    moran.results$morans_i[i] <- result$estimate[1] # moran's i stat
    moran.results$variance[i] <- result$estimate[3] # variance
    moran.results$p.value[i] <- result$p.value # p.value
  }
  return(moran.results)
}

moran.iter.neigh(wi.counties$torn_evt, # variable
                 coordinates(wi.counties), # county centroids
                 2, # number of neighbors to start with
                 16, # number of neighbors to end with
                 2) # increment of neighbors

moran.iter.neigh(wi.counties$hail_evt, # variable
                 coordinates(wi.counties), # county centroids
                 2, # number of neighbors to start with
                 16, # number of neighbors to end with
                 2) # increment of neighbors
