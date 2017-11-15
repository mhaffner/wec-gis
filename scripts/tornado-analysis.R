library(GISTools)
data(tornados)

# plot data to get an idea of what we're looking at
plot(us_states2)
plot(torn2, add = TRUE)

# subset states to get just Wisconsin
wi <- us_states2[us_states2$STATE_NAME == 'Wisconsin',]
plot(wi)
plot(torn2, add = TRUE)

# spatial intersection (analogous to ArcGIS's clip function)
wi.torn <- gIntersection(wi, torn2)
plot(wi) ; plot(wi.torn, add = TRUE)

# get tornadoes inside and within 25km of Wisconsin
wi.buf <- gBuffer(wi, width = 25000)
wi.buf.torn <- gIntersection(wi.buf, torn2)
plot(wi.buf)
plot(wi.plus.buf.torn, add = TRUE)
plot(wi, add = TRUE)

# get tornadoes only between the buffer and Wisconsin
buf.clip <- gDifference(wi.buf, wi)
buf.clip.torn <- gIntersection(buf.clip, torn2)
plot(buf.clip)
plot(buf.clip.torn, add = T)

# get number of tornadoes in Wisconsin
wi.torn.count <- poly.counts(torn2,wi)
wi.torn.count

# which state has the most tornadoes?
# get number of tornadoes in each state
us.torn.count <- poly.counts(torn2,us_states2)
hist(us.torn.count)
max(us.torn.count)
as.character(us_states2$STATE_NAME[match(max(us.torn.count),us.torn.count)])

# which state has the most tornadoes per sq. mile.?
# get number of tornadoes/land area in each state
us.torn.over.area <- poly.counts(torn2,us_states2)/us_states2$AREA
hist(us.torn.over.area)
as.character(us_states2$STATE_NAME[match(max(us.torn.over.area),us.torn.over.area)])

# are tornadoes clustered, dispersed, or random?
library(spatstat)
wi.torn.ppp <- ppp(wi.torn$x, wi.torn$x, "window")
nnd <- nndist.ppp(wi.torn)
hist(nnd)
