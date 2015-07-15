# changes over time between pre- and post-settlement (PLS vs FIA) tree lifespans.

# First standardize the PFT data so that we can compare the two.
pdata <- transform(pdata, decidstd = (decidpft + larchpft)/(decidpft+larchpft+conifpft))
fdata <- transform(fdata, decidstd = abunddecid/(abunddecid+abundconif))

plsr$decidstd <- pdata$decidstd
fiar$decidstd <- fdata$decidstd

plsraster <- brick(plsr)
fiaraster <- brick(fiar)

decidchange <- overlay(fiaraster$decidstd, plsraster$decidstd, fun = function(x,y) x - y)
lifespanchange <- overlay(fiaraster$lifespan, plsraster$lifespan, fun = function(x,y) x - y)

# Make a map with state boundaries

library(maptools)
states <- readShapePoly('us_alb.shp')

lsc <- extract(lifespanchange, extent(lifespanchange))
hist(lsc)
rangelsc <- range(lsc, na.rm = TRUE)
pdf('lifespanchange.pdf', height=5, width=5)
plot(lifespanchange, breaks = seq(-rangelsc[1], rangelsc[1], length.out=11),
     col = RColorBrewer::brewer.pal(11, 'RdBu'),
     main = 'Change in average lifespan since settlement')
plot(states, add=TRUE)
dev.off()

dc <- extract(decidchange, extent(decidchange))
hist(dc)
rangedc <- range(dc, na.rm = TRUE)
pdf('decidchange.pdf', height=5, width=5)
plot(decidchange, breaks = seq(-1, 1, length.out=11),
     col = RColorBrewer::brewer.pal(11, 'RdBu'),
     main = 'Change in %deciduous since settlement')
plot(states, add=TRUE)
dev.off()

# Do this again but aggregated so there aren't gaps in the map.

# Aggregate into larger cells

SIZE <- 3

gt_p <- with(gridparameters(plsr), GridTopology(cellcentre.offset, cellsize*SIZE, cells.dim/SIZE))
gt_f <- with(gridparameters(fiar), GridTopology(cellcentre.offset, cellsize*SIZE, cells.dim/SIZE))

plsagg <- aggregate(plsr, SpatialGrid(gt_p))
fiaagg <- aggregate(fiar, SpatialGrid(gt_f))

plsrasterAgg <- brick(plsagg)
fiarasterAgg <- brick(fiaagg)

decidchange <- overlay(fiarasterAgg$decidstd, plsrasterAgg$decidstd, fun = function(x,y) x - y)
lifespanchange <- overlay(fiarasterAgg$lifespan, plsrasterAgg$lifespan, fun = function(x,y) x - y)

lsc <- extract(lifespanchange, extent(lifespanchange))
hist(lsc)
rangelsc <- range(lsc, na.rm = TRUE)
plot(lifespanchange, breaks = seq(-rangelsc[1], rangelsc[1], length.out=11),
     col = RColorBrewer::brewer.pal(11, 'RdBu'))

dc <- extract(decidchange, extent(decidchange))
hist(dc)
rangedc <- range(dc, na.rm = TRUE)
plot(decidchange, breaks = seq(-1, 1, length.out=11),
     col = RColorBrewer::brewer.pal(11, 'RdBu'))



