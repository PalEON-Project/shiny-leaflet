#!/usr/bin/Rscript
# info on the Albers grid and subsets for the western and eastern composition modeling

###############################################################
# Setting full Albers grid ------------------------------------
###############################################################

# from rasters page on Paleon Wiki
xRange <- c(-71000, 2297000)
yRange <- c(58000, 1498000)
gridSpacing <- 8000
buffer <- 0 


if(buffer > 0) {
  xRange <- xRange + c(-1,1)*buffer*gridSpacing
  yRange <- yRange + c(-1,1)*buffer*gridSpacing
}

cat("Using overall Albers grid with grid spacing of ", gridSpacing, ", x range of (", xRange[1], " ", xRange[2], ") and y range of (", yRange[1], " ", yRange[2], ").\n", sep = "")

xGrid <- seq(xRange[1] + gridSpacing/2, xRange[2] - gridSpacing/2, by = gridSpacing)
yGrid <- seq(yRange[1] + gridSpacing/2, yRange[2] - gridSpacing/2, by = gridSpacing)

xRes <- length(xGrid)
yRes <- length(yGrid)

###############################################################
# Subset used for western -------------------------------------
###############################################################

westernDomainX <- 1:(146 + 2*buffer)  # (without buffer: thru x=1093000 out of 1:296)
westernDomainY <- 1:(180 + 2*buffer)  # full N-S
easternLimitOfWesternDomain <- max(xGrid[westernDomainX])

###############################################################
# Subset used for eastern -------------------------------------
###############################################################

# use 162-296 columns for modeling PA and east
# use 117-296 for modeling OH and east
# use 24:145 rows for PA and east
# use 24:163 rows for OH and east

print("check that eastern domain with buffer is ok")
easternDomainX <- 117:xRes
easternDomainY <- 24:(163 + 2*buffer)
