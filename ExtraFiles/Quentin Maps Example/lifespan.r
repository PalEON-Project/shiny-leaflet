# Species trait analysis for biomass paper

library(XLConnect)
pls <- readWorksheetFromFile('pls.xlsx', sheet='DATA', header=TRUE)

# Get rid of rows with species not in the PLS region
plstrait <- subset(pls, Level.3a.classification != 'NA')
plstrait <- transform(plstrait, Latin = paste(Genus, species))

# Make names match
plstrait[plstrait=='Cedar/juniper'] <- 'Cedar.juniper'
plstrait[plstrait=='Tulip poplar'] <- 'Tulip.poplar'

# Tree abundance data from the PLS data
plscount <- read.csv('plss_allcts_v0.9.csv', header = TRUE)
# PFT abundance data from PLS
plspft <- read.csv('plss_allcts_pft_v0.9.csv', header = TRUE)

# Tree abundance data from the FIA dataset
fia <- read.csv('fia.csv', header = TRUE)
fia[,-(1:3)][is.na(fia[,-(1:3)])] <- 0

# Not spatial, just overall
plssums <- apply(plscount[,-(1:4)], 2, sum)
plsprop <- plssums/sum(plssums)
fiasums <- apply(fia[,-(1:3)], 2, sum)
fiaprop <- fiasums/sum(fiasums)

# Check for holes in average lifespan data
plstrait$Latin[is.na(plstrait$TYPMORT) & is.na(plstrait$maximum.lifespan..typ)]
# Turns out not to be a big problem.

# For now just average the Silvics and Loehle lifespan estimates (Some are probably derived from the same source anyway.)
plstrait <- transform(plstrait, lifespan = apply(data.frame(TYPMORT.years., maximum.lifespan..typical..silvics.), 1, mean, na.rm = TRUE))
plstrait$lifespan[is.nan(plstrait$lifespan)] <- NA

# Aggregate lifespans from the species level to the level 3a classification
ages <- with(plstrait, tapply(lifespan, Level.3a.classification, mean, na.rm=TRUE))


ages <- ages[!is.nan(ages)] # cut out trees with no age data
decid <- rep(TRUE,length(ages))
decid[c(6,11,13,19,21,23)] <- FALSE # identify the coniferous trees.



# Real data from PLS
matches <- match(names(plsprop), names(ages)) # Leaving out the alder, ironwood, no.tree, other.hardwood, and unknown.tree
matches.fia <- match(names(fiaprop), names(ages))

plspropUsed <- plsprop[!is.na(matches)]
fiapropUsed <- fiaprop[!is.na(matches.fia)]
agesUsed <- ages[matches[!is.na(matches)]]

decid.fia <- rep(TRUE, length(fiapropUsed))
decid.fia[c(1, 7, 14, 15, 22)] <- FALSE # add 12 if tamarack is coniferous.



# Make raster map of %deciduous, %coniferous, and average lifespan in different cell sizes for the PLS data.

library(raster)
plsr <- SpatialPixelsDataFrame(points=plscount[,(2:3)], data = plscount[,-(1:4)])
fiar <- SpatialPixelsDataFrame(points=fia[,(2:3)], data = fia[,-(1:3)])
gridded(plsr) <- TRUE
gridded(fiar) <- TRUE

# Get the values for each aggregated cell
plsdata <- plsr@data
fiadata <- fiar@data
plsUsed <- plsdata[, !is.na(matches)]
fiaUsed <- fiadata[, !is.na(matches.fia)]
decidUsed <- decid[matches[!is.na(matches)]]

avglifespan <- apply(plsUsed, 1, function(w) weighted.mean(x = agesUsed, w = w))
abunddecid <- apply(plsUsed, 1, function(x) sum(x[decidUsed]))
abundconif <- apply(plsUsed, 1, function(x) sum(x[!decidUsed]))
avglifespan.fia <- apply(fiaUsed, 1, function(w) weighted.mean(agesUsed[names(fiaUsed)], w = w))
abunddecid.fia <- apply(fiaUsed, 1, function(x) sum(x[decid.fia]))
abundconif.fia <- apply(fiaUsed, 1, function(x) sum(x[!decid.fia]))

pdata <- data.frame(lifespan = avglifespan, abunddecid, abundconif, grasspft = plspft[,5], decidpft = plspft[,6], larchpft = plspft[,7], conifpft = plspft[,8])
dimnames(pdata)[[1]] <- dimnames(plsdata)[[1]]
fdata <- data.frame(lifespan = avglifespan.fia, abunddecid = abunddecid.fia, abundconif = abundconif.fia)
dimnames(fdata)[[1]] <- dimnames(fiadata)[[1]]


# add computed data to spatial data frame.
plsr$lifespan <- avglifespan
plsr$abunddecid <- with(pdata, decidpft + larchpft)
plsr$abundconif <- pdata$conifpft
plsr$propdecid <- with(pdata, (decidpft+larchpft)/(decidpft+conifpft+larchpft))

plsraster <- brick(plsr)

fiar$lifespan <- avglifespan.fia
fiar$abunddecid <- abunddecid.fia
fiar$abundconif <- abundconif.fia
fiar$propdecid <- abunddecid.fia/(abunddecid.fia + abundconif.fia)

fiaraster <- brick(fiar)


# Bin the data points and then calculate variance in lifespan for each chunk of PFT abundance types.
nbin <- 10
bindata <- plsr@data
bindata.fia <- fiar@data
bindata <- transform(bindata,
                     decidbin = cut(abunddecid, nbin),
                     conifbin = cut(abundconif, nbin))
bindata.fia <- transform(bindata.fia,
                         decidbin = cut(abunddecid, nbin),
                         conifbin = cut(abundconif, nbin))
library(plyr)
binlifespan <- ddply(bindata, .(decidbin, conifbin), summarize,
                     lifespan_mean = mean(lifespan, na.rm = TRUE),
                     lifespan_sd = sd(lifespan, na.rm = TRUE),
                     abunddecid = mean(abunddecid, na.rm = TRUE),
                     abundconif = mean(abundconif, na.rm = TRUE))
binlifespan.fia <- ddply(bindata.fia, .(decidbin, conifbin), summarize,
                         lifespan_mean = mean(lifespan, na.rm = TRUE),
                         lifespan_sd = sd(lifespan, na.rm = TRUE),
                         abunddecid = mean(abunddecid, na.rm = TRUE),
                         abundconif = mean(abundconif, na.rm = TRUE))
