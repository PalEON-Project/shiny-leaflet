require(reshape2)
require(grid)

add_map_albers <- function(plot_obj, map_data = usShp, dat){
  p <- plot_obj + geom_path(data = map_data, aes(x = long, y = lat, group = group), size = 0.1) +
    scale_x_continuous(limits = c(min(dat$X, na.rm = TRUE), max(dat$X, na.rm = TRUE))) +
    scale_y_continuous(limits = c(min(dat$Y, na.rm = TRUE), max(dat$Y, na.rm = TRUE)))
  return(p)
}

theme_clean <- function(plot_obj){
  plot_obj <- plot_obj + theme_bw() + theme(axis.ticks = element_blank(), 
                                            axis.text.y = element_blank(), 
                                            axis.text.x = element_blank(),
                                            axis.title.x = element_blank(),
                                            axis.title.y = element_blank()) 
  
  return(plot_obj)
}

make_veg_map <- function(data, breaks, coords, legendName = 'Proportions', map_data = usShp, facet = TRUE, col = terrain.colors, reverse_colors = TRUE, print = TRUE, ncol = 5, legend = TRUE) {
  
  make_map <- function() {
    
    # to avoid repeating code for facet = TRUE, FALSE; this just uses variables in make_veg_map frame
    if(facet) {
      names(taxon_dat) <- gsub("ZZZ", "/", names(taxon_dat))
      names(taxon_dat) <- gsub("\\.", " ", names(taxon_dat))
    } else {
      nm <- colnames(data)[p]
      nm <- gsub("ZZZ", "/", nm)
      nm <- gsub("\\.", " ", nm)
    }
    taxon_dat_long <- melt(taxon_dat, c('X', 'Y'))
    
    col <- col(length(breaks)-1)
    if(reverse_colors) col <- rev(col)
    if(legend) guide <- "legend" else guide <- "none"
    d <- ggplot() + geom_raster(data = taxon_dat_long, aes(x = X, y = Y, fill = factor(value))) + coord_fixed() + scale_fill_manual(labels = breaklabels, name = legendName, drop = FALSE, values = col, guide = guide) + theme(strip.text.x = element_text(size = 16), legend.key.size = unit(1.5, "cm"), legend.text = element_text(size = 16), legend.title = element_text(size = 16))
    d <- add_map_albers(plot_obj = d, map_data = map_data, dat = taxon_dat_long)
    if(facet) {
      d <- d + facet_wrap(~variable, ncol = ncol)
    } else {
      d <- d + ggtitle(nm)
    }
    d <- theme_clean(d)
    return(d)
  }
  
  data_binned <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  colnames(data_binned) <- colnames(data)
  
  for (p in seq_len(ncol(data))) {
    data_binned[ , p] <- cut(data[ , p], breaks, include.lowest = TRUE, labels = FALSE)
  }
  
  breaklabels <- apply(cbind(breaks[1:(length(breaks)-1)], breaks[2:length(breaks)]), 1, 
                       function(r) { sprintf("%0.2f - %0.2f", r[1], r[2]) })
  
  if(facet) {
    taxon_dat <- data.frame(X = coords$X, Y = coords$Y, data_binned)
    d <- make_map()
    if(print) print(d)
  } else {
    for(p in seq_len(ncol(data))) {
      taxon_dat <- data.frame(X = coords$X, Y = coords$Y, Preds = data_binned[ , p])
      d <- make_map()
      if(print) print(d)
    }
  }
  
  invisible(d)
}
