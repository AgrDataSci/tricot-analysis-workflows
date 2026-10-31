#' Plot map using leaflet
#' @param data a data frame
#' @param xy index of data for the longitude and latitude coordinates (in that order)
#' @param make.clusters logical, if TRUE coordinates are aggregated by a defined cluster size
#' @param cut.tree numeric, to define the cluster size when make.clusters = TRUE
#' @param map_provider the name of the provider (see http://leaflet-extras.github.io/leaflet-providers/preview/ 
#'        and https://github.com/leaflet-extras/leaflet-providers)
#' @param minimap logical, TRUE to add the minimap
#' @param minimap_position the position of the mini map 
#' @examples
#' lonlat <- data.frame(lon = c(15.6, 16.7, 15.55, 15.551),
#'                      lat = c(65.8, 66.3, 66.25, 66.251))
#' 
#' p <- plot_map(lonlat, xy = c(1,2), cut.tree = 0.05)
plot_map <- function(data, 
                     xy = NULL,
                     make.clusters = TRUE,
                     cut.tree = 0.05,
                     map_provider = "Esri.WorldImagery",
                     minimap = TRUE,
                     minimap_position = "bottomright", 
                     ...){
  
  d <- data[, xy]
  
  # coerce to numeric
  d[1:2] <- lapply(d[1:2], as.numeric)
  
  # remove NAs
  d <- stats::na.omit(d)
  
  nd <- dim(d)[[1]]
  
  if (isTRUE(nd == 0)) {
    stop("No remaining coordinates to plot. ",
         "Please check for NAs or if the values can be coerced to numeric. \n")
  }
  
  names(d) <- c("lon","lat")
  
  if (isTRUE(make.clusters)) {
    # to ensure the privacy of participants location
    # we can put the lonlat info into clusters of 0.5 resolution
    h <- stats::dist(d)
    
    h <- stats::hclust(h)
    
    h <- stats::cutree(h, h = cut.tree)
    
    # split the d by each defined cluster
    d <- split(d, h)
    
    # and take the mean 
    d <- lapply(d, function(x) {
      colMeans(x)
    })
    
    # back to data frame
    d <- do.call("rbind", d)
    
    d <- as.data.frame(d)
    
    names(d) <- c("lon","lat")
    
  }
  
  
  map <- leaflet::leaflet(data = d, 
                          options = leaflet::leafletOptions(maxZoom = 17))
  
  map <- leaflet::fitBounds(map = map, lng1 = min(d$lon)-0.25, lat1 = min(d$lat)-0.25,
                            lng2 = max(d$lon)+0.25, lat2 = max(d$lat)+0.25)
  
  map <- leaflet::addProviderTiles(map = map, 
                                   provider =  map_provider, 
                                   options = leaflet::providerTileOptions(maxNativeZoom = 17))
  
  #map <- leaflet::addCircleMarkers(map = map)
  
  map <- leaflet::addMarkers(map)
  
  if (isTRUE(minimap)) {
    
    map <- leaflet::addMiniMap(map = map, position = minimap_position, 
                               width = 100, height = 100)
    
  }
  
  map$x$options = list("zoomControl" = FALSE)
  
  return(map)
  
}

plot_nodes <- function(l) {
  
  ids <- 1:length(l)
  nitems <- nrow(l[[1]])
  coeffs <- do.call("rbind", l)
  coeffs$node <- rep(ids, each = nitems)
  
  # Check font size for axis X and Y, and plot title
  ggplot2::ggplot(coeffs, 
                  ggplot2::aes(x = reliabity, 
                               y = item)) +
    ggplot2::geom_vline(xintercept = 0.5, 
                        colour = "red", size = 0.8) +
    ggplot2::geom_vline(xintercept = 0.6, 
                        colour = "#E5E7E9", size = 0.8) +
    ggplot2::geom_point(pch = 21, size = 2, 
                        fill = "black",colour = "black") +
    # ggplot2::geom_errorbarh(ggplot2::aes(xmin = bmin,
    #                                      xmax = bmax),
    #                         colour="black", height = 0.1) +
    # ggplot2::scale_x_continuous(limits = c(xmin, xmax),
    #                             breaks = xbreaks,
    #                             labels = xlabs) +
    ggplot2::facet_grid(. ~ node) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, angle = 0,
                                                       hjust = 0.5, vjust = 1, 
                                                       face = "plain",
                                                       colour = "black"),
                   axis.text.y = ggplot2::element_text(size = 13, angle = 0,
                                                       hjust = 1, vjust = 0.5, 
                                                       face = "plain",
                                                       colour = "black"),
                   text = ggplot2::element_text(size = 14),
                   strip.background = ggplot2::element_blank(),
                   plot.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(colour = "black", size = 1),
                   axis.ticks = ggplot2::element_line(colour = "black", size = 0.5),
                   axis.ticks.length = grid::unit(0.3, "cm"))
  
}


#' Plot reliability
#' @param x a data.frame
plot_reliability <- function(x, ci.level = 0.95, node.ids = NULL, n.obs = NULL, ...) {
  
  
  coeffs <- x
  coeffs$estimate <- coeffs$reliability
  coeffs$quasiSE <- coeffs$reliabilitySE
  bmin <- 0L
  bmax <- 0L
  items <- unique(x$item)
  
  coeffs <- split(coeffs, coeffs$node)
  
  coeffs <- lapply(coeffs, function(X) {
    X <- within(X, {
      bmax = X$estimate + stats::qnorm(1 - (1 - ci.level)/2) * 
        X$quasiSE
      bmin = X$estimate - stats::qnorm(1 - (1 - ci.level)/2) * 
        X$quasiSE
      items <- items
    })
    return(X)
  })
  
  for (i in seq_along(node.ids)) {
    coeffs[[i]] <- within(coeffs[[i]], {
      nobs <- n.obs[i]
      node <- node.ids[i]
    })
  }
  
  coeffs <- do.call("rbind", coeffs)
  if (isFALSE(log)) {
    coeffs$bmin <- ifelse(coeffs$bmin < 0, 0, coeffs$bmin)
    coeffs$bmax <- ifelse(coeffs$bmax > 1, 1, coeffs$bmax)
  }
  
  coeffs$id <- paste0(coeffs$node, "_", coeffs$items)
  groups <- ""
  coeffs$groups <- groups
  node_lev <- unique(paste0("Node ", coeffs$node, " (n=", coeffs$nobs, 
                            ")"))
  coeffs$id <- coeffs$node
  coeffs$node <- factor(paste0("Node ", coeffs$node, " (n=", 
                               coeffs$nobs, ")"), levels = node_lev)
  coeffs$items <- factor(coeffs$items, levels = rev(sort(items)))
  xmax <- round(max(coeffs$bmax, na.rm = TRUE) + 0.01, digits = 4)
  xmin <- 0
  xinter <- 0.5
  xbreaks <- round(c(mean(c(0, xmax)), xmax), 2)
  xbreaks <- c(0, xbreaks)
  xlabs <- as.character(round(xbreaks, 2))
  xmin <- xmin + (xmin * 0.15)
  xmax <- xmax + (xmax * 0.15)
  
  ggplot2::ggplot(coeffs, ggplot2::aes(x = estimate, y = items)) + 
    ggplot2::geom_vline(xintercept = xinter, colour = "#E5E7E9", 
                        size = 0.8) + ggplot2::geom_text(ggplot2::aes(label = groups), 
                                                         position = ggplot2::position_nudge(y = 0.35, x = 0)) + 
    ggplot2::geom_point(pch = 21, size = 2, fill = "black", 
                        colour = "black") + ggplot2::geom_errorbarh(ggplot2::aes(xmin = bmin, 
                                                                                 xmax = bmax), colour = "black", height = 0.1) + ggplot2::scale_x_continuous(limits = c(xmin, 
                                                                                                                                                                        xmax), breaks = xbreaks, labels = xlabs) + ggplot2::facet_grid(. ~ 
                                                                                                                                                                                                                                         node) + ggplot2::theme_bw() + ggplot2::labs(x = "", y = "") + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, 
                                                       angle = 0, hjust = 0.5, vjust = 0.5, face = "plain", 
                                                       colour = "black"), axis.text.y = ggplot2::element_text(size = 13, 
                                                                                                              angle = 0, hjust = 1, vjust = 0.5, face = "plain", 
                                                                                                              colour = "black"), text = ggplot2::element_text(size = 14), 
                   strip.background = ggplot2::element_blank(), plot.background = ggplot2::element_blank(), 
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                   panel.border = ggplot2::element_rect(colour = "black", 
                                                        size = 1), axis.ticks = ggplot2::element_line(colour = "black", 
                                                                                                      size = 0.5), axis.ticks.length = grid::unit(0.3, 
                                                                                                                                                  "cm"))
  
  
  
}








