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

#' Get node rules 
#' 
#' Get the node rules from a party object
#' 
#' @param x an object of class party
node_rules <- function(x, ...){
  
  node_ids <- partykit::nodeids(x, terminal = TRUE)
  
  result <- data.frame()
  
  for (i in seq_along(node_ids)) {
    r <- data.frame(node = node_ids[i],
                    rules = partykit:::.list.rules.party(x, node_ids[i]))
    
    result <- rbind(result, r)
    
  }
  
  rule <- result$rules
  rule <- gsub("%in%","@", rule)
  rule <- gsub("[(]|[)]| c","", rule)
  rule <- gsub("  "," ", rule)
  rule <- gsub('"NA",',"", rule)
  rule <- gsub(', "NA"',"", rule)
  rule <- gsub(",", "COMMA", rule)
  rule <- gsub("[.]", "DOT", rule)
  rule <- gsub("@", "EQUAL", rule)
  rule <- gsub("&", " AND ", rule)
  rule <- gsub("<=", " LOWEQUAL ", rule)
  rule <- gsub("=>", " HIGHEQUAL ", rule)
  rule <- gsub("<", " LOW ", rule)
  rule <- gsub(">", " HIGH ", rule)
  
  # remove all other special characters
  rule <- gsub("[[:punct:]]", "", rule)
  
  # reposition the key special characters
  rule <- gsub("  ", " ", rule)
  rule <- gsub("LOWEQUAL", "<=", rule)
  rule <- gsub("HIGHEQUAL", "=>", rule)
  rule <- gsub("LOW", "<", rule)
  rule <- gsub("HIGH", ">", rule)
  rule <- gsub("EQUAL", "= ", rule)
  rule <- gsub("AND", "&", rule)
  rule <- gsub("COMMA", ",", rule)
  rule <- gsub("DOT", ".", rule)
  rule <- gsub(", NA", "", rule)
  rule <- gsub("NA,", "", rule)
  
  result$rules <- rule
  
  return(result)
  
}

#' Get the top items out of a tree 
top_items <- function(x, top = 5, ...) {
  
  if (length(x) > 1) {
    
    coef_x <- coef(x, log = FALSE)
    
    bestitems <- apply(coef_x, 1 , function(y) {
      names(rev(sort(y)))[1:top]
    })
    
    bestitems <- as.data.frame(bestitems)
    
    names(bestitems) <- paste0("Node", 
                               nodeids(x, terminal = TRUE))
    
    return(bestitems)
    
  }
  
  if (length(x) == 1){
    
    coef_x <- coef(x, log = FALSE)
    
    bestitems <- names(rev(sort(coef_x)))[1:top]
    
    return(bestitems)
    
  }
  
}

#' Plot a map of winning probs
#' 
#' this highlights where each item is better for each trait
#' 
#' @param object a list of PlackettLuce models
#' @param traits a vector with name of traits
winprob_map <- function(object, traits, ...) {
  
  winprobs <- combine_coeffs(object, log = TRUE, vcov = FALSE)
  
  # add name of features
  names(winprobs) <- traits
  
  winprobs <- data.frame(items = rep(dimnames(winprobs)[[1]], times = ncol(winprobs)),
                         traits = rep(dimnames(winprobs)[[2]], each = nrow(winprobs)),
                         winprob = as.numeric(unlist(winprobs)))
  
  winprobs$traits <- factor(winprobs$traits, levels = traits)
  
  items <- winprobs$items
  winprob <- winprobs$winprob
  
  lims <- max(abs(winprob)) * c(-1, 1)
  
  angle <- 0
  if (any(nchar(traits) > 30)) {
    angle <- 40  
  }
  
  p <- ggplot(winprobs, 
              aes(x = items, 
                  y = traits,
                  fill = winprob,
                  label = as.character(round(winprob, 2)))) +
    geom_tile() + 
    scale_fill_distiller(palette = "RdBu", limit = lims,
                         direction = 1,
                         na.value = "white") +
    theme_bw() +
    theme(axis.text = element_text(color = "grey20"),
          strip.text.x = element_text(color = "grey20"),
          axis.text.x = element_text(angle = 40, vjust = 1, hjust=1),
          axis.text.y = element_text(angle = angle, vjust = 1, hjust=1)) +
    ggplot2::labs(x = "", 
                  y = "",
                  fill = "")
  
  return(p)
  
  
}

#'Combine coefficients from PlackettLuce models
#' 
#' This function helps in combining coefficients 
#' from different models when not all the items 
#' are available for all models
#' 
#' @param x a list with PlackettLuce models
#' @param na.replace logical, to replace or keep NAs
#' @param ... further arguments passed to methods
combine_coeffs <- function(x, na.replace = TRUE, ...) {
  
  coeffs <- lapply(x, function(y) {psychotools::itempar(y, ...)})
  
  items <- unique(unlist(lapply(coeffs, names)))
  
  r <- data.frame(items)
  
  rownames(r) <- items
  
  r <- r[,-1]
  
  for(i in seq_along(coeffs)) {
    c_i <- coeffs[[i]]
    r[names(c_i),i] <- c_i
  }
  
  if (isTRUE(na.replace)) {
    r[is.na(r)] <- 0
  }
  
  r
  
}
