#' Plots neighborhood adjacency
#'
#' This function takes in a polygon object and its corresponding neighborhood object,
#' and plots the polygons and their neighbors on a map.
#'
#' @param poly_obj A SpatialPolygonsDataFrame representing the polygons.
#' @param nb_obj A nb object generated using the 'spdep' package representing the neighborhood relationship information.
#'
#' @details The function first extracts the coordinates from \code{poly_obj} and creates a data frame named \code{coords}
#' with columns "long" and "lat". Then, it generates a set of lines representing the neighboring relationships between polygons
#' using the \code{nb2lines} function. It then plots the polygons and their neighbors on a map using ggplot2.
#'
#' @return A ggplot object with the plotted polygons and their neighboring relationships.
#'
#' @importFrom ggplot2 geom_polygon geom_point geom_sf coord_equal theme_minimal
#' @import spdep::nb2lines
#' @importFrom sf st_as_sf st_coordinates
#'
#' @examples
#' library(sp)
#' library(spdep)
#' library(ggplot2)
#' library(sf)
#'
#' data(columbus)
#' nb_obj <- dnearneigh(coordinates(columbus), 0, 10000)
#' poly_obj <- columbus
#'
#' plot_neighbors(poly_obj, nb_obj)
#'
#' @export
#' @import sp
#' @importFrom ggplot2 ggtitle theme axis.title_x axis.title_y element_text hjust
plot_neighbors <- function(poly_obj,nb_obj) {

  coords = as.data.frame(coordinates(poly_obj))
  names(coords) = c("long","lat")
  neighbors_sl <- nb2lines(nb_obj, coords=coords, proj4string=proj4string(poly_obj))

  p <- ggplot() +
    geom_polygon(data=poly_obj,
                 aes(x = long, y = lat, group = group), fill = "gray90", color = 'blue') +
    geom_polygon(data=neighbors_sl,
                 aes(x = long, y = lat, group = group), color = 'red') +
    geom_point(data = coords,
               aes(x = long, y = lat), col="black") +
    theme_minimal() +
    ggtitle("Neighborhood Adjacency") +
    ylab("Northing") +
    xlab("Easting") +
    coord_equal() +
    theme(axis.title.x = element_text(size=22, face="bold"),
          axis.title.y =element_text(size=22, face="bold"),
          plot.title = element_text(size=24, face="bold", hjust=0.5))

  plot(p)
}
