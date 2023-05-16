#' Get Neighbors of Polygons
#'
#' This function takes a set of polygons and calculates neighbors based on
#' either polygon adjacency or polygon distance from centroid.
#'
#' @param polys A SpatialPolygons object containing the polygons for which to
#' calculate neighbourhoods.
#' @param distance The method by which to calculate distances between polygons.
#' Set to "centroid" (default) to use the distance between centroids of polygons,
#' or "polygon" to measure distances between the polygons themselves.
#' @param connect Logical. Whether to create a neighborhood list based on
#' polygon adjacency (TRUE) or polygon connectivity (FALSE).
#'
#' @return A 'neighbours' object created using the 'poly2nb' function from the
#' spdep package. The neighbours object is a list of integer vectors, with one
#' vector per spatial unit (polygon), describing indices into the \code{polys}
#' object to define neighbouring spatial units.
#'
#' @examples
#' library(spdep)
#' data(nc.sids)
#' neighbors <- get_neighbors(polygons = nc.sids, distance="polygon", connect=FALSE)
#'
#' @importFrom igraph graph.adjacency mst as_adj
#' @importFrom sf st_as_sf st_centroid st_geometry st_distance st_coordinates
#' @importFrom stats dist diag
#' @importFrom spdep poly2nb mat2listw union.nb
#'
#' @export
get_neighbors <- function(polys, distance="centroid", connect=TRUE){

  # Convert the "polys" object to an sf object.
  polys <- st_as_sf(polys)

  # Check if "connect" is false. If it is, create a neighborhood list
  # based on polygon adjacency using "poly2nb" with queen contiguity criterion.
  if(connect == FALSE){
    nb = poly2nb(polys, queen = T)
  }else{
    # If "connect" is true, create a neighborhood list based on polygon
    # connectivity using "poly2nb" with queen contiguity criterion.
    nb = poly2nb(polys, queen = T)

    # Calculate distances between centroids of polygons using the 'dist'
    # function. Create a fully connected adjacency graph using the resulting
    # distance matrix "dmat" and return its minimum spanning tree (MST).
    if(distance == "centroid"){
      coords = sf::st_coordinates(sf::st_centroid(sf::st_geometry(polys)))
      dmat = as.matrix(dist(coords))
    }else if(distance == "polygon"){
      # Calculate distances between the polygons themselves using the
      # 'st_distance' function from the 'sf' package.
      dmat = sf::st_distance(polys) + 1 # offset for adjacency

      # Set diagonal values to zero to ensure that each polygon does not appear
      # in its own neighborhood list (i.e. no self-intersections).
      diag(dmat) = 0
    }else{
      # Return an error message if the "distance" method is unknown.
      stop("Unknown distance method")
    }

    # Create a weighted, undirected adjacency graph from the MST,
    # and convert to a boolean adjacency matrix.
    gfull = igraph::graph.adjacency(dmat, weighted=TRUE, mode="undirected")
    gmst = igraph::mst(gfull)
    edgemat = as.matrix(igraph::as_adj(gmst))

    # Convert the adjacency matrix to a list of weights with spatial polygons
    # as rownames.
    edgelistw = spdep::mat2listw(edgemat, style="M")
    edgenb = edgelistw$neighbour

    # Set the neighbourhood list's "region.id" attribute to that of the input "nb".
    attr(edgenb,"region.id") = attr(nb, "region.id")

    # Combine the two neighborhood lists into one using "union.nb"
    allnb = spdep::union.nb(nb, edgenb)

    # Return the final neighborhood list object.
    allnb
  }
}
