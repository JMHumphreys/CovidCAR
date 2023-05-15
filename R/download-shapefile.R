#' Downloads and reads US jurisdiction boundary polygons
#'
#' @param output_dir character string specifying the path/to/output/directory where the extracted shapefiles will be stored
#' @param unit character string specifying the level of geographic unit for which shapefile is needed. Defaults to "state".
#' @param proj character string specifying the desired coordinate reference system (CRS). Defaults to "EPSG:5070".
#'
#' @return A SpatialPolygonsDataFrame object.
#'
#' @examples
#' download_boundaries(output_dir = "C:/MyFiles", unit = "county", proj = "EPSG:4326")
#'
#' @import ggplot2
#' @import rgdal
#' @importFrom rgeos gBuffer
#' @importFrom sf st_read
download_boundaries <- function(output_dir, unit = "state", proj = "EPSG:5070") {

  # Create output directory if it does not exist
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Specify URL
  if (unit == "county"){
    url <- "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/us-county-boundaries/exports/shp?lang=en&timezone=America%2FDenver"
    layer_name <- "georef-united-states-of-america-county-millesime"
  } else if (unit == "state") {
    url <- "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/us-state-boundaries/exports/shp"
    layer_name <- "us-state-boundaries"
  } else {
    stop("Please specify unit as 'state' or 'county'")
  }

  # Download and extract shapefiles to output directory
  temp_zip <- tempfile(fileext = ".zip")

  tryCatch({
    download.file(url, destfile = temp_zip, mode = "wb", timeout = 100)
  }, error = function(e) {
    message(sprintf("Unable to download file from URL", url))
    stop(e)
  })


  unzip(temp_zip, exdir = output_dir)
  file.remove(temp_zip)

  # Read shapefiles into sf object
  polygons <- st_read(dsn = output_dir, layer = layer_name)

  # Project and fix topology
  polygons <- st_transform(polygons, crs = proj)
  polygons <- st_buffer(polygons, dist = 0)

  # Return sf object as SpatialPolygonsDataFrame
  return(as(polygons, "Spatial"))
}
