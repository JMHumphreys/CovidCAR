#' Download US Boundaries
#'
#' Downloads and extracts shapefiles for either US states or counties. It creates a directory in the specified location if one does not exist already. The function downloads the shapefile from the specified URL, unzips it, reads it into an object of class 'sf', changes its projection to that of "EPSG:5070", fixes topology, matches state name with observation data and assigns unique identifiers.
#'
#' @param unit A character string indicating whether to download state or county borders, default is "state".
#' @param proj Projection for the shapefiles, defaults to "EPSG:5070".
#' @param sdir Directory where file will be saved.  If NULL (default) function searches for directory assigned by setup_analysis().
#' @return An object of class 'SpatialPolygonsDataFrame' which contains the state or county border polygons and their associated attributes.
#' @examples
#'
#' #Download state boundaries to a directory called 'States':
#' download_boundaries("./States")
#'
#' #Download county boundaries to a directory called 'Counties':
#' download_boundaries("./Counties", unit = "county")
#'
#'
#' @importFrom plyr mapvalues
#' @importFrom tools toTitleCase
#' @importFrom httr GET
#' @importFrom utils dir.create
#' @importFrom sf st_read st_transform st_make_valid
#' @importFrom stats as nrow
#' @export
download_boundaries <- function(unit = "state", proj = "EPSG:5070", sdir = NULL) {

  if(is.null(sdir)){
  # Create output directory if it does not exist
  dir.create(paste0(su_yaml$out_dir_name,"/polygons"), recursive = TRUE, showWarnings = FALSE)
  }

  if(unit == "county"){
    cli_alert("Warning: Intermittent timeout failures occur with county-level downloads")
  }

  # Specify URL
  url_county <- "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/us-county-boundaries/exports/shp?lang=en&timezone=America%2FDenver"
  url_state <- "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/us-state-boundaries/exports/shp"

  layer_name_county <- "georef-united-states-of-america-county-millesime"
  layer_name_state <- "us-state-boundaries"

  url <- ifelse(unit == "county", url_county, url_state)
  layer_name <- ifelse(unit == "county", layer_name_county, layer_name_state)

  # Download and extract shapefiles to output directory
  cli_alert("Downloading polygon files...")
  temp_zip <- tempfile(fileext = ".zip")

  tryCatch({
    httr::GET(url, httr::write_disk(temp_zip))
  }, error = function(e) {
    message(sprintf("Unable to download file from URL %s", url))
    stop(e)
  })

  if(is.null(sdir)){
  unzip(temp_zip, exdir = paste0(su_yaml$out_dir_name,"/polygons"))
  file.remove(temp_zip)

  } else{
  unzip(temp_zip, exdir = file.path(sdir))
  file.remove(temp_zip)
  }

  # Read shapefiles into sf object
  if(is.null(sdir)){
  polygons <- st_read(dsn = paste0(su_yaml$out_dir_name,"/polygons"), layer = layer_name)
  } else{polygons <- st_read(dsn = file.path(sdir), layer = layer_name)}

  # Project and fix topology
  polygons <- st_transform(polygons, crs = proj)
  polygons <- st_make_valid(polygons)

  if(unit == "state"){
    # Lookup table for state name adjustments
    state_lookup <- c("Commonwealth of the Northern Mariana Islands" = "Northern Mariana Islands",
                      "District Of Columbia" = "District of Columbia",
                      "United States Virgin Islands" = "Virgin Islands")

    # Adjust state names to match observation data
    polygons$name <- dplyr::recode(polygons$name, !!!state_lookup)

    # Assign unique identifiers
    polygons$Region <- 1:nrow(polygons)
    polygons$State <- tools::toTitleCase(polygons$name)
  }

  # Return indexed object as SpatialPolygonsDataFrame object
  return(as(polygons, "Spatial"))
}
