#' Downloads and reads US state boundaries shapefiles into an `sf` object
#'
#' This function downloads the US state boundaries shapefiles from a specific URL and saves them to the specified
#' output directory. The downloaded zip file is then unzipped and the resulting shapefiles are read into an `sf`
#' object using the 'st_read' function from the 'sf' package.
#'
#' @param output_dir The output directory where the shapefiles will be saved and read from.
#' @return An `sf` object containing the US state boundaries shapefiles.
#' @importFrom sf st_read
#' @examples
#' States <- download_shapefile("./polygons")
download_shapefile <- function(output_dir) {
  # Create output directory if it does not exist
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Specify URL
  url <- "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/us-state-boundaries/exports/shp?lang=en&timezone=America%2FDenver"

  # Download zip file
  temp_zip <- tempfile(fileext = ".zip")
  download.file(url, destfile = temp_zip, mode = "wb")

  # Unzip shapefiles to output directory
  unzip(temp_zip, exdir = output_dir)

  # Remove temporary zip file
  file.remove(temp_zip)

  # Read shapefiles into sf object
  States <- st_read(dsn = output_dir, layer = "us-state-boundaries")

  # Return sf object
  return(States)
}
