#' Safely read a shapefile
#'
#' This function loads a shapefile and manages warnings and errors.
#' @param file Path to the shapefile.
#' @return A list with succesfully loades shapefile.
#' @export
# Function to load a shapafile managing errors
saferead_shapefile <- function(file) {
  tryCatch({
    # Try to read shapefile
    sf_object <- st_read(file, quiet = TRUE)
    list(success = TRUE, data = sf_object)
  }, warning = function(w) {
    # Catch warnings and continue
    message("Advertencia al leer ", file, ": ", conditionMessage(w))
    list(success = FALSE, data = NULL, warning = conditionMessage(w))
  }, error = function(e) {
    # Catch errors and continue
    message("Error al leer ", file, ": ", conditionMessage(e))
    list(success = FALSE, data = NULL, error = conditionMessage(e))
  })
}
