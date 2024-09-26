#' Load all .shp files in a directory
#'
#' This function loads all the .shp files inside a directory using a safe
#' function that will continue loading files if errors or warnings are thrown.
#' @param directory directory where shapefiles are stored.
#' @param combine sets if shp files has to be combined into a single shapefile. Defaults to FALSE
#' @param target_crs converts shapefiles to a common crs. If no crs is given, all will be 
#' transformed to the crs of the fisrt shapefile processed. Otherwise a crs can be given.
#' 
#' @return A list containing all the shapefiles or a single combined shapefile.
#' @examples
#' 
#' Define directory where.shp files are located
#' shapefile_directory <- "/home/user/shapefiles_dir/"
#' Load and combine shapefiles.
#' combined_shapefile <- load_shapefiles(shapefile_directory, combine = TRUE, target_crs ="epsg:25830")
#' Load shapefiles without combining.
#' shapefiles_list <- load_shapefiles(shapefile_directory, combine = FALSE)
#' @export

# Function to load multiple shapefiles
load_shapefiles <- function(directory, combine = FALSE, target_crs = NULL) {
  # Obtener la lista de archivos .shp en el directorio
  shapefile_list <- list.files(directory, pattern = "\\.shp$", full.names = TRUE)
  
  # Load all .shp safely
  shapefiles_results <- map(shapefile_list, saferead_shapefile)
  
  # Filter by corretly loaded shapelies
  shapefiles_ok <- map(shapefiles_results, "data") %>% compact()
  
  # Assign default CRS (WGS84) to shapefiles without CRS
  shapefiles_ok <- map(shapefiles_ok, function(shp) {
    if (is.na(st_crs(shp))) {
      message("Shapefile sin CRS detectado. Asignando WGS84.")
      st_crs(shp) <- st_crs(4326)  # Asignar WGS84
    }
    return(shp)
  })
  
  # Verify and transform to a common CRS if needed
  crs_list <- map(shapefiles_ok, st_crs)
  unique_crs <- unique(crs_list)
  
  if (is.null(target_crs)) {
    if (length(unique_crs) > 1) {
      message("Se encontraron múltiples CRS. Transformando al primer CRS encontrado.")
      target_crs <- unique_crs[[1]]  # Usar el primer CRS como común si no se especifica otro
    } else {
      target_crs <- unique_crs[[1]]  # Usar el único CRS si todos son iguales
    }
  } else {
    message("Transformando todos los shapefiles al CRS especificado.")
  }
  
  # Transform all shapefiles to target CRS
  shapefiles_ok <- map(shapefiles_ok, ~ st_transform(.x, target_crs))
  
  if (combine) {
    if (length(shapefiles_ok) == 0) {
      message("No se pudieron cargar shapefiles correctamente.")
      return(NULL)
    }

    # Align columns of all shapefiles
    shapefiles_ok <- align_columns(shapefiles_ok)
    
    #Combine all shapefiles in a single sf object
    combined_shapefile <- do.call(rbind, shapefiles_ok)
    return(combined_shapefile)
  } else {
    # Return list of succesfully loaded shapefiles
    return(shapefiles_ok)
  }
}
