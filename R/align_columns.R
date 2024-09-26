#' Align columns of multiple shapefiles
#' 
#' This function ensures that all shapefiles have the same number of columns
#' filling with NA's where needed.
#' all_columns represents unique columns present in all shapefiles and
#' missing_columns find missing columns in a shapefile and fills with NA
#' @param shapefiles list of shapefiles
#' @return list of aligned shapefiles
#' @export

align_columns <- function(shapefiles) {
  all_columns <- unique(unlist(map(shapefiles, colnames)))
  aligned_shapefiles <- map(shapefiles, function(shp) {
    missing_columns <- setdiff(all_columns, colnames(shp))
    shp[missing_columns] <- NA
    shp <- shp[, all_columns]
    return(shp)
  })
  return(aligned_shapefiles)
}