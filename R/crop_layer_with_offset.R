#' Crop sf object based on bounding box of another sf object
#'
#' This function crops an sf object based on the bounding box calculated from another sf objetc
#' and a desired offset, that may be applied to the four sides of the bounding box or may be specified
#' for each side.
#' When specifying your offset take into account if sf objects are projected or not.
#' @param layer_to_crop Sf object that is going to be cropped.
#' @param bbox_layer Layer from which the bounding box is obtained.
#' @param offset Desired offset to apply to the bounding box.
#' @return A cropped sf object.
#' @examples
#' # Crop specifying different offsets for each side, order is xmin, xmax, ymin, ymax.
#' cropped_layer <- crop_layer_with_offset(sf_object_to_crop, sf_object_from_which_bbox_is_obtained, offset = c(0.01, 0.01, 0.2, 0.01))
#' # Crop specifying global offset.
#' cropped_layer <- crop_layer_with_offset(sf_object_to_crop, sf_object_from_which_bbox_is_obtained, offset = 10)
#' @export
crop_layer_with_offset <- function(layer_to_crop, bbox_layer, offset = c(0, 0, 0, 0)) {
  # Make sure inputs are sf objects

  if (!inherits(layer_to_crop, c("sf", "data.frame"))) {
    stop("layer_to_crop must be an 'sf' object.")
  }

  # Check if bbox_layer is an sf object
  if (!inherits(bbox_layer, c("sf", "data.frame"))) {
    stop("bbox_layer must be an 'sf' object.")
  }

  # Convert to sf objects if necessary
  #layer_to_crop <- st_as_sf(layer_to_crop, check = FALSE)
  #bbox_layer <- st_as_sf(bbox_layer, check = FALSE)

  # Comprbamos que los CRS son compatibles
  if (!st_crs(layer_to_crop) == st_crs(bbox_layer)) {
    stop("CRS mismatch between layers. Consider reprojection first.")
  }

  # Obtenemos los límites
  bbox <- st_bbox(bbox_layer)

  # Manejamos offsets individiales o global
  if (length(offset) == 4) {
    xmin <- bbox[["xmin"]] - offset[1]
    xmax <- bbox[["xmax"]] + offset[2]
    ymin <- bbox[["ymin"]] - offset[3]
    ymax <- bbox[["ymax"]] + offset[4]
  } else if (length(offset) == 1) {
    xmin <- bbox[["xmin"]] - offset
    xmax <- bbox[["xmax"]] + offset
    ymin <- bbox[["ymin"]] - offset
    ymax <- bbox[["ymax"]] + offset
  } else {
    stop("Invalid offset length. Provide either a single value for global offset or four values for individual offsets.")
  }

  # Recortamos la capa
  cropped_layer <- st_crop(layer_to_crop, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)

  return(cropped_layer)
}
