#' Mark vessel point inside or outside a harbour
#'
#' This function creates the column SI_HARB that indicates if the point is inside (TRUE) or outside (FALSE) a harbour.
#'
#' @param vessel_track_sf An sf object with vessel tracks.
#' @param harbour_dataframe A dataframe with the location (longitude, latitude) of the harbour and a range column (in km) inside which we consider the vessel is inside the harbour.
#' @param lon_col Name of the longitude column in the harbour_dataframe Default is "LON".
#' @param lat_col Name of the latitude column in the harbour_dataframe Default is "LAT".
#' @param range_col Name of the range column in the harbour_dataframe Default is "RANGO".
#' @return An sf object with the SI_HARB (TRUE or FALSE values) column added.
#' @export
mark_harbour <- function(vessel_track_sf, harbour_dataframe, lon_col = "LON", lat_col = "LAT", range_col = "RANGO") {

  # Convert harbour dataframe to an sf object with circular buffers
  puertos_sf <- sf::st_as_sf(harbour_dataframe, coords = c(lon_col, lat_col), crs = 4326) %>%
    sf::st_buffer(dist = harbour_dataframe[[range_col]] * 1000)

  # Identify points inside or outside the buffer
  vessel_track_sf$SI_HARB <- lengths(sf::st_intersects(vessel_track_sf, puertos_sf)) != 0

  # Arrange final data
  vessel_track_sf <- vessel_track_sf %>%
    dplyr::arrange(SI_TIMESTAMP)

  return(vessel_track_sf)
}
