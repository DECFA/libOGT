#' Create fishing diary from two OGT devices
#' 
#' This function combines the two files coming from two OGT devices into a single
#' one where operations are marked. Understanding how OGTs work is essential to 
#' understand what this function is doing. 
#' From these two files start and end of setting and hauling events are 
#' identified.
#' Before using this function, data from OGTs has to be processed with  
#' process_white_gps_data() function from this library.
#' 
#' @param df1 Data frame with the gps info from the first OGT
#' @param df2 Data frame with the gps info from the second OGT
#' @param threshold Time difference in seconds between two measures that we 
#' consider before marking a point, time less that this amount is considered 
#' spurious or false and is not taken into account
#' @returns A composed data frame with the
#' 
#' @examples
#' create_diary(ogt1, ogt2, 600)
#' @export
create_diary <- function(df1, df2, threshold) {
  
  # Filter rows where SI_TDIFF is larger than the threshold
  # grouped by day
  
  df1_filtered <- df1 %>%
    group_by()
    dplyr::filter(SI_TDIFF > threshold)
  df2_filtered <- dplyr::filter(df2, SI_TDIFF > threshold)
  
  # Ensure SI_TIMESTAMP is in POSIXct format for correct date-time operations
  df1_filtered <- dplyr::mutate(df1_filtered, SI_TIMESTAMP = lubridate::ymd_hms(SI_TIMESTAMP))
  df2_filtered <- dplyr::mutate(df2_filtered, SI_TIMESTAMP = lubridate::ymd_hms(SI_TIMESTAMP))
  
  # Extract the relevant timestamps and create the diary entries
  fec_cal <- lubridate::as_date(df1_filtered$SI_TIMESTAMP - lubridate::days(1))
  ini_largada <- hms::as_hms(df1_filtered$SI_TIMESTAMP - lubridate::days(1))
  fin_largada <- hms::as_hms(df2_filtered$SI_TIMESTAMP - lubridate::days(1))
  fec_vir <- lubridate::as_date(df2_filtered$SI_TIMESTAMP - lubridate::days(1))
  ini_virada <- hms::as_hms(df2_filtered$SI_TIMESTAMP)
  fin_virada <- hms::as_hms(df1_filtered$SI_TIMESTAMP)
  
  # Create a tibble for the final diary
  diario <- tibble::tibble(
    Fecha = fec_cal,
    fec_cal = fec_cal,
    ini_largada = ini_largada,
    fin_largada = fin_largada,
    fec_vir = fec_vir,
    ini_virada = ini_virada,
    fin_virada = fin_virada
  )
  
  return(diario)
}

