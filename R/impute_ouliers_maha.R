#' Find and impute if necessary outliers in processed with process_white_gps_data function. This function can only
#' be used if data has assigned SI_FOPER values, so can only be used with _formatted or _formatted_final files.
#'
#' @param data A data frame or sf object containing the GPS data with columns SI_LATI (latitude), SI_LONG (longitude), and SI_TIMESTAMP (timestamp).
#' @param threshold A numeric value between 0 and 1 for percentile-based outlier detection, defaults to 0.85.
#' @param method The method used for imputation (linear, spline, etc.), defaults to linear.
#' @param outlier_value The value to assign to outliers in the head or tail of operation classes, defaluts to ST.
#' @param outlier_method he method for determining outliers (percentile or distance), defaults to percentile.
#' @param factor A factor for distance-based outlier threshold calculation, defaults to 3.
#' @param head_tail_size The size of the window for determining head/tail of operation classes, defaults to 3.
#' @param del_points Whether to delete outliers (Y) or impute them (N).
#'
#' @return Data with imputed outlier values
#' @export
impute_gps_outliers_maha <- function(data, threshold = 0.85, method = "linear",
                                     outlier_value = "ST", outlier_method = "percentile",
                                     factor = 3, head_tail_size = 3, del_points = "N") {
  # Check for NA's in SI_FOPER
  if (sum(is.na(data$SI_FOPER)) != 0) {
    stop("column SI_FOPER has NA values")
  }

  # Check if data is an sf object and drop geometry if so
  is_sf <- inherits(data, "sf")
  if (is_sf) {
    geometry <- sf::st_geometry(data)
    data <- sf::st_drop_geometry(data)
  }

  # Ensure SI_TIMESTAMP is numeric
  data <- data %>%
    dplyr::mutate(SI_TIMESTAMP = as.numeric(SI_TIMESTAMP))

  # Verify the conversion
  if (!is.numeric(data$SI_TIMESTAMP)) {
    stop("SI_TIMESTAMP column is not numeric")
  }

  # Select the relevant columns for Mahalanobis distance calculation
  numeric_data <- data %>%
    dplyr::select(SI_LATI, SI_LONG, SI_TIMESTAMP)

  # Ensure all selected columns are numeric
  if (!all(sapply(numeric_data, is.numeric))) {
    stop("Not all selected columns are numeric")
  }

  # Calculate covariance matrix
  cov_mat <- stats::cov(numeric_data, use = "pairwise.complete.obs")

  # Calculate Mahalanobis distance for each data point
  md <- stats::mahalanobis(numeric_data, center = colMeans(numeric_data), cov = cov_mat)

  # Calculate the threshold for outlier detection
  if (outlier_method == "percentile") {
    threshold_value <- stats::quantile(md, threshold)
  } else if (outlier_method == "distance") {
    avg_dist <- mean(stats::dist(data[, c("SI_LATI", "SI_LONG")], method = "euclidean"))
    threshold_value <- factor * avg_dist
  } else {
    stop("Invalid outlier_method argument. Choose 'percentile' or 'distance'")
  }

  # Identify the outliers
  outliers <- md > threshold_value

  # Check if outlier is in the head, tail or middle of its operation class
  idx <- which(outliers)
  classify <- data$SI_FOPER[idx]

  # Calculate lead/lag of head_tail_size points
  head_tail <- classify != dplyr::lag(classify, n = head_tail_size, default = classify[1]) |
    classify != dplyr::lead(classify, n = head_tail_size, default = classify[length(classify)])

  # Reclassify outliers in the head/tail, impute outliers in the middle
  data$SI_FOPER[idx[head_tail]] <- outlier_value
  data[idx[!head_tail], c("SI_LATI", "SI_LONG")] <- NA

  if (del_points == "N") {
    # Impute NA values
    data_out <- imputeTS::na_interpolation(data, option = method)

    if (is_sf) {
      # Restore geometry
      data_out <- sf::st_as_sf(data_out, coords = c("SI_LONG", "SI_LATI"), crs = sf::st_crs(geometry))
    }

    return(data_out)
  } else if (del_points == "Y") {
    if (is_sf) {
      data <- sf::st_as_sf(data, coords = c("SI_LONG", "SI_LATI"), crs = sf::st_crs(geometry))
    }
    return(data)
  }
}
