% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute_ouliers_maha.R
\name{impute_gps_outliers_maha}
\alias{impute_gps_outliers_maha}
\title{Find and impute if necessary outliers in processed with process_white_gps_data function. This function can only
be used if data has assigned SI_FOPER values, so can only be used with _formatted or _formatted_final files.}
\usage{
impute_gps_outliers_maha(
  data,
  threshold = 0.85,
  method = "linear",
  outlier_value = "ST",
  outlier_method = "percentile",
  factor = 3,
  head_tail_size = 3,
  del_points = "N"
)
}
\arguments{
\item{data}{A data frame or sf object containing the GPS data with columns SI_LATI (latitude), SI_LONG (longitude), and SI_TIMESTAMP (timestamp).}

\item{threshold}{A numeric value between 0 and 1 for percentile-based outlier detection, defaults to 0.85.}

\item{method}{The method used for imputation (linear, spline, etc.), defaults to linear.}

\item{outlier_value}{The value to assign to outliers in the head or tail of operation classes, defaluts to ST.}

\item{outlier_method}{he method for determining outliers (percentile or distance), defaults to percentile.}

\item{factor}{A factor for distance-based outlier threshold calculation, defaults to 3.}

\item{head_tail_size}{The size of the window for determining head/tail of operation classes, defaults to 3.}

\item{del_points}{Whether to delete outliers (Y) or impute them (N).}
}
\value{
Data with imputed outlier values
}
\description{
Find and impute if necessary outliers in processed with process_white_gps_data function. This function can only
be used if data has assigned SI_FOPER values, so can only be used with _formatted or _formatted_final files.
}
