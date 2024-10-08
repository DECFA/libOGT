% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_white_gps_data.R
\name{process_white_gps_data}
\alias{process_white_gps_data}
\title{Process white gps data (i-gotU)}
\usage{
process_white_gps_data(
  dir,
  input_file = NULL,
  vessel_code,
  le_met4_value = NA,
  le_met6_value = NA,
  le_met7_value = NA,
  process_all = FALSE
)
}
\arguments{
\item{dir}{Directory where the .csv files are.}

\item{input_file}{Input file, only needed if process_all is set to FALSE.}

\item{vessel_code}{CFR code of the vessel.}

\item{le_met4_value}{Metier DCF level 4 if known (optional).}

\item{le_met6_value}{Metier DCF level 6 if known (optional).}

\item{le_met7_value}{Target species, AL3 code if known (optional).}

\item{process_all}{Wheter to process all csv files or not, defaults to FALSE}
}
\value{
A .csv and a RData file with the same base name as the
input and _formatted, _formatted_final appended to the file name.
}
\description{
This function loads .csv files generated by the i-gotU gps and generates .rds
files that have an associated geometry column so they can be used as sf
objects. The CRS associated with the generated files is epsg:4326.
Function will also calculate speed and course over ground (COG) and will
remove unnecessary columns.
}
\details{
One advantage is that function can process all .csv files under a directory
or can do the process for single files.
Also a gear for the vessel may be specified if it is known.
}
\examples{
# Process only one file
process_white_gps_data("/dir/path/to/files/", "filename.csv", "cfr_code", "3_letter_met4_code", "metier_6_code", "Target_species_AL3")
# Process all files in directory
process_white_gps_data("/dir/path/to/files/", "cfr_code", "3_letter_met4_code", proces_all = TRUE)
}
