#' Reader wrapper to read csv or RData files
#'
#' This function allows to use both csv and RData files where needed. It is
#' intended to use inside another function so no need to do direct calls
#' @export
read_file <- function(file_path) {
  # Obtain file extension
  file_extension <- tools::file_ext(file_path)

  # Read file based on extension
  if (file_extension == "csv") {
    data <- read.csv(file_path, sep = ";", dec = ".", header = TRUE)
  } else if (file_extension == "RData") {
    # Load .RData in temporal environment.
    env <- new.env()
    load(file_path, envir = env)
    # Identify loaded object name.
    data <- get(ls(envir = env)[1], envir = env)
  } else {
    stop("Formato de archivo no soportado. Solo se aceptan archivos .csv o .RData.")
  }

  return(data)
}
