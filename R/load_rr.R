#' Function to load rr signal either from local file or URL.
#' @param path A path to a local file or URL to a file from data source.
#' @param header A header of the file. Options:
#'                FALSE - no header is loaded.
#'                TRUE - first row of a file is assumed to be header
#'                vector c('name1', 'name2'...) - given names of headers.
#'                The default value is set to FALSE.
#' @return rr_signal: list of rr intervals loaded from a file.
#' @examples
#' rr_signal <- load_rr(path='https://github.com/Mladog/rri/blob/main/rr01', header=c('RR'))
#' @export load_rr


load_rr <- function(path, header=FALSE) {

  # Read data from the URL
  rr_signal <- readr::read_table(path, col_names=header)

  # Return the loaded data
  return(rr_signal)
}
