#' Funtion to count hrv parameters in time domain for a given vector of
#' RR interval values.
#' Arguments:
#' @param rr_signal a dataframe containing a numerical vector of RR intervals
#'                   (time interval between two corresponding R beats in ECG signal).
#' @param unit a character value informing about the unit used in the rr_signal
#'              vector. Can take two values: 'miliseconds' or 'seconds'.
#'              The default value is set to 'miliseconds'.
#' @return Examination: A S3 class object containig the information about the signal.
#'                       It contains two atributes:
#'                       *rr_signal - the vector of RR_intervals defined in miliseconds.
#'                       *hrv_time_domain - a list of heart rate variability parameters
#'                                          (meanRR, minRR, maxRR, sdnn, rmssd).
#' @examples
#' data(rr01)
#' exam <- count_hrv_time(rr01, 'seconds')
#' @export count_hrv_time


count_hrv_time <- function(rr_signal, unit='miliseconds'){
  # input check
  stopifnot(is.numeric(rr_signal$RR))
  stopifnot(is.character(unit))

  if(length(rr_signal$RR)<2){
    stop("Error: rr_signal must be a vector.");
  }

  # check and handle unit conversion
  if (unit == 'seconds'){
    message("Converting signal to miliseconds")
    rr_signal = rr_signal * 1000
  } else if (unit != "miliseconds"){
    stop("Provide correct input. 'unit' must be 'miliseconds' or 'seconds'.")
  }

  # compute HRV parameters
  hrv_time_domain <- list()
  diffSeg <- diff(rr_signal)

  hrv_time_domain$mean <- mean(rr_signal$RR)
  hrv_time_domain$minRR <- min(rr_signal$RR)
  hrv_time_domain$maxRR <- max(rr_signal$RR)
  hrv_time_domain$sdnn <- stats::sd(rr_signal$RR)

  # create Examination object
  examination <- list(rr_signal = data.table::data.table(signal = c(rr_signal$RR)), hrv_time_domain = hrv_time_domain)
  class(examination) <- "Examination"
  return(examination)

}
