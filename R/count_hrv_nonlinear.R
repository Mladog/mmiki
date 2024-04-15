#' Funtion to count hrv non-linear parameters sd1 and sd2 for examination object.
#' @param examination Class object containig the information about the signal.
#'                       It contains two atributes:
#'                       *rr_signal - the vector of RR_intervals defined in miliseconds.
#'                       *hrv_time_domain - a list of heart rate variability parameters
#'                                          (meanRR, minRR, maxRR, sdnn, rmssd).
#' @return Poincare: a S3 class object containing two hrv nonlinear parameters: SD1 and SD2
#' @examples
#' examinationObj <- list(
#'   rr_signal = data.frame(signal = c(1000, 900, 1100, 950, 1050)),
#'   hrv_time_domain = list(meanRR = 1000, minRR = 900, maxRR = 1100, sdnn = 75.54)
#' )
#' class(examinationObj) <- "Examination"
#' hrv_Poincare <- count_hrv_nonlinear(examination = examinationObj)
#' @export count_hrv_nonlinear
#'


count_hrv_nonlinear <- function(examination) {
  stopifnot(class(examination) == "Examination")

  # Accessing the rr_signal dataframe properly
  RR <- examination$rr_signal$signal
  hrv_Poincare <- list()

  # Calculate differences between RR intervals
  diff_rr_intervals <- diff(examination$rr_signal$signal)

  if (length(diff_rr_intervals) == 0) {
    warning("No RR interval differences found.")
    return(NULL)
  }

  # Calculate sd1
  sd1_numerator <- stats::var(diff_rr_intervals, na.rm = TRUE) * 0.5
  hrv_Poincare$sd1 <- sqrt(sd1_numerator)

  # Calculate sd2
  sd2_numerator <- 2 * stats::var(examination$rr_signal$signal, na.rm = TRUE)^2 - 0.5 * stats::var(examination$rr_signal$signal, na.rm = TRUE)^2
  hrv_Poincare$sd2 <- sqrt(sd2_numerator)

  # create Poincare object
  poincare <- list(sd1 = sqrt(sd1_numerator), sd2 = sqrt(sd2_numerator))
  class(poincare) <- "Poincare"
  return(poincare)
}
