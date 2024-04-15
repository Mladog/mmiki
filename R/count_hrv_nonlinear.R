# Funtion to count hrv non-linear parameters sd1 and sd2 for examination object.

# Usage:
# hrv_nonlinear_params <- count_hrv_nonlinear(exam)

# Arguments:
# # @param Examination: A S3 class object containig the information about the signal.
#                       It contains two atributes:
#                       *rr_signal - the vector of RR_intervals defined in miliseconds.
#                       *hrv_time_domain - a list of heart rate variability parameters
#                                          (meanRR, minRR, maxRR, sdnn, rmssd).
#
# @return Poincare: a S3 class object containing two hrv nonlinear parameters: SD1 and SD2
#
# Examples:
# hrv_Poincare <- count_hrv_nonlinear(Examination)
#


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
  sd1_numerator <- var(diff_rr_intervals, na.rm = TRUE) * 0.5
  hrv_Poincare$sd1 <- sqrt(sd1_numerator)

  # Calculate sd2
  sd2_numerator <- 2 * var(examination$rr_signal$signal, na.rm = TRUE)^2 - 0.5 * var(examination$rr_signal$signal, na.rm = TRUE)^2
  hrv_Poincare$sd2 <- sqrt(sd2_numerator)

  # create Poincare object
  poincare <- list(sd1 = sqrt(sd1_numerator), sd2 = sqrt(sd2_numerator))
  class(poincare) <- "Poincare"
  return(poincare)
}
