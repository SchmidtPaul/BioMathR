#' @title Automatically round numbers
#'
#' @description
#' This function automatically rounds numbers so that the results have as few digits as possible, but als many as necessary. It is similar to \code{signif()}, but it never alters the part before the decimal separator and also allows a cutoff value for a maximum acceptable number of digits after the decimal separator.
#'
#' @param x A number of vector of numbers
#' @param signif_digits The number of significant digits (default: 1) like in \code{signif()}, but only applied to the part after the decimal separator.
#' \itemize{
#' \item \code{round_smart(1.0001234, signif_digits = 1)} returns \code{1.0001}
#' \item \code{round_smart(1.0001234, signif_digits = 2)} returns \code{1.00012}
#' }
#' @param max_digits The maximum number of digits (default: 6) to round to.
#' \itemize{
#' \item \code{round_smart(1.0000001234, max_digits = 6)} returns \code{1}
#' \item \code{round_smart(1.0000001234, max_digits = Inf)} returns \code{1.0000001}
#' }
#' @export
#'
#' @examples
#' library(BioMathR)
#' library(dplyr)
#'
#' before <- data.frame(
#' V1 = c(123456, 1234),
#' V2 = c(-123, -0.12345),
#' V3 = c(1.0012345, 0.1),
#' V4 = c(1.1, 0.0012345),
#' V5 = c(1.000000012345, 0),
#' V6 = c(NA, -5.0018),
#' V7 = c(NA_real_, NA_real_)
#' )
#'
#' before
#' mutate(before, across(everything(), ~round_smart(.)))
#' mutate(before, across(everything(), ~round_smart(., signif_digits = 3)))
#' mutate(before, across(everything(), ~round_smart(., signif_digits = 3, max_digits = Inf)))
round_smart <- function(x,
                        signif_digits = 1,
                        max_digits = 6) {
  if (all(is.na(x))) {
    return(x)
  }

  # split number (e.g. -12.34)
  int_part  <- trunc(x)          # int_part  => -12
  frac_part <- abs(x - int_part) # frac_part => 0.34

  if (all(frac_part == 0 | is.na(frac_part))) {
    return(x)
  }

  # deal with potential floating point problems
  originally_provided_digits <- max(nchar(sub(".*\\.", "", as.character(x))), na.rm = TRUE)
  frac_part <- round(frac_part, digits = originally_provided_digits)

  # number of 0s after decimal separator: e.g. c(1.01, 1.001) => c(1, 2)
  zeros_after_decsep <-
    -floor(log10(signif(frac_part[frac_part != 0], 1))) - 1

  # ignore those that would need too many digits to be rounded to
  relevant_zeros_after_decsep <-
    zeros_after_decsep[zeros_after_decsep < max_digits]

  # get maximum
  if (length(relevant_zeros_after_decsep) == 0) {
    max_relevant_zeros_after_decsep <- 0
  } else {
    max_relevant_zeros_after_decsep <-
      max(relevant_zeros_after_decsep, na.rm = TRUE)
  }

  # combine minimum digits needed for max_relevant_zeros_after_decsep with set signif_digits
  digits <- (max_relevant_zeros_after_decsep + 1) + (signif_digits - 1)

  # in case adding signif_digits lead to digits > max_digits => reduce down to max_digits again
  digits <- if_else(digits > max_digits, 6, digits)

  # round only frac_part, leave int_part untouched
  rounded_x <- int_part + sign(x) * round(frac_part, digits = digits)

  return(rounded_x)
}
