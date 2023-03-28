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
#' V2 = c(123, 0.12345),
#' V3 = c(1.0012345, 0.1),
#' V4 = c(1.1, 0.0012345),
#' V5 = c(1.000000012345, 0)
#' )
#'
#' mutate(before, across(everything(), ~round_smart(.)))
#' mutate(before, across(everything(), ~round_smart(., signif_digits = 3)))
#' mutate(before, across(everything(), ~round_smart(., signif_digits = 3, max_digits = Inf)))
round_smart <- function(x,
                        signif_digits = 1,
                        max_digits = 6) {

  # split number (e.g. -12.34)
  int_part  <- trunc(x)          # int_part  => -12
  frac_part <- abs(x - int_part) # frac_part => 0.34

  # deal with potential floating point problems
  originally_provided_digits <- max(nchar(sub(".*\\.", "", as.character(x))))
  frac_part <- round(frac_part, digits = originally_provided_digits)

  if (all(frac_part == 0)) {
    rounded_x <- x
  } else {
    # max number of 0s after decimal separator: e.g. c(1.01, 1.001) => 2
    max_0s_after_decsep <-
      max(-floor(log10(signif(frac_part[frac_part != 0], 1)))) - 1

    # combine minimum digits needed for max_0s_after_decsep with set signif_digits
    digits <- (max_0s_after_decsep + 1) + (signif_digits - 1)

    # use max_digits if < digits
    digits <- min(digits, max_digits)

    # round only frac_part, leave int_part untouched
    rounded_x <-
      int_part + sign(x) * round(frac_part, digits = digits)
  }

  return(rounded_x)
}
