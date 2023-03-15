#' @title Format p-values
#'
#' @description This function is very much just a wrapper function for \code{insight::format_p()} with opinionated defaults.
#'
#' @param p value or vector of p-values.
#' @param name Name prefixing the text. Default: \code{NULL}.
#' @param stars Add significance stars (e.g., p < .001***). Default: \code{TRUE}.
#' @param whitespace Logical, if `FALSE` (default) all whitespace characters are removed from the returned string.
#' @param lang Language for the result, which here only determines the decimal separator.
#' @param ... other arguments passed to \code{insight::format_p()}.
#'
#' @importFrom insight format_p
#' @export
#'
#' @examples
#' BioMathR::format_p(c(0.04, 0.004, 0.0004, 0.00004, 0.000004))
format_p <- function(p,
                     name = NULL,
                     whitespace = FALSE,
                     stars = TRUE,
                     lang = c("eng", "ger")[1],
                     ...) {
  if (lang == "ger") {
    decimal_separator <- ","
  } else {
    decimal_separator <- "."
  }

  insight::format_p(
    p,
    name = name,
    whitespace = whitespace,
    stars = stars,
    decimal_separator = decimal_separator,
    ...
  )
}


