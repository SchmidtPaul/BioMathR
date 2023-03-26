#' @title Helper function: Negation/Opposite of \code{\%in\%}
#' @description This helper function checks if elements
#' of one vector are not found in another vector.
#'
#' @param a vector or \code{NULL}: the values to be matched.
#' @param b vector or \code{NULL}: the values to be matched against.
#'
#' @return A logical vector, indicating if a match was
#' not located for each element of \code{a}. The values
#' are \code{TRUE} or \code{FALSE} and never \code{NA}.
#'
#' @export
#'
#' @import dplyr
#'
#' @examples
#' c(1,2,3) %not_in% c(3,4,5,6,7)
#'
#' library(dplyr)
#' PlantGrowth %>% filter(group %not_in% c("ctrl", "trt2"))

`%not_in%` <- function(a, b) {
  !a %in% b
}
