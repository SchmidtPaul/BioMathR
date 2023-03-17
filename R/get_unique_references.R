#' @title Get unique reference notations
#'
#' @description When multiple references (formatted as e.g. `Dawkins (2008)`) have an identical author (or authors) and publication year, it is common practice to include a lowercase letter after the year. This function does this for a vector containing multiple references.
#'
#' @param x a character vector with multiple strings/references
#'
#' @return a character vector
#' @export
#'
#' @importFrom stats ave
#' @importFrom stringr str_c
#' @importFrom stringr str_replace
#'
#' @examples
#' refs <- c("Dawkins (2008)", "Dawkins (2008)", "Stephenson (2008)")
#'
#' BioMathR::get_unique_references(refs)

get_unique_references <-  function(x) {
  v1 <- ave(x, x, FUN = function(x) if(length(x) > 1) letters[seq_along(x)] else "")
  stringr::str_replace(x, "\\)", stringr::str_c(v1, ")"))
}

# source: https://stackoverflow.com/questions/71254769/makes-duplicate-elements-of-a-character-vector-unique-but-not-like-make-unique/71254837#71254837
