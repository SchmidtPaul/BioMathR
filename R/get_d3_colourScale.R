#' @title Make named color vector compatible with D3 Sankey Network Graphs
#'
#' @description This function does a very specific job only needed when working with \code{sankeyD3::sankeyNetwork()}. It converts a named vector with colors as the values to a format that can be passed to the \code{colourScale =} argument in aforementioned function.
#'
#' @param namedvector a named character vector where the names are the levels and the values are the respective colors
#'
#' @return a string
#'
#' @export
#'
#' @examples
#' BioMathR::get_d3_colourScale(c("A" = "blue", "B" = "red"))
#'
#' # also see here: https://github.com/fbreitwieser/sankeyD3/issues/20#issuecomment-999624150

get_d3_colourScale <- function(namedvector){
  paste0(
    "d3.scaleOrdinal() .domain([",
    paste0('"', names(namedvector), '"', collapse = ", "),
    "]) .range([",
    paste0('"', namedvector, '"', collapse = ", "),
    "])"
  )
}
