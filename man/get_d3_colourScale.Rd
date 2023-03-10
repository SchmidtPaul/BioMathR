% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_d3_colourScale.R
\name{get_d3_colourScale}
\alias{get_d3_colourScale}
\title{Make named color vector compatible with D3 Sankey Network Graphs}
\usage{
get_d3_colourScale(namedvector)
}
\arguments{
\item{namedvector}{a named character vector where the names are the levels and the values are the respective colors}
}
\value{
a string
}
\description{
This function does a very specific job only needed when working with \code{sankeyD3::sankeyNetwork()}. It converts a named vector with colors as the values to a format that can be passed to the \code{colourScale =} argument in aforementioned function.
}
\examples{
BioMathR::get_d3_colourScale(c("A" = "blue", "B" = "red"))

# also see here: https://github.com/fbreitwieser/sankeyD3/issues/20#issuecomment-999624150
}
