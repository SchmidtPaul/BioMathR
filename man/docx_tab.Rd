% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/docx_tab.R
\name{docx_tab}
\alias{docx_tab}
\title{Auto-format tables for printing in MS-Word documents}
\usage{
docx_tab(
  x,
  lang = c("eng", "ger")[1],
  pvalform = "p.value",
  asft = TRUE,
  digits = "round_smart",
  ...
)
}
\arguments{
\item{x}{Table to be formatted}

\item{lang}{Language for column names.}

\item{pvalform}{Names of columns are that formatted via \code{BioMathR::format_p()}. Can be set to \code{NULL}. The default is \code{"p.value"}, but note that this function first unifies multiple column names such as \code{"Pr(>F)"} or \code{"P(>|Chi|)"} into \code{"p.value"}.}

\item{asft}{If \code{TRUE}, output is formatted as flextable}

\item{digits}{Number of digits all numeric columns are rounded to. The default is actually \code{"round_smart"} which applies \code{BioMathR::round_smart()} to each numeric column individually.}

\item{...}{Other arguments passed to \code{BioMathR::round_smart()}}
}
\description{
Auto-format tables for printing in MS-Word documents
}
\examples{
library(BioMathR)

anova <- anova(lm(weight ~ group, data = PlantGrowth))
docx_tab(anova, lang = "ger")
docx_tab(anova, lang = "eng", asft = FALSE)

before <- data.frame(
  V1 = c(123456, 1234),
  V2 = c(-123, -0.12345),
  V3 = c(1.0012345, 0.1),
  V4 = c(1.1, 0.0012345),
  V5 = c(1.000000012345, 0),
  V6 = c(NA, -5.0018),
  V7 = c(NA_real_, NA_real_)
)
docx_tab(before)
docx_tab(before, digits = 2)
}
