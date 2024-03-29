% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/partial_block_pour_docx.R
\name{partial_block_pour_docx}
\alias{partial_block_pour_docx}
\title{Pour part of a docx file in the resulting docx from an 'R Markdown' document}
\usage{
partial_block_pour_docx(file, heading1title, includeheader = TRUE)
}
\arguments{
\item{file}{external docx file path}

\item{heading1title}{name of the header (of MSWord style heading 1) that denotes the relevant part.}

\item{includeheader}{should the header of the relevant be part of the pour?}
}
\description{
This function is a wrapper for \code{officer::block_pour_docx()}. It extends its functionality so that not the entire docx is poured into the RMarkdown file, but only a part of it.
}
\examples{
# see https://github.com/davidgohel/officedown/discussions/97#discussioncomment-4839382
}
