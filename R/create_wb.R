#' @title Create a Workbook object
#'
#' @description This function is a wrapper for \code{openxlsx::createWorkbook()}, but also adds an info sheet including e.g. the time it was created.
#'
#' @param fontSize font size
#' @param fontName Name of a font
#' @param infosheet Should an info sheet be created?
#' @param infosheetlabel Label for the info sheet
#' @param ... Other arguments passed to \code{openxlsx::createWorkbook()}
#'
#' @return Workbook object (see {openxlsx})
#'
#' @export
#'
#' @import openxlsx
#' @importFrom here here
#' @importFrom rstudioapi getSourceEditorContext isAvailable
#' @importFrom utils tail
#'
#' @examples
#' wb1 <- create_wb()
#'
#' wb2 <- create_wb(infosheet = FALSE)

create_wb <- function(fontSize = 10,
                      fontName = "Arial",
                      infosheet = TRUE,
                      infosheetlabel = "BioMath GmbH",
                      ...) {
  wb <- openxlsx::createWorkbook(...)
  openxlsx::modifyBaseFont(wb, fontSize = fontSize, fontName = fontName)

  if (infosheet) {
    info <-
      data.frame(
        col1 = c(infosheetlabel, "Created on:", "Created via:"),
        col2 = c(
          " ",
          paste(Sys.time()),
          # this extracts the name of the R script where create_wb() is run
          gsub(
            x = if (rstudioapi::isAvailable()) {rstudioapi::getSourceEditorContext()$path} else {""},
            pattern = here::here(),
            replacement = ""
          )
        )
      )

    openxlsx::addWorksheet(wb = wb,
                           sheetName = "info",
                           gridLines = FALSE)

    openxlsx::writeData(
      wb = wb,
      sheet = "info",
      x = info,
      borders = "n",
      startCol = 2,
      startRow = 2,
      colNames = FALSE,
      rowNames = FALSE
    )

    openxlsx::setColWidths(
      wb = wb,
      sheet = utils::tail(openxlsx::worksheetOrder(wb), n = 1),
      cols = seq_len(ncol(info)),
      widths = 15
    )
  }

  return(wb)
}
