#' @title Add a worksheet to a Workbook object
#'
#' @description This function is a wrapper for \code{openxlsx::addWorksheet()} and \code{openxlsx::writeData()}, but also does some extra formatting via more {openxlsx} functions.
#'
#' @param wb A Workbook object to attach the new worksheet
#' @param data Object to be written. For classes supported look at the examples in {openxlsx}.
#' @param sheetName The name of the worksheet that is to be created and have \code{data} written into. Defaults to the \code{data} object name if left empty.
#' @param colWidth For all columns: column width. Can either be Excel column width units or "auto" for automatic sizing.
#' @param colWidthMin For all columns: minimum column width
#' @param colWidthMax For all columns: maximum column width
#' @param gridLines If \code{FALSE}, the worksheet grid lines will be hidden.
#' @param freezefirstRow If \code{TRUE}, freezes the first row
#' @param freezefirstCol If \code{TRUE}, freezes the first row
#' @param addFilters If \code{TRUE}, adds filters to worksheet columns
#'
#' @export
#'
#' @import openxlsx
#' @importFrom utils tail
#'
#' @examples
#' library(BioMathR)
#' wb1 <- create_wb()
#' wb1 <- add_sheet(wb = wb1, data = mtcars)
#'
#' library(dplyr, warn.conflicts = FALSE)
#' wb2 <- create_wb() %>%
#'    add_sheet(mtcars, "The cars sheet")

add_sheet <-
  function(wb,
           data,
           sheetName = NULL,
           colWidth = "auto",
           colWidthMin = 5,
           colWidthMax = 25,
           gridLines = FALSE,
           freezefirstRow = TRUE,
           freezefirstCol = FALSE,
           addFilters = TRUE)
  {
    # sheetname default: data-object name
    if (is.null(sheetName)) {
      sheetName <- substitute(data)
    }

    # add worksheet to wb
    openxlsx::addWorksheet(wb = wb,
                           sheetName = sheetName,
                           gridLines = gridLines)

    # add data to worksheet
    openxlsx::writeData(
      wb = wb,
      sheet = sheetName,
      x = data,
      borders = "all",
      headerStyle = openxlsx::createStyle(textDecoration="bold", border = "TopBottomLeftRight")
    )

    # freeze pane
    openxlsx::freezePane(
      wb = wb,
      sheet = utils::tail(openxlsx::worksheetOrder(wb), n = 1),
      firstRow = freezefirstRow,
      firstCol = freezefirstCol
    )

    # add filters
    if (addFilters) {
      openxlsx::addFilter(
        wb = wb,
        sheet = utils::tail(openxlsx::worksheetOrder(wb), n = 1),
        rows = 1,
        cols = 1:ncol(data)
      )
    }

    # column widths
    options("openxlsx.minWidth" = colWidthMin)
    options("openxlsx.maxWidth" = colWidthMax)

    openxlsx::setColWidths(
      wb = wb,
      sheet = utils::tail(openxlsx::worksheetOrder(wb), n = 1),
      cols = c(1:ncol(data)),
      widths = "auto"
    )

    # TO DO: auto-add dlookr::diagnose(df1)[,1:5]?

  }
