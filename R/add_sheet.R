#' @title Add a worksheet to a Workbook object
#'
#' @description This function is a wrapper for \code{openxlsx::addWorksheet()} and \code{openxlsx::writeData()}, but also does some extra formatting via more \{openxlsx\} functions.
#'
#' @param wb A Workbook object to attach the new worksheet
#' @param data Object to be written. For classes supported look at the examples in \{openxlsx\}.
#' @param sheetName The name of the worksheet that is to be created and have \code{data} written into. Defaults to the \code{data} object name if left empty.
#' @param colWidth For all columns: column width. Can either be Excel column width units or "auto" for automatic sizing
#' @param colWidthMin For all columns: minimum column width
#' @param colWidthMax For all columns: maximum column width
#' @param gridLines If \code{FALSE}, the worksheet grid lines will be hidden
#' @param freezefirstRow If \code{TRUE}, freezes the first row
#' @param freezefirstCol If \code{TRUE}, freezes the first row
#' @param addFilters If \code{TRUE}, adds filters to worksheet columns
#' @param rowheight Row height in Excel row height units
#' @param textwrap If \code{TRUE}, all cells are formatted via text wrap so their content never spills into adjacent empty cells
#' @param dateformat If not \code{"none"} (default is \code{"yyyy-mm-dd"}), all date-formatted columns in \code{data} are explicitly formatted with \code{numFmt = "yyyy-mm-dd"}
#' @param datetimeformat If not \code{"none"} (default is \code{"yyyy-mm-dd hh:mm:ss"}), all datetime-formatted columns in \code{data} are explicitly formatted with \code{numFmt = "yyyy-mm-dd hh:mm:ss"}
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
           addFilters = TRUE,
           rowheight = 15,
           textwrap = TRUE,
           dateformat = "yyyy-mm-dd",
           datetimeformat = "yyyy-mm-dd hh:mm:ss")
  {
    # sheetname default: data-object name
    if (is.null(sheetName)) {
      sheetName <- substitute(data)
    }

    # add worksheet to wb
    openxlsx::addWorksheet(wb = wb,
                           sheetName = sheetName,
                           gridLines = gridLines)

    # define styles
    BMheaderstyle <- openxlsx::createStyle(
      textDecoration = "bold",
      halign = "left",
      valign = "top",
      border = "TopBottomLeftRight",
      wrapText = FALSE
    )

    BMbodystyle <- openxlsx::createStyle(
      halign = "left",
      valign = "top",
      wrapText = textwrap,
      border = "TopBottomLeftRight",
      numFmt = openxlsx::openxlsx_getOp("numFmt", NULL),
    )

    # add data to worksheet
    openxlsx::writeData(
      wb = wb,
      sheet = sheetName,
      x = data,
      borders = "all",
      headerStyle = BMheaderstyle
    )

    # format worksheet
    bodyrows <- seq_len(nrow(data)) + 1

    ## general
    openxlsx::addStyle(
      wb = wb,
      sheet = utils::tail(openxlsx::worksheetOrder(wb), n = 1),
      style = BMbodystyle,
      cols = seq_len(ncol(data)),
      rows = bodyrows,
      gridExpand = TRUE
    )

    ## date columns
    is_date <- function(x) inherits(x, 'Date')
    date_cols <- unname(which(sapply(data, is_date)))

    if (dateformat != "none" & length(date_cols) > 0) {
      openxlsx::addStyle(
        wb = wb,
        sheet = utils::tail(openxlsx::worksheetOrder(wb), n = 1),
        style = openxlsx::createStyle(numFmt = dateformat),
        stack = TRUE,
        cols = date_cols,
        rows = bodyrows,
        gridExpand = TRUE
      )
    }

    ## datetime columns
    is_datetime <- function(x) {inherits(x, "POSIXt") && attr(x, "tzone") != "" && all(!is.na(as.numeric(x)))}
    datetime_cols <- unname(which(sapply(data, is_datetime)))

    if (datetimeformat != "none" & length(datetime_cols) > 0) {
      openxlsx::addStyle(
        wb = wb,
        sheet = utils::tail(openxlsx::worksheetOrder(wb), n = 1),
        style = openxlsx::createStyle(numFmt = datetimeformat),
        stack = TRUE,
        cols = datetime_cols,
        rows = bodyrows,
        gridExpand = TRUE
      )
    }

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
        cols = seq_len(ncol(data))
      )
    }

    # column widths
    options("openxlsx.minWidth" = colWidthMin)
    options("openxlsx.maxWidth" = colWidthMax)

    openxlsx::setColWidths(
      wb = wb,
      sheet = utils::tail(openxlsx::worksheetOrder(wb), n = 1),
      cols = seq_len(ncol(data)),
      widths = "auto"
    )

    # row height
    openxlsx::setRowHeights(
      wb = wb,
      sheet = utils::tail(openxlsx::worksheetOrder(wb), n = 1),
      rows = c(1, bodyrows),
      heights = rowheight
    )

  }
