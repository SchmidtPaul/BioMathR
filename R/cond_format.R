#' @title Add conditional formatting to columns of a worksheet
#'
#' @description This function is a wrapper for `openxlsx::conditionalFormatting()` allowing for a more convenient application of conditional formatting to a sheet in a Workbook object. The arguments `colourScale`, `contains` and `expression` serve as shortcuts to obtaining the respective conditional formatting, but it is still possible to obtain \href{https://rdrr.io/cran/openxlsx/man/conditionalFormatting.html}{all possible conditional formatting styles available in {openxlsx}} via the arguments `rule`, `style` and `type` which are directly passed to `openxlsx::conditionalFormatting()`.
#'
#' @param wb a Workbook object
#' @param sheetName a name or index of a worksheet
#' @param columns column names or indices to apply conditional formatting to
#' @param colourScale shortcut argument to apply "colourScale" formatting, e.g. `c("red" = 0, "grey" = 50, "green" = 100)`
#' @param contains shortcut argument to apply "contains" formatting, e.g. `"word"`
#' @param expression shortcut argument to apply "expression" formatting, e.g. `">=50"`
#' @param type directly passed to `openxlsx::conditionalFormatting()`
#' @param rule directly passed to `openxlsx::conditionalFormatting()`
#' @param style directly passed to `openxlsx::conditionalFormatting()`. By default, cells will have a red fill and white font color.
#' @param ... directly passed to `openxlsx::conditionalFormatting()`
#'
#' @export
#'
#' @import openxlsx

cond_format <- function(wb,
                        sheetName = NULL,
                        columns,
                        colourScale = NULL, # c("red" = 0, "grey" = 50, "green" = 100)
                        contains = NULL, # "word"
                        expression = NULL, # ">=50"
                        type = NULL,
                        rule = NULL,
                        style = openxlsx::createStyle(fontColour = "white", bgFill = "#ad0000"),
                        ...) {

  # re-import data from Workbook
  dat_copy <- openxlsx::readWorkbook(xlsxFile = wb,
                                     sheet = sheetName,
                                     sep.names = " ")

  # get column index for columns to be conditionally formatted
  if (is.character(columns)) {
    columnindex <- match(columns, names(dat_copy))
  } else {
    columnindex <- columns
  }

  # get row index for rows to be conditionally formatted
  rowindex <- (1:nrow(dat_copy))+1


  # docolourScale -----------------------------------------------------------
  doColourScale <-
    all(
      !is.null(colourScale),
      is.null(contains),
      is.null(expression),
      is.null(type),
      is.null(rule)
    )

  if (doColourScale) {
    type <- "colourScale"
    rule <- unname(colourScale)
    style <- names(colourScale)
  }


  # doContains --------------------------------------------------------------
  doContains <-
    all(
      is.null(colourScale),
      !is.null(contains),
      is.null(expression),
      is.null(type),
      is.null(rule)
    )

  if (doContains) {
    type <- "contains"
    rule <- contains
    style <- style
  }


  # doExpression ------------------------------------------------------------
  doExpression <-
    all(
      is.null(colourScale),
      is.null(contains),
      !is.null(expression),
      is.null(type),
      is.null(rule)
    )

  if (doExpression) {
    type <- "expression"
    rule <- expression
    style <- style
  }


  # execute conditional formatting ------------------------------------------
  openxlsx::conditionalFormatting(
    wb = wb,
    sheet = sheetName,
    cols = columnindex,
    rows = rowindex,
    type = type,
    rule = rule,
    style = style,
    ...
  )
}
