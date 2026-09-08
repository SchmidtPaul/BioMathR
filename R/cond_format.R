#' @title Add conditional formatting to columns of a worksheet
#'
#' @description This function is a wrapper for \code{openxlsx2::wb_add_conditional_formatting()} allowing for a more convenient application of conditional formatting to a sheet in a Workbook object. The arguments \code{colour_scale}, \code{contains} and \code{expression} serve as shortcuts to obtaining the respective conditional formatting, but it is still possible to obtain all conditional formatting styles available in \{openxlsx2\} via the arguments \code{rule}, \code{style} and \code{type}. Note that \{openxlsx2\} workbooks are modified in place, so \code{wb} is updated whether or not the result is assigned.
#'
#' @param wb a Workbook object
#' @param sheet a name or index of a worksheet
#' @param columns column names or indices to apply conditional formatting to. Unknown names or indices outside \code{1:ncol} raise an error.
#' @param rows optional row indices (as in the spreadsheet, i.e. the header is row 1) to apply conditional formatting to. If NULL (default), applies to all data rows. Non-consecutive rows are formatted as separate blocks. On a sheet without data rows nothing is formatted.
#' @param colour_scale shortcut argument to apply "colorScale" formatting, e.g. \code{c("red" = 0, "grey" = 50, "green" = 100)}
#' @param contains shortcut argument to apply "containsText" formatting, e.g. \code{"word"}
#' @param expression shortcut argument to apply "expression" formatting, e.g. \code{">=50"}
#' @param type directly passed to \code{openxlsx2::wb_add_conditional_formatting()}. Matching is case-insensitive and type names of the predecessor package \{openxlsx\} (e.g. \code{"colourScale"}, \code{"databar"}, \code{"contains"}) are translated automatically.
#' @param rule directly passed to \code{openxlsx2::wb_add_conditional_formatting()}
#' @param style For \code{type = "colorScale"}: a vector of 2-3 colours. For \code{"dataBar"} and \code{"iconSet"}: as in \code{openxlsx2::wb_add_conditional_formatting()}. For all other types: the name of a dxf style that was registered in \code{wb} via \code{openxlsx2::wb_add_dxfs_style()}. If \code{NULL} (default), a style is registered on the fly from \code{font_colour} and \code{bg_fill}.
#' @param font_colour font colour of the formatted cells, used when \code{style = NULL}
#' @param bg_fill background colour of the formatted cells, used when \code{style = NULL}
#' @param ... directly passed to \code{openxlsx2::wb_add_conditional_formatting()}
#' @param sheetName `r lifecycle::badge("deprecated")` Use \code{sheet}.
#' @param colourScale `r lifecycle::badge("deprecated")` Use \code{colour_scale}.
#'
#' @return The Workbook object, invisibly
#'
#' @export
#'
#' @importFrom lifecycle deprecated
#'
#' @examples
#' wb <- create_wb()
#' add_sheet(wb, mtcars, "cars")
#' cond_format(wb, "cars", columns = "mpg", expression = ">25")

cond_format <- function(wb,
                        sheet = NULL,
                        columns,
                        rows = NULL,
                        colour_scale = NULL, # c("red" = 0, "grey" = 50, "green" = 100)
                        contains = NULL, # "word"
                        expression = NULL, # ">=50"
                        type = NULL,
                        rule = NULL,
                        style = NULL,
                        font_colour = "white",
                        bg_fill = "#ad0000",
                        ...,
                        sheetName = deprecated(),
                        colourScale = deprecated()) {

  sheet <- resolve_deprecated(
    sheet, sheetName, "cond_format", "sheetName", "sheet"
  )
  colour_scale <- resolve_deprecated(
    colour_scale, colourScale, "cond_format", "colourScale", "colour_scale"
  )

  if (inherits(style, "Style")) {
    stop(
      "As of BioMathR 0.9.0, 'style' no longer accepts an ",
      "openxlsx::createStyle() object. Use 'font_colour' and 'bg_fill', or ",
      "register a style via openxlsx2::wb_add_dxfs_style() and pass its name.",
      call. = FALSE
    )
  }

  # re-import data from Workbook to resolve column names and row count
  dat_copy <- openxlsx2::wb_to_df(wb, sheet = sheet)

  # get column index for columns to be conditionally formatted
  if (is.character(columns)) {
    columnindex <- match(columns, names(dat_copy))

    if (anyNA(columnindex)) {
      stop(
        "Column(s) not found in sheet: ",
        paste(columns[is.na(columnindex)], collapse = ", "),
        call. = FALSE
      )
    }
  } else {
    columnindex <- as.integer(columns)
    out_of_range <- is.na(columnindex) |
      columnindex < 1L |
      columnindex > ncol(dat_copy)

    if (any(out_of_range)) {
      stop(
        "Column index out of range (sheet has ", ncol(dat_copy), " columns): ",
        paste(columns[out_of_range], collapse = ", "),
        call. = FALSE
      )
    }
  }

  if (length(columnindex) == 0L) {
    stop("'columns' must select at least one column.", call. = FALSE)
  }

  # get row index for rows to be conditionally formatted
  # If rows parameter is provided, use it; otherwise format all data rows
  if (!is.null(rows)) {
    rowindex <- rows
  } else {
    rowindex <- seq_len(nrow(dat_copy)) + 1
  }


  # docolourScale -----------------------------------------------------------
  doColourScale <-
    all(
      !is.null(colour_scale),
      is.null(contains),
      is.null(expression),
      is.null(type),
      is.null(rule)
    )

  if (doColourScale) {
    type <- "colorScale"
    rule <- unname(colour_scale)
    style <- names(colour_scale)
  }


  # doContains --------------------------------------------------------------
  doContains <-
    all(
      is.null(colour_scale),
      !is.null(contains),
      is.null(expression),
      is.null(type),
      is.null(rule)
    )

  if (doContains) {
    type <- "containsText"
    rule <- contains
  }


  # doExpression ------------------------------------------------------------
  doExpression <-
    all(
      is.null(colour_scale),
      is.null(contains),
      !is.null(expression),
      is.null(type),
      is.null(rule)
    )

  if (doExpression) {
    type <- "expression"
    rule <- expression
  }

  if (is.null(type)) {
    type <- "expression"
  }

  type <- translate_cf_type(type)


  # nothing to format (e.g. zero-row sheet) - do not emit a rule on the header
  if (length(rowindex) == 0L) {
    return(invisible(wb))
  }


  # style -------------------------------------------------------------------
  # colorScale, dataBar and iconSet take their colours directly (or use the
  # {openxlsx2} defaults), everything else needs the name of a dxf style that
  # is registered in the workbook
  needs_dxf <- !type %in% c("colorScale", "dataBar", "iconSet")

  if (needs_dxf && is.null(style)) {
    style <- register_cf_style(wb, font_colour, bg_fill)
  }


  # execute conditional formatting ------------------------------------------
  # one call per contiguous cell range, so that e.g. a colour scale is scaled
  # within each block instead of across the gaps
  for (row_block in split_consecutive(rowindex)) {
    for (col_block in split_consecutive(columnindex)) {
      wb$add_conditional_formatting(
        sheet = sheet,
        dims  = openxlsx2::wb_dims(rows = row_block, cols = col_block),
        type  = type,
        rule  = rule,
        style = style,
        ...
      )
    }
  }

  invisible(wb)
}

#' Register a dxf style for conditional formatting
#'
#' Styles are named after their colours, so that repeated \code{cond_format()}
#' calls with the same colours reuse one style instead of piling up duplicates.
#'
#' @param wb a Workbook object
#' @param font_colour font colour
#' @param bg_fill background colour
#'
#' @return the style name
#' @noRd
register_cf_style <- function(wb, font_colour, bg_fill) {
  style_name <- paste0(
    "BioMathR_",
    gsub("[^A-Za-z0-9]", "", font_colour),
    "_on_",
    gsub("[^A-Za-z0-9]", "", bg_fill)
  )

  if (!style_name %in% wb$styles_mgr$dxf$name) {
    wb$add_dxfs_style(
      name       = style_name,
      font_color = as_wb_colour(font_colour),
      bg_fill    = as_wb_colour(bg_fill)
    )
  }

  style_name
}

#' Turn a colour string into an \{openxlsx2\} colour
#'
#' @param x a hex colour (with or without leading "#") or an R colour name
#'
#' @return a wbColour object
#' @noRd
as_wb_colour <- function(x) {
  if (inherits(x, "wbColour")) {
    return(x)
  }

  if (grepl("^#?([0-9A-Fa-f]{6}|[0-9A-Fa-f]{8})$", x)) {
    openxlsx2::wb_color(hex = x)
  } else {
    openxlsx2::wb_color(x)
  }
}
