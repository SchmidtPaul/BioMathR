#' @title Add a worksheet to a Workbook object
#'
#' @description This function is a wrapper for \code{openxlsx2::wb_add_worksheet()} and \code{openxlsx2::wb_add_data()}, but also does some extra formatting via more \{openxlsx2\} functions. Note that \{openxlsx2\} workbooks are modified in place, so \code{wb} is updated whether or not the result is assigned.
#'
#' @param wb A Workbook object to attach the new worksheet
#' @param data Object to be written. For classes supported look at the examples in \{openxlsx2\}.
#' @param sheet_name The name of the worksheet that is to be created and have \code{data} written into. Defaults to the \code{data} object name if left empty. Must not be the name of an existing sheet (case-insensitive); illegal characters are removed and names are shortened to 31 characters by \{openxlsx2\} (with a warning).
#' @param col_width For all columns: column width. Can either be a spreadsheet column width or "auto" for automatic sizing
#' @param col_width_min For all columns: minimum column width
#' @param col_width_max For all columns: maximum column width
#' @param grid_lines If \code{FALSE}, the worksheet grid lines will be hidden
#' @param freeze_first_row If \code{TRUE}, freezes the first row
#' @param freeze_first_col If \code{TRUE}, freezes the first column
#' @param add_filters If \code{TRUE}, adds filters to worksheet columns
#' @param row_height Row height in spreadsheet row height units
#' @param text_wrap If \code{TRUE}, all cells are formatted via text wrap so their content never spills into adjacent empty cells
#' @param date_format If not \code{"none"} (default is \code{"yyyy-mm-dd"}), all date-formatted columns in \code{data} are explicitly formatted with \code{numfmt = "yyyy-mm-dd"}
#' @param datetime_format If not \code{"none"} (default is \code{"yyyy-mm-dd hh:mm:ss"}), all datetime-formatted columns in \code{data} are explicitly formatted with \code{numfmt = "yyyy-mm-dd hh:mm:ss"}
#' @param na Value used for replacing \code{NA} values from \code{data}. The default \code{NULL} leaves those cells empty.
#' @param sheetName `r lifecycle::badge("deprecated")` Use \code{sheet_name}.
#' @param colWidth `r lifecycle::badge("deprecated")` Use \code{col_width}.
#' @param colWidthMin `r lifecycle::badge("deprecated")` Use \code{col_width_min}.
#' @param colWidthMax `r lifecycle::badge("deprecated")` Use \code{col_width_max}.
#' @param gridLines `r lifecycle::badge("deprecated")` Use \code{grid_lines}.
#' @param freezefirstRow `r lifecycle::badge("deprecated")` Use \code{freeze_first_row}.
#' @param freezefirstCol `r lifecycle::badge("deprecated")` Use \code{freeze_first_col}.
#' @param addFilters `r lifecycle::badge("deprecated")` Use \code{add_filters}.
#' @param rowheight `r lifecycle::badge("deprecated")` Use \code{row_height}.
#' @param textwrap `r lifecycle::badge("deprecated")` Use \code{text_wrap}.
#' @param dateformat `r lifecycle::badge("deprecated")` Use \code{date_format}.
#' @param datetimeformat `r lifecycle::badge("deprecated")` Use \code{datetime_format}.
#'
#' @return The Workbook object, invisibly
#'
#' @export
#'
#' @importFrom lifecycle deprecated
#'
#' @examples
#' library(BioMathR)
#' wb1 <- create_wb()
#' add_sheet(wb = wb1, data = mtcars)
#'
#' wb2 <- create_wb()
#' add_sheet(wb2, mtcars, "The cars sheet")

add_sheet <-
  function(wb,
           data,
           sheet_name = NULL,
           col_width = "auto",
           col_width_min = 5,
           col_width_max = 25,
           grid_lines = FALSE,
           freeze_first_row = TRUE,
           freeze_first_col = FALSE,
           add_filters = TRUE,
           row_height = 15,
           text_wrap = TRUE,
           date_format = "yyyy-mm-dd",
           datetime_format = "yyyy-mm-dd hh:mm:ss",
           na = NULL,
           sheetName = deprecated(),
           colWidth = deprecated(),
           colWidthMin = deprecated(),
           colWidthMax = deprecated(),
           gridLines = deprecated(),
           freezefirstRow = deprecated(),
           freezefirstCol = deprecated(),
           addFilters = deprecated(),
           rowheight = deprecated(),
           textwrap = deprecated(),
           dateformat = deprecated(),
           datetimeformat = deprecated())
  {
    sheet_name <- resolve_deprecated(
      sheet_name, sheetName, "add_sheet", "sheetName", "sheet_name"
    )
    col_width <- resolve_deprecated(
      col_width, colWidth, "add_sheet", "colWidth", "col_width"
    )
    col_width_min <- resolve_deprecated(
      col_width_min, colWidthMin, "add_sheet", "colWidthMin", "col_width_min"
    )
    col_width_max <- resolve_deprecated(
      col_width_max, colWidthMax, "add_sheet", "colWidthMax", "col_width_max"
    )
    grid_lines <- resolve_deprecated(
      grid_lines, gridLines, "add_sheet", "gridLines", "grid_lines"
    )
    freeze_first_row <- resolve_deprecated(
      freeze_first_row, freezefirstRow, "add_sheet",
      "freezefirstRow", "freeze_first_row"
    )
    freeze_first_col <- resolve_deprecated(
      freeze_first_col, freezefirstCol, "add_sheet",
      "freezefirstCol", "freeze_first_col"
    )
    add_filters <- resolve_deprecated(
      add_filters, addFilters, "add_sheet", "addFilters", "add_filters"
    )
    row_height <- resolve_deprecated(
      row_height, rowheight, "add_sheet", "rowheight", "row_height"
    )
    text_wrap <- resolve_deprecated(
      text_wrap, textwrap, "add_sheet", "textwrap", "text_wrap"
    )
    date_format <- resolve_deprecated(
      date_format, dateformat, "add_sheet", "dateformat", "date_format"
    )
    datetime_format <- resolve_deprecated(
      datetime_format, datetimeformat, "add_sheet",
      "datetimeformat", "datetime_format"
    )

    # sheet name default: data-object name (after resolving the alias, so
    # that an explicit sheetName = NULL behaves like sheet_name = NULL)
    if (is.null(sheet_name)) {
      sheet_name <- deparse(substitute(data))[1]
    }
    sheet_name <- as.character(sheet_name)

    if (length(sheet_name) != 1L || is.na(sheet_name) || !nzchar(sheet_name)) {
      stop("'sheet_name' must be a single non-empty string.", call. = FALSE)
    }

    # {openxlsx2} does not stop on a duplicate sheet name but silently creates
    # "name (1)" - the data below would then be written into the *existing*
    # sheet. Excel treats sheet names case-insensitively.
    existing <- unname(openxlsx2::wb_get_sheet_names(wb))

    if (tolower(sheet_name) %in% tolower(existing)) {
      stop(
        "A sheet named '", sheet_name, "' already exists in the workbook ",
        "(sheet names are case-insensitive).",
        call. = FALSE
      )
    }

    has_rows <- nrow(data) > 0

    # add worksheet to wb
    wb$add_worksheet(sheet = sheet_name, grid_lines = grid_lines)

    # {openxlsx2} may have cleaned the name (illegal characters, more than 31
    # characters) - from here on use the name it actually assigned
    sheet_name <- unname(openxlsx2::wb_get_sheet_names(wb))[length(existing) + 1L]

    # add data to wb
    wb$add_data(sheet = sheet_name, x = data, na = na)

    # format worksheet
    dims_all <- openxlsx2::wb_dims(x = data)
    dims_head <- openxlsx2::wb_dims(x = data, select = "col_names")

    ## header
    wb$
      add_cell_style(
        sheet      = sheet_name,
        dims       = dims_head,
        horizontal = "left",
        vertical   = "top",
        wrap_text  = FALSE
      )$
      add_font(
        sheet  = sheet_name,
        dims   = dims_head,
        bold   = "1",
        update = "bold"
      )

    ## body
    if (has_rows) {
      wb$add_cell_style(
        sheet      = sheet_name,
        dims       = openxlsx2::wb_dims(x = data, select = "data"),
        horizontal = "left",
        vertical   = "top",
        wrap_text  = text_wrap
      )
    }

    ## borders around all cells, empty ones included
    wb$add_border(
      sheet       = sheet_name,
      dims        = dims_all,
      inner_hgrid = "thin",
      inner_vgrid = "thin"
    )

    ## date columns
    date_cols <- unname(which(vapply(data, inherits, logical(1), "Date")))

    if (has_rows && date_format != "none" && length(date_cols) > 0) {
      wb$add_numfmt(
        sheet  = sheet_name,
        dims   = openxlsx2::wb_dims(x = data, cols = date_cols),
        numfmt = date_format
      )
    }

    ## datetime columns
    datetime_cols <- unname(which(vapply(data, inherits, logical(1), "POSIXt")))

    if (has_rows && datetime_format != "none" && length(datetime_cols) > 0) {
      wb$add_numfmt(
        sheet  = sheet_name,
        dims   = openxlsx2::wb_dims(x = data, cols = datetime_cols),
        numfmt = datetime_format
      )
    }

    # freeze pane
    wb$freeze_pane(
      sheet      = sheet_name,
      first_row  = freeze_first_row,
      first_col  = freeze_first_col
    )

    # add filters
    if (add_filters) {
      wb$add_filter(
        sheet = sheet_name,
        rows  = 1,
        cols  = seq_len(ncol(data))
      )
    }

    # column widths
    op <- options(
      openxlsx2.minWidth = col_width_min,
      openxlsx2.maxWidth = col_width_max
    )
    on.exit(options(op), add = TRUE)

    wb$set_col_widths(
      sheet  = sheet_name,
      cols   = seq_len(ncol(data)),
      widths = col_width
    )

    # row height
    wb$set_row_heights(
      sheet   = sheet_name,
      rows    = seq_len(nrow(data) + 1),
      heights = row_height
    )

    invisible(wb)
  }
