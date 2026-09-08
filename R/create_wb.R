#' @title Create a Workbook object
#'
#' @description This function is a wrapper for \code{openxlsx2::wb_workbook()}, but also sets the base font and adds an info sheet including e.g. the time it was created.
#'
#' @param font_size font size
#' @param font_name Name of a font
#' @param infosheet Should an info sheet be created?
#' @param infosheet_label Label for the info sheet
#' @param ... Other arguments passed to \code{openxlsx2::wb_workbook()}
#' @param fontSize `r lifecycle::badge("deprecated")` Use \code{font_size}.
#' @param fontName `r lifecycle::badge("deprecated")` Use \code{font_name}.
#' @param infosheetlabel `r lifecycle::badge("deprecated")` Use \code{infosheet_label}.
#'
#' @return Workbook object (see \{openxlsx2\})
#'
#' @export
#'
#' @importFrom here here
#' @importFrom lifecycle deprecated
#' @importFrom rstudioapi getSourceEditorContext isAvailable
#'
#' @examples
#' wb1 <- create_wb()
#'
#' wb2 <- create_wb(infosheet = FALSE)

create_wb <- function(font_size = 10,
                      font_name = "Arial",
                      infosheet = TRUE,
                      infosheet_label = "BioMath GmbH",
                      ...,
                      fontSize = deprecated(),
                      fontName = deprecated(),
                      infosheetlabel = deprecated()) {

  font_size <- resolve_deprecated(
    font_size, fontSize, "create_wb", "fontSize", "font_size"
  )
  font_name <- resolve_deprecated(
    font_name, fontName, "create_wb", "fontName", "font_name"
  )
  infosheet_label <- resolve_deprecated(
    infosheet_label, infosheetlabel, "create_wb",
    "infosheetlabel", "infosheet_label"
  )

  wb <- openxlsx2::wb_workbook(...)
  wb$set_base_font(font_size = font_size, font_name = font_name)

  if (infosheet) {
    info <-
      data.frame(
        col1 = c(infosheet_label, "Created on:", "Created via:"),
        col2 = c(
          " ",
          paste(Sys.time()),
          # this extracts the name of the R script where create_wb() is run.
          # In Positron, rstudioapi::isAvailable() is TRUE even during
          # rmarkdown::render(), but getSourceEditorContext()$path returns NULL
          # (no active editor). gsub(x = NULL) yields character(0), which makes
          # col2 shorter than col1 and data.frame() fails.
          {
            path <- if (rstudioapi::isAvailable()) rstudioapi::getSourceEditorContext()$path else ""
            if (is.null(path) || length(path) == 0) path <- ""
            gsub(x = path, pattern = here::here(), replacement = "")
          }
        )
      )

    wb$
      add_worksheet(sheet = "info", grid_lines = FALSE)$
      add_data(
        sheet     = "info",
        x         = info,
        dims      = "B2",
        col_names = FALSE,
        na        = NULL
      )$
      set_col_widths(sheet = "info", cols = 2:3, widths = 15)
  }

  return(wb)
}
