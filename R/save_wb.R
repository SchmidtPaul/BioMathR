#' @title Save a Workbook object to Excel file
#'
#' @description This function is a wrapper for \code{openxlsx2::wb_save()}, but can also open the Excel file immediately after saving. File opening is cross-platform compatible (Windows, macOS, and Linux). If the first sheet is the \code{"info"} sheet of \code{create_wb()} and there is a second sheet, that second sheet is the active and selected one when the file is opened.
#'
#' @param wb A Workbook object to write to file
#' @param file A character string naming an xlsx file
#' @param overwrite If \code{TRUE}, overwrites the previous excel file
#' @param open_after_save If \code{TRUE}, opens the excel file after it was created
#'
#' @return The file path, invisibly
#'
#' @export
#'
#' @importFrom here here

save_wb <-
  function(wb,
           file = here::here("untitled.xlsx"),
           overwrite = FALSE,
           open_after_save = TRUE) {

    sheet_names <- openxlsx2::wb_get_sheet_names(wb)

    # If the 1st sheet is an info sheet, make the 2nd sheet the active one
    # when opening the file. activeTab alone leaves tabSelected on sheet 1,
    # so Excel would show two selected tabs (grouped sheets).
    if (length(sheet_names) > 1 && unname(sheet_names)[1] == "info") {
      wb$set_active_sheet(sheet = 2)
      wb$set_selected(sheet = 2)
    }

    wb$save(file = file, overwrite = overwrite)

    if (open_after_save) {
      open_file(file)
    }

    invisible(file)
  }
