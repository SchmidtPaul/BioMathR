#' @title Save a Workbook object to Excel file
#'
#' @description This function is a wrapper for\code{openxlsx::saveWorkbook()}, but can also open the Excel file immediately after saving. File opening is cross-platform compatible (Windows, macOS, and Linux).
#'
#' @param wb A Workbook object to write to file
#' @param file A character string naming an xlsx file
#' @param overwrite If \code{TRUE}, overwrites the previous excel file
#' @param open_after_save If \code{TRUE}, opens the excel file after it was created
#' @param ... Other arguments passed to\code{openxlsx::saveWorkbook()}
#'
#' @export
#'
#' @import openxlsx
#' @importFrom here here

save_wb <-
  function(wb,
           file = here::here("untitled.xlsx"),
           overwrite = FALSE,
           open_after_save = TRUE,
           ...) {

    # If the 1st sheet is an info sheet, make the
    # 2nd sheet the active one when opening the file
    if (names(wb)[1] == "info") {
      openxlsx::activeSheet(wb = wb) <- 2
    }


    openxlsx::saveWorkbook(wb = wb,
                           file = file,
                           overwrite = overwrite,
                           ...)

    if (open_after_save) {
      open_file(file)
    }
  }
