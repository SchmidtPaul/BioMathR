#' @title Open a File with the System's Default Application
#'
#' @description Opens a file using the system's default application in a
#'   cross-platform manner. Works on Windows, macOS, and Linux.
#'
#' @param path A character string specifying the path to the file to open.
#'
#' @return Invisible NULL. The function is called for its side effect of
#'   opening a file.
#'
#' @details This function detects the operating system and uses the appropriate
#'   command to open the file:
#'   \itemize{
#'     \item Windows: Uses \code{shell.exec()}
#'     \item macOS: Uses \code{system2("open", ...)}
#'     \item Linux: Uses \code{system2("xdg-open", ...)}
#'   }
#'
#' @examples
#' \dontrun{
#' # Open a PDF file
#' open_file("path/to/document.pdf")
#'
#' # Open an Excel file
#' open_file("path/to/spreadsheet.xlsx")
#'
#' # Open a PNG image
#' open_file("path/to/image.png")
#' }
#'
#' @export
open_file <- function(path) {
  # Normalize to absolute path for proper file existence checking and opening
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)

  if (!file.exists(path)) {
    warning("File does not exist: ", path)
    return(invisible(NULL))
  }

  sysname <- Sys.info()["sysname"]

  if (sysname == "Windows") {
    shell.exec(path)
  } else if (sysname == "Darwin") {  # macOS
    system2("open", shQuote(path))
  } else {  # Linux and other Unix-like systems
    system2("xdg-open", shQuote(path))
  }

  invisible(NULL)
}
