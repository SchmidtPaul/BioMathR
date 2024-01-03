#' Write a Data Frame to an ASCII Text File
#'
#' This function writes a data frame to a specified file path as a tab-separated ASCII text file.
#' It offers flexibility in formatting the output, including the choice of decimal point character,
#' whether to include quotes, row names, and column names. The function can operate using either
#' the base R `write.table` function or `data.table`'s `fwrite` function for file writing.
#'
#' @param data A data frame that will be written to a text file.
#' @param path The file path where the text file will be written.
#' @param dec The character to be used as decimal point. Default is ".".
#' @param sep The field separator character. Default is "\\t" (tab).
#' @param quote Logical; if TRUE, fields will be surrounded by quotes.
#'              Default is FALSE, meaning no quotes are used.
#' @param row.names Logical; if TRUE, row names are written to the file.
#'                  Default is FALSE, meaning row names are not included.
#' @param col.names Logical; if TRUE, column names are written to the file.
#'                  Default is TRUE, meaning column names are included.
#' @param use_fwrite A boolean flag indicating whether to use `data.table::fwrite()`
#'                   for file writing. Default is FALSE, which uses `write.table`.
#'
#' @return NULL. The function is invoked for its side effect, which is writing
#' the data frame to a text file.
#'
#' @export
write_ascii <- function(data,
                        path,
                        dec = ".",
                        sep = "\t",
                        quote = FALSE,
                        row.names = FALSE,
                        col.names = TRUE,
                        use_fwrite = FALSE) {
    if (!use_fwrite) {
      utils::write.table(
        data,
        file = path,
        dec = dec,
        sep = sep,
        quote = quote,
        row.names = row.names,
        col.names = col.names,
        fileEncoding = "UTF-8"
      )
    } else {
      if (!requireNamespace("data.table", quietly = TRUE)) {
        stop("data.table package is required when use_fwrite is TRUE")
      }
      data.table::fwrite(
        data,
        file = path,
        dec = dec,
        sep = sep,
        quote = quote,
        row.names = row.names,
        col.names = col.names
        # Note: "UTF-8" is the default encoding in fwrite
      )
    }
  }
