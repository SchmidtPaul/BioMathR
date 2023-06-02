#' Write a Data Frame to an ASCII Text File
#'
#' This function takes a data frame, a file path, and a decimal point character as input,
#' and writes the data frame to the file as a tab-separated ASCII text file,
#' with no quotes around the values, no row names. The decimal point representation
#' is by default a dot, but can be changed by the user.
#'
#' @param data A data frame that will be written to a text file.
#' @param path The file path where the text file will be written.
#' @param dec The character to be used as decimal point. Default is ".".
#'
#' @return NULL. The function is invoked for its side effect, which is writing the data frame
#' to a text file.
#'
#' @export
write_ascii <- function(data, path, dec = ".") {
  write.table(
    data,
    file = path,
    quote = FALSE,
    sep = "\t",
    dec = dec,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}
