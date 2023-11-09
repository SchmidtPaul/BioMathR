#' @title Collapse a Vector of Strings with Custom Separators
#'
#' @description
#' This function takes a vector of strings and collapses it into a single string,
#' using specified separators. By default, an error is thrown if the vector is NULL,
#' empty, contains NA values, or empty strings, but these behaviors can be modified.
#'
#' @param strings A character vector to be collapsed.
#' @param sep A string separator to use between elements, default is ", ".
#' @param final_sep A string separator to use before the last element, default is " and ".
#' @param wrap_each_in A single character or a string of two characters used to wrap each string element. If two characters are given, the first is used as the prefix and the second as the suffix.
#' @param error_when_NULL Logical; if TRUE (default), the function throws an error when `strings` is NULL or empty.
#' @param error_when_NA Logical; if TRUE (default), the function throws an error when `strings` contains NA or empty strings.
#' @param na_replace The string to replace NA or empty strings with, if `error_when_NA` is FALSE.
#'
#' @return A single collapsed string. If `strings` is NULL or empty and `error_when_NULL` is FALSE, NULL is returned.
#' @examples
#' for(i in 1:4){
#'   print(str_collapse_with(letters[1:i]))
#' }
#'
#' str_collapse_with(levels(PlantGrowth$group), final_sep = " und ", wrap_each_in = '`')
#' str_collapse_with(letters[1:5], sep = " & ", final_sep = " as well as ", wrap_each_in = '{}')
#'
#' @export
str_collapse_with <- function(strings,
                          sep = ", ",
                          final_sep = " and ",
                          wrap_each_in = NULL,
                          error_when_NULL = TRUE,
                          error_when_NA = TRUE,
                          na_replace = "-") {

  # Check for NULL or empty input
  if (is.null(strings) || length(strings) == 0) {
    if (error_when_NULL) {
      stop("Input vector is NULL or empty.")
    } else {
      return(NULL)
    }
  }

  # Check for NA or empty strings and handle accordingly
  if (error_when_NA && any(is.na(strings) | strings == "")) {
    stop("Input vector contains NA or empty strings.")
  }

  # Replace NA or empty strings if error_when_NA is FALSE
  if (!error_when_NA) {
    strings[is.na(strings) | strings == ""] <- na_replace
  }

  # Apply wrapping to each element if specified
  if (!is.null(wrap_each_in)) {
    if (nchar(wrap_each_in) == 2) {
      wrap_start <- substr(wrap_each_in, 1, 1)
      wrap_end <- substr(wrap_each_in, 2, 2)
    } else {
      wrap_start <- wrap_end <- wrap_each_in
    }
    strings <- paste0(wrap_start, strings, wrap_end)
  }

  # Collapse the string vector
  len <- length(strings)
  if (len == 1) {
    return(strings)
  } else {
    paste(paste(strings[-len], collapse = sep), strings[len], sep = final_sep)
  }
}
