#' @title `str_wrap()` with <br> instead
#'
#' @param string Input vector. Either a character vector, or something coercible to one.
#' @param whitespace_only If `TRUE` wrapping will only occur at whitespace. If `FALSE` (the default), can break on any non-word character (e.g. /, -). Note that this default is different from the underlying `stringr::str_wrap()`.
#' @param ... other arguments passed to `stringr::str_wrap()`
#'
#' @export
str_wrap_br <- function(string, whitespace_only = FALSE, ...) {
  out <- stringr::str_wrap(string, whitespace_only = whitespace_only, ...)
  stringr::str_replace_all(out, "\n", "<br>")
}
