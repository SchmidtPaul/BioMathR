#' @title Deprecated version of get_emmeans
#'
#' @description This function has been deprecated. Please use \code{\link{get_emmeans}} instead.
#'
#' @param ... Arguments to be passed to \code{\link{get_emmeans}}.
#'
#' @seealso \code{\link{get_emmeans}}
#'
#' @export

emmeans_BM <- function(...) {
  # Display a warning about the deprecation
  .Deprecated(new = "get_emmeans", package = "your_package_name", msg = "emmeans_BM will be deprecated in a future version. Please use get_emmeans instead.")

  # Call the new function
  get_emmeans(...)
}
