#' @title Format dlookr::describe() output
#'
#' @param dlookr_describe_out a table create via \code{dlookr::describe()}
#' @param lang language for the result table column names
#'
#' @export
#'
#' @import dplyr
format_dlookrdescribe <- function(dlookr_describe_out,
                                  lang = c("ger", "eng")[1]) {

  IQR <- p00 <- p100 <- p50 <- sd <- NULL # avoid package check warning

  renames <- list(
    ger =
      c(
        "MW" = "mean",
        "StdAbw" = "sd",
        "IQA" = "IQR",
        "Fehl" = "na"
      ),
    eng =
      c(
        "Mean" = "mean",
        "StdDev" = "sd",
        "IQR" = "IQR",
        "Miss" = "na"
      )
  )

  out <- dlookr_describe_out %>%
    dplyr::ungroup() %>%
    dplyr::rename("Variable" = 1) %>%
    dplyr::select(1:sd, IQR, p00, p50, p100) %>%
    dplyr::rename(
      "N" = "n",
      "Min" = "p00",
      "Median" = "p50",
      "Max" = "p100"
    ) %>%
    dplyr::rename(any_of(renames[[lang]]))

  out
}
