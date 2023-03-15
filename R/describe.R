#' @title Compute descriptive statistics
#'
#' @description This function computes common descriptive statistics of numeric variables for grouped or ungrouped data and converts it to a tidy formatted table. This function tried to copy \code{dlookr::describe()} because of \href{https://github.com/choonghyunryu/dlookr/issues/79}{this issue}.
#'
#' @param data a data.frame or a \code{\link{tbl_df}} or a \code{\link{grouped_df}}.
#' @param yvars a character vector with the names of the columns to be described.
#' @param ungroupafter If \code{TRUE}, the results will be \code{ungroup()}-ed.
#' @param lang If \code{"eng"} (default) or  \code{"ger"} columns will be renamed accordingly.
#'
#' @examples
#' library(dplyr)
#' library(BioMathR)
#' PlantGrowth %>%
#'   group_by(group) %>%
#'   describe("weight")
#'
#' PlantGrowth %>%
#'   mutate(weight2 = weight + 1) %>%
#'   group_by(group) %>%
#'   describe(c("weight", "weight2"))
#' @return A tibble
#' @export
describe <- function(data, yvars, ungroupafter = TRUE, lang = c("eng", "ger")[1]) {

  Variable <- name <- value <- NULL # avoid package check warning

  data <- data %>% select(all_of(group_vars(data)), all_of(yvars))

  out <- data %>%
    summarise(across(
      all_of(yvars),
      .names = "{.col}_{fn}",
      .fns = list(
        n = ~ sum(!is.na(.)),
        na = ~ sum(is.na(.)),
        mean = ~ mean(., na.rm = TRUE),
        sd = ~ sd(., na.rm = TRUE),
        IQR = ~ quantile(., 0.75, na.rm = TRUE) - quantile(., 0.25, na.rm = TRUE),
        p00 = ~ min(., na.rm = TRUE),
        p50 = ~ median(., na.rm = TRUE),
        p100 = ~ max(., na.rm = TRUE)
      )
    )) %>%
    suppressMessages() %>%
    tidyr::pivot_longer(cols = -all_of(group_vars(data))) %>%
    tidyr::separate(name, into = c("Variable", "Stat"), sep = "_") %>%
    tidyr::pivot_wider(names_from = Stat, values_from = value) %>%
    select(Variable, everything()) %>%
    arrange(Variable)

  if (ungroupafter) {
    out <- ungroup(out)
  }

  if (lang %in% c("eng", "ger")) {

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

    out <- out %>%
      rename(
        "N" = "n",
        "Min" = "p00",
        "Median" = "p50",
        "Max" = "p100"
      ) %>%
      rename(any_of(renames[[lang]]))
  }

  out
}

