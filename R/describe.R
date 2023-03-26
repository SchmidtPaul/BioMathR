#' @title Compute descriptive statistics
#'
#' @description This function computes common descriptive statistics of numeric variables for grouped or ungrouped data and converts it to a tidy formatted table. This function tried to copy \code{dlookr::describe()} because of \href{https://github.com/choonghyunryu/dlookr/issues/79}{this issue}.
#'
#' @param data A data.frame or a \code{\link{tbl_df}} or a \code{\link{grouped_df}}.
#' @param yvars A character vector with the names of the columns to be described.
#' @param digits Number of digits the numeric columns should be rounded to.
#' @param ungroupafter If \code{TRUE}, the results will be \code{ungroup()}-ed.
#' @param lang Language for table names.
#'
#' @return A tibble
#' @examples
#' library(dplyr)
#' library(BioMathR)
#' PlantGrowth %>%
#'   group_by(group) %>%
#'   describe("weight", lang = "ger")
#'
#' PlantGrowth %>%
#'   mutate(
#'     group2 = as.factor(rep(c("A", "A", "B",  "B", "B"), 6)),
#'     weight2 = weight * 2,
#'     weight3 = rep(c(3, NA), 15)
#'   ) %>%
#'   group_by(group, group2) %>%
#'   describe(c("weight", "weight2", "weight3"))
#' @export
describe <-
  function(data,
           yvars,
           digits = 2,
           ungroupafter = TRUE,
           lang = c("eng", "ger")[1]) {

  Variable <- NAMESNAMES <- VALUESVALUES <- STATSTAT <- NULL # avoid package check warning

  data <- data %>% select(all_of(group_vars(data)), all_of(yvars))

  assertthat::assert_that("Variable" %not_in% names(data),
                          msg = "You must not have a column named 'Variable' in your data!")

  out <- data %>%
    summarise(across(
      all_of(yvars),
      .names = "{.col}__________{fn}",
      .fns = list(
        N = ~ sum(!is.na(.)),
        Miss = ~ sum(is.na(.)),
        Mean = ~ mean(., na.rm = TRUE),
        StdDev = ~ sd(., na.rm = TRUE),
        IQR = ~ quantile(., 0.75, na.rm = TRUE) - quantile(., 0.25, na.rm = TRUE),
        Min = ~ min(., na.rm = TRUE),
        Median = ~ median(., na.rm = TRUE),
        Max = ~ max(., na.rm = TRUE)
      )
    )) %>%
    suppressMessages() %>%
    tidyr::pivot_longer(cols = -all_of(group_vars(data)), names_to = "NAMESNAMES" , values_to = "VALUESVALUES") %>%
    tidyr::separate(NAMESNAMES, into = c("Variable", "STATSTAT"), sep = "__________") %>%
    tidyr::pivot_wider(names_from = STATSTAT, values_from = VALUESVALUES) %>%
    select(Variable, everything()) %>%
    arrange(Variable)

  if (!is.null(digits)) {
    out <- out %>%
      mutate(across(
        c("Mean", "StdDev", "IQR", "Min", "Median", "Max"),
        ~ round(., digits = digits)
      ))
  }

  if (ungroupafter) {
    out <- ungroup(out)
  }

  if (lang == "ger") {
    rename_ger <- c(
      "MW" = "Mean",
      "StdAbw" = "StdDev",
      "IQA" = "IQR",
      "Fehl" = "Miss"
    )

    out <- out %>%
      rename(any_of(rename_ger))
  }

  return(out)
}

