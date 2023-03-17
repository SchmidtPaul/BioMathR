#' @title Compute descriptive statistics
#'
#' @description This function computes common descriptive statistics of numeric variables for grouped or ungrouped data and converts it to a tidy formatted table. This function tried to copy `dlookr::describe()` because of \href{https://github.com/choonghyunryu/dlookr/issues/79}{this issue}.
#'
#' @param data a data.frame or a `\link{tbl_df}` or a `\link{grouped_df}`.
#' @param yvars a character vector with the names of the columns to be described.
#' @param ungroupafter If `TRUE`, the results will be `ungroup()`-ed.
#' @param lang Language for table names.
#'
#' @return A tibble
#' @examples
#' library(dplyr)
#' library(BioMathR)
#' PlantGrowth %>%
#'   group_by(group) %>%
#'   describe("weight")
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
describe <- function(data, yvars, ungroupafter = TRUE, lang = c("eng", "ger")[1]) {

  Variable <- NAMES_NAMES <- VALUES_VALUES <- NULL # avoid package check warning

  data <- data %>% select(all_of(group_vars(data)), all_of(yvars))

  out <- data %>%
    summarise(across(
      all_of(yvars),
      .names = "{.col}_{fn}",
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
    tidyr::pivot_longer(cols = -all_of(group_vars(data)), names_to = "NAMES_NAMES" , values_to = "VALUES_VALUES") %>%
    tidyr::separate(NAMES_NAMES, into = c("Variable", "Stat"), sep = "_") %>%
    tidyr::pivot_wider(names_from = Stat, values_from = VALUES_VALUES) %>%
    select(Variable, everything()) %>%
    arrange(Variable)

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

