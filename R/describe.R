#' @title Compute descriptive statistics
#'
#' @description This function computes common descriptive statistics of numeric variables for grouped or ungrouped data and converts it to a tidy formatted table. This function tried to copy \code{dlookr::describe()} because of \href{https://github.com/choonghyunryu/dlookr/issues/79}{this issue}.
#'
#' @param data A data.frame or a \code{\link{tbl_df}} or a \code{\link{grouped_df}}.
#' @param yvars A character vector with the names of the columns to be described.
#' @param lang Language for table names.
#' @param ungroupafter If \code{TRUE}, the results will be \code{ungroup()}-ed.
#' @param digits Number of digits all numeric columns are rounded to. The default is actually \code{"round_smart"} which applies \code{BioMathR::round_smart()} to each numeric column individually.
#' @param addstats Character vector with additional statistics to be calculated besides the default. Possible entries: c("Skewness", "Kurtosis", "Mode").
#' @param ... Other arguments passed to \code{BioMathR::round_smart()}
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
#'   describe(c("weight", "weight2", "weight3"), lang = "ger") %>%
#'   docx_tab()
#' @export
describe <-
  function(data,
           yvars,
           lang = c("eng", "ger")[1],
           ungroupafter = TRUE,
           digits = "round_smart",
           addstats = character(),
           ...) {

  Variable <- temp_names <- temp_values <- temp_stat <- NULL # avoid package check warning

  data <- data %>% select(all_of(group_vars(data)), all_of(yvars))

  assertthat::assert_that("Variable" %not_in% names(data),
                          msg = "You must not have a column named 'Variable' in your data!")

  assertthat::assert_that(all(addstats %in% c("Skewness", "Kurtosis", "Mode")),
                          msg = "Unknown statistic in 'addstats'. Known statistics are: 'Skewness', 'Kurtosis', 'Mode'")

  # Helper function for mode (most frequent value)
  mode_fn <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    ux <- unique(x)
    tab <- tabulate(match(x, ux))
    ux[which.max(tab)]
  }

  Skewness_fn <- if ("Skewness" %in% addstats) list(Skewness = ~ e1071::skewness(., na.rm = TRUE)) else list()
  Kurtosis_fn <- if ("Kurtosis" %in% addstats) list(Kurtosis = ~ e1071::kurtosis(., na.rm = TRUE)) else list()
  Mode_fn <- if ("Mode" %in% addstats) list(Mode = ~ mode_fn(.)) else list()

  out <- data %>%
    summarise(across(
      all_of(yvars),
      .names = "{.col}__________{fn}",
      .fns = c(
        list(
          N = ~ sum(!is.na(.)),
          Miss = ~ sum(is.na(.)),
          Mean = ~ mean(., na.rm = TRUE),
          StdDev = ~ sd(., na.rm = TRUE),
          IQR = ~ quantile(., 0.75, na.rm = TRUE) - quantile(., 0.25, na.rm = TRUE),
          Min = ~ min(., na.rm = TRUE),
          Median = ~ median(., na.rm = TRUE),
          Max = ~ max(., na.rm = TRUE)
        ),
      Skewness_fn,
      Kurtosis_fn,
      Mode_fn)
    )) %>%
    suppressMessages() %>%
    tidyr::pivot_longer(cols = -all_of(group_vars(data)), names_to = "temp_names" , values_to = "temp_values") %>%
    tidyr::separate(temp_names, into = c("Variable", "temp_stat"), sep = "__________") %>%
    tidyr::pivot_wider(names_from = temp_stat, values_from = temp_values) %>%
    select(Variable, everything()) %>%
    arrange(Variable)

  # round
  cols_to_round <- c("Mean", "StdDev", "IQR", "Min", "Median", "Max", addstats)

  if (is.numeric(digits)) {
    out <- out %>%
      mutate(across(
        all_of(cols_to_round),
        ~ round(., digits = digits)
      ))
  }
  if (digits == "round_smart") {
    out <- out %>%
      mutate(across(
        all_of(cols_to_round),
        ~ BioMathR::round_smart(., ...)
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
      "Fehl" = "Miss",
      "Schiefe" = "Skewness",
      "Kurtosis" = "Kurtosis",  # Kurtosis is the same in German
      "Modus" = "Mode"
    )

    out <- out %>%
      rename(any_of(rename_ger))
  }

  return(out)
}

