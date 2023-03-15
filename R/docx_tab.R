#' @title Auto-format tables for printing in MS-Word documents
#'
#' @param x table to be formatted
#' @param lang language for the result table column names
#' @param digits number of digits all numeric columns are rounded to
#' @param autopvalform if `TRUE`, p-value columns are formatted via `BioMathR::format_p()`
#' @param asft If `TRUE`, output is formatted as flextable
#'
#' @export
docx_tab <- function(x,
                     lang = c("eng", "ger")[1],
                     digits = 1,
                     autopvalform = TRUE,
                     asft = TRUE) {
  unifynames <- c(
    "Df" = "df",
    "Chi.Df" = "df",
    "Mean Sq" = "meansq",
    "Sum Sq" = "sumsq",
    "Sum of Sq" = "sumsq",
    "F value" = "F.value",
    "F" = "F.value",
    "Chisq" = "statistic",
    "Chi.sq" = "statistic",
    "LR.Chisq" = "statistic",
    "LR Chisq" = "statistic",
    "Pr(>F)" = "p.value",
    "P(>|Chi|)" = "p.value",
    "Pr(>|Chi|)" = "p.value",
    "Pr(>Chi)" = "p.value",
    "Pr..Chisq." = "p.value",
    "Pr..Chi." = "p.value",
    ".rownames" = "term"
  )

  names(unifynames) <- make.names(names(unifynames))

  renamers <- list(
    eng = c(
      "F.value" = "F value",
      "meansq" = "Mean Sq",
      "p.value" = "p value",
      "sumsq" = "Sum Sq",
      "term" = "Term"
    ),
    ger = c(
      "df" = "FG",
      "DenDF" = "Nenner-FG",
      "F.value" = "F-Wert",
      "meansq" = "MQ",
      "NumDF" = "Z\u00E4hler-FG",
      "p.value" = "p-Wert",
      "statistic" = "Statistik",
      "sumsq" = "SQ",
      "term" = "Term"
    )
  )

  # convert x into tibble
  tab <- tibble::as_tibble(x)
  if (has_rownames(x)) {
    tab <- mutate(tab,
                  .rownames = rownames(x),
                  .before = everything())
  }
  colnames(tab) <- make.names(names(tab))

  # unify column names
  colnames(tab) <- dplyr::recode(colnames(tab),!!!unifynames)

  # format p-values
  if (autopvalform) {
    tab <- tab %>%
      mutate(across(any_of("p.value"), ~ BioMathR::format_p(., lang = lang)))
  }

  # round all numeric cols
  if (!is.null(digits)) {
    tab <- tab %>%
      mutate(across(where(is.numeric), ~ round(., digits)))
  }

  # rename columns according to language
  colnames(tab) <- dplyr::recode(colnames(tab),!!!renamers[[lang]])

  # make flextable format
  if (asft) {
    assertthat::assert_that(requireNamespace("flextable", quietly = TRUE),
                            msg = "When makeft = TRUE, package 'flextable' must be installed.")

    tab <- flextable::flextable(tab) %>%
      flextable::theme_booktabs() %>%
      flextable::padding(padding = 2, part = "all") %>%
      flextable::fontsize(size = 9, part = "all") %>%
      flextable::colformat_num(na_str = "-") %>%
      flextable::bold(bold = TRUE, part = "header") %>%
      flextable::hline_top(border = officer::fp_border(width = 0.1), part = "all") %>%
      flextable::hline_bottom(border = officer::fp_border(width = 0.1), part = "all") %>%
      flextable::autofit()
  }

  tab
}
