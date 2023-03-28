#' @title Auto-format tables for printing in MS-Word documents
#'
#' @param x Table to be formatted
#' @param lang Language for column names.
#' @param digits Number of digits all numeric columns are rounded to
#' @param pvalform Names of columns are that formatted via \code{BioMathR::format_p()}. Can be set to \code{NULL}. The default is \code{"p.value"}, but note that this function first unifies multiple column names such as \code{"Pr(>F)"} or \code{"P(>|Chi|)"} into \code{"p.value"}.
#' @param asft If \code{TRUE}, output is formatted as flextable
#'
#' @export
docx_tab <- function(x,
                     lang = c("eng", "ger")[1],
                     digits = 1,
                     pvalform = "p.value",
                     asft = TRUE) {

  # format column names -----------------------------------------------------
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
      "meansq" = "MS",
      "p.value" = "p value",
      "sumsq" = "SS",
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


  # format column contents --------------------------------------------------
  # format p-values
  if (!is.null(pvalform)) {
    tab <- tab %>%
      mutate(across(any_of(pvalform), ~ BioMathR::format_p(., lang = lang)))
  }

  # round all numeric cols
  if (!is.null(digits)) {
    tab <- tab %>%
      mutate(across(where(is.numeric), ~ round(., digits)))
  }

  # rename columns according to language
  colnames(tab) <- dplyr::recode(colnames(tab),!!!renamers[[lang]])


  # Flextable ---------------------------------------------------------------
  if (asft) {
    assertthat::assert_that(requireNamespace("flextable", quietly = TRUE),
                            msg = "When makeft = TRUE, package 'flextable' must be installed.")

    flextable::set_flextable_defaults(digits = digits) # change defaults temporarily

    if (lang == "ger") {
      flextable::set_flextable_defaults(decimal.mark = ",",
                                        big.mark = ".")
    }

    tab <- flextable::flextable(tab) %>%
      flextable::theme_booktabs() %>%
      flextable::padding(padding = 2, part = "all") %>%
      flextable::fontsize(size = 9, part = "all") %>%
      flextable::bold(bold = TRUE, part = "header") %>%
      flextable::hline_top(border = officer::fp_border(width = 0.1), part = "all") %>%
      flextable::hline_bottom(border = officer::fp_border(width = 0.1), part = "all")

    tab <- tab %>% flextable::autofit()

    flextable::init_flextable_defaults() # undo changes to default
  }

  tab
}
