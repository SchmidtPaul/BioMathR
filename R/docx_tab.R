#' @title Auto-format tables for printing in MS-Word documents
#'
#' @param x Table to be formatted
#' @param lang Language for column names.
#' @param pvalform Names of columns are that formatted via \code{BioMathR::format_p()}. Can be set to \code{NULL}. The default is \code{"p.value"}, but note that this function first unifies multiple column names such as \code{"Pr(>F)"} or \code{"P(>|Chi|)"} into \code{"p.value"}.
#' @param asft If \code{TRUE}, output is formatted as flextable
#' @param digits Number of digits all numeric columns are rounded to. The default is actually \code{"round_smart"} which applies \code{BioMathR::round_smart()} to each numeric column individually.
#' @param ... Other arguments passed to \code{BioMathR::round_smart()}
#'
#' @export
#'
#' @examples
#' library(BioMathR)
#'
#' anova <- anova(lm(weight ~ group, data = PlantGrowth))
#' docx_tab(anova, lang = "ger")
#' docx_tab(anova, lang = "eng", asft = FALSE)
#'
#' before <- data.frame(
#'   V1 = c(123456, 1234),
#'   V2 = c(-123, -0.12345),
#'   V3 = c(1.0012345, 0.1),
#'   V4 = c(1.1, 0.0012345),
#'   V5 = c(1.000000012345, 0),
#'   V6 = c(NA, -5.0018),
#'   V7 = c(NA_real_, NA_real_)
#' )
#' docx_tab(before)
#' docx_tab(before, digits = 2)
docx_tab <- function(x,
                     lang = c("eng", "ger")[1],
                     pvalform = "p.value",
                     asft = TRUE,
                     digits = "round_smart",
                     ...) {

  # format column names -----------------------------------------------------
  unifynames <- c(
    "Df" = "df",
    "Chi.Df" = "df",
    "Df.res" = "DenDF",
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

  # special case for car::Anova()
  if (all(c("Df", "Df.res") %in% colnames(x))) {
    colnames(x)[colnames(x) == "Df"] <- "NumDF"
    colnames(x)[colnames(x) == "Df.res"] <- "DenDF"
  }

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
  if (tibble::has_rownames(x)) {
    tab <- mutate(tab,
                  .rownames = rownames(x),
                  .before = everything())
  }
  colnames(tab) <- make.names(names(tab))

  # unify column names
  colnames(tab) <- dplyr::recode(colnames(tab),!!!unifynames)

  if (anyDuplicated(colnames(tab))) {
    dups <- duplicated(colnames(tab))
    colnames(tab)[dups] <- paste0(colnames(tab)[dups], ".", seq_along(dups)[dups])
  }

  # format column contents --------------------------------------------------
  # format p-values
  if (!is.null(pvalform)) {
    if (length(pvalform) == 1 & all(pvalform == "p.value")) {
      tab <- tab %>% mutate(across(
        starts_with(pvalform),
        ~ BioMathR::format_p(., lang = lang)))
    } else {
      tab <- tab %>% mutate(across(
        any_of(pvalform),
        ~ BioMathR::format_p(., lang = lang)))
    }
  }

  # round all remaining numeric cols
  if (is.numeric(digits)) {
    tab <- tab %>%
      mutate(across(where(is.numeric), ~ round(., digits)))
  }
  if (digits == "round_smart") {
    tab <- tab %>%
      mutate(across(where(is.numeric), ~ BioMathR::round_smart(., ...)))
  }

  # rename columns according to language
  colnames(tab) <- dplyr::recode(colnames(tab),!!!renamers[[lang]])


  # Flextable ---------------------------------------------------------------
  if (!asft) {
    return(tab)
  }

  assertthat::assert_that(requireNamespace("flextable", quietly = TRUE),
                          msg = "When makeft = TRUE, package 'flextable' must be installed.")

  if (lang == "ger") {
    flextable::set_flextable_defaults(decimal.mark = ",",
                                      big.mark = ".")
  }

  ftab <- flextable::flextable(tab) %>%
    flextable::theme_booktabs() %>%
    flextable::padding(padding = 2, part = "all") %>%
    flextable::fontsize(size = 9, part = "all") %>%
    flextable::bold(bold = TRUE, part = "header") %>%
    flextable::hline_top(border = officer::fp_border(width = 0.1), part = "all") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 0.1), part = "all") %>%
    suppressWarnings() # this is to avoid: "#> Warning: 'big.mark' and 'decimal.mark' are both '.', which could be confusing"

  # Column widths
  ftab <- ftab %>% flextable::autofit()

  flextable::init_flextable_defaults() # undo changes to default

  return(ftab)
}
