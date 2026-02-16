#' @title Format tables for Quarto rendering via knitr::kable
#'
#' @description
#' Formats statistical tables (ANOVA, emmeans, etc.) for Quarto documents using
#' `knitr::kable()` with pipe format. Produces clean output compatible with
#' Quarto cross-references (`@tbl-*`) across HTML, PDF (Typst), and DOCX.
#'
#' Uses the same column unification, formatting, and renaming pipeline as
#' [docx_tab()], but outputs a `knitr_kable` object instead of a flextable.
#' Visual formatting (borders, font size, padding) is handled by the output
#' template (e.g., `reference.docx` Table style), not inline.
#'
#' @param x Table to be formatted (data.frame, tibble, anova, etc.)
#' @param lang Language for column names. Either `"eng"` (default) or `"ger"`.
#' @param pvalform Names of columns formatted via [format_p()]. Default is
#'   `"p.value"`. Set to `NULL` to skip p-value formatting.
#' @param digits Number of digits for rounding. Default `"round_smart"` applies
#'   [round_smart()] per column. Can be a numeric value for fixed rounding.
#' @param add_abbrev_footnote If `TRUE` (default), stores an abbreviation
#'   footnote as an attribute. Retrieve with [bm_footnote()].
#' @param align Column alignment. `NULL` (default) auto-detects: numeric
#'   columns right-aligned, others left-aligned. Can be a character vector
#'   (e.g., `c("l", "r", "r")`).
#' @param as_kable If `TRUE` (default), returns a `knitr_kable` object.
#'   If `FALSE`, returns the processed tibble.
#' @param verbose If `TRUE`, prints transformation details. Default `FALSE`.
#' @param ... Other arguments passed to [round_smart()].
#'
#' @return A `knitr_kable` object (pipe format) when `as_kable = TRUE`,
#'   or a tibble when `as_kable = FALSE`. The kable object may have a
#'   `"footnote"` attribute containing abbreviation explanations.
#'
#' @seealso [bm_footnote()] to print the footnote, [docx_tab()] for
#'   flextable-based output (non-Quarto workflows).
#'
#' @export
#'
#' @examples
#' library(BioMathR)
#'
#' # Basic usage with ANOVA table
#' anova <- anova(lm(weight ~ group, data = PlantGrowth))
#' bm_kable(anova)
#' bm_kable(anova, lang = "ger")
#'
#' # Return tibble instead of kable
#' bm_kable(anova, as_kable = FALSE)
#'
#' # Footnote retrieval
#' tab <- bm_kable(anova)
#' bm_footnote(tab)
#'
#' # Custom alignment
#' bm_kable(anova, align = c("l", "r", "r", "r", "r", "r"))
bm_kable <- function(x,
                     lang = c("eng", "ger")[1],
                     pvalform = "p.value",
                     digits = "round_smart",
                     add_abbrev_footnote = TRUE,
                     align = NULL,
                     as_kable = TRUE,
                     verbose = FALSE,
                     ...) {

  vcat <- function(...) if (verbose) cat("[bm_kable]", ..., "\n")

  vcat("Starting bm_kable...")

  # Input validation
  assertthat::assert_that(
    lang %in% c("eng", "ger"),
    msg = "lang must be either 'eng' or 'ger'"
  )
  assertthat::assert_that(
    is.numeric(digits) || identical(digits, "round_smart"),
    msg = "digits must be numeric or 'round_smart'"
  )

  # Reuse docx_tab helpers (available in package namespace)
  vcat("Preparing table...")
  tab <- prepare_table(x)

  vcat("Unifying column names...")
  tab <- unify_column_names(tab, verbose = verbose)

  vcat("Formatting columns...")
  tab <- format_columns(tab, pvalform, digits, lang, verbose = verbose, ...)

  vcat("Renaming columns...")
  tab <- rename_columns(tab, lang, verbose = verbose)
  vcat("Final columns:", paste(colnames(tab), collapse = ", "))

  if (!as_kable) {
    vcat("Returning tibble (as_kable = FALSE)")
    return(tab)
  }

  # Compute alignment before German decimal conversion
  align <- compute_alignment(tab, align)
  vcat("Alignment:", paste(align, collapse = ""))

  # German decimal format: convert numeric columns to character with comma
  if (lang == "ger") {
    vcat("Applying German decimal format (comma)")
    tab <- format_german_decimals(tab)
  }

  # Build footnote
  footnote_text <- NULL
  if (add_abbrev_footnote) {
    footnote_text <- build_abbreviation_footnote(tab, lang)
    if (!is.null(footnote_text)) {
      vcat("Footnote:", footnote_text)
    }
  }

  # Create kable
  vcat("Creating kable (pipe format)...")
  result <- knitr::kable(tab, format = "pipe", align = align)

  # Attach footnote as attribute
  if (!is.null(footnote_text)) {
    attr(result, "footnote") <- footnote_text
  }

  vcat("Done!")
  return(result)
}


#' Print abbreviation footnote from bm_kable output
#'
#' Retrieves the footnote stored by [bm_kable()] and outputs it as italic
#' text via [knitr::asis_output()]. Intended for use in a separate code chunk
#' below the table to preserve Quarto cross-references.
#'
#' @param x A `knitr_kable` object returned by [bm_kable()].
#'
#' @return A `knitr::asis_output()` with italic markdown text, or `NULL`
#'   invisibly if no footnote is present.
#'
#' @export
#'
#' @examples
#' library(BioMathR)
#' tab <- bm_kable(anova(lm(weight ~ group, data = PlantGrowth)))
#' bm_footnote(tab)
bm_footnote <- function(x) {
  fn <- attr(x, "footnote")
  if (is.null(fn) || nchar(fn) == 0) {
    return(invisible(NULL))
  }
  knitr::asis_output(paste0("*", fn, "*"))
}


# Internal helpers --------------------------------------------------------

#' Auto-detect column alignment
#' @noRd
compute_alignment <- function(tab, align = NULL) {
  if (!is.null(align)) return(align)
  vapply(tab, function(col) {
    if (is.numeric(col)) "r" else "l"
  }, character(1), USE.NAMES = FALSE)
}

#' Convert numeric columns to German decimal format (comma)
#' @noRd
format_german_decimals <- function(tab) {
  tab <- dplyr::mutate(tab, dplyr::across(
    tidyselect::where(is.numeric),
    ~ format(., decimal.mark = ",", big.mark = "")
  ))
  tab
}

#' Build abbreviation footnote string
#'
#' Same abbreviation dictionaries as add_abbreviation_footnotes() in docx_tab.R,
#' but returns a plain text string instead of flextable footnotes.
#' @noRd
build_abbreviation_footnote <- function(tab, lang) {
  abbreviations <- list(
    eng = list(
      "df" = "degrees of freedom",
      "MS" = "mean squares",
      "SS" = "sum of squares",
      "NumDF" = "numerator degrees of freedom",
      "DenDF" = "denominator degrees of freedom",
      "Resid. df" = "residual degrees of freedom",
      "Std. Error" = "standard error",
      "Lower CL" = "lower confidence limit",
      "Upper CL" = "upper confidence limit"
    ),
    ger = list(
      "FG" = "Freiheitsgrade",
      "MQ" = "Mittelquadrate",
      "SQ" = "Summe der Quadrate",
      "Z\u00E4hler-FG" = "Z\u00E4hler-Freiheitsgrade",
      "Nenner-FG" = "Nenner-Freiheitsgrade",
      "Residual-FG" = "Residual-Freiheitsgrade",
      "Statistik" = "Teststatistik",
      "Sch\u00E4tzwert" = "gesch\u00E4tzter Wert",
      "Standardfehler" = "Standardfehler",
      "Untere KG" = "untere Konfidenzgrenze",
      "Obere KG" = "obere Konfidenzgrenze",
      "Devianz" = "Devianz"
    )
  )

  abbrev_dict <- abbreviations[[lang]]
  col_names <- colnames(tab)
  present <- intersect(col_names, names(abbrev_dict))

  if (length(present) == 0) return(NULL)

  # Build "abbr = full name; abbr2 = full name2" string
  parts <- vapply(present, function(nm) {
    paste0(nm, " = ", abbrev_dict[[nm]])
  }, character(1))

  paste(parts, collapse = "; ")
}
