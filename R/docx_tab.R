#' @title Auto-format tables for printing in MS-Word documents
#'
#' @param x Table to be formatted
#' @param lang Language for column names. Either \code{"eng"} (default) or \code{"ger"}.
#' @param pvalform Names of columns that are formatted via \code{BioMathR::format_p()}. Can be set to \code{NULL}. The default is \code{"p.value"}, but note that this function first unifies multiple column names such as \code{"Pr(>F)"} or \code{"P(>|Chi|)"} into \code{"p.value"}.
#' @param asft If \code{TRUE} (default), output is formatted as flextable. If \code{FALSE}, returns a tibble with formatted values.
#' @param digits Number of digits all numeric columns are rounded to. The default is \code{"round_smart"} which applies \code{BioMathR::round_smart()} to each numeric column individually. Can also be a numeric value.
#' @param fit_mode How to fit the flextable to page width. Options:
#' \itemize{
#'   \item \code{"smart"} (default): Apply \code{smart_fit()} to optimize column widths
#'   \item \code{"page"}: Stretch to full page width while maintaining smart_fit proportions
#'   \item \code{"none"}: Use flextable's default autofit, no additional fitting
#' }
#' @param page_width Page width for \code{fit_mode}. Can be \code{"A4"} (default), \code{"letter"}, \code{"legal"}, \code{"executive"}, or a numeric value in cm. Ignored if \code{fit_mode = "none"}.
#' @param landscape Logical. If \code{TRUE}, use landscape orientation for standard page sizes. Default is \code{FALSE}. Ignored if \code{page_width} is numeric or \code{fit_mode = "none"}.
#' @param verbose If \code{TRUE}, prints detailed information about transformations. Useful for debugging. Default is \code{FALSE}.
#' @param ft_padding Padding for flextable cells (in points). Default is 2.
#' @param ft_fontsize Font size for flextable (in points). Default is 9.
#' @param ft_border_width Border width for flextable (in points). Default is 0.1.
#' @param ... Other arguments passed to \code{BioMathR::round_smart()}
#'
#' @export
#'
#' @examples
#' library(BioMathR)
#'
#' # Basic usage with ANOVA table
#' anova <- anova(lm(weight ~ group, data = PlantGrowth))
#' docx_tab(anova, lang = "eng")
#' docx_tab(anova, lang = "ger")
#'
#' # Return tibble instead of flextable
#' docx_tab(anova, asft = FALSE)
#'
#' # Different rounding options
#' before <- data.frame(
#'   V1 = c(123456, 1234),
#'   V2 = c(-123, -0.12345),
#'   V3 = c(1.0012345, 0.1),
#'   V4 = c(1.1, 0.0012345),
#'   V5 = c(1.000000012345, 0),
#'   V6 = c(NA, -5.0018),
#'   V7 = c(NA_real_, NA_real_)
#' )
#' docx_tab(before)  # smart rounding
#' docx_tab(before, digits = 2)  # fixed 2 digits
#'
#' # Different fit modes
#' docx_tab(anova, fit_mode = "smart")  # default: optimized widths
#' docx_tab(anova, fit_mode = "page")   # full page width
#' docx_tab(anova, fit_mode = "none")   # no fitting
#'
#' # Customize flextable appearance
#' docx_tab(anova, ft_fontsize = 11, ft_padding = 3)
#'
#' # Verbose output to see transformations
#' docx_tab(anova, verbose = TRUE)
#'
#' # Fit to different page sizes
#' docx_tab(anova, page_width = "letter", landscape = TRUE)
docx_tab <- function(x,
                     lang = c("eng", "ger")[1],
                     pvalform = "p.value",
                     asft = TRUE,
                     digits = "round_smart",
                     fit_mode = c("smart", "page", "none")[1],
                     page_width = "A4",
                     landscape = FALSE,
                     verbose = FALSE,
                     ft_padding = 2,
                     ft_fontsize = 9,
                     ft_border_width = 0.1,
                     ...) {

  # Helper function for verbose output
  vcat <- function(...) {
    if (verbose) cat("[docx_tab]", ..., "\n")
  }

  vcat("Starting docx_tab...")
  vcat("Input dimensions:", nrow(x), "rows,", ncol(x), "columns")

  # Input validation --------------------------------------------------------
  assertthat::assert_that(
    lang %in% c("eng", "ger"),
    msg = "lang must be either 'eng' or 'ger'"
  )

  assertthat::assert_that(
    is.numeric(digits) || identical(digits, "round_smart"),
    msg = "digits must be numeric or 'round_smart'"
  )

  assertthat::assert_that(
    fit_mode %in% c("smart", "page", "none"),
    msg = "fit_mode must be 'smart', 'page', or 'none'"
  )

  vcat("Parameters: lang =", lang, "| digits =", digits, "| fit_mode =", fit_mode)

  # Main workflow -----------------------------------------------------------
  vcat("\n--- Table preparation ---")
  tab <- prepare_table(x)
  vcat("Converted to tibble")

  vcat("\n--- Column name unification ---")
  original_names <- colnames(tab)
  tab <- unify_column_names(tab, verbose = verbose)
  if (verbose && !identical(original_names, colnames(tab))) {
    vcat("Unified names:", paste(colnames(tab), collapse = ", "))
  }

  vcat("\n--- Column formatting ---")
  tab <- format_columns(tab, pvalform, digits, lang, verbose = verbose, ...)

  vcat("\n--- Column renaming ---")
  tab <- rename_columns(tab, lang, verbose = verbose)
  vcat("Final column names:", paste(colnames(tab), collapse = ", "))

  # Flextable ---------------------------------------------------------------
  if (!asft) {
    vcat("\nReturning tibble (asft = FALSE)")
    return(tab)
  }

  vcat("\n--- Creating flextable ---")
  ftab <- create_flextable(tab, lang, ft_padding, ft_fontsize, ft_border_width, verbose = verbose)

  # Apply fit mode
  if (fit_mode != "none") {
    vcat("\n--- Applying fit_mode:", fit_mode, "---")
    ftab <- apply_fit_mode(ftab, fit_mode, page_width, landscape, verbose = verbose)
  } else {
    vcat("\nfit_mode = 'none', skipping column width fitting")
  }

  vcat("\nDocx_tab complete!\n")
  return(ftab)
}

# Helper functions --------------------------------------------------------

#' Prepare table for formatting
#' @noRd
prepare_table <- function(x) {
  # convert x into tibble
  tab <- tibble::as_tibble(x)
  if (tibble::has_rownames(x)) {
    tab <- mutate(tab,
                  .rownames = rownames(x),
                  .before = everything())
  }
  colnames(tab) <- make.names(names(tab))
  return(tab)
}

#' Unify column names to standard format
#' @noRd
unify_column_names <- function(tab, verbose = FALSE) {
  vcat <- function(...) if (verbose) cat("[docx_tab]", ..., "\n")
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
  if (all(c("Df", "Df.res") %in% colnames(tab))) {
    colnames(tab)[colnames(tab) == "Df"] <- "NumDF"
    colnames(tab)[colnames(tab) == "Df.res"] <- "DenDF"
  }

  names(unifynames) <- make.names(names(unifynames))

  # unify column names
  colnames(tab) <- dplyr::recode(colnames(tab), !!!unifynames)

  # handle duplicates
  if (anyDuplicated(colnames(tab))) {
    dups <- duplicated(colnames(tab))
    colnames(tab)[dups] <- paste0(colnames(tab)[dups], ".", seq_len(sum(dups)))
  }

  return(tab)
}

#' Format column contents (p-values and rounding)
#' @noRd
format_columns <- function(tab, pvalform, digits, lang, verbose = FALSE, ...) {
  vcat <- function(...) if (verbose) cat("[docx_tab]", ..., "\n")

  # format p-values
  if (!is.null(pvalform)) {
    if (length(pvalform) == 1 && all(pvalform == "p.value")) {
      vcat("Formatting p-values: columns starting with", pvalform)
      tab <- tab %>% mutate(across(
        starts_with(pvalform),
        ~ BioMathR::format_p(., lang = lang)))
    } else {
      vcat("Formatting p-values in columns:", paste(pvalform, collapse = ", "))
      tab <- tab %>% mutate(across(
        any_of(pvalform),
        ~ BioMathR::format_p(., lang = lang)))
    }
  } else {
    vcat("Skipping p-value formatting (pvalform = NULL)")
  }

  # round all remaining numeric cols
  if (is.numeric(digits)) {
    vcat("Rounding numeric columns to", digits, "digits")
    tab <- tab %>%
      mutate(across(where(is.numeric), ~ round(., digits)))
  } else if (identical(digits, "round_smart")) {
    vcat("Applying round_smart() to numeric columns")
    tab <- tab %>%
      mutate(across(where(is.numeric), ~ BioMathR::round_smart(., ...)))
  }

  return(tab)
}

#' Rename columns according to language
#' @noRd
rename_columns <- function(tab, lang, verbose = FALSE) {
  vcat <- function(...) if (verbose) cat("[docx_tab]", ..., "\n")

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

  vcat("Translating column names to", lang)
  colnames(tab) <- dplyr::recode(colnames(tab), !!!renamers[[lang]])
  return(tab)
}

#' Create and format flextable
#' @noRd
create_flextable <- function(tab, lang, ft_padding = 2, ft_fontsize = 9, ft_border_width = 0.1, verbose = FALSE) {
  vcat <- function(...) if (verbose) cat("[docx_tab]", ..., "\n")

  assertthat::assert_that(
    requireNamespace("flextable", quietly = TRUE),
    msg = "When asft = TRUE, package 'flextable' must be installed."
  )

  vcat("Creating flextable with", nrow(tab), "rows,", ncol(tab), "columns")
  vcat("Formatting: padding =", ft_padding, "| fontsize =", ft_fontsize, "| border =", ft_border_width)

  if (lang == "ger") {
    vcat("Setting German number format (decimal.mark = ',', big.mark = '.')")
    flextable::set_flextable_defaults(
      decimal.mark = ",",
      big.mark = "."
    )
  }

  # Suppress only the specific warning about decimal.mark and big.mark being the same
  ftab <- suppressWarnings({
    flextable::flextable(tab) %>%
      flextable::theme_booktabs() %>%
      flextable::padding(padding = ft_padding, part = "all") %>%
      flextable::fontsize(size = ft_fontsize, part = "all") %>%
      flextable::bold(bold = TRUE, part = "header") %>%
      flextable::hline_top(border = officer::fp_border(width = ft_border_width), part = "all") %>%
      flextable::hline_bottom(border = officer::fp_border(width = ft_border_width), part = "all")
  })

  # Column widths with autofit
  vcat("Applying autofit for initial column widths")
  ftab <- ftab %>% flextable::autofit()

  flextable::init_flextable_defaults() # undo changes to default

  return(ftab)
}

#' Apply fit mode to flextable
#' @noRd
apply_fit_mode <- function(ftab, fit_mode, page_width, landscape, verbose = FALSE) {
  vcat <- function(...) if (verbose) cat("[docx_tab]", ..., "\n")

  if (fit_mode == "smart") {
    vcat("Applying smart_fit() with page_width =", page_width, ", landscape =", landscape)
    ftab <- BioMathR::smart_fit(ftab, width = page_width, landscape = landscape, verbose = verbose)

  } else if (fit_mode == "page") {
    vcat("Applying smart_fit() then stretching to full page width")

    # First apply smart_fit to get optimal proportions
    ftab_smart <- BioMathR::smart_fit(ftab, width = page_width, landscape = landscape, verbose = verbose)

    # Extract the proportions and scale to full page width
    # Get smart_fit widths
    smart_widths <- ftab_smart$col_keys
    current_widths <- flextable::dim(ftab_smart)$widths

    # Calculate available page width in inches (smart_fit returns inches)
    if (is.numeric(page_width)) {
      available_width <- page_width / 2.54  # convert cm to inches
    } else {
      standard_widths <- data.frame(
        type = c("A4", "letter", "legal", "executive"),
        portrait = c(21, 8.5, 8.5, 7.25),
        landscape = c(29.7, 11, 14, 10.5),
        total_margin = c(4.8, 3, 3, 3),
        stringsAsFactors = FALSE
      )
      matched_width <- standard_widths[standard_widths$type == page_width, ]
      margin <- matched_width$total_margin
      page_width_cm <- ifelse(landscape, matched_width$landscape, matched_width$portrait) - margin
      available_width <- page_width_cm / 2.54  # convert cm to inches
    }

    # Scale proportionally to fill page width
    current_total <- sum(current_widths)
    scale_factor <- available_width / current_total

    vcat(sprintf("Scaling from %.2f inches to %.2f inches (scale factor: %.2f)",
                 current_total, available_width, scale_factor))

    new_widths <- current_widths * scale_factor
    ftab <- ftab_smart %>% flextable::width(width = new_widths)

  }

  return(ftab)
}
