#' @title Adjust column widths for flextables
#'
#' @param ft Flextable object
#' @param width Maximum width allowed for the table. Can be a numeric value (in cm) or one of the following strings referring to standard paper sizes \code{"A4"} (=default), \code{"letter"}, \code{"legal"} or \code{"executive"}.
#' @param landscape If \code{TRUE}, landscape width for standard paper size provided in \code{width} is used. Default is \code{FALSE}, i.e. portrait width. Ignored if \code{width} is provided as a numeric value.
#' @param page_margin Total page margin (sum of left and right margin) that is subtracted from the total page width of the standard paper width provided in \code{width}. Ignored if \code{width} is provided as a numeric value.
#' @param verbose If \code{TRUE}, prints detailed information about width calculations. Useful for debugging. Default is \code{FALSE}.
#'
#' @export
smart_fit <- function(ft,
                     width = c("A4", "letter", "legal", "executive")[1],
                     landscape = FALSE,
                     page_margin = "default",
                     verbose = FALSE) {

  min_body <- min_head <- ideal_width <- NULL # avoid package check warning

  # Helper function for verbose output
  vcat <- function(...) {
    if (verbose) cat("[smart_fit]", ..., "\n")
  }

  vcat("Starting smart_fit for flextable...")

  # Check if 'ft' is a flextable object
  if (!inherits(ft, "flextable")) {
    warning("The 'ft' object is not a flextable object. Converting it to flextable.")
    ft <- flextable::flextable(ft)
  }

  # get underlying data.frame
  df <- ft$body$dataset

  # Check for empty data frame
  if (nrow(df) == 0 || ncol(df) == 0) {
    warning("Flextable has no data (0 rows or 0 columns). Returning unchanged.")
    return(ft)
  }

  vcat("Data frame has", nrow(df), "rows and", ncol(df), "columns")

  # Determine page width
  if (is.numeric(width)) {
    page_width <- width
    vcat("Using numeric width:", page_width, "cm")
  } else {
    standard_widths <- data.frame(
      type = c("A4", "letter", "legal", "executive"),
      portrait = c(21, 8.5, 8.5, 7.25),
      landscape = c(29.7, 11, 14, 10.5),
      total_margin = c(4.8, 3, 3, 3),
      stringsAsFactors = FALSE
    )

    # Validate width parameter
    assertthat::assert_that(width %in% standard_widths$type,
                            msg = paste0("Invalid width type '", width, "'. Must be one of: ",
                                        paste(standard_widths$type, collapse = ", ")))

    matched_width <- standard_widths[standard_widths$type == width, ]
    margin <- ifelse(page_margin == "default", matched_width$total_margin, page_margin)
    page_width <- ifelse(landscape, matched_width$landscape, matched_width$portrait) - margin

    vcat("Using", width, ifelse(landscape, "landscape", "portrait"),
         "- Width:", ifelse(landscape, matched_width$landscape, matched_width$portrait),
         "cm, Margin:", margin, "cm, Available:", page_width, "cm")
  }

  vcat("\n--- Calculating width requirements ---")

  # Get width requirements from flextable
  wi <- data.frame(
    name = names(df),
    min_head = flextable::dim_pretty(ft, part = "header")$widths,
    min_body = flextable::dim_pretty(ft, part = "body")$widths,
    row.names = NULL
  )

  # Convert inches to cm
  wi <- wi %>% dplyr::mutate(dplyr::across(c(min_head, min_body), function(x) {x * 2.54}))

  # Add buffer like autofit() does (0.1 inches = 0.254 cm per column)
  # dim_pretty() returns absolute minimum, but rendering needs this buffer
  buffer_cm <- 0.254
  wi <- wi %>% dplyr::mutate(dplyr::across(c(min_head, min_body), function(x) {x + buffer_cm}))

  if (verbose) {
    vcat("\nColumn width requirements (in cm, with autofit buffer):")
    for (i in seq_len(nrow(wi))) {
      vcat(sprintf("  %-25s | header: %5.2f | body: %5.2f",
                   wi$name[i], wi$min_head[i], wi$min_body[i]))
    }
  }

  # Calculate ideal width (width needed to eliminate all line breaks)
  wi <- wi %>% dplyr::mutate(ideal_width = pmax(min_head, min_body))

  total_ideal <- sum(wi$ideal_width)

  vcat("\n--- Determining final column widths ---")
  vcat(sprintf("Ideal total width (no line breaks): %.2f cm", total_ideal))
  vcat(sprintf("Page width available: %.2f cm", page_width))

  # NEW SIMPLIFIED LOGIC
  if (total_ideal <= page_width) {
    # Case 1: Everything fits without breaks - use ideal widths
    vcat("\n✓ All content fits without line breaks!")
    vcat(sprintf("  Using ideal widths (table will be %.2f cm wide)", total_ideal))
    vcat(sprintf("  Unused space: %.2f cm", page_width - total_ideal))

    wi <- wi %>% dplyr::mutate(final_width = ideal_width)

  } else {
    # Case 2: Content too wide - need to scale down proportionally
    vcat("\n⚠ Content too wide to fit without line breaks")
    vcat(sprintf("  Need %.2f cm, but only %.2f cm available", total_ideal, page_width))

    # Special case: If we're close to page width (within 2.2 cm), just use full width
    if (total_ideal >= (page_width - 2.2)) {
      vcat("  Content is close to page width, using full page width")
      vcat("  Distributing line breaks proportionally across columns")
    } else {
      vcat("  Distributing line breaks proportionally across columns")
    }

    # Scale proportionally: each column gets (ideal_width / total_ideal) × page_width
    wi <- wi %>%
      dplyr::mutate(final_width = (ideal_width / total_ideal) * page_width)

    if (verbose) {
      vcat("\nProportional distribution:")
      for (i in seq_len(nrow(wi))) {
        pct <- (wi$ideal_width[i] / total_ideal) * 100
        vcat(sprintf("  %-25s | %.1f%% of total = %.2f cm",
                     wi$name[i], pct, wi$final_width[i]))
      }
    }
  }

  # Final validation
  vcat("\n--- Final validation ---")
  final_total <- sum(wi$final_width)
  vcat(sprintf("Final table width: %.2f cm (limit: %.2f cm)", final_total, page_width))

  if (final_total > page_width + 0.01) {  # Allow tiny rounding error
    # Safety check - should not happen with new logic, but just in case
    scale_factor <- page_width / final_total
    wi <- wi %>% dplyr::mutate(final_width = final_width * scale_factor)
    warning(sprintf("Table was %.2f cm too wide. Scaled all columns by %.1f%% to fit.",
                    final_total - page_width, scale_factor * 100))
    vcat("  SCALED DOWN by", sprintf("%.1f%%", scale_factor * 100), "to fit")
    final_total <- sum(wi$final_width)
  }

  if (final_total < page_width - 0.01) {
    unused <- page_width - final_total
    vcat(sprintf("  ✓ Table fits with %.2f cm unused (intentional - minimizes line breaks)", unused))
  } else {
    vcat("  ✓ Table uses full page width")
  }

  if (verbose) {
    vcat("\nFinal column widths:")
    for (i in seq_len(nrow(wi))) {
      vcat(sprintf("  %-25s | %.2f cm", wi$name[i], wi$final_width[i]))
    }
    vcat(sprintf("\nTotal: %.2f cm / %.2f cm available\n", final_total, page_width))
  }

  # Apply widths to flextable
  ft %>%
    flextable::width(width = wi$final_width, unit = "cm")
}
