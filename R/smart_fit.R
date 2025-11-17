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

  make_wider <- min_body <- min_body_lwrd <- min_head <- min_head_lwrd <- NULL # avoid package check warning

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

  # Determine final_width of table
  if (is.numeric(width)) {
    final_width <- width
    vcat("Using numeric width:", final_width, "cm")
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
    final_width <- ifelse(landscape, matched_width$landscape, matched_width$portrait) - margin

    vcat("Using", width, ifelse(landscape, "landscape", "portrait"),
         "- Width:", ifelse(landscape, matched_width$landscape, matched_width$portrait),
         "cm, Margin:", margin, "cm, Available:", final_width, "cm")
  }

  # Set total_width as the maximum allowed width
  total_width <- final_width

  vcat("\n--- Calculating minimum widths ---")

  # get widths information
  wi <- data.frame(
    name = names(df),
    min_head = flextable::dim_pretty(ft, part = "header")$widths,
    min_body = flextable::dim_pretty(ft, part = "body")$widths,
    min_head_lwrd = sapply(names(df), function(col_name) {
      if (nchar(col_name) == 0) return(0)  # Handle empty column names
      words <- unlist(strsplit(col_name, " "))
      if (length(words) == 0) return(0)
      word_fts <- lapply(words, function(word) {
        if (nchar(word) == 0) return(0)
        word_ft <- flextable::flextable(data.frame(value = word, check.names = FALSE))
        flextable::dim_pretty(word_ft, part = "body")$widths[1]
      })
      max(unlist(word_fts))
    }),
    min_body_lwrd = sapply(names(df), function(col_name) {
      words <- unlist(strsplit(as.character(df[[col_name]]), " "))
      # Remove empty strings and NAs
      words <- words[nchar(words) > 0 & !is.na(words)]
      if (length(words) == 0) return(0)

      # FIX: Sort by character length (descending), not alphabetically
      words_by_length <- words[order(nchar(words), decreasing = TRUE)]
      # Take top 10 longest words to measure (for performance)
      top_words <- utils::head(words_by_length, min(10, length(words_by_length)))

      # Measure actual widths
      word_widths <- sapply(top_words, function(word) {
        if (nchar(word) == 0) return(0)
        word_ft <- flextable::flextable(data.frame(value = word, check.names = FALSE))
        flextable::dim_pretty(word_ft, part = "body")$widths[1]
      })
      max(word_widths)
    }),
    row.names = NULL
  )

  # inch -> cm
  wi <- wi %>% dplyr::mutate(dplyr::across(contains("min_"), function(x) {x * 2.54}))

  if (verbose) {
    vcat("\nColumn width requirements (in cm):")
    for (i in seq_len(nrow(wi))) {
      vcat(sprintf("  %-20s | head: %5.2f | body: %5.2f | head_word: %5.2f | body_word: %5.2f",
                   wi$name[i], wi$min_head[i], wi$min_body[i],
                   wi$min_head_lwrd[i], wi$min_body_lwrd[i]))
    }
  }

  # function to check if flextable has reached maximum width
  toowide <- function(wi){sum(wi$final_width) > total_width}

  # default final_width: longest word
  wi <- dplyr::mutate(wi, final_width = pmax(min_head_lwrd, min_body_lwrd))

  vcat("\n--- Step 1: Starting with longest word widths ---")
  vcat("Total width with longest words:", sprintf("%.2f", sum(wi$final_width)), "cm (limit:", total_width, "cm)")

  if (toowide(wi)) {
    ft <- flextable::autofit(ft)
    warning(sprintf("Even default width of longest word per column is already too wide! (%.2f cm > %.2f cm)\n 'flextable::autofit(ft)' is applied",
                    sum(wi$final_width), total_width))
    return(ft)
  }

  # Allow column names that are only slightly longer than final_width
  vcat("\n--- Step 2: Checking if we can fit full column names (if close) ---")
  wi2 <- wi %>% dplyr::mutate(
    final_width = dplyr::case_when(
      min_head > 0 & 0.8 < final_width / min_head & final_width / min_head < 1 ~ min_head,
      TRUE ~ final_width
    )
  )

  if (!toowide(wi2)) {
    n_changed <- sum(wi2$final_width != wi$final_width)
    if (n_changed > 0) vcat("  Expanded", n_changed, "columns to fit full header names")
    wi <- wi2
    vcat("  New total:", sprintf("%.2f", sum(wi$final_width)), "cm")
  } else {
    vcat("  Would exceed limit, keeping previous widths")
  }

  # Allow cell contents that are only slightly longer than final_width
  vcat("\n--- Step 3: Checking if we can fit full cell contents (if close) ---")
  wi2 <- wi %>% dplyr::mutate(
    final_width = dplyr::case_when(
      min_body > 0 & 0.8 < final_width / min_body & final_width / min_body < 1 ~ min_body,
      TRUE ~ final_width
    )
  )

  if (!toowide(wi2)) {
    n_changed <- sum(wi2$final_width != wi$final_width)
    if (n_changed > 0) vcat("  Expanded", n_changed, "columns to fit full body content")
    wi <- wi2
    vcat("  New total:", sprintf("%.2f", sum(wi$final_width)), "cm")
  } else {
    vcat("  Would exceed limit, keeping previous widths")
  }

  # Allow extra width for all columns that could still be wider because of their content
  vcat("\n--- Step 4: Distributing remaining width to columns that need it ---")
  wi2 <- wi %>% dplyr::mutate(make_wider = min_body > final_width) # which columns want to be wider

  if (any(wi2$make_wider)) {
    vcat("  Columns wanting more width:", sum(wi2$make_wider))
    remaining_width <- total_width - sum(wi2$final_width) # how much width is left
    vcat("  Remaining width available:", sprintf("%.2f", remaining_width), "cm")

    if (remaining_width > 0) {
      # Proportional distribution
      p_remain <- wi2[wi2$make_wider, "min_body"] / sum(wi2[wi2$make_wider, "min_body"])
      wi2[wi2$make_wider, "final_width"] <- wi2[wi2$make_wider, "final_width"] + p_remain * remaining_width

      # Cap at actual needed width
      wi2 <- wi2 %>%
        dplyr::mutate(final_width = dplyr::if_else(
          make_wider & final_width > pmax(min_head, min_body, min_head_lwrd, min_body_lwrd),
          pmax(min_head, min_body, min_head_lwrd, min_body_lwrd),
          final_width
        ))

      if (!toowide(wi2)) {
        vcat("  Distributed extra width successfully")
        wi <- wi2
        vcat("  New total:", sprintf("%.2f", sum(wi$final_width)), "cm")
      } else {
        vcat("  Distribution would exceed limit, keeping previous widths")
      }
    } else {
      vcat("  No remaining width to distribute")
    }
  } else {
    vcat("  No columns need more width")
  }

  # If there is still remaining width, remove line breaks for columns that are relatively narrow (e.g. "A 2")
  vcat("\n--- Step 5: Removing line breaks for short header names ---")
  if (sum(wi$final_width) < total_width) {
    remaining <- total_width - sum(wi$final_width)
    vcat("  Still have", sprintf("%.2f", remaining), "cm remaining")

    wi2 <- wi %>% dplyr::mutate(final_width = dplyr::if_else(
      min_head > final_width & min_head < 3,
      min_head,
      final_width
    ))

    if (!toowide(wi2)) {
      n_changed <- sum(wi2$final_width != wi$final_width)
      if (n_changed > 0) vcat("  Expanded", n_changed, "narrow columns to fit headers without wrapping")
      wi <- wi2
      vcat("  New total:", sprintf("%.2f", sum(wi$final_width)), "cm")
    } else {
      vcat("  Would exceed limit, keeping previous widths")
    }
  } else {
    vcat("  Table already uses all available width")
  }

  # Final safety check: ensure table fits
  vcat("\n--- Final validation ---")
  final_total <- sum(wi$final_width)
  vcat("Final table width:", sprintf("%.2f", final_total), "cm (limit:", total_width, "cm)")

  if (final_total > total_width) {
    # Scale down proportionally to fit
    scale_factor <- total_width / final_total
    wi <- wi %>% dplyr::mutate(final_width = final_width * scale_factor)
    warning(sprintf("Table was %.2f cm too wide (%.2f cm > %.2f cm). Scaled all columns by %.1f%% to fit.",
                    final_total - total_width, final_total, total_width, scale_factor * 100))
    vcat("  SCALED DOWN by", sprintf("%.1f%%", scale_factor * 100), "to fit")
    vcat("  New total:", sprintf("%.2f", sum(wi$final_width)), "cm")
  } else if (final_total < total_width) {
    unused <- total_width - final_total
    vcat("  SUCCESS: Table fits with", sprintf("%.2f", unused), "cm to spare")
  } else {
    vcat("  SUCCESS: Table fits perfectly")
  }

  if (verbose) {
    vcat("\nFinal column widths (in cm):")
    for (i in seq_len(nrow(wi))) {
      vcat(sprintf("  %-20s | %.2f cm", wi$name[i], wi$final_width[i]))
    }
    vcat("\nTotal: %.2f cm / %.2f cm available\n", sum(wi$final_width), total_width)
  }

  # Apply widths to flextable
  ft %>%
    flextable::width(width = wi$final_width, unit = "cm")
}
