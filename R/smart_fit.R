#' @title Adjust column widths for flextables
#'
#' @param ft Flextable object
#' @param width Maximum width allowed for the table. Can be a numeric value (in cm) or one of the following strings referring to standard paper sizes \code{"A4"} (=default), \code{"letter"}, \code{"legal"} or \code{"executive"}.
#' @param landscape If \code{TRUE}, landscape width for standard paper size provided in \code{width} is used. Default is \code{FALSE}, i.e. portrait width. Ignored if \code{width} is provided as a numeric value.
#' @param page_margin Total page margin (sum of left and right margin) that is subtracted from the total page width of the standard paper width provided in \code{width}. Ignored if \code{width} is provided as a numeric value.
#'
#' @export
smart_fit <- function(ft,
                     width = c("A4", "letter", "legal", "executive")[1],
                     landscape = FALSE,
                     page_margin = "default") {

  make_wider <- min_body <- min_body_lwrd <- min_head <- min_head_lwrd <- NULL # avoid package check warning

  # Check if 'ft' is a flextable object
  if (!inherits(ft, "flextable")) {
    warning("The 'ft' object is not a flextable object. Converting it to flextable.")
    ft <- flextable::flextable(ft)
  }

  # get underlying data.frame
  df <- ft$body$dataset

  # Determine final_width of table
  if (is.numeric(width)) {
    final_width <- width
  } else {
    standard_widths <- data.frame(
      type = c("A4", "letter", "legal", "executive"),
      portrait = c(21, 8.5, 8.5, 7.25),
      landscape = c(29.7, 11, 14, 10.5),
      total_margin = c(4.8, 3, 3, 3),
      stringsAsFactors = FALSE
    )
    matched_width <- standard_widths[standard_widths$type == width, ]
    margin <- ifelse(page_margin == "default", matched_width$total_margin, page_margin)
    total_width <- ifelse(landscape, matched_width$landscape, matched_width$portrait) - margin
  }

  # get widths information
  wi <- data.frame(
    name = names(df),
    min_head = flextable::dim_pretty(ft, part = "header")$widths,
    min_body = flextable::dim_pretty(ft, part = "body")$widths,
    min_head_lwrd = sapply(names(df), function(col_name) {
      words <- unlist(strsplit(col_name, " "))
      word_fts <- lapply(words, function(word) {
        word_ft <- flextable::flextable(data.frame(value = word, check.names = FALSE))
        flextable::dim_pretty(word_ft, part = "body")$widths[1]
      })
      max(unlist(word_fts))
    }),
    min_body_lwrd = sapply(names(df), function(col_name) {
      words <- unlist(strsplit(as.character(df[[col_name]]), " "))
      sorted_words <- sort(words, decreasing = TRUE, method = "radix", index.return = FALSE, na.last = NA)
      top_words <- utils::head(sorted_words, 3)
      word_fts <- lapply(top_words, function(word) {
        word_ft <- flextable::flextable(data.frame(value = word, check.names = FALSE))
        flextable::dim_pretty(word_ft, part = "body")$widths[1]
      })
      max(unlist(word_fts))
    }),
    row.names = NULL
  )

  # inch -> cm
  wi <- wi %>% mutate(across(contains("min_"), function(x) {x * 2.54}))

  # function to check if flextable has reached maximum width
  toowide <- function(wi){sum(wi$final_width) > total_width}

  # default final_width: longest word
  wi <- mutate(wi, final_width = pmax(min_head_lwrd, min_body_lwrd))

  if (toowide(wi)) {
    ft <- flextable::autofit(ft)
    warning("Even default width of longest word per column is already too wide!\n 'flextable::autofit(ft)' is applied")
    return(ft)
  }

  # Allow column names that are only slightly longer than final_width
  wi2 <- wi %>% mutate(
    final_width = case_when(
      0.8 < final_width / min_head & final_width / min_head < 1 ~ min_head,
      TRUE ~ final_width
    )
  )

  if (!toowide(wi2)) {wi <- wi2}

  # Allow cell contents that are only slightly longer that final_width
  wi2 <- wi %>% mutate(
    final_width = case_when(
      0.8 < final_width / min_body & final_width / min_body < 1 ~ min_body,
      TRUE ~ final_width
    )
  )

  if (!toowide(wi2)) {wi <- wi2}

  # Allow extra width for all columns that could still be wider because of their content
  wi2 <- wi %>% mutate(make_wider = min_body > final_width) # which columns wanted to be wider
  remaining_width <- total_width - sum(wi2$final_width) # how much width is left
  p_remain <- wi2[wi2$make_wider, "min_body"] / sum(wi2[wi2$make_wider, "min_body"]) # proportional desired width
  wi2[wi2$make_wider, "final_width"] <- wi2[wi2$make_wider, "final_width"] + p_remain * remaining_width # distribute width proportionally

  wi2 <- wi2 %>%
    mutate(final_width = if_else(
      make_wider & final_width > pmax(min_head, min_body, min_head_lwrd, min_body_lwrd),
      pmax(min_head, min_body, min_head_lwrd, min_body_lwrd),
      final_width
    ))

  if (!toowide(wi2)) {wi <- wi2}

  # If there is still remaining width, remove line breaks for columns that are relatively narrow (e.g. "A 2")
  if (sum(wi$final_width) < total_width) {
    wi2 <- wi %>% mutate(final_width = if_else(
      min_head > final_width & min_head < 3,
      min_head,
      final_width
    ))

    if (!toowide(wi2)) {wi <- wi2}
  }

  # Apply widths to flextable
  ft %>%
    flextable::width(width = wi$final_width, unit = "cm")
}
