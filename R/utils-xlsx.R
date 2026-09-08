#' Resolve a deprecated argument alias
#'
#' Returns \code{old} (and warns) if the deprecated argument was supplied by
#' the user, otherwise returns \code{new}.
#'
#' @param new value of the current argument
#' @param old value of the deprecated argument
#' @param fn function name, e.g. \code{"add_sheet"}
#' @param old_name deprecated argument name, e.g. \code{"sheetName"}
#' @param new_name current argument name, e.g. \code{"sheet_name"}
#' @param when version in which the argument was deprecated
#'
#' @importFrom lifecycle deprecated
#' @noRd
resolve_deprecated <- function(new,
                               old,
                               fn,
                               old_name,
                               new_name,
                               when = "0.9.0") {
  if (lifecycle::is_present(old)) {
    lifecycle::deprecate_warn(
      when = when,
      what = paste0(fn, "(", old_name, ")"),
      with = paste0(fn, "(", new_name, ")"),
      user_env = rlang::caller_env(2)
    )
    return(old)
  }
  new
}

#' Split an integer vector into runs of consecutive values
#'
#' Conditional formatting must be applied per contiguous cell range, so that
#' e.g. a colour scale is scaled within each block instead of across the gaps.
#'
#' @param x integer vector
#'
#' @return list of integer vectors
#' @noRd
split_consecutive <- function(x) {
  x <- sort(unique(as.integer(x)))

  if (length(x) == 0L) {
    return(list())
  }

  unname(split(x, cumsum(c(1L, diff(x) != 1L))))
}

#' Translate \{openxlsx\} conditional formatting type names to \{openxlsx2\}
#'
#' Keeps the type names used by \{openxlsx\} (and by earlier versions of
#' \code{cond_format()}) working after the move to \{openxlsx2\}. Matching is
#' case-insensitive, as it was in \{openxlsx\}. Unknown types are passed
#' through unchanged so that \{openxlsx2\} reports them.
#'
#' @param type a single conditional formatting type
#'
#' @return the corresponding \{openxlsx2\} type
#' @noRd
translate_cf_type <- function(type) {
  openxlsx2_types <- c(
    "expression", "colorScale", "dataBar", "iconSet", "duplicatedValues",
    "uniqueValues", "containsErrors", "notContainsErrors", "containsBlanks",
    "notContainsBlanks", "containsText", "notContainsText", "beginsWith",
    "endsWith", "between", "topN", "bottomN"
  )

  lookup <- c(
    stats::setNames(openxlsx2_types, tolower(openxlsx2_types)),
    colourscale = "colorScale",
    databar     = "dataBar",
    contains    = "containsText",
    notcontains = "notContainsText",
    duplicates  = "duplicatedValues",
    unique      = "uniqueValues",
    blanks      = "containsBlanks",
    notblanks   = "notContainsBlanks"
  )

  key <- tolower(as.character(type))

  if (length(key) == 1L && key %in% names(lookup)) {
    return(unname(lookup[key]))
  }
  type
}
