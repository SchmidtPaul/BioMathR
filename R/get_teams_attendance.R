#' @title Summarise Microsoft Teams attendance reports
#'
#' @description Combines several Microsoft Teams attendance reports (one
#'   downloaded CSV file per workshop day) into a single tibble with one row per
#'   unique participant and one column per day holding the attendance duration.
#'
#' @param files Character vector of paths to the Teams attendance CSV files
#'   (one per day).
#' @param unit Either \code{"hours"} (default) or \code{"minutes"}; the unit of
#'   the per-day duration cells. Hours are rounded to one decimal place.
#' @param match_by_email Logical; if \code{TRUE} (default), authenticated users
#'   are merged across days via their e-mail address (column \code{userName}),
#'   so that display-name variants like \code{"Max M."} and
#'   \code{"Max Mustermann"} collapse into one row. Anonymous guests (no e-mail)
#'   are always matched by display name only.
#' @param merge_contained_names Logical; if \code{TRUE} (default), display-name
#'   variants are merged when one is a prefix of another after normalisation
#'   (lowercased, ignoring spaces and punctuation), provided the shorter name
#'   has at least two word tokens. Merging is transitive, so all variants
#'   sharing a common name prefix collapse into one row. For example
#'   \code{"Max Mustermann"}, \code{"Max Mustermann, MRI"},
#'   \code{"Max Mustermann_FIRMA"}, \code{"Max Mustermann BLE 624"} and
#'   \code{"Max Mustermann BLE624"} all collapse into a single participant. The
#'   two-word minimum keeps single-word names such as \code{"Max"} from pulling
#'   in unrelated people (e.g. \code{"Maximilian"} or \code{"Max Power"}).
#' @param export Logical; if \code{TRUE} (default), an Excel workbook with two
#'   worksheets is written: \code{"Zusammengeführt"} (the consolidated table)
#'   and \code{"Roh"} (the unconsolidated reference table). The function then
#'   returns the consolidated tibble invisibly. Set \code{FALSE} to skip the file
#'   and return the tibble visibly, as before.
#' @param xlsx_path Optional path for the exported workbook. If \code{NULL}
#'   (default), the file is written as \code{teams_attendance.xlsx} into the
#'   folder of the first input file (\code{dirname(files[1])}). Supplying a path
#'   implies \code{export = TRUE}; a missing \code{.xlsx} extension is added.
#'   An existing file is overwritten without asking.
#'
#' @details The expected input is the flat, comma-separated CSV that Teams
#'   provides, with one row per join/leave segment. The relevant columns are
#'   \code{display} (display name), \code{userName} (e-mail / UPN, often empty),
#'   \code{joinDateTime} and \code{leaveDateTime} (ISO 8601 timestamps). The day
#'   of a report is taken from \code{joinDateTime}; multiple segments (rejoins)
#'   on the same day are summed.
#'
#'   File encoding is detected automatically via the byte-order mark: files
#'   starting with \code{FF FE} are read as UTF-16LE, otherwise UTF-8 is assumed.
#'
#'   The canonical name shown for a merged participant is the longest display
#'   name in the group (ties broken by frequency, then alphabetically).
#'   Participations without a display name (e.g. dial-ins) are labelled
#'   \code{"(ohne Namen)"}; those without an e-mail collapse into one such row.
#'
#'   The returned tibble carries an attribute \code{"unmerged"} holding the same
#'   table without any consolidation (one row per distinct display name, no
#'   e-mail and no name merging). It is meant as a side-by-side reference, e.g.
#'   as a second sheet in an exported workbook: \code{attr(result, "unmerged")}.
#'   Access it directly, as most \pkg{dplyr} operations drop attributes.
#'
#'   When \code{export = TRUE} (or \code{xlsx_path} is supplied), an Excel
#'   workbook is written with two worksheets: \code{"Zusammengeführt"} (the
#'   consolidated table) and \code{"Roh"} (the unconsolidated table). Both sheets
#'   get a bold, frozen header row, auto-fitted column widths and a white-to-green
#'   colour scale on the per-day duration columns (white = low, green = high
#'   attendance); empty cells (absences) stay uncoloured.
#'
#'   Known matching limits: name merging is intentionally aggressive, so two
#'   different people whose names share a common prefix (e.g. the same first and
#'   last name) may be merged into one row. Use the \code{"unmerged"} attribute
#'   to cross-check. One person appearing under different e-mails stays in
#'   separate rows unless their names also merge by prefix.
#'
#' @return A tibble with columns \code{name}, \code{email} (often empty) and one
#'   numeric column per day (named by the date). A cell value of \code{NA} means
#'   the participant did not attend on that day; \code{0} means they joined but
#'   without measurable duration. The attribute \code{"unmerged"} holds the
#'   unconsolidated version of the same table. With \code{export = TRUE} the
#'   tibble is returned invisibly and an Excel file is written as a side effect.
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' files <- list.files(
#'   system.file("extdata", "teams_attendance", package = "BioMathR"),
#'   full.names = TRUE
#' )
#' if (length(files) > 0) {
#'   res <- get_teams_attendance(files, export = FALSE)
#'   res
#'
#'   # Unconsolidated reference version (e.g. for a second worksheet):
#'   attr(res, "unmerged")
#' }
#'
#' \donttest{
#' if (length(files) > 0) {
#'   # Export both sheets to an Excel workbook:
#'   get_teams_attendance(files, xlsx_path = tempfile(fileext = ".xlsx"))
#' }
#' }
get_teams_attendance <- function(files,
                                 unit = c("hours", "minutes"),
                                 match_by_email = TRUE,
                                 merge_contained_names = TRUE,
                                 export = TRUE,
                                 xlsx_path = NULL) {

  unit <- match.arg(unit)

  assertthat::assert_that(
    is.character(files), length(files) >= 1L,
    msg = "`files` must be a non-empty character vector of file paths."
  )

  missing_files <- files[!file.exists(files)]
  assertthat::assert_that(
    length(missing_files) == 0L,
    msg = sprintf("These `files` do not exist:\n%s", paste0("- ", missing_files, collapse = "\n"))
  )

  # Read and parse every file into one long tibble.
  long <- purrr::map(files, parse_teams_file) |>
    dplyr::bind_rows()

  long <- dplyr::mutate(long, email = ifelse(is.na(.data$email), "", trimws(.data$email)))

  # Shared, chronologically ordered set of day columns for both output versions.
  date_cols <- as.character(sort(unique(long$date)))

  # Merge key: e-mail when available (and requested), else display name.
  long_merged <- dplyr::mutate(
    long,
    key = if (match_by_email) {
      ifelse(.data$email != "", tolower(.data$email), .data$display)
    } else {
      .data$display
    }
  )

  # Optionally fold display-name variants (one name contained in another).
  if (merge_contained_names) {
    key_names <- canonical_names(long_merged)
    mapping <- merge_contained_keys(key_names$key, key_names$name)
    long_merged$key <- unname(mapping[long_merged$key])
  }

  result <- build_attendance_table(long_merged, unit, date_cols)

  # Unconsolidated reference: one row per distinct display name, no merging.
  long_raw <- dplyr::mutate(long, key = .data$display)
  unmerged <- build_attendance_table(long_raw, unit, date_cols)

  attr(result, "unmerged") <- unmerged

  if (export || !is.null(xlsx_path)) {
    path <- resolve_xlsx_path(xlsx_path, files)
    write_attendance_xlsx(result, unmerged, path)
    rlang::inform(sprintf("Excel-Datei geschrieben: %s", path))
    return(invisible(result))
  }

  result
}

#' Resolve the export path: default file in the input folder, or a user path
#' with a `.xlsx` extension ensured.
#' @noRd
resolve_xlsx_path <- function(xlsx_path, files) {
  if (is.null(xlsx_path)) {
    return(file.path(dirname(files[1]), "teams_attendance.xlsx"))
  }
  if (!grepl("\\.xlsx$", xlsx_path, ignore.case = TRUE)) {
    xlsx_path <- paste0(xlsx_path, ".xlsx")
  }
  xlsx_path
}

#' Write the consolidated and raw attendance tables to a two-sheet workbook.
#'
#' Both sheets get a bold, frozen header row, auto column widths and a
#' white-to-green colour scale on the per-day duration columns (everything but
#' `name` and `email`). An existing file is overwritten.
#' @noRd
write_attendance_xlsx <- function(merged, unmerged, path) {
  wb <- openxlsx2::wb_workbook()

  write_sheet <- function(sheet, data) {
    wb$
      add_worksheet(sheet = sheet)$
      add_data(sheet = sheet, x = data, na = NULL)$
      add_font(
        sheet  = sheet,
        dims   = openxlsx2::wb_dims(x = data, select = "col_names"),
        bold   = "1",
        update = "bold"
      )$
      freeze_pane(sheet = sheet, first_row = TRUE)$
      set_col_widths(sheet = sheet, cols = seq_along(data), widths = "auto")

    day_cols <- setdiff(seq_along(data), match(c("name", "email"), names(data)))
    if (length(day_cols) > 0L && nrow(data) > 0L) {
      wb$add_conditional_formatting(
        sheet = sheet,
        dims  = openxlsx2::wb_dims(
          rows = seq_len(nrow(data)) + 1L,
          cols = day_cols
        ),
        type  = "colorScale",
        style = c("#FFFFFF", "#63BE7B")
      )
    }
  }

  write_sheet("Zusammengef\u00fchrt", merged)
  write_sheet("Roh", unmerged)

  wb$save(path, overwrite = TRUE)
  invisible(path)
}

#' Build the wide attendance tibble from a long table keyed by `key`.
#'
#' Picks the canonical (longest) display name per key, a representative e-mail,
#' sums durations per key and day, and pivots to one column per day.
#' @noRd
build_attendance_table <- function(long, unit, date_cols) {
  canon <- canonical_names(long)

  email_lookup <- long |>
    dplyr::filter(.data$email != "") |>
    dplyr::group_by(.data$key) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::select("key", "email")

  per_day <- long |>
    dplyr::group_by(.data$key, .data$date) |>
    dplyr::summarise(minutes = sum(.data$minutes), .groups = "drop")

  if (unit == "hours") {
    per_day <- dplyr::mutate(per_day, minutes = round(.data$minutes / 60, 1))
  }

  wide <- per_day |>
    dplyr::mutate(date = as.character(.data$date)) |>
    tidyr::pivot_wider(names_from = "date", values_from = "minutes")

  result <- canon |>
    dplyr::left_join(email_lookup, by = "key") |>
    dplyr::mutate(email = ifelse(is.na(.data$email), "", .data$email)) |>
    dplyr::left_join(wide, by = "key") |>
    dplyr::arrange(.data$name) |>
    dplyr::select("name", "email", dplyr::all_of(date_cols))

  tibble::as_tibble(result)
}

#' Canonical display name per key: longest, ties broken by frequency then name.
#' @noRd
canonical_names <- function(long) {
  long |>
    dplyr::group_by(.data$key, .data$display) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(.data$key, dplyr::desc(nchar(.data$display)), dplyr::desc(.data$n), .data$display) |>
    dplyr::group_by(.data$key) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::transmute(.data$key, name = .data$display)
}

#' Normalise a display name for prefix comparison: lowercase, alphanumerics only
#' (drops spaces, commas, underscores, parentheses, ...).
#' @noRd
normalise_name <- function(x) {
  tolower(gsub("[^[:alnum:]]+", "", x))
}

#' Count the whole-word tokens (alphanumeric runs) in a display name.
#' @noRd
count_name_words <- function(x) {
  lengths(regmatches(x, gregexpr("[[:alnum:]]+", x)))
}

#' Group keys whose names share a common prefix into one key.
#'
#' Two names are merged when the shorter (normalised) name is a prefix of the
#' longer and the shorter consists of at least two word tokens (so single-word
#' names like "Max" never pull others in). Merging is transitive (union-find on
#' integer positions, safe for empty or duplicate names): all variants sharing a
#' prefix collapse into one group. Returns a named vector (key -> root key).
#' @noRd
merge_contained_keys <- function(keys, names) {
  n <- length(keys)
  parent <- seq_len(n)
  norm <- normalise_name(names)
  nchars <- nchar(norm)
  nwords <- count_name_words(names)

  find_root <- function(i) {
    while (parent[i] != i) {
      i <- parent[i]
    }
    i
  }

  if (n >= 2L) {
    for (i in seq_len(n - 1L)) {
      for (j in (i + 1L):n) {
        # The shorter normalised string is the potential prefix / anchor.
        if (nchars[i] <= nchars[j]) {
          s <- i; l <- j
        } else {
          s <- j; l <- i
        }
        if (nwords[s] >= 2L && nzchar(norm[s]) && startsWith(norm[l], norm[s])) {
          ri <- find_root(i)
          rj <- find_root(j)
          if (ri != rj) {
            parent[ri] <- rj
          }
        }
      }
    }
  }

  roots <- vapply(seq_len(n), find_root, integer(1))
  stats::setNames(keys[roots], keys)
}

#' Detect the encoding of a Teams CSV via its byte-order mark.
#' @noRd
detect_teams_encoding <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  bom <- readBin(con, what = "raw", n = 2L)
  if (length(bom) == 2L && bom[1] == as.raw(0xFF) && bom[2] == as.raw(0xFE)) {
    "UTF-16LE"
  } else {
    "UTF-8"
  }
}

#' Parse a single Teams attendance CSV into a long tibble.
#' @noRd
parse_teams_file <- function(path) {
  enc <- detect_teams_encoding(path)

  df <- utils::read.csv(
    path,
    fileEncoding = enc,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character"
  )

  required <- c("display", "joinDateTime", "leaveDateTime")
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0L) {
    stop(sprintf(
      "File '%s' is missing expected column(s): %s.\nColumns found: %s.",
      basename(path),
      paste(missing_cols, collapse = ", "),
      paste(names(df), collapse = ", ")
    ), call. = FALSE)
  }

  join <- as.POSIXct(df$joinDateTime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  leave <- as.POSIXct(df$leaveDateTime, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  minutes <- as.numeric(difftime(leave, join, units = "mins"))
  minutes[is.na(minutes) | minutes < 0] <- 0

  email <- if ("userName" %in% names(df)) df$userName else NA_character_

  display <- trimws(df$display)
  display[is.na(display) | display == ""] <- "(ohne Namen)"

  tibble::tibble(
    display = display,
    email = email,
    date = as.Date(join),
    minutes = minutes
  )
}
