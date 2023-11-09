#' Join Multiple ANOVA Tables
#'
#' This function takes a list of ANOVA tables and joins them into a single tibble.
#' If an element of the ANOVA list is NULL, then an empty column is added.
#'
#' @param anova_list A list of ANOVA tables.
#' @param anova_names A character vector specifying the names for each ANOVA in the list.
#'
#' @return A tibble with merged ANOVA results.
#'
#' @export
join_anovas <- function(anova_list, anova_names) {

  Term <- NULL # avoid package check warning

  # Process individual Anova table
  process_anova_table <- function(table) {
    # Check if NULL
    if(is.null(table[[1]])) {
      return(tibble(Term = "(Intercept)", pvalue = NA_real_))
    }

    table %>%
      rownames_to_column(var = "Term") %>%
      as_tibble() %>%
      rename_with(~ ifelse(str_starts(.x, "Pr"), "pvalue", .x)) %>%
      select("Term", "pvalue")
  }

  # Process all Anova tables in the list
  processed_list <- anova_list %>%
    map(~ process_anova_table(.x))

  # Merge the processed tables together
  merged_table <- purrr::reduce(
    .x = processed_list,
    .f = full_join,
    by = "Term"
  )

  names(merged_table) <- c("Term", anova_names)

  merged_table <- merged_table %>%
    mutate(across(all_of(anova_names), ~ifelse(is.na(.), "", BioMathR::format_p(.)))) %>%
    filter(Term != "Residuals" & Term != "(Intercept)")

  return(merged_table)
}
