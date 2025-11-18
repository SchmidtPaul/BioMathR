#' @title Get all descriptive tables
#'
#' @param data Data object.
#' @param yvars Column names of variables for which descriptive measures should be calculated.
#' @param groupvars Column names of variables for which grouping should be done.
#' @param lang Language for the result table column names.
#' @param xlsx_path Path to where the excel file should be saved.
#' @param xlsx_overwrite If \code{TRUE}, overwrites the previous excel file.
#' @param xlsx_open If \code{TRUE}, opens the excel file after it was created.
#' @param xlsx_data_sheet If \code{TRUE}, adds a excel sheet with the underyling data.
#'
#' @export
desc_tabs <-
  function(data,
           yvars,
           groupvars,
           lang = c("eng", "ger")[1],
           xlsx_path = here::here("desctabs.xlsx"),
           xlsx_overwrite = FALSE,
           xlsx_open = FALSE,
           xlsx_data_sheet = TRUE) {

    Variable <- name <- value <- NULL # avoid package check warning

    # validate groupvars parameter
    assertthat::assert_that(length(groupvars) > 0,
                            msg = "'groupvars' must contain at least one variable name")

    wb <- BioMathR::create_wb()

    for (i in seq_along(groupvars)) {
      combs_i <- t(combn(groupvars, i))

      for (ij in seq_len(nrow(combs_i))) {
        comb_ij     <- combs_i[ij, ]
        comb_ij_lab <- comb_ij %>% str_sub(1, 3) %>% str_c(collapse = "-")

        # calculate table
        comb_ij_tab <- data %>%
          dplyr::group_by(across(all_of(comb_ij))) %>%
          BioMathR::describe(yvars = yvars, lang = lang)

        # export table
        BioMathR::add_sheet(wb = wb,
                            sheetName = comb_ij_lab,
                            data = comb_ij_tab)

        # Apply conditional formatting separately for each variable to handle different scales
        if ("Variable" %in% names(comb_ij_tab)) {
          # Get unique variables
          unique_vars <- unique(comb_ij_tab$Variable)

          # Apply conditional formatting for each variable's rows separately
          for (var in unique_vars) {
            # Get row indices for this variable (add 1 for header row in Excel)
            var_rows <- which(comb_ij_tab$Variable == var) + 1

            # Apply color scale only to this variable's rows
            BioMathR::cond_format(
              wb = wb,
              sheetName = comb_ij_lab,
              columns = which(names(comb_ij_tab) %in% c("MW", "Mean"), arr.ind = TRUE),
              style = c("#ed6a5a", "#f0a202", "#00923f"),
              rule = NULL,
              type = "colourScale",
              rows = var_rows  # Only format rows for this specific variable
            )
          }
        } else {
          # Fallback: if no Variable column, apply formatting to all rows as before
          BioMathR::cond_format(
            wb = wb,
            sheetName = comb_ij_lab,
            columns = which(names(comb_ij_tab) %in% c("MW", "Mean"), arr.ind = TRUE),
            style = c("#ed6a5a", "#f0a202", "#00923f"),
            rule = NULL,
            type = "colourScale"
          )
        }
      }
    }

    if (xlsx_data_sheet) {
      BioMathR::add_sheet(wb = wb,
                          sheetName = "data",
                          data = data)
    }

    BioMathR::save_wb(wb,
                      file = xlsx_path,
                      overwrite = xlsx_overwrite,
                      open_file = xlsx_open)
  }
