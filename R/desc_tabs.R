#' @title Get descriptive tables
#'
#' @param data data object
#' @param yvars column names of variables for which descriptive measures should be calculated. These go into \code{dlookr::describe()}.
#' @param groupvars column names of variables for which grouping should be done
#' @param lang language for the result table column names
#' @param xlsx_path path to where the excel file should be saved
#' @param xlsx_overwrite If \code{TRUE}, overwrites the previous excel file
#' @param xlsx_open If \code{TRUE}, opens the excel file after it was created
#' @param xlsx_data_sheet If \code{code}, adds a excel sheet with the underyling data
#'
#' @importFrom dlookr describe
#' @importFrom utils combn
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

    assertthat::assert_that(requireNamespace("dlookr", quietly = TRUE),
                            msg = "The 'dlookr' package is required but is not installed.")

    wb <- BioMathR::create_wb()

    for (i in 1:length(groupvars)) {
      combs_i <- t(combn(groupvars, i))

      for (ij in 1:nrow(combs_i)) {
        comb_ij     <- combs_i[ij, ]
        comb_ij_lab <- comb_ij %>% str_sub(1, 3) %>% str_c(collapse = "-")

        comb_ij_tab <- data %>%
          dplyr::group_by(across(all_of(comb_ij))) %>%
          dlookr::describe(all_of(yvars)) %>%
          BioMathR::format_dlookrdescribe(lang = lang)

        BioMathR::add_sheet(wb = wb,
                            sheetName = comb_ij_lab,
                            data = comb_ij_tab)

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
