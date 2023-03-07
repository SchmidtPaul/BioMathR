#' @title Get all descriptive tables
#'
#' @param data data object
#' @param yvars column names of variables for which descriptive measures should be calculated.
#' @param groupvars column names of variables for which grouping should be done
#' @param lang language for the result table column names
#' @param xlsx_path path to where the excel file should be saved
#' @param xlsx_overwrite If \code{TRUE}, overwrites the previous excel file
#' @param xlsx_open If \code{TRUE}, opens the excel file after it was created
#' @param xlsx_data_sheet If \code{code}, adds a excel sheet with the underyling data
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

    wb <- BioMathR::create_wb()

    for (i in 1:length(groupvars)) {
      combs_i <- t(combn(groupvars, i))

      for (ij in 1:nrow(combs_i)) {
        comb_ij     <- combs_i[ij, ]
        comb_ij_lab <- comb_ij %>% str_sub(1, 3) %>% str_c(collapse = "-")

        comb_ij_tab <- data %>%
          dplyr::group_by(across(all_of(comb_ij))) %>%
          # the following lines are reproducing dlookr::describe() to avoid loading dlookr
          dplyr::summarise(across(all_of(yvars),
            .names = "{.col}_{fn}",
            .fns = list(
              n = ~ sum(!is.na(.)),
              na = ~ sum(is.na(.)),
              mean = ~ mean(., na.rm = TRUE),
              sd = ~ sd(., na.rm = TRUE),
              IQR = ~ quantile(., 0.75, na.rm = TRUE) - quantile(., 0.25, na.rm = TRUE),
              p00 = ~ min(., na.rm = TRUE),
              p50 = ~ median(., na.rm = TRUE),
              p100 = ~ max(., na.rm = TRUE)
            )
          )) %>%
          suppressMessages() %>%
          pivot_longer(cols = -all_of(comb_ij)) %>%
          tidyr::separate(name, into = c("Variable", "Stat"), sep = "_") %>%
          pivot_wider(names_from = Stat, values_from = value) %>%
          select(Variable, everything()) %>%
          arrange(Variable) %>%
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
