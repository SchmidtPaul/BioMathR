#' @title Wrapper function for `desplot::desplot()` across multiple columns
#'
#' @description The goal of this function is to allow the user a quick and easy glance at the experimental layout of a trial by creating multiple `desplot`s at once - one for each variable that is provided (see example below).
#'
#' @param data A data frame.
#' @param vars Vector with variables/column names for which a desplot should be created.
#' @param formright A formula like `x*y|location`, i.e. the right-hand side of the formula `yield~x*y|location` that would usually be passed to `desplot::desplot(form = ...)`. Note that `x` and `y` are numeric and the default is `"col + row"`.
#' @param lang Language for plots labels.
#' @param title Can either be `"none"`, `"short"` or `"long"`. For the respective `var`, it gives information about the number of levels and their respective frequency in the data.
#' @param flip see `desplot::desplot()` documentation - set to opinionated default here
#' @param out1.gpar see `desplot::desplot()` documentation - set to opinionated default here
#' @param out2.gpar see `desplot::desplot()` documentation - set to opinionated default here
#' @param ... Other arguments passed to `desplot::desplot()`.
#'
#' @export
#' @return A list of desplots.
#'
#' @examples
#' library(BioMathR)
#'
#' dps <- desplot_across(data = agridat::yates.oats,
#'                       vars = c("nitro", "gen", "block"),
#'                       cex = 1)
#'
#' dps$nitro
#' dps$gen
#' dps$block

desplot_across <-
  function(data,
           vars,
           formright = "col + row",
           lang = c("eng", "ger")[1],
           title = c("none", "short", "long")[3],
           flip = TRUE,
           out1.gpar = list(col = "black", lwd = 1, lty = "dotted"),
           out2.gpar = list(col = "black", lwd = 1, lty = "dotted"),
           ...) {
    assertthat::assert_that(requireNamespace("desplot", quietly = TRUE),
                            msg = "To use desplot_across(), package 'desplot' must be installed.")
    assertthat::assert_that(all(vars %in% names(data)),
                            msg = "Not all provided variables exist in data")

    # TODO: use dplyr's tidyselect for vars

    # language labels
    txt <- list(
      eng = c(
        levels = "levels",
        balanced = "N/level: NNN",
        unbalanced = "N/level: ~NNN (MIN-MAX)"
      ),
      ger = c(
        levels = "Stufen",
        balanced = "N/Stufe: NNN",
        unbalanced = "N/Stufe: ~NNN (MIN-MAX)"
      )
    )

    dps <- list()

    # add copies of row & column variables given in form as "row" & "col"
    colrow <- str_extract_all(formright, "\\w+")[[1]][1:2]
    data$col <- data[[colrow[1]]]
    data$row <- data[[colrow[2]]]

    for (var_i in vars) {
      # rename i-th var to "var_i"
      data_i <- rename(data, var_i = {{ var_i }})

      # short title
      title_i <-
        str_c(var_i, ": ", n_distinct(data_i$var_i), " ", txt[[lang]][["levels"]])

      # long title
      if (title == "long") {
        x <- table(data[[var_i]])

        min  <-  min(x, na.rm = T) %>% round(0) %>% str_c()
        mean <- mean(x, na.rm = T) %>% round(2) %>% str_c()
        max  <-  max(x, na.rm = T) %>% round(0) %>% str_c()

        if (min == max) {
          title_extra <- str_replace(txt[[lang]][["balanced"]], "NNN", min)
        } else {
          title_extra <- txt[[lang]][["unbalanced"]] %>%
            str_replace("NNN", mean) %>%
            str_replace("MIN", min) %>%
            str_replace("MAX", max)
        }

        title_i <- str_c(title_i, "\n", title_extra)

      }

      # no title
      title_i <- ifelse(title == "none", NULL, title_i)

      # formula
      form_i <- stats::formula(paste("var_i ~", formright))

      # data type
      type_i <- typeof(data_i$var_i)

      if (type_i %in% c("integer", "factor", "character")) {
        # desplot
        dps[[var_i]] <- desplot::desplot(
          data = data_i,
          form = form_i,
          text = var_i,
          main = title_i,
          out1 = col,
          out2 = row,
          flip = flip,
          out1.gpar = out1.gpar,
          out2.gpar = out1.gpar,
          show.key = FALSE,
          ...
        )
      } else if (type_i %in% c("double", "numeric")) {
        # desplot
        dps[[var_i]] <- desplot::desplot(
          data = data_i,
          form = form_i,
          text = var_i, shorten="none",
          main = title_i,
          out1 = col,
          out2 = row,
          flip = flip,
          out1.gpar = out1.gpar,
          out2.gpar = out1.gpar,
          show.key = FALSE,
          ...
        )
      } else {
        stop('vars should be "integer", "factor", "character", "double" or "numeric"!')
      }
    }

    return(dps)
  }
