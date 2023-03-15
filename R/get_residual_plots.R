#' Get residual plots for model diagnostics
#'
#' @param model A model object.
#' @param col_dots Color of the dots
#' @param col_line Color of the lines
#' @param aslist If \code{TRUE}, output is a list of plots. If \code{FALSE} (default), output is a combined plot.
#'
#' @export
get_residual_plots <-
  function(model,
           col_dots = BioMathR::palette_getset("BioMath", setforggplot = FALSE)[["grey"]],
           col_line = BioMathR::palette_getset("BioMath", setforggplot = FALSE)[["green"]],
           aslist = FALSE) {
    assertthat::assert_that(requireNamespace("performance", quietly = TRUE),
                            msg = "package 'performance' must be installed to use get_residual_plots().")
    assertthat::assert_that(requireNamespace("see", quietly = TRUE),
                            msg = "package 'see' must be installed to use get_residual_plots().")

    # Homoscedasticity
    rp1 <-
      plot(performance::check_heteroscedasticity(model)) + BioMathR::theme_BioMath()
    rp1$layers[[1]]$aes_params$colour <- col_dots
    rp1$layers[[2]]$aes_params$colour <- col_line
    rp1$layers[[2]]$aes_params$fill   <- col_line

    # Normal distribution
    rp2 <-
      plot(performance::check_normality(model), type = "qq") + BioMathR::theme_BioMath()
    rp2$layers[[1]]$aes_params$fill   <- col_line
    rp2$layers[[2]]$aes_params$colour <- col_dots
    rp2$layers[[3]]$aes_params$colour <- col_line

    out <- list(rp1, rp2)

    if (!aslist) {
      assertthat::assert_that(requireNamespace("cowplot", quietly = TRUE),
                              msg = "When aslist=FALSE, package 'cowplot' must be installed to obtain plot grid.")
      out <- cowplot::plot_grid(plotlist = out, nrow = 1)
    }

    return(out)

  }
