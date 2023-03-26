#' Get residual plots for model diagnostics
#'
#' @param model A model object.
#' @param lang Language for plots labels.
#' @param col_dots Color of the dots.
#' @param col_line Color of the lines.
#' @param aslist If \code{TRUE}, output is a list of plots. If \code{FALSE} (default), output is a combined plot.
#'
#' @export
get_residual_plots <-
  function(model,
           lang = c("eng", "ger")[1],
           col_dots = BioMathR::palette_getset("BioMath", setforggplot = FALSE)[["grey"]],
           col_line = BioMathR::palette_getset("BioMath", setforggplot = FALSE)[["green"]],
           aslist = FALSE) {
    assertthat::assert_that(requireNamespace("performance", quietly = TRUE),
                            msg = "package 'performance' must be installed to use get_residual_plots().")
    assertthat::assert_that(requireNamespace("see", quietly = TRUE),
                            msg = "package 'see' must be installed to use get_residual_plots().")

    # Homoscedasticity --------------------------------------------------------
    rp1 <- plot(performance::check_heteroscedasticity(model))

    rp1 <- rp1 +
      BioMathR::theme_BioMath(grid_y = TRUE) +
      # reset y axis formatting to allow for equation in label:
      theme(axis.title.y = element_blank()) +
      theme(axis.title.y = element_text(angle = 90, hjust = 1))

    # adjust colors
    rp1$layers[[1]]$aes_params$colour <- col_dots
    rp1$layers[[2]]$aes_params$colour <- col_line
    rp1$layers[[2]]$aes_params$fill   <- col_line

    # adjust labels
    if (lang == "eng") {
      rp1$labels$x <- "Fitted/Predicted values"
      rp1$labels$subtitle <- "Dots should form an approx. horizontal band around the reference line."
    }

    if (lang == "ger") {
      rp1$labels$y <- expression(sqrt("|Std. Residuen|"))
      rp1$labels$x <- "Vorhergesagte Werte"
      rp1$labels$title <- "Varianzhomogenit\u00E4t"
      rp1$labels$subtitle <- "Punkte sollten ein horizontales Band um die Referenzlinie bilden."
    }


    # Normal distribution -----------------------------------------------------
    rp2 <- plot(performance::check_normality(model), type = "qq")
    rp2 <- rp2 + BioMathR::theme_BioMath()

    # adjust colors
    rp2$layers[[1]]$aes_params$fill   <- col_line
    rp2$layers[[2]]$aes_params$colour <- col_dots
    rp2$layers[[3]]$aes_params$colour <- col_line

    # adjust labels
    if (lang == "ger") {
      rp2$labels$y <- "Stichproben-Quantile"
      rp2$labels$x <- "Quantile der Standardnormalverteilung"
      rp2$labels$title <- "Normalverteilung der Residuen"
      rp2$labels$subtitle <- "Punkte sollten entlang der Linie liegen."
    }


    # Combine plots -----------------------------------------------------------
    out <- list(rp1, rp2)

    if (!aslist) {
      assertthat::assert_that(requireNamespace("cowplot", quietly = TRUE),
                              msg = "When aslist=FALSE, package 'cowplot' must be installed to obtain plot grid.")
      out <- cowplot::plot_grid(plotlist = out, nrow = 1)
    }

    return(out)
  }
