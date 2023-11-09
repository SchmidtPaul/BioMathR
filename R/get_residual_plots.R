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

    # return NULL if model is NULL
    if (is.null(model)) {
      return(NULL)
    }

    pred <- sqrt_abs_res <- y <- NULL # avoid package check warning

    assertthat::assert_that(
      requireNamespace("performance", quietly = TRUE),
      msg = "package 'performance' must be installed to use get_residual_plots()."
    )

    assertthat::assert_that(
      requireNamespace("see", quietly = TRUE),
      msg = "package 'see' must be installed to use get_residual_plots()."
    )

    assertthat::assert_that(
      requireNamespace("qqplotr", quietly = TRUE),
      msg = "package 'qqplotr' must be installed to use get_residual_plots()."
    )


    # Homoscedasticity --------------------------------------------------------
    if (class(model) %in% c("lm", "lmerMod", "lmerModLmerTest")) {
      rp1 <- plot(performance::check_heteroscedasticity(model))
    } else if (inherits(model, "glmmTMB")) {
      x <- data.frame(
        sqrt_abs_res = sqrt(abs(resid(model))),
        pred = predict(model)
      )
      rp1 <- ggplot2::ggplot(x) +
        ggplot2::aes(x = pred, sqrt_abs_res) +
        see::geom_point2(
          colour = col_dots,
          size = 2,
          alpha = 0.8
        ) +
        ggplot2::stat_smooth(
          method = "loess",
          se = TRUE,
          alpha = 0.2,
          formula = y ~ x,
          linewidth = 0.8,
          colour = col_line,
          fill = col_line
        ) +
        ggplot2::labs(
          title = "Homogeneity of Variance",
          subtitle = "Reference line should be flat and horizontal",
          y = expression(sqrt("|residuals|")),
          x = "Fitted values"
        )
    }

    rp1 <- rp1 +
      BioMathR::theme_BioMath(grid_y = TRUE) +
      # reset y axis formatting to allow for equation in label:
      ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(angle = 90, hjust = 1))

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


    # Normality ---------------------------------------------------------------
    if (class(model) %in% c("lm", "lmerMod", "lmerModLmerTest")) {
      rp2 <- plot(performance::check_normality(model), type = "qq")
    } else if (inherits(model, "glmmTMB")) {

      res_ <- suppressMessages(sort(stats::residuals(model), na.last = NA))
      dat <- stats::na.omit(data.frame(y = res_))
      rp2 <- ggplot2::ggplot(dat) +
        ggplot2::aes(sample = y) +
        qqplotr::stat_qq_band(
          alpha = 0.2,
          fill = col_line,
          bandType = "ell",
          detrend = FALSE
        ) +
        qqplotr::stat_qq_point(
          shape = 16,
          stroke = 0,
          size = 2,
          colour = col_dots,
          alpha = 0.8,
          detrend = FALSE
        ) +
        ggplot2::stat_qq_line(
          linewidth = 0.8,
          colour = col_line,
          detrend = FALSE
        ) +
        ggplot2::labs(
          title = "Normality of Residuals",
          subtitle = "Dots should fall along the line",
          y = "Sample Quantiles",
          x = "Standard Normal Distribution Quantiles"
        )
    }

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
      assertthat::assert_that(
        requireNamespace("cowplot", quietly = TRUE),
        msg = "When aslist=FALSE, package 'cowplot' must be installed to obtain plot grid."
      )
      out <- cowplot::plot_grid(plotlist = out, nrow = 1)
    }

    return(out)
  }
