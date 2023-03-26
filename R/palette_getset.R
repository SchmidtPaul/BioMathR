#' @title Get and set color palettes
#'
#' @param palette Choose color palette \itemize{ \item{\code{"BioMath"} - \href{https://www.biomath.de/}{BioMath} corporate design (\href{https://coolors.co/f0a202-495057-00923f-201e50-e4572e}{see preview})} \item{\code{"UBA"} - Umweltbundesamt \href{https://www.umweltbundesamt.de/dokument/corporate-design-des-umweltbundesamtes}{corporate design} (\href{https://coolors.co/5ead35-007626-0b90d5-005f85-9d579a-622f63-ce1f5e-83053c-fabb00-d78400}{see preview})}}
#' @param setforggplot If \code{TRUE} (default), the color palette is set as default \code{ggplot2.discrete.fill} and \code{ggplot2.discrete.colour}.
#'
#' @return The color palette as a named vector
#'
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' library(ggplot2)
#'
#' p1 <- ggplot(data = PlantGrowth) +
#'   aes(y = weight, x = group, fill = group) +
#'   geom_bar(stat = "summary", fun = "mean")
#'
#' p1
#'
#' BioMathR::palette_getset("BioMath")
#'
#' p1
#'
#' p1 + BioMathR::theme_BioMath()

palette_getset <- function(palette,
                           setforggplot = TRUE) {


  # BioMath -----------------------------------------------------------------
  if (palette == "BioMath") {
    cols <-
      c(
        green  = "#00923f",
        grey   = "#495057",
        orange = "#f0a202",
        red    = "#e4572e",
        blue   = "#96c5f7"
      )

    if (setforggplot) {
      assertthat::assert_that(requireNamespace("MetBrewer", quietly = TRUE),
                              msg = "When palette = 'BioMath', package 'MetBrewer' must be installed.")

      options(
        ggplot2.discrete.fill = list(
          unname(cols),
          MetBrewer::met.brewer("Hokusai1", 6),
          MetBrewer::met.brewer("Hokusai1", 7),
          MetBrewer::met.brewer("Hokusai1", 8),
          MetBrewer::met.brewer("Hokusai1", 9),
          MetBrewer::met.brewer("Hokusai1", 10),
          MetBrewer::met.brewer("Hokusai1", 11),
          MetBrewer::met.brewer("Hokusai1", 12)
        ),
        ggplot2.discrete.colour = list(
          unname(cols),
          MetBrewer::met.brewer("Hokusai1", 6),
          MetBrewer::met.brewer("Hokusai1", 7),
          MetBrewer::met.brewer("Hokusai1", 8),
          MetBrewer::met.brewer("Hokusai1", 9),
          MetBrewer::met.brewer("Hokusai1", 10),
          MetBrewer::met.brewer("Hokusai1", 11),
          MetBrewer::met.brewer("Hokusai1", 12)
        )
      )
    }
  }


  # Umweltbundesamt (UBA) ---------------------------------------------------
  if (palette == "UBA") {
    cols <-
      c(
        gruen = "#54ba4f",
        dunkelgruen = "#097f36",
        blau = "#069edb",
        dunkelblau = "#015e85",
        flieder = "#985ca6",
        dunkelflieder = "#5b2e67",
        fuchsia = "#d12e69",
        dunkelfuchsia = "#7e013e",
        ocker = "#fdb912",
        dunkelocker = "#d68119",
        hellgrau = "#d1d2d4",
        dunkelgrau = "#4d4d4f"
      )

    if (setforggplot) {
      options(
        ggplot2.discrete.fill = list(unname(cols)),
        ggplot2.discrete.colour = list(unname(cols))
      )
    }
  }

  return(cols)
}
