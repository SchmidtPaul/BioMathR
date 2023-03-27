#' @title A {ggplot2} theme based on the \href{https://www.biomath.de/}{BioMath} corporate design.
#'
#' @param base_size Base font size, given in pts.
#' @param base_family Base font family
#' @param base_color Base color for text and lines
#' @param axis_title_bold Should axis titles be bold?
#' @param axis_title_just Justification of axis titles. \code{rt} translates to "right" (x-axis) "top" (y-axis).
#' @param ticks Should ticks be drawn on the axes?
#' @param grid_x Should there be major grid lines for the x-axis?
#' @param grid_y Should there be major grid lines for the y-axis?
#' @param grid_color Color of the grid lines
#' @param grid_linetype Linetype of the grid lines
#' @param facette_distance Distance between facettes
#' @param facette_box Should a rectangle be drawn around each facette?
#' @param facette_box_color Color of the rectangle around each facette
#' @param title_size Title font size, given in pts.
#' @param subtitle_size Subtitle font size, given in pts.
#' @param axistext_size Axis text font size, given in pts.
#' @param ggtext_axis If \code{FALSE}, axis texts are rendered via \code{ggplot2::element_text()} instead of \code{ggtext::element_markdown()}. This can prevent potential rendering issues.
#' @param whitebg If \code{TRUE}, plot background is white, otherwise transparent.
#' @param ... Other arguments passed to the underlying \code{ggplot2::theme_minimal()}
#'
#' @export
#'
#' @import ggplot2
#' @import ggtext
#'
#' @examples
#' library(ggplot2)
#'
#' p1 <- ggplot(data = PlantGrowth) +
#'   aes(y = weight, x = group, fill = group) +
#'   geom_bar(stat = "summary", fun = "mean")
#'
#' BioMathR::palette_getset("BioMath")
#'
#' p1
#'
#' p1 + BioMathR::theme_BioMath()

theme_BioMath <- function(base_size = 11,
                          base_family = "",
                          base_color = "#001509",
                          axis_title_bold = FALSE,
                          axis_title_just = "rt",
                          ticks = FALSE,
                          grid_x = FALSE,
                          grid_y = FALSE,
                          grid_color = "#C0BCB5",
                          grid_linetype = "dotted",
                          facette_distance = 1,
                          facette_box = FALSE,
                          facette_box_color = "#C0BCB5",
                          title_size = 14,
                          subtitle_size = 10,
                          axistext_size = 10,
                          ggtext_axis = TRUE,
                          whitebg = TRUE,
                          ...) {

  tBM <- ggplot2::theme_minimal(base_family = base_family, base_size = base_size, ...)
  tBM <- tBM + theme(legend.background = element_blank())
  tBM <- tBM + theme(legend.key = element_blank())


  # Grid Lines --------------------------------------------------------------
  tBM <- tBM + theme(panel.grid=element_blank())

  if (grid_x) {
    tBM <- tBM +
      theme(panel.grid.major.x = element_line(
        linewidth = 0.2,
        color = grid_color,
        linetype = grid_linetype
      ))
  }

  if (grid_y) {
    tBM <- tBM +
      theme(panel.grid.major.y = element_line(
        linewidth = 0.2,
        color = grid_color,
        linetype = grid_linetype
      ))
  }

  # Facette -----------------------------------------------------------------
  # Facette Box Lines
  if (facette_box) {
    tBM <- tBM +
      theme(
        panel.border = element_rect(fill = NA, colour = facette_box_color),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text.x = ggtext::element_textbox(size = base_size, color = facette_box_color),
        strip.text.y = ggtext::element_textbox(size = base_size, color = facette_box_color),
      )
  }

  # Facette Distance
  tBM <- tBM +
    theme(
      panel.spacing = unit(facette_distance, "lines"),
      plot.margin = margin(3,3,3,3)
    )

  # Facette Strip Labels
  tBM <- tBM +
    theme(
      strip.text.x = ggtext::element_textbox(
        hjust = 0,
        size = base_size,
        color = facette_box_color
      ),
      strip.text.y = ggtext::element_textbox(
        hjust = 0,
        size = base_size,
        color = facette_box_color,
        orientation = "right-rotated"
      ),
      panel.border = element_rect(fill = NA, colour = NA),
      strip.background = element_rect(fill = NA, colour = NA)
    )

  if (facette_box) {
    tBM <- tBM +
      theme(panel.border = element_rect(fill = NA, colour = facette_box_color))
  }


  # Axes --------------------------------------------------------------------
  # Axis Lines
  tBM <- tBM + theme(axis.line=element_line(color=base_color, linewidth=0.15))

  if (ticks) {
    tBM <- tBM + theme(axis.ticks = element_line(linewidth=0.15))
    tBM <- tBM + theme(axis.ticks.x = element_line(linewidth=0.15))
    tBM <- tBM + theme(axis.ticks.y = element_line(linewidth=0.15))
    tBM <- tBM + theme(axis.ticks.length = grid::unit(3, "pt"))
  }

  # Axis Title
  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)

  tBM <- tBM +
    theme(
      axis.title.x = ggtext::element_markdown(hjust=xj, size = base_size, color = base_color),
      axis.title.y = ggtext::element_markdown(hjust=yj, size = base_size, color = base_color)
    )

  # Axis Text
  if(ggtext_axis){
    tBM <- tBM +
      theme(
        axis.text.x = ggtext::element_markdown(
          size = axistext_size,
          color = base_color,
          margin = margin(t = 1)
        ),
        axis.text.y = ggtext::element_markdown(
          size = axistext_size,
          color = base_color,
          margin = margin(r = 1)
        )
      )
  } else {
    tBM <- tBM +
      theme(
        axis.text.x = element_text(
          size = axistext_size,
          color = base_color,
          margin = margin(t = 1)
        ),
        axis.text.y = element_text(
          size = axistext_size,
          color = base_color,
          margin = margin(r = 1)
        )
      )
  }


  # Title, Subtitle, Caption ------------------------------------------------
  # Title
  tBM <- tBM +
    theme(
      plot.title.position = "plot",
      plot.title = ggtext::element_textbox_simple(
        size = title_size,
        hjust = 0,
        color = base_color,
        margin = margin(b = 9)
      )
    )

  # Subtitle
  tBM <- tBM + theme(
    plot.subtitle = ggtext::element_textbox_simple(
      size = subtitle_size,
      face = "italic",
      hjust = 0,
      color = base_color,
      margin = margin(-6, 0, 9, 0)
    )
  )

  # Caption
  tBM <- tBM + theme(
    plot.caption.position = "plot",
    plot.caption = ggtext::element_textbox_simple(
      hjust = 1, halign = 1,
      color = "#C0BCB5",
      size = axistext_size,
      margin = margin(10, 0, 6, 0)
    )
  )

  if (whitebg) {
    tBM <- tBM + theme(plot.background = element_rect(fill = "white", colour = NA))
  }

  return(tBM)
}
