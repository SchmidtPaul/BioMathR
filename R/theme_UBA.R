#' @title A {ggplot2} theme based on the \href{https://www.umweltbundesamt.de/dokument/corporate-design-des-umweltbundesamtes}{Umweltbundesamt corporate design}.
#'
#' @param base_size Base font size, given in pts.
#' @param base_family Base font family. Should be Calibri, but is not set as default.
#' @param base_color Base color for text and lines
#' @param axis_title_bold Should axis titles be bold?
#' @param axis_title_just Justification of axis titles. `rt` translates to "right" (x-axis) "top" (y-axis).
#' @param ticks Should ticks be drawn on the axes?
#' @param grid_x Should there be major grid lines for the x-axis?
#' @param grid_y Should there be major grid lines for the y-axis?
#' @param grid_color Color of the grid lines
#' @param grid_linetype Linetype of the grid lines
#' @param facette_distance Distance between facettes
#' @param facette_box Should a rectangle be drawn around each facette?
#' @param facette_box_color Color of the rectangle around each facette
#' @param title_size Title font size, given in pts.
#' @param title_margin Margin around tile
#' @param subtitle_size Subtitle font size, given in pts.
#' @param subtitle_margin Margin around subtitle
#' @param axistext_size Axis text font size, given in pts.
#' @param is_subplot If `TRUE`, font sizes and margins of title, subtitle and axis text are decreased. This is useful if multiple plots are to be combined.
#' @param legend_custom_bottom If `TRUE`, the legend is formatted in a specific way at the bottom of the plot
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
#' BioMathR::palette_getset("UBA")
#'
#' p1
#'
#' p1 + BioMathR::theme_UBA()

theme_UBA <- function(base_size = 11,
                      base_family = "",
                      base_color = "black",
                      axis_title_bold = TRUE,
                      axis_title_just = "cc",
                      ticks = FALSE,
                      grid_x = TRUE,
                      grid_y = TRUE,
                      grid_color = "black",
                      grid_linetype = "solid",
                      facette_distance = 1,
                      facette_box = TRUE,
                      facette_box_color = "black",
                      title_size = 12,
                      title_margin = margin(b = 10),
                      subtitle_size = 9,
                      subtitle_margin = margin(b = 10),
                      axistext_size = 10,
                      is_subplot = FALSE,
                      legend_custom_bottom = FALSE
){


  if (is_subplot) {
    title_size = 10
    title_margin = margin(b = 3)
    subtitle_size = 9
    subtitle_margin = margin(b = 3)
    axistext_size = 8
  }

  tUBA <- ggplot2::theme_bw(base_family = base_family, base_size = base_size)


  # Plot background ---------------------------------------------------------
  tUBA <- tUBA + theme(panel.background = element_rect(fill = "grey95", color = NA))


  # Legend ------------------------------------------------------------------
  tUBA <- tUBA + theme(
    legend.margin = margin(),
    legend.background = element_blank()
  )

  if (legend_custom_bottom) {
    tUBA <- tUBA + theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.key.size = unit(0, 'lines'),
      legend.key = element_blank(),
      legend.text = element_text(hjust = 0, margin = margin(0, r = 5, 0, 0))
    )
  }


  # Grid Lines --------------------------------------------------------------
  tUBA <- tUBA + theme(panel.grid=element_blank())

  if (grid_x) {
    tUBA <- tUBA +
      theme(panel.grid.major.x = element_line(
        linewidth = 0.2,
        color = grid_color,
        linetype = grid_linetype
      ))
  }

  if (grid_y) {
    tUBA <- tUBA +
      theme(panel.grid.major.y = element_line(
        linewidth = 0.2,
        color = grid_color,
        linetype = grid_linetype
      ))
  }

  # Facette -----------------------------------------------------------------
  # Facette Box Lines
  if (facette_box) {
    tUBA <- tUBA +
      theme(
        panel.border = element_rect(fill = NA, colour = facette_box_color),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text.x = ggtext::element_textbox(size = base_size, color = facette_box_color),
        strip.text.y = ggtext::element_textbox(size = base_size, color = facette_box_color),
      )
  }

  # Facette Distance
  tUBA <- tUBA +
    theme(
      panel.spacing = unit(facette_distance, "lines"),
      plot.margin = margin(3,3,3,3)
    )

  # Facette Strip Labels
  tUBA <- tUBA +
    theme(
      strip.text.x = ggtext::element_textbox(
        hjust = 0.5,
        size = base_size,
        color = facette_box_color
      ),
      strip.text.y = ggtext::element_textbox(
        hjust = 0.5,
        size = base_size,
        color = facette_box_color,
        orientation = "right-rotated"
      ),
      panel.border = element_rect(fill = NA, colour = NA),
      strip.background = element_rect(fill = NA, colour = NA)
    )

  if (facette_box) {
    tUBA <- tUBA +
      theme(panel.border = element_rect(fill = NA, colour = facette_box_color))
  }

  # Axes --------------------------------------------------------------------
  # Axis Lines
  tUBA <-
    tUBA + theme(
      axis.line.x = element_line(color = base_color, linewidth = 0.25),
      axis.line.y = element_line(color = base_color, linewidth = 0.25),
      axis.ticks.x = element_line(linewidth = 0.5),
      axis.ticks.y = element_blank(),
      axis.ticks.length = grid::unit(3, "pt")
    )

  # Axis Title
  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)

  tUBA <- tUBA +
    theme(
      axis.title.x = element_text(hjust = xj, size = base_size, color = base_color, face = "bold"),
      axis.title.y = element_text(hjust = yj, size = base_size, color = base_color, face = "bold")
    )

  # Axis Text
  tUBA <- tUBA +
    theme(
      axis.text.x = ggtext::element_markdown(
        size = axistext_size,
        color = base_color,
        margin = margin(t = 3)
      ),
      axis.text.y = ggtext::element_markdown(
        size = axistext_size,
        color = base_color,
        margin = margin(r = 3)
      )
    )

  # Title, Subtitle, Caption ------------------------------------------------
  # Title
  tUBA <- tUBA +
    theme(
      plot.title.position = "plot",
      plot.title = ggtext::element_textbox_simple(
        size = title_size,
        face = "bold",
        hjust = 0,
        color = base_color,
        margin = title_margin
      )
    )

  # Subtitle
  tUBA <- tUBA + theme(
    plot.subtitle = ggtext::element_textbox_simple(
      size = subtitle_size,
      face = "bold",
      hjust = 0,
      color = base_color,
      margin = subtitle_margin
    )
  )

  # Caption
  tUBA <- tUBA + theme(
    plot.caption.position = "plot",
    plot.caption = ggtext::element_textbox_simple(
      hjust = 1, halign = 1,
      color = base_color,
      size = 6,
      margin = margin(5, 0, 5, 0)
    )
  )

  tUBA <- tUBA + theme(plot.margin = margin(5, 25, 5, 5))

  return(tUBA)
}
