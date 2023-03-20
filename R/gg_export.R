#' @title Conveniently export (gg)plots
#'
#' @description This function is mostly a wrapper function for `ggplot2::ggsave()` that makes it easy to \itemize{ \item{simultaneously export a plot into pdf, svg and/or png (even multiple pngs with different dpi)} \item{immediately open the exported files in your OS}} Additionally, it offers alternatives to rendering via `ggplot2::ggsave()` - see arguments `png_from_pdf` and `svg_device`.
#'
#' @param plot_obj Plot object to save.
#' @param folder_path Path to the destination folder (i.e. correct: `"Folder/Subfolder"`, wrong: `"Folder/Subfolder/File.png"`).
#' @param file_name File name without file extension (i.e. correct: `"File"`, wrong: `"File.png"`).
#' @param width_cm Plot width in cm.
#' @param height_cm Plot height in cm.
#' @param png Should a png file be created and/or immediately opened? Can be either `"none"`, `"create"` or `"open"`.
#' @param pdf Should a pdf file be created and/or immediately opened? Can be either `"none"`, `"create"` or `"open"`.
#' @param svg Should a svg file be created and/or immediately opened? Can be either `"none"`, `"create"` or `"open"`.
#' @param bg Background colour. If `NULL`, uses the `plot.background` fill value from the plot theme.
#' @param png_dpi Plot resolution of png file. Can be a vector of multiple values so that multiple png files will be created.
#' @param png_fast If `TRUE`, the png file is not exported via `ggplot2::ggsave(..., device = "png")`, but instead via `ggplot2::ggsave(..., device = ragg:agg_png())`, which \href{https://ragg.r-lib.org/articles/ragg_performance.html}{should be faster}.
#' @param png_from_pdf If `TRUE`, the png file is not exported via `ggplot2::ggsave(..., device = "png")`, but instead converted/rendered from the pdf created via `ggplot2::ggsave(device = "pdf")`. This can in some cases circumvent issues where pdf and png e.g. have different font sizes.
#' @param svg_device If `"svg"`, the svg file is not exported via `ggplot2::ggsave(..., device = "svg")`, but instead via `grDevices::svg()`/`grDevices::dev.off()`. This can in some cases circumvent issues with e.g. transparency.
#'
#' @export
#'
#' @import ggplot2
#' @import here
#' @importFrom grDevices cairo_pdf dev.off svg
#' @importFrom stringr str_c str_replace str_replace_all
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(data = PlantGrowth) +
#'   aes(y = weight, x = group) +
#'   geom_point()
#'
#' gg_export(
#'   plot_obj = p,
#'   folder_path = here::here(),
#'   file_name = "myplot",
#'   width_cm = 12,
#'   height_cm = 8,
#'   pdf = "open",
#'   png = "open",
#'   svg = "none",
#'   png_dpi = c(96, 300)
#' )
#' }
#'

gg_export <-
  function(plot_obj = NULL,
           folder_path = here::here(),
           file_name = "temp",
           width_cm = 16,
           height_cm = 10,
           pdf = "none",
           png = "create",
           svg = "none",
           bg = "white",
           png_dpi = 300,
           png_fast = FALSE,
           png_from_pdf = FALSE,
           svg_device = "ggsave") {


    # convert non-ggplot2 objects ---------------------------------------------
    # for a manually curated list of non-ggplot2 plot objects,
    # the object is converted to a ggplot-object to make things work:
    others <- c("trellis") # {desplot}


    if (class(plot_obj)[1] %in% others) {
      if (!requireNamespace("ggplotify", quietly = TRUE)) {
        stop("When providing a non-ggplot2 object, package 'ggplotify' must be installed.")
      }
      plot_obj <- ggplotify::as.ggplot(plot_obj)
    }


    # pdf ---------------------------------------------------------------------
    pdf_needed <- any(pdf %in% c("create", "open"),
                      all(png_from_pdf, png %in% c("create", "open")))
    pdf_path  <- stringr::str_c(folder_path, "/", file_name, ".pdf")

    if (pdf_needed) {
      ggplot2::ggsave(
        filename = pdf_path,
        plot = plot_obj,
        width = width_cm,
        height = height_cm,
        units = "cm",
        scale = 1,
        device = cairo_pdf,
        bg = bg
      )
    }


    # png ---------------------------------------------------------------------
    png_needed <- png %in% c("create", "open")

    if (png_needed) {
      png_path  <- stringr::str_c(folder_path, "/", file_name, ".png")

      # if multiple dpi provided, multiple files will be created
      for (dpi_i in png_dpi) {

        png_path_i <- png_path
        if (length(png_dpi) > 1) {
          png_path_i <-
            stringr::str_replace(png_path, ".png", stringr::str_c("_", dpi_i, "dpi.png"))
        }

        # create png the standard way
        assertthat::assert_that(!png_from_pdf || !png_fast,
                                msg = "Do not set both 'png_from_pdf' and 'png_fast' to 'TRUE'.")

        png_standard <- !png_from_pdf && !png_fast

        if (png_standard) {
          ggplot2::ggsave(
            filename = png_path_i,
            plot = plot_obj,
            width = width_cm,
            height = height_cm,
            units = "cm",
            scale = 1,
            dpi = dpi_i,
            bg = bg
          )
        }

        # create png via device = ragg:agg_png()
        if (png_fast) {
          assertthat::assert_that(requireNamespace("ragg", quietly = TRUE),
                                  msg = "To use 'png_fast = TRUE', package 'ragg' must be installed.")

          ggplot2::ggsave(
            filename = png_path_i,
            plot = plot_obj,
            width = width_cm,
            height = height_cm,
            units = "cm",
            scale = 1,
            dpi = dpi_i,
            bg = bg,
            device = ragg::agg_png()
          )
        }

        # create png by forcing it to be copy of pdf
        if (png_from_pdf) {

          assertthat::assert_that(requireNamespace("pdftools", quietly = TRUE),
                                  msg = "To use 'png_from_pdf = TRUE', package 'pdftools' must be installed.")

          assertthat::assert_that(requireNamespace("png", quietly = TRUE),
                                  msg = "To use 'png_from_pdf = TRUE', package 'png' must be installed.")

          bitmap <- pdftools::pdf_render_page(pdf_path, page = 1, dpi = dpi_i)
          png::writePNG(bitmap, png_path_i, dpi = dpi_i)
        }
      }
    }

    # in case pdf was only needed to convert to png: delete!
    keep_pdf <- pdf %in% c("create", "open")
    if (all(!keep_pdf, file.exists(pdf_path))){
      file.remove(pdf_path)
    }


    # svg ---------------------------------------------------------------------
    if (svg %in% c("create", "open")) {

      if (!requireNamespace("svglite", quietly = TRUE)) {
        stop("When exporting an svg, package 'svglite' must be installed.")
      }

      svg_path  <- stringr::str_c(folder_path, "/", file_name, ".svg")

      if (svg_device == "ggsave") {
        ggplot2::ggsave(
          filename = svg_path,
          plot = plot_obj,
          width = width_cm,
          height = height_cm,
          units = "cm",
          scale = 1,
          bg = bg
        )
      }

      if (svg_device == "svg") {
        grDevices::svg(filename = svg_path,
                       width = width_cm / 2.54,
                       height = height_cm / 2.54)
        plot(plot_obj)
        grDevices::dev.off()
      }
    }

    # open files --------------------------------------------------------------
    # open pdf
    if (pdf == "open") {
      system(stringr::str_c('open "', pdf_path, '"'))
    }

    # open png
    if (png == "open") {
      for (dpi_i in png_dpi) {
        png_path_i <- png_path
        if (length(png_dpi) > 1) {
          png_path_i <- stringr::str_replace(png_path, ".png", stringr::str_c("_", dpi_i, "dpi.png"))
        }
        system(stringr::str_c('open "', png_path_i, '"'))
      }
    }

    # open svg
    if (svg == "open") {
      system(stringr::str_c('open "', svg_path, '"'))
    }
  }
