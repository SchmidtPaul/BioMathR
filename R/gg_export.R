#' @title Conveniently export (gg)plots - mostly based on \code{ggplot2::ggsave()}.
#'
#' @param plot_obj Plot object to save.
#' @param folder_path Path to the destination folder (i.e. correct: \code{"Folder/Subfolder"}, wrong: \code{"Folder/Subfolder/File.png"}).
#' @param file_name File name without file extension (i.e. correct: \code{"File"}, wrong: \code{"File.png"}).
#' @param width_cm Plot width in cm.
#' @param height_cm Plot height in cm.
#' @param pdf Should a pdf file be created and/or immediately opened? Can be either \code{"none"}, \code{"create"} or \code{"open"}.
#' @param png Should a png file be created and/or immediately opened? Can be either \code{"none"}, \code{"create"} or \code{"open"}.
#' @param svg Should a svg file be created and/or immediately opened? Can be either \code{"none"}, \code{"create"} or \code{"open"}.
#' @param png_dpi Plot resolution of png file. Can be a vector of multiple values so that multiple png files will be created.
#' @param png_from_pdf If \code{TRUE}, the png file is not exported via \code{ggplot2::ggsave(..., device = "png")}, but instead converted/rendered from the pdf created via \code{ggplot2::ggsave(device = "pdf")}. This can in some cases circumvent issues where pdf and png e.g. have different font sizes.
#' @param svg_device If \code{"svg"}, the svg file is not exported via \code{ggplot2::ggsave(..., device = "svg")}, but instead via \code{grDevices::svg()}/\code{grDevices::dev.off()}. This can in some cases circumvent issues with e.g. transparency.
#'
#' @export
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
#' @import ggplot2
#' @import here
#' @import svglite
#' @importFrom ggplotify as.ggplot
#' @importFrom grDevices cairo_pdf dev.off svg
#' @importFrom pdftools pdf_render_page
#' @importFrom png writePNG
#' @importFrom stringr str_c str_replace str_replace_all
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
           png_dpi = 300,
           png_from_pdf = FALSE,
           svg_device = "ggsave") {


    # convert non-ggplot2 objects ---------------------------------------------
    # for a manually curated list of non-ggplot2 plot objects,
    # the object is converted to a ggplot-object to make things work:
    others <- c("trellis") # {desplot}


    if (class(plot_obj)[1] %in% others) {
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
        device = cairo_pdf
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
        if (!png_from_pdf) {
          ggplot2::ggsave(
            filename = png_path_i,
            plot = plot_obj,
            width = width_cm,
            height = height_cm,
            units = "cm",
            scale = 1,
            dpi = dpi_i
          )
        }

        # create png by forcing it to be copy of pdf
        if (png_from_pdf) {
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
      svg_path  <- stringr::str_c(folder_path, "/", file_name, ".svg")

      if (svg_device == "ggsave") {
        ggplot2::ggsave(
          filename = svg_path,
          plot = plot_obj,
          width = width_cm,
          height = height_cm,
          units = "cm",
          scale = 1
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
