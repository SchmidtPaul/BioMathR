#' @title Pour part of a docx file in the resulting docx from an 'R Markdown' document
#'
#' @description This function is a wrapper for \code{officer::block_pour_docx()}. It extends its functionality so that not the entire docx is poured into the RMarkdown file, but only a part of it.
#'
#' @param file external docx file path
#' @param heading1title name of the header (of MSWord style heading 1) that denotes the relevant part.
#'
#' @export
#'
#' @import dplyr
#' @import officer
#' @importFrom knitr knit_print
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' # see https://github.com/davidgohel/officedown/discussions/97#discussioncomment-4839382

partial_block_pour_docx <- function(file, heading1title){

  # import and summary ------------------------------------------------------
  doc <- officer::read_docx(file)
  doc_summ <- officer::docx_summary(doc)
  index_max <- max(doc_summ$doc_index)

  # info on all parts (i.e. sections separated by "heading 1" headers)
  doc_parts_info <- doc_summ %>%
    dplyr::filter(.data$style_name == "heading 1") %>%
    dplyr::transmute(
      text = .data$text,
      index_start = .data$doc_index,
      index_end = lead(.data$doc_index) - 1
    )


  # delete unwanted parts ---------------------------------------------------
  # prepare
  docpart <- doc

  i <- doc_parts_info %>%
    dplyr::filter(.data$text == heading1title) %>%
    tidyr::pivot_longer(-.data$text) %>%
    dplyr::pull(.data$value)

  # delete everything after part
  if (!is.na(i[2])) {
    for (j in (i[2] + 1):index_max) {
      docpart <- docpart %>% officer::cursor_end() %>% officer::body_remove()
    }
  }

  # delete everything before part
  if (i[1]>1) {
    for (j in 1:(i[1]-1)) {
      docpart <- docpart %>% officer::cursor_begin() %>% officer::body_remove()
    }
  }


  # pour --------------------------------------------------------------------
  # create temporary docx file
  print(docpart, target = "tempfileplsdelete.docx")

  # pour it into Rmd
  knitr::knit_print(officer::block_pour_docx("tempfileplsdelete.docx"))

}
