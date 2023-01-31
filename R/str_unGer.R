#' @title Replace German special characters in string
#'
#' @description This function replaces Umlaute (Ä, ä, Ö, ö, Ü, ü) and the letter ß in a string.
#'
#' @param string a character string possibly including Ä, ä, Ö, ö, Ü, ü or ß
#'
#' @return a string
#'
#' @export
#' @importFrom stringi stri_trans_general
#'
#' @examples
#' BioMathR::str_unGer("Über grüßen; Öfter größer; Änderung ändern; Ä, ä, Ö, ö, Ü, ü or ß.")

str_unGer <- function (string) {
  stringi::stri_trans_general(string, "de-ASCII; Latin-ASCII")
}

