
#' Kill the joy of NYT spelling bee
#'
#' Removes the enjoyment of the NYT spelling bee app by giving you the answers
#'
#' @importFrom stringr str_split
#' @importFrom  stringr str_length
#' @import words
#' @importFrom  glue glue_collapse
#'
#' @param key Middle letter
#' @param circle Other letters around circle as a string
#' @return vector of words from scrabble dictionary that fit requirements
#' @export
#'
#' @examples library(IhateNYTSpellBee)
#'   kill_the_bee("f", "ailmce")
kill_the_bee <- function(key, circle) {
  cir <- stringr::str_split(circle, pattern = "")[[1]]
  l <- letters[!letters %in% key & !letters %in% cir]
  w <- words::words$word
  w <- w[stringr::str_length(w)>3]
  w <- w[grepl(key,w)]
  w <- w[!grepl(glue::glue_collapse(l, sep = "|"), w)]
w
}
