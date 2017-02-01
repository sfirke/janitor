#' Zap curly quotes to standard.
#'
#' @param x character string to process
#'
#' @return the processed string
#' @export
#'
#' @examples
#' x <- "it\u2019s"
#' x
#' x == "it's"
#' zap_curly_quotes(x) == "it's"
#' 
zap_curly_quotes <- function(x){
  gsub("[\u2018\u2019\u201A\u201B\u2032\u2035]", "'", x)
}
