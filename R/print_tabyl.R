#' @export

print.tabyl <- function(x, ...) {
  print.data.frame(x, row.names = FALSE)
}
