#' @export

print.tabyl <- function(x, ...) {
  print.data.frame(x, row.names = FALSE)
}

#' @export

print.tabyl_3way <- function(x, ...) {
  attr(x, "var_names") <- NULL
  attr(x, "tabyl_type") <- NULL
  class(x) <- c("list") # this strips out the attributes I wanted in the first place?
  # but if class is still tabyl_3way this becomes an infinite loop
  print(x)
}
