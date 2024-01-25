#' @section Package context:
#' Advanced users can do most things covered here, but they can do it
#' faster with janitor and save their thinking for more fun tasks.
#' @keywords internal
"_PACKAGE"
## quiets concerns of R CMD check re: the .'s that appear in pipelines
## and the "n" that is produced by dplyr::count() in a pipeline
if (getRversion() >= "2.15.1") utils::globalVariables(c(".", "n"))
