#' janitor
#'
#' janitor has simple little tools for examining and cleaning dirty data.
#'
#' @section Main functions:
#' The main janitor functions can: perfectly format data.frame
#' column names; provide quick counts of variable combinations (i.e.,
#' frequency tables and crosstabs); and explore duplicate records. Other
#' janitor functions nicely format the tabulation results. These
#' tabulate-and-report functions approximate popular features of SPSS and
#' Microsoft Excel.
#'
#' @section Package context:
#' This package follows the principles of the "tidyverse" and works
#' well with the pipe function `%>%`.
#'
#' janitor was built with beginning-to-intermediate R users in mind
#' and is optimized for user-friendliness.  Advanced users can do most
#' things covered here, but they can do it faster with janitor and save
#' their thinking for more fun tasks.
#'
"_PACKAGE"
## quiets concerns of R CMD check re: the .'s that appear in pipelines
## and the "n" that is produced by dplyr::count() in a pipeline
if (getRversion() >= "2.15.1") utils::globalVariables(c(".", "n"))
