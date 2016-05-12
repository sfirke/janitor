#' @importFrom magrittr "%>%"
#'
#' @title Generate a frequency table of a factor grouped by top-2, bottom-2, and all other levels.
#'
#' @description
#' Get a frequency table of a factor variable, grouped into level categories.
#'
#' @param input_vec the factor variable to tabulate.
#' @param show_na should cases where the variable is NA be shown?
#' @param sort should the resulting table be sorted in descending order?
#' @return Returns a data.frame (actually a \code{tbl_df}) with the frequencies of the grouped, tabulated variable.  Includes counts and percentages, and valid percentages (calculated omitting \code{NA} values, if present in the vector and \code{show_na = TRUE}.)
#' @export
#' @examples
#' top_2(as.factor(mtcars$hp))

# todo: test for when there are ~5 classes to factor but only 3 are present in vector - does it work?
top_2 <- function(input_vec, show_na = FALSE, sort = FALSE){
  if(!is.factor(input_vec)){stop("factor_vec is not of type 'factor'")}
  num_levels <- max(as.numeric(input_vec), na.rm = TRUE)
  if(!num_levels > 2){stop("input factor variable must have at least 3 levels")}
  top_2_lvls <- paste(levels(input_vec)[1:2], collapse = ", ")
  bot_2_lvls <- paste(levels(input_vec)[max(3, num_levels-1):num_levels], collapse = ", ")
  if(num_levels > 4){ mid_lvls <- paste(levels(input_vec)[3:(num_levels-2)], collapse = ", ")}

  new_vec <- ifelse(is.na(input_vec), NA,
                    ifelse(as.numeric(input_vec) %in% c(1,2), top_2_lvls,
                           ifelse(as.numeric(input_vec) %in% max(3, num_levels-1):num_levels, bot_2_lvls,
                                  mid_lvls)))
  if(num_levels > 4){new_vec <- ordered(new_vec, levels = c(top_2_lvls, mid_lvls, bot_2_lvls))
  } else{
    new_vec <- as.ordered(new_vec, levels = c(top_2_lvls, bot_2_lvls))
  }

  cat(paste0("\nValues used for top-2: ", paste(levels(input_vec)[1:2], collapse = ", ")), "\n\n")
  tabyl(new_vec, show_na = show_na, sort = sort) # %>%
    # dplyr::arrange(desc(new_vec)) # to make the ordering of the result reverse alphabetical and get top-2, other, bot-2
}


