#' @importFrom magrittr "%>%"
#'
#' @title Generate a frequency table of a factor grouped into top-n, bottom-n, and all other levels.
#'
#' @description
#' Get a frequency table of a factor variable, grouped into categories by level.
#'
#' @param input_vec the factor variable to tabulate.
#' @param num_lvls number of levels to include in top and bottom groups
#' @param show_na should cases where the variable is NA be shown?
#' @param sort should the resulting table be sorted in descending order?
#' @return Returns a data.frame (actually a \code{tbl_df}) with the frequencies of the grouped, tabulated variable.  Includes counts and percentages, and valid percentages (calculated omitting \code{NA} values, if present in the vector and \code{show_na = TRUE}.)
#' @export
#' @examples
#' top_levels(as.factor(mtcars$hp), 2)

# todo: test for when there are ~5 classes to factor but only 3 are present in vector - does it work?
# todo: generate warning if num_lvls = 3, factor has only 4 levels - groups will sum to >100%

top_levels <- function(input_vec, num_lvls = 2, show_na = FALSE, sort = FALSE){
  # handle bad inputs
  if(!is.factor(input_vec)){stop("factor_vec is not of type 'factor'")}
  num_levels_in_var <- max(as.numeric(input_vec), na.rm = TRUE)
  if(!num_levels_in_var > 2){stop("input factor variable must have at least 3 levels")}
  if(num_lvls > num_levels_in_var ){stop("num_lvls cannot exceed the count of levels in the input factor variable")}
  
  top_n_lvls <- paste(levels(input_vec)[1:num_lvls], collapse = ", ")
  bot_n_lvls <- paste(levels(input_vec)[max(3, num_lvls-1):num_lvls], collapse = ", ")
  
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


