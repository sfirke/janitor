#' @title Apply stats::chisq.test to a two-way tabyl
#' 
#' @description
#' This generic function overrides stats::chisq.test. If the passed table 
#' is a two-way tabyl, it runs it through janitor::chisq.test.tabyl, otherwise
#' it just calls stats::chisq.test.
#' 
#' @return
#' The result is the same as the one of stats::chisq.test. If `tabyl_results` 
#' is TRUE, the returned tables `observed`, `expected`, `residuals` and `stdres` 
#' are converted to tabyls.
#' 
#' @param x a two-way tabyl, a numeric vector or a factor
#' @param ... other parameters passed to stats::chisq.test
#'
#' @examples
#' tab <- tabyl(mtcars, gear, cyl)
#' chisq.test(tab)
#' chisq.test(tab)$residuals
#' 
#' @export

chisq.test <- function(x, ...) {
  UseMethod("chisq.test")
}


#' @rdname chisq.test
#' @method chisq.test default
#' @param y if x is a vector, must be another vector or factor of the same length
#' @export

chisq.test.default <- function(x, y = NULL, ...) {
  
  # keep track of object names to keep `data.name` attribute
  if (!is.null(y)) {
    dname_x <- deparse(substitute(x))
    dname_y <- deparse(substitute(y))
    dname <- paste(dname_x, "and", dname_y)
  } else {
    dname <- deparse(substitute(x))
  }
  
  result <- stats::chisq.test(x, y, ...)
  
  # Replace object name in result for strict equality with stats::chisq.test
  result$data.name <- dname
  if (!is.null(y)) {
    names(attr(result$observed, "dimnames")) <- c(dname_x, dname_y)
    names(attr(result$expected, "dimnames")) <- c(dname_x, dname_y)
    names(attr(result$residuals, "dimnames")) <- c(dname_x, dname_y)
    names(attr(result$stdres, "dimnames")) <- c(dname_x, dname_y)
  }
  
  result
}


#' @rdname chisq.test
#' @method chisq.test tabyl
#' @param tabyl_results if TRUE and x is a tabyl object, also return `observed`, `expected`, `residuals` and `stdres` as tabyl
#' @export

chisq.test.tabyl <- function(x, tabyl_results = TRUE, ...) {
  
  # keep track of object name to keep `data.name` attribute
  dname <- deparse(substitute(x))
  
  # check if table is a two-way tabyl
  if (!(inherits(x, "tabyl") && attr(x, "tabyl_type") == "two_way")) {
    stop("chisq.test.tabyl() must be applied to a two-way tabyl object")
  }
  
  # check for and remove totals row / column, if present
  if(!is.null(attr(x, "totals"))){
    if("row" %in% attr(x, "totals")){
      x <- x[-nrow(x), ]
    }
    if("col" %in% attr(x, "totals")){
      # this causes the var_names attribute to become NULL, not sure why
      x[ncol(x)] <- NULL
    }
    warning("janitor::chisq.test.tabyl() detected a totals row and/or column.  The totals were removed from the tabyl before the test was run.
            If you intend to include the totals row and/or column in the test, first call untabyl() on the data.frame, then proceed from there.")
  }
  
  rownames(x) <- x[[1]]
  
  result <- x %>%
    dplyr::select(-1) %>%
    as.matrix() %>% 
    as.table() %>% 
    stats::chisq.test(...)
  
  # Replace values and attributes for strict object equality
  result$data.name <- dname
  names(attr(result$observed, "dimnames")) <- c("", "")
  names(attr(result$expected, "dimnames")) <- c("", "")
  names(attr(result$residuals, "dimnames")) <- c("", "")
  names(attr(result$stdres, "dimnames")) <- c("", "")
  
  # Return results tables as tabyl
  if (tabyl_results) {
    
    # Keep track of row names column name and var_names attributes
    rownames_column <- names(x)[1]
    var_names <- attr(x, "var_names")
    
    # For each returned table, convert it to a two-way tabyl
    tables <- c("observed", "expected", "residuals", "stdres")
    for (table in tables) {
      tab <- result[[table]]
      ttab <- as.data.frame.matrix(tab)
      ttab[[rownames_column]] <- rownames(tab)
      ttab <- ttab %>% dplyr::select(!!rownames_column, dplyr::everything())
      ttab <- as_tabyl(ttab)
      attr(ttab, "var_names") <- var_names
      result[[table]] <- ttab
    }
  }
  
  result
}



#' @title Apply stats::fisher.test to a two-way tabyl
#' 
#' @description
#' This generic function overrides stats::fisher.test. If the passed table 
#' is a two-way tabyl, it runs it through janitor::fisher.test.tabyl, otherwise
#' it just calls stats::fisher.test.
#' 
#' @return
#' The result is the same as the one of stats::fisher.test.
#' 
#' @param x a two-way tabyl, a numeric vector or a factor
#' @param ... other parameters passed to stats::fisher.test
#'
#' @examples
#' tab <- tabyl(mtcars, gear, cyl)
#' fisher.test(tab)
#' 
#' @export

fisher.test <- function(x, ...) {
  UseMethod("fisher.test")
}


#' @rdname fisher.test
#' @method fisher.test default
#' @param y if x is a vector, must be another vector or factor of the same length
#' @export

fisher.test.default <- function(x, y = NULL, ...) {
  
  # keep track of object names to keep `data.name` attribute
  if (!is.null(y)) {
    dname_x <- deparse(substitute(x))
    dname_y <- deparse(substitute(y))
    dname <- paste(dname_x, "and", dname_y)
  } else {
    dname <- deparse(substitute(x))
  }

  result <- stats::fisher.test(x, y, ...)
  result$data.name <- dname
  
  result
}


#' @rdname fisher.test
#' @method fisher.test tabyl
#' @export

fisher.test.tabyl <- function(x, ...) {
  
  # keep track of object name to keep `data.name` attribute
  dname <- deparse(substitute(x))

  # check if table is a two-way tabyl
  if (!(inherits(x, "tabyl") && attr(x, "tabyl_type") == "two_way")) {
    stop("fisher.test.tabyl() must be applied to a two-way tabyl object")
  }
  
  # check for and remove totals row / column, if present
  if(!is.null(attr(x, "totals"))){
    if("row" %in% attr(x, "totals")){
      x <- x[-nrow(x), ]
    }
    if("col" %in% attr(x, "totals")){
      x[ncol(x)] <- NULL
    }
    warning("janitor::fisher.test.tabyl() detected a totals row and/or column.  The totals were removed from the tabyl before the test was run.
            If you intend to include the totals row and/or column in the test, first call untabyl() on the data.frame, then proceed from there.")
  }
  
  rownames(x) <- x[[1]]
  
  result <- x %>%
    dplyr::select(-1) %>%
    as.matrix() %>% 
    as.table() %>% 
    stats::fisher.test(...)
  
  # Replace values and attributes for strict object equality
  result$data.name <- dname

  result
}


