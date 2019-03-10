#' @title Apply stats::chisq.test to a two-way tabyl
#' 
#' @description
#' This generic function overrides stats::chisq.test. If the passed table 
#' is a two-way tabyl, it runs it through janitor::chis.test.tabyl, otherwise
#' it just calls stats::chisq.test.
#' 
#' @return
#' The result is the same as the one of stats::chisqt.test. If `tabyl_results` 
#' is TRUE, the returned tables `observed`, `expected`, `residuals` and `stdres` 
#' are converted to tabyls.
#' 
#' @param dat a two-way tabyl
#' @param tabyl_results if TRUE and dat is a tabyl object, also return `observed`, `expected`, `residuals` and `stdres` as tabyl
#' @param ... other parameters passed to stats::chisq.test or other methods
#'
#' @examples
#' tab <- tabyl(mtcars, gear, cyl)
#' chisq.test(tab)
#' chisq.test(tab)$residuals
#' 
#' @export

chisq.test <- function(dat, tabyl_results = TRUE, ...) {
  UseMethod("chisq.test")
}


#' @rdname chisq.test
#' @method chisq.test default
#' @export

chisq.test.default <- function(dat, tabyl_results = TRUE, ...) {
  
  # keep track of object name to keep `data.name` attribute
  dname <- deparse(substitute(dat))
  
  result <- stats::chisq.test(dat, ...)
  result$data.name <- dname
  
  result
}


#' @rdname chisq.test
#' @method chisq.test tabyl
#' @export

chisq.test.tabyl <- function(dat, tabyl_results = TRUE, ...) {
  
  # keep track of object name to keep `data.name` attribute
  dname <- deparse(substitute(dat))
  
  # check if table is a two-way tabyl
  if (!(inherits(dat, "tabyl") && attr(dat, "tabyl_type") == "two_way")) {
    stop("chisq.test.tabyl() must be applied to a two-way tabyl object")
  }
  
  rownames(dat) <- dat[[1]]
  
  result <- dat %>%
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
    rownames_column <- names(dat)[1]
    var_names <- attr(dat, "var_names")
    
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